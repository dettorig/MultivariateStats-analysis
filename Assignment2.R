
######## Task 1########

dataset <- load ("C:/Users/dettorig/Downloads/task2.Rdata")

# Center the variables
centered_data <- scale(train.data, center = TRUE, scale = FALSE)

# Perform PCA
pca_result <- prcomp(centered_data)

# Calculate cumulative variance explained by each component
cumulative_var <- cumsum(pca_result$sdev^2) / sum(pca_result$sdev^2)

# Scenario 1: Components that account for 80% of the variance
num_components_80 <- which(cumulative_var >= 0.8)[1]

# Scenario 2: Components that account for 90% of the variance
num_components_90 <- which(cumulative_var >= 0.9)[1]

# Output the selected number of components
cat("Number of components for 80% variance:", num_components_80, "\n")
cat("Number of components for 90% variance:", num_components_90, "\n")

#install.packages(c("caret", "MASS", "class", "randomForest", "hdme"))

# Load required libraries
library(caret)
library(MASS)
library(class)
library(randomForest)
library(hdme)
library(HDclassif)


# PCA for scenarios
train_pca_scenario1 <- predict(pca_result, train.data)[, 1:num_components_80]
train_pca_scenario2 <- predict(pca_result, train.data)[, 1:num_components_90]

# Error rate function
error_rate <- function(true_labels, predictions) {
  return(mean(true_labels != predictions))
}

# Training error function
training_error <- function(model_func, data, labels, ...) {
  model <- model_func(data, labels, ...)
  predictions <- predict(model, data)$class
  return(error_rate(labels, predictions))
}

# Leave-One-Out Cross-Validation (LOOCV)
loocv_error <- function(model_func, data, labels, ...) {
  errors <- sapply(1:nrow(data), function(i) {
    train_data <- data[-i, , drop = FALSE]
    test_data <- data[i, , drop = FALSE]
    train_labels <- labels[-i]
    test_label <- labels[i]
    model <- model_func(train_data, train_labels, ...)
    prediction <- predict(model, test_data)$class
    return(as.integer(prediction != test_label))
  })
  mean(errors)
}

# LDA LOOCV and training error for both scenarios
train.comp.1 = centered_data %*% pca_result$rotation[, 1:num_components_80]
#compute lda on components
ldacomp.out.1 = lda(train.comp.1, train.target)
print(ldacomp.out.1)

#hit rate training set
predlda.train.1 = predict(ldacomp.out.1, train.comp.1)
tab = table(train.target, predlda.train.1$class)
lda_training_error_scenario1 = 1 - sum(diag(tab))/sum(tab)

#hit rate test set
ldacomp.out.loocv.1 = lda(train.comp.1, train.target, CV = TRUE)
print(ldacomp.out.loocv.1)
tab = table(train.target, ldacomp.out.loocv.1$class)
lda_loocv_error_scenario1 = 1 - sum(diag(tab))/sum(tab)



train.comp.2 = centered_data %*% pca_result$rotation[, 1:num_components_90]
#compute lda on components
ldacomp.out.2 = lda(train.comp.2, train.target)
print(ldacomp.out.2)

#hit rate training set
predlda.train.2 = predict(ldacomp.out.2, train.comp.2)
tab = table(train.target, predlda.train.2$class)
lda_training_error_scenario2 = 1 - sum(diag(tab))/sum(tab)

#hit rate test set
ldacomp.out.loocv.2 = lda(train.comp.2, train.target, CV = TRUE)
print(ldacomp.out.loocv.2)
tab = table(train.target, ldacomp.out.loocv.2$class)
lda_loocv_error_scenario2 = 1 - sum(diag(tab))/sum(tab)

cat("LDA LOOCV error (Scenario 1):", lda_loocv_error_scenario1, "\n")
cat("LDA Training error (Scenario 1):", lda_training_error_scenario1, "\n")
cat("LDA LOOCV error (Scenario 2):", lda_loocv_error_scenario2, "\n")
cat("LDA Training error (Scenario 2):", lda_training_error_scenario2, "\n")

#create training data with class-specific principal components

# Compute class-specific principal components for Scenario 1
ntrain <- nrow(train.data)
train_pca_scenario1_qda <- matrix(0, nrow = ntrain, ncol = num_components_80)

for (i in unique(train.target)) {
  prcomp_out <- prcomp(train.data[train.target == i, ], center = TRUE, scale. = FALSE)
  train_pca_scenario1_qda[train.target == i, ] <- train.data[train.target == i, ] %*% prcomp_out$rotation[, 1:num_components_80]
}

# Compute class-specific principal components for Scenario 2
train_pca_scenario2_qda <- matrix(0, nrow = ntrain, ncol = num_components_90)

for (i in unique(train.target)) {
  prcomp_out <- prcomp(train.data[train.target == i, ], center = TRUE, scale. = FALSE)
  train_pca_scenario2_qda[train.target == i, ] <- train.data[train.target == i, ] %*% prcomp_out$rotation[, 1:num_components_90]
}

# QDA LOOCV and training error for both scenarios
qda_loocv_error_scenario1 <- loocv_error(function(x, y) qda(x, grouping = y), train_pca_scenario1_qda, train.target)
qda_loocv_error_scenario2 <- loocv_error(function(x, y) qda(x, grouping = y), train_pca_scenario2_qda, train.target)
qda_training_error_scenario1 <- training_error(function(x, y) qda(x, grouping = y), train_pca_scenario1_qda, train.target)
qda_training_error_scenario2 <- training_error(function(x, y) qda(x, grouping = y), train_pca_scenario2_qda, train.target)

cat("QDA LOOCV error (Scenario 1):", qda_loocv_error_scenario1, "\n")
cat("QDA Training error (Scenario 1):", qda_training_error_scenario1, "\n")
cat("QDA LOOCV error (Scenario 2):", qda_loocv_error_scenario2, "\n")
cat("QDA Training error (Scenario 2):", qda_training_error_scenario2, "\n")

# KNN LOOCV with tuning for both scenarios
knn_tuning <- function(data, labels, k_max) {
  errors <- sapply(1:k_max, function(k) {
    errors_for_k <- sapply(1:nrow(data), function(i) {
      train_data <- data[-i, , drop = FALSE]
      test_data <- data[i, , drop = FALSE]
      train_labels <- labels[-i]
      test_label <- labels[i]
      pred <- knn(train_data, test_data, train_labels, k = k)  # KNN prediction
      return(as.integer(pred != test_label))  # Compare predicted label with true label
    })
    return(mean(errors_for_k))  # Return the mean error for each k
  })
  return(errors)
}

# Tuning KNN for Scenario 1
k_max <- 30
knn_errors_scenario1 <- knn_tuning(train_pca_scenario1, train.target, k_max)
k_min_scenario1 <- which.min(knn_errors_scenario1)
cat("Optimal k for Scenario 1:", k_min_scenario1, "\n")

# Tuning KNN for Scenario 2
knn_errors_scenario2 <- knn_tuning(train_pca_scenario2, train.target, k_max)
k_min_scenario2 <- which.min(knn_errors_scenario2)
cat("Optimal k for Scenario 2:", k_min_scenario2, "\n")

# Function to calculate KNN training error
knn_training_error <- function(data, labels, k) {
  predictions <- knn(data, data, labels, k = k)  # KNN prediction on training data
  return(error_rate(labels, predictions))       # Calculate the training error
}

# Calculate KNN training error for Scenario 1
knn_training_error_scenario1 <- knn_training_error(train_pca_scenario1, train.target, k_min_scenario1)
cat("KNN Training error (Scenario 1):", knn_training_error_scenario1, "\n")

# Calculate KNN training error for Scenario 2
knn_training_error_scenario2 <- knn_training_error(train_pca_scenario2, train.target, k_min_scenario2)
cat("KNN Training error (Scenario 2):", knn_training_error_scenario2, "\n")


# Bagging model function
bagging_model <- function(data, labels, ntree, mtry) {
  labels <- as.factor(labels)  # Ensure labels are factors for classification
  model <- randomForest(data, labels, ntree = ntree, mtry = mtry, importance = TRUE)
  return(model)
}

training_error <- function(model_func, data, labels) {
  labels <- as.factor(labels)  # Ensure labels are factors for classification
  model <- model_func(data, labels)  # Train the model
  predictions <- predict(model, data)  # Predict using the model
  return(mean(predictions != labels))  # Calculate and return the error rate
}


# Tuning bagging (mtry and ntree)
bagging_tuning <- function(data, labels, ntree_values, mtry_values) {
  results <- expand.grid(ntree = ntree_values, mtry = mtry_values)
  results$OOB_error <- NA
  
  for (i in 1:nrow(results)) {
    labels <- as.factor(labels)  # Ensure labels are factors
    model <- randomForest(data, labels, ntree = results$ntree[i], mtry = results$mtry[i])
    results$OOB_error[i] <- tail(model$err.rate[, "OOB"], 1)  # Extract final OOB error
  }
  
  return(results)
}

# Scenario 1: Bagging
bagging_training_error_scenario1 <- training_error(
  function(x, y) randomForest(x, as.factor(y), ntree = 500, mtry = ncol(x)),
  train_pca_scenario1, train.target
)

# Scenario 2: Bagging
bagging_training_error_scenario2 <- training_error(
  function(x, y) randomForest(x, as.factor(y), ntree = 500, mtry = ncol(x)),
  train_pca_scenario2, train.target
)

# Train models for OOB error evaluation
bagging_scenario1 <- randomForest(train_pca_scenario1, as.factor(train.target), ntree = 500, mtry = ncol(train_pca_scenario1))
bagging_scenario2 <- randomForest(train_pca_scenario2, as.factor(train.target), ntree = 500, mtry = ncol(train_pca_scenario2))

# Display results
cat("Bagging OOB error (Scenario 1):", tail(bagging_scenario1$err.rate[, "OOB"], 1), "\n")
cat("Bagging Training error (Scenario 1):", bagging_training_error_scenario1, "\n")
cat("Bagging OOB error (Scenario 2):", tail(bagging_scenario2$err.rate[, "OOB"], 1), "\n")
cat("Bagging Training error (Scenario 2):", bagging_training_error_scenario2, "\n")


# Define tuning grid
ntree_values <- seq(100, 1000, by = 100)
mtry_values <- seq(1, ncol(train_pca_scenario1), by = 1)

# Perform Bagging tuning
bagging_oob_errors_scenario1 <- bagging_tuning(train_pca_scenario1, train.target, ntree_values, mtry_values)
bagging_oob_errors_scenario2 <- bagging_tuning(train_pca_scenario2, train.target, ntree_values, mtry_values)

# Find the optimal ntree and corresponding OOB error for Scenario 1
optimal_scenario1 <- bagging_oob_errors_scenario1[which.min(bagging_oob_errors_scenario1$OOB_error), ]
optimal_ntree_bagging_scenario1 <- optimal_scenario1$ntree
optimal_mtry_bagging_scenario1 <- optimal_scenario1$mtry
optimal_oob_error_scenario1 <- optimal_scenario1$OOB_error

# Find the optimal ntree and corresponding OOB error for Scenario 2
optimal_scenario2 <- bagging_oob_errors_scenario2[which.min(bagging_oob_errors_scenario2$OOB_error), ]
optimal_ntree_bagging_scenario2 <- optimal_scenario2$ntree
optimal_mtry_bagging_scenario2 <- optimal_scenario2$mtry
optimal_oob_error_scenario2 <- optimal_scenario2$OOB_error

# Print results
cat("Optimal number of trees for Bagging (Scenario 1):", optimal_ntree_bagging_scenario1, 
    "with mtry:", optimal_mtry_bagging_scenario1, 
    "and OOB error:", optimal_oob_error_scenario1, "\n")
cat("Optimal number of trees for Bagging (Scenario 2):", optimal_ntree_bagging_scenario2, 
    "with mtry:", optimal_mtry_bagging_scenario2, 
    "and OOB error:", optimal_oob_error_scenario2, "\n")

# Random Forests for both scenarios
rf_training_error_scenario1 <- training_error(function(x, y) randomForest(x, y, ntree = 500), train_pca_scenario1, train.target)
rf_training_error_scenario2 <- training_error(function(x, y) randomForest(x, y, ntree = 500), train_pca_scenario2, train.target)

cat("Random Forest OOB error (Scenario 1):", rf_model_scenario1$err.rate[500, "OOB"], "\n")
cat("Random Forest Training error (Scenario 1):", rf_training_error_scenario1, "\n")
cat("Random Forest OOB error (Scenario 2):", rf_model_scenario2$err.rate[500, "OOB"], "\n")
cat("Random Forest Training error (Scenario 2):", rf_training_error_scenario2, "\n")

# Tuning Random Forest for both scenarios
rf_tuning <- function(data, labels, ntree_values) {
  oob_errors <- sapply(ntree_values, function(ntree) {
    model <- randomForest(data, as.factor(labels), ntree = ntree)
    return(model$err.rate[ntree, "OOB"])
  })
  return(oob_errors)
}

ntree_values <- seq(100, 1000, by = 100)
rf_oob_errors_scenario1 <- rf_tuning(train_pca_scenario1, train.target, ntree_values)
rf_oob_errors_scenario2 <- rf_tuning(train_pca_scenario2, train.target, ntree_values)

optimal_ntree_scenario1 <- ntree_values[which.min(rf_oob_errors_scenario1)]
optimal_ntree_scenario2 <- ntree_values[which.min(rf_oob_errors_scenario2)]

cat("Optimal number of trees for Random Forest (Scenario 1):", optimal_ntree_scenario1, "with OOB error:", min(rf_oob_errors_scenario1), "\n")
cat("Optimal number of trees for Random Forest (Scenario 2):", optimal_ntree_scenario2, "with OOB error:", min(rf_oob_errors_scenario2), "\n")


# Find the optimal k for Scenario 1 and Scenario 2
optimal_k_scenario1 <- which.min(knn_errors_scenario1)  # k with the minimum error in Scenario 1
optimal_k_scenario2 <- which.min(knn_errors_scenario2)  # k with the minimum error in Scenario 2

# Print the optimal k values
cat("Optimal k for Scenario 1:", optimal_k_scenario1, "\n")
cat("Optimal k for Scenario 2:", optimal_k_scenario2, "\n")

# Visualize errors
error_data <- data.frame(
  Classifier = c("LDA", "QDA", "KNN", "Bagging", "RandomForest"),
  Scenario1_Training_Error = c(lda_training_error_scenario1, qda_training_error_scenario1, knn_training_error_scenario1, bagging_training_error_scenario1, rf_training_error_scenario1),
  Scenario1_Test_Error = c(lda_loocv_error_scenario1, qda_loocv_error_scenario1, knn_errors_scenario1[optimal_k_scenario1], optimal_oob_error_scenario1, min(rf_oob_errors_scenario1)),
  Scenario2_Training_Error = c(lda_training_error_scenario2, qda_training_error_scenario2, knn_training_error_scenario2, bagging_training_error_scenario2, rf_training_error_scenario2),
  Scenario2_Test_Error = c(lda_loocv_error_scenario2, qda_loocv_error_scenario2, knn_errors_scenario2[optimal_k_scenario2], optimal_oob_error_scenario2, min(rf_oob_errors_scenario2))
)

library(ggplot2)
library(reshape2)
error_data_melted <- melt(error_data, id.vars = "Classifier", variable.name = "Scenario", value.name = "ErrorValue")

ggplot(error_data_melted, aes(x = Classifier, y = ErrorValue, fill = Scenario)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.7, width = 0.7) +
  labs(title = "Comparison of Errors by Classifier and Scenario", y = "Error Rate", x = "Classifier") +
  theme_minimal()

print(error_data_melted)


########Task 2########

######TASK 2 - a.

library(heplots)
library(paran)
library(car)

#import data
load("C:/Users/dettorig/Downloads/task2.Rdata")

#center the variables of train.data
train.data.centered<-scale(train.data, center = TRUE, scale = FALSE)

#conduct PCA on centered variables
pca.centered<-prcomp(train.data.centered, center = FALSE, scale. = FALSE)
screeplot(pca.centered,type="lines")

#extract eigenvalues and use them to calculate the total amount of variance in the data
eigenvalues<-pca.centered$sdev^2
total_variance<-sum(eigenvalues)

#calculate proportion of variance explained by each component
proportion_variance<-eigenvalues/total_variance
print(proportion_variance)

#calculate cumulative proportion of variance explained by components
cumulative_variance<-cumsum(proportion_variance)
print(cumulative_variance)

#find the number of components that account for 90% of the data
nr_components<-which(cumulative_variance>=0.90)[1]
print(nr_components)

#select the components that account for 90% of the data
selected.components <- pca.centered$x[, 1:nr_components]


######Task 2 - b.

library("ggdendro")
require("graphics")
library("FNN")
library(fBasics)
library(mclust)

#Hierarchical clustering#

#compute euclidean distance
dist.components<-dist(selected.components, method = "euclidean", diag = TRUE, upper = TRUE)

#hierarchical clustering on squared Euclidean distances using the method of Ward
hiclust_ward <- hclust(dist.components, method = "ward.D2")

#plot the dendrogram
par(pty="s")
plot(hiclust_ward,labels=train.target,hang=-1,main="Ward's method")

#extract 3 clusters
clustvar<-cutree(hiclust_ward, k=3)
print(clustvar)

#K-means clustering#

#set number of clusters to 3
nclust<-3

#perform k-means clustering
kmean3<-kmeans(selected.components,3,nstart=50,iter.max=200)
print(kmean3)

#visualize clusters 
plot(selected.components,col=kmean3$cluster,pch=1,main="K-means clustering results",xlab="PC1",ylab="PC2")
legend("topright",legend=unique(kmean3$cluster),col=unique(kmean3$cluster),pch=19)

#Common dimension hddc() models#

# HDDC with hierarchical clustering starting solution, threshold = 0.2
hddc_hierarchical_02 <- hddc(selected.components, model = "AkjBkQkD", threshold = 0.2, init = "vector", init.vector = clustvar, K = 3)

# HDDC with hierarchical clustering starting solution, threshold = 0.05
hddc_hierarchical_005 <- hddc(selected.components, model = "AkjBkQkD", threshold = 0.05, init = "vector", init.vector = clustvar, K = 3)

# HDDC with k-means starting solution, threshold = 0.2
hddc_kmeans_02 <- hddc(selected.components, model = "AkjBkQkD", threshold = 0.2, init = "kmeans", K = 3)

# HDDC with k-means starting solution, threshold = 0.05
hddc_kmeans_005 <- hddc(selected.components, model = "AkjBkQkD", threshold = 0.05, init = "kmeans", K = 3)

#extract clusters
hddc_hc_clusters_02 <- hddc_hierarchical_02$class
hddc_hc_clusters_005 <- hddc_hierarchical_005$class
hddc_km_clusters_02 <- hddc_kmeans_02$class
hddc_km_clusters_005 <- hddc_kmeans_005$class

# Compute ARIs
ari_hddc_hc_02 <- adjustedRandIndex(hddc_hc_clusters_02, train.target)
ari_hddc_hc_005 <- adjustedRandIndex(hddc_hc_clusters_005, train.target)
ari_hddc_km_02 <- adjustedRandIndex(hddc_km_clusters_02, train.target)
ari_hddc_km_005 <- adjustedRandIndex(hddc_km_clusters_005, train.target)

print(c(ari_hddc_hc_02, ari_hddc_hc_005, ari_hddc_km_02, ari_hddc_km_005))


######Task 2 - c.

library(patchwork)

#define best clusters
best_clusters_02 <- hddc_hc_clusters_02
best_clusters_005 <- hddc_hc_clusters_005

# Observed data - extract the first two principal components
pc_data <- as.data.frame(selected.components[, 1:2])
colnames(pc_data) <- c("PC1", "PC2")

# Add observed and predicted class labels
pc_data$Observed <- train.target  
pc_data$Predicted_02 <- best_clusters_02
pc_data$Predicted_005 <- best_clusters_005

# Plot observed class labels
plot_observed <- ggplot(pc_data, aes(x = PC1, y = PC2, color = as.factor(Observed))) +
  geom_point(size = 3) +
  labs(
    title = "Observed Class Labels",
    x = "Principal Component 1",
    y = "Principal Component 2",
    color = "Class"
  ) +
  theme_minimal()

# Plot predicted class labels for threshold = 0.2
plot_predicted_02 <- ggplot(pc_data, aes(x = PC1, y = PC2, color = as.factor(Predicted_02))) +
  geom_point(size = 3) +
  labs(
    title = "Predicted Class Labels (Threshold = 0.2)",
    x = "Principal Component 1",
    y = "Principal Component 2",
    color = "Cluster"
  ) +
  theme_minimal()

# Plot predicted class labels for threshold = 0.05
plot_predicted_005 <- ggplot(pc_data, aes(x = PC1, y = PC2, color = as.factor(Predicted_005))) +
  geom_point(size = 3) +
  labs(
    title = "Predicted Class Labels (Threshold = 0.05)",
    x = "Principal Component 1",
    y = "Principal Component 2",
    color = "Cluster"
  ) +
  theme_minimal()

# Combine plots for comparison
(plot_observed | plot_predicted_02) / plot_predicted_005


########Task 3########


# Load necessary libraries 
install.packages("ca")
library(ca)           
install.packages("FactoMineR")
install.packages("factoextra")
library(FactoMineR)   
library(factoextra)   

# Load the dataset
load("task3.Rdata")

# Explore the dataset
cat("Dataset structure:\n")
str(important)
cat("\nFirst few rows of data:\n")
print(head(important))

#Chi-square test
chisq.test(important)

# Perform Correspondence Analysis
ca_results <- ca(important, graph = FALSE)  
cat("\nSummary of Correspondence Analysis Results:\n")
print(summary(ca_results))


# Base CA biplot visualization
plot(ca_results,main = "Correspondence Analysis Biplot",labels = c("rows", "columns"))

# visualization with FactoMineR and factoextra
fviz_ca_biplot(ca_results,repel = TRUE,arrows = c(FALSE, TRUE), col.row = "blue",col.col = "darkred", label = "col", title = "Correspondence Analysis")+labs(x = "Dimension 1 (73.1%)", y = "Dimension 2 (15.7%)")


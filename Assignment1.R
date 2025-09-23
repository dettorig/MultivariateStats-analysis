install.packages("dplyr")
install.packages("lavaan")
library(dplyr)
library(lavaan)
library(psych)
##########TASK 1 a##########
load("/Users/dettorig/Desktop/Multivariate Statistics/ess.Rdata")
head(ess)
ess_centered <- ess %>% 
  mutate(across(c(2:13), ~ scale(., center = TRUE, scale = FALSE)))
head(ess_centered)


#check if mean is 0
colMeans(ess_centered[,2:13])


#CFA model
cfa_model <- '
  attitude_refugees =~ NA*refugee1 + refugee2 + refugee3
  attitude_immigration =~ NA*immigration1 + immigration2 + immigration3
  religiousness =~ NA*religion1 + religion2 + religion3
  social_trust =~ NA*socialtrust1 + socialtrust2 + socialtrust3


  attitude_refugees  ~~ 1*attitude_refugees
  attitude_immigration  ~~ 1*attitude_immigration
  religiousness  ~~ 1*religiousness
  social_trust  ~~ 1*social_trust
  attitude_refugees ~~ attitude_immigration
  attitude_refugees ~~ religiousness
  attitude_refugees ~~ social_trust
  attitude_immigration ~~ religiousness
  attitude_immigration ~~ social_trust
  religiousness ~~ social_trust
'

cov_matrix <- cov(ess[,-1])

fit_cfa <- cfa(cfa_model, sample.cov = cov_matrix, sample.nobs = 3357)
summary(fit_cfa, fit.measures = TRUE, standardized = TRUE)
loadings_std <- standardizedSolution(fit_cfa)


loading_factor1 <- loadings_std[loadings_std$lhs == "attitude_refugees", "est.std"]
residuals_factor1 <- 1-loading_factor1^2
loading_factor2 <- loadings_std[loadings_std$lhs == "attitude_immigration", "est.std"]
residuals_factor2 <- 1-loading_factor2^2
loading_factor3 <- loadings_std[loadings_std$lhs == "religiousness", "est.std"]
residuals_factor3 <- 1-loading_factor3^2
loading_factor4 <- loadings_std[loadings_std$lhs == "social_trust", "est.std"]
residuals_factor4 <- 1-loading_factor4^2

loadings <- inspect(fit_cfa, "std")$lambda
squared_loadings <- loadings^2
residuals <- 1-squared_loadings

# Calculate AVE

AVE <- function(loadings, residuals) {
  sum(loadings^2) / (sum(loadings^2) + sum(residuals))
  
}

ave_value1 <- AVE(loading_factor1, residuals_factor1)
ave_value2 <- AVE(loading_factor2, residuals_factor2)
ave_value3 <- AVE(loading_factor3, residuals_factor3)
ave_value4 <- AVE(loading_factor4, residuals_factor4)

ave_value1
ave_value2
ave_value3
ave_value4

# Calculate Composite Reliability (CR)
total_squared_loadings <- colSums(squared_loadings)
total_error_variances <- colSums(1 - squared_loadings)
CR <- total_squared_loadings / (total_squared_loadings + total_error_variances)

return(list(AVE = AVE, CR = CR))
}

# Assuming your CFA model is stored in the object 'fit'
validity_metrics <- convergent_validity(fit)
validity_metrics$AVE
validity_metrics$CR
# Get the standardized solution to extract factor loadings and variances
std_solution <- inspect(fit_cfa, "std")
std_solution

# Calculate Average Variance Extracted (AVE) for each factor
# AVE is the mean of the squared standardized loadings for each factor
AVE <- function(loadings) {
  sum(loadings^2) / length(loadings)
}
# Extract the factor loadings and calculate reliability manually
loadings <- std_solution$lambda
errors <- diag(std_solution$theta)
loadings
errors

# Composite reliability calculation
composite_reliability <- function(loadings, errors) {
  (sum(loadings)^2) / ((sum(loadings)^2) + sum(errors))
}

# Calculate AVE for each latent variable
ave_results <- apply(loadings, 2, AVE)
ave_results

# Apply the function to each latent variable's loadings and error variances
reliabilities <- apply(loadings, 2, function(l) composite_reliability(l, errors))
reliabilities

# Get the correlation matrix between latent variables
latent_correlation <- inspect(fit_cfa, "cor.lv")

# Square the correlations (since we compare AVE with squared correlations)
squared_correlations <- latent_correlation^2

# Compare AVE with squared correlations (AVE should be greater)
ave_vs_squared_correlations <- ave_results > apply(squared_correlations, 2, max)
ave_vs_squared_correlations

# Composite reliability function
composite_reliability <- function(loadings, errors) {
  sum(loadings)^2 / (sum(loadings)^2 + sum(errors))
}

# Extract the error variances (diagonal of theta matrix)
errors <- diag(std_solution$theta)

# Calculate Composite Reliability (CR) for each latent variable
cr_results <- apply(loadings, 2, function(l) composite_reliability(l, errors))
cr_results

# Specific fit indices
fitMeasures(fit_cfa, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"))

#All AVE's are below 0.5
#All CR's are below 0.7
# Fit indicates that the model fits the data well, despite the significant chi-square.

##########TASK 1 b##########

sem_model1 <- '

  attitude_refugees =~ NA*refugee1 + refugee2 + refugee3
  attitude_immigration =~ NA*immigration1 + immigration2 + immigration3
  religiousness =~ NA*religion1 + religion2 + religion3
  social_trust =~ NA*socialtrust1 + socialtrust2 + socialtrust3

  attitude_immigration ~ social_trust + religiousness
  attitude_refugees ~ social_trust + religiousness
'

fit_sem1 <- sem(sem_model1, data = ess_centered, group = "cntry")
summary(fit_sem1, fit.measures = TRUE, standardized = TRUE)

fit_sem2 <- sem(sem_model1, data = ess_centered, group = "cntry", group.equal = "regressions")
summary(fit_sem2, fit.measures = TRUE, standardized = TRUE)

fit_sem3 <- sem(sem_model1, data = ess_centered, group = "cntry", group.equal = c("loadings"))
summary(fit_sem3, fit.measures = TRUE, standardized = TRUE)

fit_sem4 <- sem(sem_model1, data = ess_centered, group = "cntry", group.equal = c("loadings", "regressions"))
summary(fit_sem4, fit.measures = TRUE, standardized = TRUE)


# Compare model fits using AIC, BIC
fitMeasures(fit_sem1, fit.measures = c("aic", "bic"))
fitMeasures(fit_sem2, fit.measures = c("aic", "bic"))
fitMeasures(fit_sem3, fit.measures = c("aic", "bic"))
fitMeasures(fit_sem4, fit.measures = c("aic", "bic"))
# fit_sem1 has lowest values for AIC and BIC


# Chi-square difference test
anova(fit_sem1, fit_sem2, fit_sem3, fit_sem4)
# All p-values are significant. More complex models provide better fit
# Based on RMSEA fit_sem2 has the best fit

##########TASK 2 a##########
load("C:/Users/dettorig/Downloads/grades.Rdata")
library(heplots)
library(candisc)
library(MASS)

#split the 2009 data into two: X variables and Y variables
X_2009<-data09[, cbind("jan_accountingA", "jan_bankstocks", "jan_English", "jan_French", "jan_peopleorg", "jan_microeconA", "jan_scientificthinking", "jan_mathematicsA")]
Y_2009<-data09[, cbind("jun_accountingB", "jun_civillaw", "jun_German", "jun_historyecon", "jun_philosophy", "jun_microeconB", "jun_psychology", "jun_Spanish", "jun_statistics", "jun_mgtproject", "jun_MathematicsB")]

#split the 2010 data into two: X variables and Y variables
X_2010<-data10[, cbind("jan_accountingA", "jan_bankstocks", "jan_English", "jan_French", "jan_peopleorg", "jan_microeconA", "jan_scientificthinking", "jan_mathematicsA")]
Y_2010<-data10[, cbind("jun_accountingB", "jun_civillaw", "jun_German", "jun_historyecon", "jun_philosophy", "jun_microeconB", "jun_psychology", "jun_Spanish", "jun_statistics", "jun_mgtproject", "jun_MathematicsB")]

#standardize each set of variables:
X_2009<-scale(X_2009, center=TRUE, scale=TRUE)
X_2010<-scale(X_2010, center=TRUE, scale=TRUE)
Y_2009<-scale(Y_2009, center=TRUE, scale=TRUE)
Y_2010<-scale(Y_2010, center=TRUE, scale=TRUE)

lm.out<-lm(Y_2009~jan_accountingA+jan_bankstocks+jan_English+jan_French+jan_peopleorg+jan_microeconA+jan_scientificthinking+jan_mathematicsA,data=data09)
man.out<-Manova(lm.out,test.statistic="Wilks")

#conduct canonical correlation analysis
cancor.out<-cancor(Y_2009~jan_accountingA+jan_bankstocks+jan_English+jan_French+jan_peopleorg+jan_microeconA+jan_scientificthinking+jan_mathematicsA,data=data09)
summary(cancor.out)

#compute using matrix operations
X <- as.matrix(X_2009)
Y <- as.matrix(Y_2009)
RXX<-cor(X)
RYY<-cor(Y)
RXY<-cor(X,Y)
RYX<-cor(Y,X)
eigX<-eigen(solve(RXX)%*%RXY%*%solve(RYY)%*%RYX)
eigY<-eigen(solve(RYY)%*%RYX%*%solve(RXX)%*%RXY)

#rescale eigenvectors
cvX<-as.matrix(X)%*%eigX$vectors
eigX$vectors%*%diag(1/apply(cvX,2,sd))

cvY<-as.matrix(Y)%*%eigY$vectors
eigY$vectors%*%diag(1/apply(cvY,2,sd))

ST<-t(Y)%*%Y
SH<-t(Y)%*%Y-man.out$SSPE
SE<-man.out$SSPE
det(SE)/det(ST)

#compute canonical loadings
cancor.out$structure$X.xscores
cancor.out$structure$Y.yscores

#compute canonical variates
options(max.print = 5000) 
cancor.out$scores$X
cancor.out$scores$Y

#plot can1 (=canonical variate 1)
can1<-cbind(cancor.out$scores$X[,1],cancor.out$scores$Y[,1])
rownames(can1)<-as.character(data09[,1])
plot(can1,xlab="u1",ylab="t1",xlim=c(-3,2),ylim=c(-3,2))
#identify points in the plot
identify(can1,labels=as.character(data09[,1]))

#plot can2 (=canonical variate 2)
can2<-cbind(cancor.out$scores$X[,2],cancor.out$scores$Y[,2])
rownames(can2)<-as.character(data09[,1])
plot(can2,xlab="u2",ylab="t2",xlim=c(-3,3),ylim=c(-3,3))
#identify points in the plot
identify(can2,labels=as.character(data09[,1]))

#plot can3 (=canonical variate 3)
can3<-cbind(cancor.out$scores$X[,3],cancor.out$scores$Y[,3])
rownames(can3)<-as.character(data09[,1])
plot(can3,xlab="u3",ylab="t3",xlim=c(-3,3),ylim=c(-3.5,3))
#identify points in the plot
identify(can3,labels=as.character(data09[,1]))

#plot can4 (=canonical variate 4)
can4<-cbind(cancor.out$scores$X[,4],cancor.out$scores$Y[,4])
rownames(can4)<-as.character(data09[,1])
plot(can4,xlab="u4",ylab="t4",xlim=c(-3,3),ylim=c(-3.5,3))
#identify points in the plot
identify(can4,labels=as.character(data09[,1]))

#compute redundancies using the redundancy function
redu<-redundancy(cancor.out)
round(redu$Ycan,3)

#compute redundancies from output (using equations)
R2tu <- cancor.out$cancor^2
VAFYbyt <- apply(cancor.out$structure$Y.yscores^2, 2, sum) / 11
redund <- R2tu * VAFYbyt
round(cbind(R2tu, VAFYbyt, redund, total = cumsum(redund)), 3)

##########TASK 2 b-c##########
#standardize data09:
data09_standardized <- scale(data09, center=TRUE,scale=TRUE)

#split data in two parts: a calibration set (X,Y) and a validation set (X*,Y*)
data09_standardized <- as.matrix(data09_standardized)
train_data <- data09_standardized
valid_data <- data10

#standardize data2010 (valid)
valid_data <- scale(valid_data, center=TRUE, scale =TRUE)

#conduct canonical correlation analysis using train_data and valid_data
cancor_train <- cancor(train_data[,1:8],train_data[,9:19])
cancor_valid <- cancor(valid_data[,1:8],valid_data[,9:19])
class(train_data)
class(valid_data)
summary(cancor_train)
summary(cancor_valid)

#compute canonical variates for the calibration set
train_data.X1 <- cancor_train$score$X 
train_data.Y1 <- cancor_train$score$Y

#compute canonical variates using train_data and coefficients estimated on valid_data
train_data.X2 <- as.matrix(train_data[,1:8])%*%cancor_valid$coef$X
train_data.Y2 <- as.matrix(train_data[,9:19])%*%cancor_valid$coef$Y


#R(T,T*) and R(U,U*)
round(cor(train_data.Y1,train_data.Y2)[1:4,1:4],3)
round(cor(train_data.X1,train_data.X2)[1:4,1:4],3)

#R(U,T) versus R(U*,T*)
round(cor(train_data.X1,train_data.Y1)[1:4,1:4],3)
round(cor(train_data.X2,train_data.Y2)[1:4,1:4],3)

#R(T*,T*) and R(U*,U*)
round(cor(train_data.Y2,train_data.Y2)[1:4,1:4],3)
round(cor(train_data.X2,train_data.X2)[1:4,1:4],3)

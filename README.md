# MultivariateStats-analysis
R scripts for multivariate statistical analysis. Covers CFA/SEM, canonical correlation and PCA with classification (LDA, QDA, KNN, Bagging, RF), clustering (hierarchical, k-means, HDDC), and correspondence analysis with visualization.

## Contents  

### **Task 1 – CFA & SEM (ESS Data)**  
- Centering and preprocessing of ESS dataset.  
- Confirmatory Factor Analysis (CFA) with convergent/discriminant validity checks (AVE, CR).  
- Structural Equation Models (SEM) comparing country-level models with fit indices (AIC, BIC, RMSEA, etc.).  

### **Task 2 – Canonical Correlation & Classification**  
- Canonical correlation analysis (CCA) on 2009–2010 student grades.  
- Comparison of canonical variates across calibration/validation sets.  
- Principal Component Analysis (PCA) with dimensionality reduction scenarios (80%/90% variance explained).  
- Classification models:  
  - Linear Discriminant Analysis (LDA)  
  - Quadratic Discriminant Analysis (QDA)  
  - k-Nearest Neighbors (KNN) with tuning  
  - Bagging & Random Forests with tuning and error comparison  
- Error rates visualized with `ggplot2`.  

### **Task 2 – Clustering**  
- PCA-based clustering with:  
  - Hierarchical clustering (Ward’s method)  
  - k-Means  
  - High-Dimensional Data Clustering (HDDC)  
- Cluster evaluation with Adjusted Rand Index (ARI).  
- Visualizations comparing observed vs predicted class labels.  

### **Task 3 – Correspondence Analysis**  
- Chi-square test of independence.  
- Correspondence Analysis (CA) with `ca`, `FactoMineR`, and `factoextra`.  
- Biplot visualizations of dimensions and category associations.  

## Requirements  
- R (≥ 4.0)  
- Key packages: `dplyr`, `lavaan`, `psych`, `heplots`, `candisc`, `MASS`, `ggplot2`, `randomForest`, `FNN`, `mclust`, `HDclassif`, `ca`, `FactoMineR`, `factoextra`  

Install packages as needed:  
```R
install.packages(c("dplyr","lavaan","psych","heplots","candisc","MASS",
                   "ggplot2","randomForest","FNN","mclust","HDclassif",
                   "ca","FactoMineR","factoextra"))

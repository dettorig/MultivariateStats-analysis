# MultivariateStats-analysis
R scripts for multivariate statistical analysis. Covers CFA/SEM, canonical correlation and PCA with classification (LDA, QDA, KNN, Bagging, RF), clustering (hierarchical, k-means, HDDC), and correspondence analysis with visualization.

## Assignment 1

### Task 1 — CFA & Multigroup SEM (ESS Round 8)
Data: ess.Rdata (Round 8, 2016): 3,357 respondents from Austria & Belgium; 12 items measuring four constructs—social trust, religiousness, attitudes toward immigration, attitudes toward refugees. Analyses:
- CFA on centered items; report AVE, CR, discriminant validity, and fit indices.
- Multigroup SEM (country as group): compare configural/metric models with/without equal regressions; select best model via fit indices and/or model comparison tests; discuss standardized paths and fit.

### Task 2 — Canonical Correlation on Grades (2009–2010)
Data: grades.Rdata with data09, data10 (first-year business administration).
- CCA (2009) on standardized variables: X = 8 Jan exams; Y = 11 Jun exams. Interpret number/significance of canonical correlations and variance explained in Y by X.
- Validation: use 2010 as validation set; assess reliability and interpret important canonical variate pairs.

## Assignment 2

### Task 1 — PCA → Classification (Fashion-MNIST subset)
Data: task1.Rdata derived from Fashion-MNIST (Zalando). train.data: 10,000 28×28 images of 5 classes (0 T-shirt/top, 1 Trouser, 2 Pullover, 3 Dress, 4 Coat). train.target: labels.
Analyses:
- PCA on the covariance matrix (centered variables). Choose PCs for 80% (Scenario 1) and 90% (Scenario 2) variance.
- Classifiers (on unstandardized PCs): LDA, QDA, KNN, Bagging, Random Forest.
  - Train error: all training obs.
  - Test error: LOOCV for LDA/QDA/KNN; OOB for Bagging/RF.
  - Tune KNN (k) and RF (ntree, mtry) to reduce test error.
- Create a summary table and a comparison plot of training/test errors.

Public dataset source: Fashion-MNIST (Kaggle mirror by Zalando Research). See assignment brief for link.

### Task 2 — Clustering (Fashion-MNIST subset, 3 classes)
Data: task2.Rdata from Fashion-MNIST: train.data = 18,000 images of 3 classes (1 Trouser, 7 Sneaker, 8 Bag); train.target = labels.
Analyses:
- PCA on covariance matrix; select PCs explaining 90% variance.
- Unsupervised clustering on unstandardized selected PCs; extract k=3 clusters:
  - Hierarchical (Ward on squared Euclidean distances)
  - k-means
  - HDDC “AkjBkQkD” with Cattell selection; thresholds 0.2 and 0.05; init from hierarchical and from k-means
- Evaluate via Adjusted Rand Index (ARI) and discuss results.
- Visualize observed vs predicted labels in the first two PCs for the best ARI model.

Public dataset source: Fashion-MNIST (Kaggle mirror by Zalando Research). See assignment brief for link.

### Task 3 — Correspondence Analysis (WVS Wave 7)
Data: task3.Rdata, table important: counts of 100 respondents per country (66 countries) rating the importance of six aspects (family, friends, leisure, politics, work, religion), based on World Values Survey Wave 7 (2017–2022).
Analyses:
- Chi-square test of independence.
- Correspondence Analysis (CA); summarize inertia/dimensions.
- Biplot of countries and aspects in 2D and interpret associations.
Reference: WVS Wave 7, version 4.0.0, DOI in assignment brief.

## Requirements
- R (≥ 4.0)
- Core packages used across tasks:
  dplyr, lavaan, psych, heplots, candisc, MASS, ggplot2,
  randomForest, FNN, mclust, HDclassif, patchwork,
  ca, FactoMineR, factoextra, reshape2, car, paran

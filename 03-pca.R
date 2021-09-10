# -----------------------------------------------------------------------------
#
# TIDYMODELS: Prostate Cancer Data
#
# -----------------------------------------------------------------------------
# Let's load the libraries
library(tidyverse)
library(tidymodels)
# Reading the data
ProsCancerdata = read.delim("ProstateCancerData.txt", sep=",")
ProsCancer = ProsCancerdata[,c(1:400,552)]
ProsCancer$tissue = as.factor(ProsCancer$tissue)
rec <- recipe( ~ ., data = ProsCancer)
pca_trans <- rec %>%
  update_role(tissue, new_role = "id") %>%
  step_impute_knn(all_predictors()) %>%
  step_normalize(all_predictors()) %>%
  step_pca(all_predictors())
 
pca_estimates <- prep(pca_trans)


tidied_pca <- tidy(pca_estimates,2)



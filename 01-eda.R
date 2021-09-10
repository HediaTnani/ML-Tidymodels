# -----------------------------------------------------------------------------
#
# TIDYMODELS: Prostate Cancer Data
#
# -----------------------------------------------------------------------------

# Loading libraries
install.packages(c("tidyverse", "tidymodels"))
install.packages("skimr")
install.packages("DataExplorer")
install.packages("GGally")
install.packages("visdat")
# Let's load the libraries
library(tidyverse)
library(tidymodels)
library(skimr)
library(DataExplorer)
library(GGally)
library(visdat)
# Reading the data
ProsCancer = read.delim("ProstateCancerData.txt", sep=",")
ProsCancer$tissue = as.factor(ProsCancer$tissue)
head(ProsCancer)
dim(ProsCancer)
# we have 502 rows and 552 features

# Let's see how many class do we have 
ProsCancer %>% 
  count(tissue) %>% unique()

# Let's see the proportion of each class
ProsCancer %>% 
  count(tissue) %>% 
  mutate(prop = n/sum(n))

# Let's have an overview of the data
skim(ProsCancer)

# Let's filter the rows where there are missing data
ProsCancer %>%
  skim() %>%
  focus(n_missing) %>% filter(n_missing > 0)

# Let's have a look at the proportion of missing data per tissue
ProsCancer %>% select(-sample_id) %>% 
  dplyr::group_by(tissue) %>%
  skim() %>%
  focus(tissue,n_missing) %>% filter(n_missing > 0)

# option 1
ProsCancer %>% summarise_all(list(name = ~sum(is.na(.))/length(.)))

# option 2
ProsCancer %>% map_dbl(.f = function(x){sum(is.na(x))})
skim(ProsCancer)

# {recipes} ---------------------------------------------------------------
#there are two strategies to deal with missing values in the data set
# Option1: Remove entire observations with missing values.
# Option2: Replace (impute) the missing values with some other value.
# Let's start by option 1
#The removal of entire observations containing NAs 
#is the quickest way to get rid of missing values in a dataset
#For this, we apply the step_naomit() function
ProsCancer_recipe <- recipe(tissue ~ ., data = ProsCancer %>% select(-sample_id)) %>%
  step_naomit(all_numeric(), -all_outcomes())

ProsCancer_recipe %>% prep() %>%
  bake(new_data = NULL) %>%
  skim() %>%
  focus(n_missing) 

# Let's demonstrate what this step is doing
ProsCancer_recipe %>% prep() %>%
  bake(new_data = NULL) %>% dim()

#The percentage of observations with missing values decreased but so did the total number of observations

# Let's move to Option 2
# Mean imputation by filling in the mean of the variable
ProsCancer_recipe_1 <- recipe(tissue ~ ., data = ProsCancer %>% select(-sample_id)) %>%
  step_impute_mean(all_numeric(), -all_outcomes())

# Median imputation by filling in the median of the variable
# Using the median value for imputation is usually preferred when the variable includes extreme values
ProsCancer_recipe_2 <- recipe(tissue ~ ., data = ProsCancer %>% select(-sample_id)) %>%
  step_impute_median(all_numeric(), -all_outcomes())

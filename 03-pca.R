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
ProsCancer = ProsCancerdata[,c(1:551,552)]
ProsCancer$tissue = as.factor(ProsCancer$tissue)
ProsCancer = ProsCancer %>% relocate(tissue)
#ProsCancer = ProsCancer %>% group_by(tissue) %>% summarise(across(AAK1:ROCK1, mean)) 

rec <- recipe( ~ ., data = ProsCancer)
pca_trans <- rec %>%
  update_role(tissue, new_role = "id") %>%
  step_impute_knn(all_numeric()) %>%
  # center the data
  step_center(all_numeric()) %>%
  # center the data
  step_scale(all_numeric()) %>%
  # pca on all numeric variables
  step_pca(all_numeric(),num_comp = 10)

pca_trans
 
pca_estimates <- prep(pca_trans)
names(pca_estimates)
pca_estimates$var_info
sdev <- pca_estimates$steps[[5]]$res$sdev
percent_variation <- (100*sdev^2) / (sum(sdev^2))
var_df <- data.frame(PC=paste0("PC",1:length(sdev)),
                     var_explained=percent_variation,
                     stringsAsFactors = FALSE)

var_df %>%
  mutate(PC = fct_inorder(PC)) %>%
  filter(var_explained > 1) %>%
  ggplot(aes(x=PC,y=var_explained))+geom_col()

juice(pca_estimates) 

juice(pca_estimates) %>%
  ggplot(aes(PC01, PC02)) +
  geom_point(aes(color = tissue), alpha = 0.7, size = 2)+
  labs(title="PCA from tidymodels")

juice(pca_estimates) %>%
  ggplot(aes(PC01, PC02, label = tissue)) +
  geom_point(aes(color = tissue), alpha = 0.7, size = 2,show.legend = FALSE) +
  geom_text(check_overlap = TRUE) + 
  labs(title="PCA with Tidymodels")





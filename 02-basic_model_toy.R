# -----------------------------------------------------------------------------

source("./01-eda_toy.R")

# {rsample} ---------------------------------------------------------------
#Create an initial split using rsample

set.seed(777)
# Put 3/4 of the data into the training set 
data_split <- initial_split(ProsCancer, prop = 3/4, strata = tissue)

# Create data frames for the two sets:
train <- training(data_split) 
test  <- testing(data_split)

train %>% 
  skim() %>%
  focus(n_missing) %>% filter(n_missing > 0) 

test %>% 
  skim() %>%
  focus(n_missing) %>% filter(n_missing > 0) 

# {recipes} ---------------------------------------------------------------

# Data Pre-processing

rec <- recipe(tissue ~ ., data = train) %>% 
  step_impute_mean(all_numeric(), -all_outcomes()) %>% 
  step_log(all_numeric(), -all_outcomes(), base = 10) 

# {parsnip} ---------------------------------------------------------------

# Model Building 

# Decision tree model spec
tree_mod <- decision_tree() %>% 
  set_engine("rpart") %>% 
  set_mode("classification")

# {workflows} -------------------------------------------------------------

# Creating a workflow object
wf <- workflow() %>% 
  add_recipe(rec) 


# Fitting the data to the model
tree_mod_fit <- wf %>% 
  add_model(tree_mod) %>% 
  fit(train)

# Creating our first predictions
tree_mod_pred <- predict(tree_mod_fit, test) %>% 
  bind_cols(select(test, tissue)) 

# {yardstick} -------------------------------------------------------------

# Let's see how well did our model perform on the test set
cmat <- conf_mat(data = tree_mod_pred, 
                 truth = tissue, 
                 estimate = .pred_class)
autoplot(cmat, type="heatmap")
accuracy(tree_mod_pred, truth = tissue, estimate = .pred_class)
precision(tree_mod_pred, truth = tissue, estimate = .pred_class)
recall(tree_mod_pred, truth = tissue, estimate = .pred_class)

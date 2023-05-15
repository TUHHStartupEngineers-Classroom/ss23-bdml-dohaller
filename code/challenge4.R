library(tidyverse)
library(h2o)
library(rsample)
library(recipes)

# Load Data
product_backorders <- read_csv("~/GitHub/ss23-bdml-dohaller/code/product_backorders.csv")

# Split Data
set.seed(123)
split_obj <- initial_split(product_backorders, prop = 0.85)
train_readable_tbl <- training(split_obj)
test_readable_tbl <- testing(split_obj)

product_rec <- recipe(went_on_backorder ~ ., data = train_readable_tbl) %>% 
  step_dummy(all_nominal(), -all_outcomes()) %>% 
  step_zv(all_predictors()) %>% 
  prep()

train_data <- bake(product_rec, new_data = train_readable_tbl)
test_data <- bake(product_rec, new_data = test_readable_tbl)

#Model
h2o.init()

split_h2o <- h2o.splitFrame(as.h2o(train_data), ratios = c(0.85), seed = 1234)
train_h2o <- split_h2o[[1]]
valid_h2o <- split_h2o[[2]]
test_h2o  <- as.h2o(test_data)

y <- "went_on_backorder"
x <- setdiff(names(train_h2o),y)


automl_models_h2o <- h2o.automl(
  x = x,
  y = y,
  training_frame    = train_h2o,
  validation_frame  = valid_h2o,
  leaderboard_frame = test_h2o,
  max_runtime_secs  = 30,
  nfolds            = 5 
)


automl_models_h2o@leaderboard

predictions <- h2o.predict(automl_models_h2o@leader, newdata = as.h2o((test_data)))
predictions_tbl <- predictions %>% 
  as_tibble()
predictions_tbl
automl_models_h2o@leader
h2o.getModel("StackedEnsemble_AllModels_2_AutoML_5_20230515_170512") %>% 
  h2o.saveModel(path = "/Users/Domin/Documents/GitHub/ss23-bdml-dohaller/code/models/")

h2o.getModel("StackedEnsemble_AllModels_1_AutoML_5_20230515_170512") %>% 
  h2o.saveModel(path = "/Users/Domin/Documents/GitHub/ss23-bdml-dohaller/code/models/")
h2o.getModel("StackedEnsemble_BestOfFamily_3_AutoML_5_20230515_170512") %>% 
  h2o.saveModel(path = "/Users/Domin/Documents/GitHub/ss23-bdml-dohaller/code/models/")
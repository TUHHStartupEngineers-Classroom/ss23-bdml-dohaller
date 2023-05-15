# Standard
library(tidyverse)

# Modeling
library(parsnip)

# Preprocessing & Sampling
library(recipes)
library(rsample)
library(workflows)

# Modeling Error Metrics
library(yardstick)

# Plotting Decision Trees
library(rpart.plot)

#bike_orderlines <- read_rds("./bike_orderlines.rds")
#Prepare Data
data <- bike_orderlines %>% 
  #Select needed Columns
  select(model, model_year, category_1, category_2, category_3, price, frame_material) %>% 
  #Remove Duplicates and prepare rows
  distinct() %>% 
  na.omit() %>% 
  mutate_if(is.character, as.factor)

glimpse(data)

#Split Data
set.seed(123)

data_split <- initial_split(data, prop = 3/4)

train_data <- training(data_split)
test_data <- testing(data_split)

#Create recipe
bikes_rec <- recipe(price ~ ., data = train_data) %>% 
  #Add roles
  update_role(model, new_role = "ID") %>% 
  #Add dummy variables
  step_dummy(all_nominal(), -all_outcomes()) %>% 
  step_zv(all_predictors())

#Create Model
lr_mod <-
  linear_reg() %>% 
  set_engine("glm") 
  
#Create Workflow
bikes_wflow <- workflow() %>% 
  add_model(lr_mod) %>% 
  add_recipe(bikes_rec)
bikes_wflow

#Fit Model
bikes_fit <- bikes_wflow %>% 
  fit(data = train_data)

#Predict
bikes_pred <- predict(bikes_fit, test_data) %>% 
  bind_cols(test_data %>% select(price, model))

bikes_pred %>% 
  metrics(truth = price, estimate = .pred) 
library(tidyverse)
library(h2o)
library(rsample)
library(recipes)

# Load Data
product_backorders <- read_csv("product_backorders.csv")

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

h2o.init()

split_h2o <- h2o.splitFrame(as.h2o(train_data), ratios = c(0.85), seed = 1234)
train_h2o <- split_h2o[[1]]
valid_h2o <- split_h2o[[2]]
test_h2o  <- as.h2o(test_data)

y <- "went_on_backorder"
x <- setdiff(names(train_h2o), y)

#Load Models
model1 <- h2o.loadModel("./models/StackedEnsemble_AllModels_1_AutoML_5_20230515_170512")
model2 <- h2o.loadModel("./models/StackedEnsemble_AllModels_2_AutoML_5_20230515_170512")
model3 <- h2o.loadModel("./models/StackedEnsemble_BestOfFamily_3_AutoML_5_20230515_170512")


#Grid Search
h2o.performance(model1, newdata = as.h2o(test_data))

deeplearning_grid_01 <- h2o.grid(
  algorithm = "deeplearning",
  grid_id = "deeplearning_grid_01",
  x = x,
  y = y,
  training_frame   = train_h2o,
  validation_frame = valid_h2o,
  nfolds = 5,
  hyper_params = list(
    hidden = list(c(10, 10, 10), c(50, 20, 10), c(20, 20, 20)),
    epochs = c(10, 50, 100)
  )
)

deeplearning_grid_01_model_1 <- h2o.getModel("deeplearning_grid_01_model_1")
deeplearning_grid_01_model_1 %>% h2o.auc(train = T, valid = T, xval = T)
deeplearning_grid_01_model_1 %>%
  h2o.performance(newdata = as.h2o(test_data))

#Theme
theme_new <- theme(
  legend.position  = "bottom",
  legend.key       = element_blank(),
  panel.background = element_rect(fill   = "transparent"),
  panel.border     = element_rect(color = "black", fill = NA, size = 0.5),
  panel.grid.major = element_line(color = "grey", size = 0.333)
) 

#Optimal Threshold
performance_h2o <- h2o.performance(model1, newdata = as.h2o(test_data))

performance_tbl <- performance_h2o %>%
  h2o.metric() %>%
  as_tibble() 

performance_tbl %>%
  filter(f1 == max(f1))

performance_tbl %>%
  ggplot(aes(x = threshold)) +
  geom_line(aes(y = precision), color = "blue", size = 1) +
  geom_line(aes(y = recall), color = "red", size = 1) +
  
  # Insert line where precision and recall are harmonically optimized
  geom_vline(xintercept = h2o.find_threshold_by_max_metric(performance_h2o, "f1")) +
  labs(title = "Precision vs Recall", y = "value") +
  theme_new

#ROC Plot
path <- "models/StackedEnsemble_AllModels_1_AutoML_5_20230515_170512"
load_model_performance_metrics <- function(path, test_tbl) {
  path
  model_h2o <- h2o.loadModel(path)
  perf_h2o  <- h2o.performance(model_h2o, newdata = as.h2o(test_tbl)) 
  
  perf_h2o %>%
    h2o.metric() %>%
    as_tibble() %>%
    mutate(auc = h2o.auc(perf_h2o)) %>%
    select(tpr, fpr, auc, recall, precision)
  
}


model_metrics_tbl <- fs::dir_info(path = "models/") %>%
  select(path) %>%
  mutate(metrics = map(path, load_model_performance_metrics, test_data)) %>%
  unnest(cols = metrics)

model_metrics_tbl %>%
    # Extract the model names
    mutate(
      # Extract the model names
      path = str_split(path, pattern = "/", simplify = T)[,2] %>% as_factor(),
      auc  = auc %>% round(3) %>% as.character() %>% as_factor()
    ) %>%
  ggplot(aes(fpr, tpr, color = path, linetype = auc)) +
  geom_line(size = 1) +
  
  # just for demonstration purposes
  geom_abline(color = "red", linetype = "dotted") +
  
  theme_new +
  theme(
    legend.direction = "vertical",
  ) +
  labs(
    title = "ROC Plot",
    subtitle = "Performance of 3 Top Performing Models"
  )

#Precision Vs Recall Plot
model_metrics_tbl %>%
  mutate(
    path = str_split(path, pattern = "/", simplify = T)[,2] %>% as_factor(),
    auc  = auc %>% round(3) %>% as.character() %>% as_factor()
  ) %>%
  ggplot(aes(recall, precision, color = path, linetype = auc)) +
  geom_line() +
  theme_new + 
  theme(
    legend.direction = "vertical",
  ) +
  labs(
    title = "Precision vs Recall Plot",
    subtitle = "Performance of 3 Top Performing Models"
  )

# Gain Chart
gain_lift_tbl <- performance_h2o %>%
  h2o.gainsLift() %>%
  as.tibble()


gain_transformed_tbl <- gain_lift_tbl %>% 
  select(group, cumulative_data_fraction, cumulative_capture_rate, cumulative_lift) %>%
  select(-contains("lift")) %>%
  mutate(baseline = cumulative_data_fraction) %>%
  rename(gain     = cumulative_capture_rate) %>%
  # prepare the data for the plotting (for the color and group aesthetics)
  pivot_longer(cols = c(gain, baseline), values_to = "value", names_to = "key")

gain_transformed_tbl %>%
  ggplot(aes(x = cumulative_data_fraction, y = value, color = key)) +
  geom_line(size = 1.5) +
  labs(
    title = "Gain Chart",
    x = "Cumulative Data Fraction",
    y = "Gain"
  ) +
  theme_new
  

## Lift Plot

lift_transformed_tbl <- gain_lift_tbl %>% 
  select(group, cumulative_data_fraction, cumulative_capture_rate, cumulative_lift) %>%
  select(-contains("capture")) %>%
  mutate(baseline = 1) %>%
  rename(lift = cumulative_lift) %>%
  pivot_longer(cols = c(lift, baseline), values_to = "value", names_to = "key")

lift_transformed_tbl %>%
  ggplot(aes(x = cumulative_data_fraction, y = value, color = key)) +
  geom_line(size = 1.5) +
  labs(
    title = "Lift Chart",
    x = "Cumulative Data Fraction",
    y = "Lift"
  ) +
  theme_new
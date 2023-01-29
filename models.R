# create and evaluate models
# import libs and data
library(pacman)
p_load(rio, tidyverse, tidymodels, GGally, mice)

houses <- import("data/houses.rds")
apartments <- import("data/apartments.rds")

# clean data
houses_cleaned <- houses %>% 
  mutate(usable_area = as.numeric(usable_area),
         built_up_area = as.numeric(built_up_area),
         land_area = as.numeric(land_area),
         district = as_factor(district),
         municipality = as_factor(municipality),
         type = as_factor(type),
         commission_in_price = as_factor(commission_in_price)
  )

apartments_cleaned <- apartments %>% 
  mutate(usable_area = as.numeric(usable_area),
         rooms = as.numeric(rooms),
         district = as_factor(district),
         municipality = as_factor(municipality),
         condition = as_factor(condition),
         type = as_factor(type),
         commission_in_price = as_factor(commission_in_price)
  )

summary(apartments_cleaned)

# impute missing values
set.seed(123)
houses_cleaned <- 
  houses_cleaned %>% 
  mice(m = 5,maxit = 5, method = "rf", ntree = 10)
houses_cleaned <- complete(houses_cleaned, 1)

set.seed(234)
apartments_cleaned <- 
  apartments_cleaned %>% 
  mice(m = 5,maxit = 5, method = "rf", ntree = 10)
apartments_cleaned <- complete(apartments_cleaned, 1)

# split dataframes to train(80)/test(20)
set.seed(345)
houses_train_split <- initial_split(houses_cleaned, prop = 0.8)

houses_train <- training(houses_train_split)
houses_test <- testing(houses_train_split)

set.seed(456)
apartments_train_split <- initial_split(apartments_cleaned, prop = 0.8)

apartments_train <- training(apartments_train_split)
apartments_test <- testing(apartments_train_split)

set.seed(567)
houses_train_folds <- vfold_cv(houses_train, strata = type)

set.seed(678)
apartments_train_folds <- vfold_cv(apartments_train, strata = type)

# recipes
houses_xgboost_recipe <- recipe(houses_train, price ~ .) %>%
  step_rm(lat, lon) %>%
  step_log(usable_area, built_up_area, land_area) %>%
  step_normalize(usable_area, built_up_area, land_area) %>%
  step_other(all_nominal(), threshold = 0.01) %>%
  step_dummy(all_nominal()) 

apartments_xgboost_recipe <- recipe(apartments_train, price ~ .) %>%
  step_rm(lat, lon) %>%
  step_log(usable_area, built_up_area, land_area) %>%
  step_normalize(usable_area, built_up_area, land_area) %>%
  step_other(all_nominal(), threshold = 0.01) %>%
  step_dummy(all_nominal())

# models
xgb_model <- 
  boost_tree(
    trees = tune(), learn_rate = tune(),
    tree_depth = tune(), min_n = tune(),
    loss_reduction = tune(), 
    sample_size = tune(), mtry = tune(), 
  ) %>% 
  set_mode("regression") %>% 
  set_engine("xgboost", nthread = 6) # use 6 cores for multithreading

# workflows
houses_workflow <- workflow() %>% add_recipe(houses_xgboost_recipe) %>% add_model(xgb_model)
apartments_workflow <- workflow() %>% add_recipe(apartments_xgboost_recipe) %>% add_model(xgb_model)

# parameters
houses_xgboost_params <- parameters(
  trees(), learn_rate(),
  tree_depth(), min_n(), 
  loss_reduction(),
  sample_size = sample_prop(), finalize(mtry(), houses_train)  
)

apartments_xgboost_params <- parameters(
  trees(), learn_rate(),
  tree_depth(), min_n(), 
  loss_reduction(),
  sample_size = sample_prop(), finalize(mtry(), apartments_train)  
)

# optimization
set.seed(789)
houses_xgboost_tune <-
  houses_workflow %>%
  tune_bayes(
    resamples = houses_train_folds,
    param_info = houses_xgboost_params,
    iter = 20, 
    metrics = metric_set(rmse, mape),
    control = control_bayes(no_improve = 10, 
                            save_pred = T, verbose = T)
  )

autoplot(houses_xgboost_tune)

set.seed(890)
apartments_xgboost_tune <-
  houses_workflow %>%
  tune_bayes(
    resamples = apartments_train_folds,
    param_info = apartments_xgboost_params,
    iter = 20, 
    metrics = metric_set(rmse, mape),
    control = control_bayes(no_improve = 10, 
                            save_pred = T, verbose = T)
  )

autoplot(apartments_xgboost_tune)


houses_best_model <- select_best(houses_xgboost_tune, "rmse")
apartments_best_model <- select_best(apartments_xgboost_tune, "rmse")

# finalize models
houses_final_model <- finalize_model(xgb_model, houses_best_model)
houses_workflow    <- houses_workflow %>% update_model(houses_final_model)
houses_xgb_fit     <- fit(houses_workflow, data = houses_train)

apartments_final_model <- finalize_model(xgb_model, apartments_best_model)
apartments_workflow    <- apartments_workflow %>% update_model(apartments_final_model)
apartments_xgb_fit     <- fit(apartments_workflow, data = apartments_train)

# evaluate
houses_pred <- 
  predict(houses_xgb_fit, houses_test) %>% 
  mutate(modelo = "XGBoost",
         .pred = exp(.pred)) %>% 
  bind_cols(houses_test)

houses_plot1 <- 
  houses_pred %>% 
  ggplot(aes(x = .pred, y = price))+
  geom_point() + 
  geom_abline(intercept = 0, col = "red")


houses_plot2 <- 
  houses_pred %>% 
  select(.pred, price) %>% 
  gather(key, value) %>% 
  ggplot(aes(x = value, volor = key, fill = key)) + 
  geom_density(alpha = .2) + 
  labs(x = "", y = "")

houses_plot1 / houses_plot2

apartments_pred <- 
  predict(apartments_xgb_fit, apartments_test) %>% 
  mutate(modelo = "XGBoost",
         .pred = exp(.pred)) %>% 
  bind_cols(houses_test)

apartments_plot1 <- 
  apartments_pred %>% 
  ggplot(aes(x = .pred, y = price))+
  geom_point() + 
  geom_abline(intercept = 0, col = "red")


apartments_plot2 <- 
  apartments_pred %>% 
  select(.pred, price) %>% 
  gather(key, value) %>% 
  ggplot(aes(x = value, volor = key, fill = key)) + 
  geom_density(alpha = .2) + 
  labs(x = "", y = "")

house_plot1 / house_plot2

# to do
# check for julia silge video on many factor levels
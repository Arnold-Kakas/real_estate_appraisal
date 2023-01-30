# create and evaluate models
# import libs and data
library(pacman)
p_load(rio, tidyverse, tidymodels, GGally, mice, patchwork)

houses <- import("data/houses.rds")
apartments <- import("data/apartments.rds")

# clean data
houses_cleaned <- houses %>% 
  mutate(price = log10(price+1),
         usable_area = as.numeric(usable_area),
         built_up_area = as.numeric(built_up_area),
         land_area = as.numeric(land_area)
  )

apartments_cleaned <- apartments %>% 
  mutate(price = log10(price),
         usable_area = as.numeric(usable_area),
         rooms = as.numeric(rooms)
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


# mutate character vector (didn't do it in first cleaning due to slow imputation after this change)
houses_cleaned <- houses_cleaned %>% 
  mutate(condition = replace_na(condition, "not provided"),
         district = as_factor(district),
         municipality = as_factor(municipality),
         type = as_factor(type),
         commission_in_price = as_factor(commission_in_price)
  )

apartments_cleaned <- apartments_cleaned %>% 
  mutate(condition = replace_na(condition, "not provided"),
         district = as_factor(district),
         municipality = as_factor(municipality),
         condition = as_factor(condition),
         type = as_factor(type),
         commission_in_price = as_factor(commission_in_price)
  )

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
houses_train_boots <- bootstraps(houses_train, times = 25)

set.seed(987)
houses_folds <- vfold_cv(houses_train, strata = type, v = 5)
set.seed(876)
apartments_folds <- vfold_cv(apartments_train, strata = type, v = 5)


set.seed(678)
apartments_train_boots <- bootstraps(apartments_train, times = 25)

# recipes
houses_lr_recipe <- recipe(houses_train, price ~ .) %>%
  step_rm(lat_rotated, lon_rotated, municipality) %>%
  step_log(usable_area, built_up_area, land_area) %>%
  step_normalize(usable_area, built_up_area, land_area) %>%
  step_other(all_nominal(), threshold = 0.01) %>%
  step_dummy(all_nominal()) 

houses_xgboost_recipe <- recipe(houses_train, price ~ .) %>%
  step_rm(lat, lon, municipality) %>%
  step_log(usable_area, built_up_area, land_area) %>%
  step_normalize(usable_area, built_up_area, land_area) %>%
  step_other(all_nominal(), threshold = 0.01) %>%
  step_dummy(all_nominal()) 

apartments_xgboost_recipe <- recipe(apartments_train, price ~ .) %>%
  step_rm(lat, lon, municipality) %>%
  step_impute_knn(all_nominal_predictors()) %>% 
  step_log(usable_area) %>%
  step_normalize(usable_area) %>%
  step_other(all_nominal(), threshold = 0.01) %>%
  step_dummy(all_nominal())

# models
houses_lr_model <- linear_reg() %>%
  set_engine(engine = "lm")

xgb_model <- 
  boost_tree(
    trees = 1000, loss_reduction = tune(),
    tree_depth = tune(), min_n = tune(),
    mtry = tune(), sample_size = tune(),
    learn_rate = tune()
  ) %>% 
  set_mode("regression") %>% 
  set_engine("xgboost", nthread = 6) # use 6 cores for multithreading

# workflows
houses_lr_workflow <- workflow() %>% add_recipe(houses_lr_recipe) %>% add_model(houses_lr_model)
houses_workflow <- workflow() %>% add_recipe(houses_xgboost_recipe) %>% add_model(xgb_model)
apartments_workflow <- workflow() %>% add_recipe(apartments_xgboost_recipe) %>% add_model(xgb_model)

# parameters
houses_xgboost_params <- parameters(
  trees(),
  tree_depth(), min_n(), 
  finalize(mtry(), houses_train)  
)

apartments_xgboost_params <- parameters(
  trees(), learn_rate(),
  tree_depth(), min_n(), 
  finalize(mtry(), apartments_train)  
)

# optimization
set.seed(789)
houses_xgboost_grid <- grid_latin_hypercube(
  tree_depth(),
  min_n(),
  loss_reduction(),
  sample_size = sample_prop(),
  finalize(mtry(), houses_train),
  learn_rate(),
  size = 30
)


set.seed(890)
apartments_xgboost_grid <-
  grid_latin_hypercube(
    tree_depth(),
    min_n(),
    loss_reduction(),
    sample_size = sample_prop(),
    finalize(mtry(), apartments_train),
    learn_rate(),
    size = 30
  )

houses_lr_res <- fit_resamples(
  houses_lr_workflow,
  resamples = houses_folds,
  control = control_resamples(save_pred = TRUE)
)

houses_xgboost_res <- tune_grid(
  houses_workflow,
  resamples = houses_train_boots,
  grid = houses_xgboost_grid,
  control = control_grid(save_pred = TRUE)
)

apartments_xgboost_res <- tune_grid(
  apartments_workflow,
  resamples = apartments_train_boots,
  grid = apartments_xgboost_grid,
  control = control_grid(save_pred = TRUE)
)

houses_best_lr_model <- select_best(houses_lr_res, "rmse")
houses_best_model <- select_best(houses_xgboost_res, "rmse")
apartments_best_model <- select_best(apartments_xgboost_res, "rmse")

# finalize models
houses_lr_final_model <- finalize_model(houses_lr_model, houses_best_model)
houses_lr_workflow    <- houses_lr_workflow %>% update_model(houses_lr_final_model)
houses_lr_fit     <- fit(houses_lr_workflow, data = houses_train)

houses_final_model <- finalize_model(xgb_model, houses_best_model)
houses_workflow    <- houses_workflow %>% update_model(houses_final_model)
houses_xgb_fit     <- fit(houses_workflow, data = houses_train)

apartments_final_model <- finalize_model(xgb_model, apartments_best_model)
apartments_workflow    <- apartments_workflow %>% update_model(apartments_final_model)
apartments_xgb_fit     <- fit(apartments_workflow, data = apartments_train)

library(vip)
houses_lr_fit %>%
  fit(data = houses_train) %>%
  pull_workflow_fit() %>%
  vip(geom = "point")

houses_xgb_fit %>%
  fit(data = houses_train) %>%
  pull_workflow_fit() %>%
  vip(geom = "point")

apartments_xgb_fit %>%
  fit(data = apartments_train) %>%
  pull_workflow_fit() %>%
  vip(geom = "point")

# evaluate
houses_lr_pred <- 
  predict(houses_lr_fit, houses_test) %>%
  bind_cols(houses_test)

houses_lr_plot1 <- 
  houses_lr_pred %>% 
  ggplot(aes(x = .pred, y = price))+
  geom_point() + 
  geom_abline(intercept = 0, col = "red")


houses_lr_plot2 <-
  houses_lr_pred %>%
  select(.pred, price) %>%
  gather(key, value) %>%
  ggplot(aes(x = value, volor = key, fill = key)) +
  geom_density(alpha = .2) +
  labs(x = "", y = "")

houses_lr_plot1 / houses_lr_plot2


houses_pred <- 
  predict(houses_xgb_fit, houses_test) %>%
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
  bind_cols(apartments_test)

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

apartments_plot1/apartments_plot2

# to do
# check for julia silge video on many factor levels
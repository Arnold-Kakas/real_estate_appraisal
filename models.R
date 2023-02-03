# create and evaluate models
# import libs and data
library(pacman)
p_load(rio, tidyverse, tidymodels, GGally, mice, patchwork, vip, dataPreparation)

houses <- import("data/houses.rds")
apartments <- import("data/apartments.rds")

# clean data
houses_cleaned <- houses %>%
  mutate( # price = log10(price+1),
    usable_area = as.numeric(usable_area),
    built_up_area = as.numeric(built_up_area),
    land_area = as.numeric(land_area)
  )

apartments_cleaned <- apartments %>%
  mutate( # price = log10(price),
    usable_area = as.numeric(usable_area),
    rooms = as.numeric(rooms)
  )

# impute missing values
set.seed(123)
houses_cleaned <-
  houses_cleaned %>%
  mice(m = 5, maxit = 5, method = "rf", ntree = 10)
houses_cleaned <- complete(houses_cleaned, 1)

set.seed(234)
apartments_cleaned <-
  apartments_cleaned %>%
  mice(m = 5, maxit = 5, method = "rf", ntree = 10)
apartments_cleaned <- complete(apartments_cleaned, 1)

# mutate character vector (didn't do it in first cleaning due to slow imputation after this change)
houses_cleaned <- houses_cleaned %>%
  mutate(
    condition = replace_na(condition, "not provided"),
    district = as_factor(district),
    municipality = as_factor(municipality),
    type = as_factor(type),
    commission_in_price = as_factor(commission_in_price)
  ) %>% 
  remove_sd_outlier(cols = c("price", "usable_area", "built_up_area", "land_area"), n_sigmas = 2) # remove outliers with value +-mean +2sd

apartments_cleaned <- apartments_cleaned %>%
  mutate(
    condition = replace_na(condition, "not provided"),
    district = as_factor(district),
    municipality = as_factor(municipality),
    condition = as_factor(condition),
    type = as_factor(type),
    commission_in_price = as_factor(commission_in_price)
  ) %>% 
  remove_sd_outlier(cols = c("price", "usable_area"), n_sigmas = 2) # remove outliers with value +-mean +2sd

ggpairs(houses_cleaned[, c(1, 6, 7, 10, 11)])
ggpairs(apartments_cleaned[, c(1, 6, 8, 11, 12)])

# EDA
houses_BoxCox <- recipe(houses_train, price ~ .) %>%
  step_rm(lon, lat, lon_rotated, lat_rotated) %>%
  step_BoxCox(all_numeric_predictors()) %>%
  prep() %>%
  bake(new_data = NULL)

houses_log <- recipe(houses_train, price ~ .) %>%
  step_rm(lon, lat, lon_rotated, lat_rotated) %>%
  step_log(all_numeric_predictors()) %>%
  prep() %>%
  bake(new_data = NULL)

houses_norm <- recipe(houses_train, price ~ .) %>%
  step_rm(lon, lat, lon_rotated, lat_rotated) %>%
  step_normalize(all_numeric_predictors()) %>%
  prep() %>%
  bake(new_data = NULL)

# Plot distribution of built_up_area before and after transformations
p1 <- ggplot(data = houses_cleaned, aes(x = built_up_area)) +
  geom_density() +
  ggtitle("price before transformations")

p2 <- ggplot(data = houses_BoxCox, aes(x = built_up_area)) +
  geom_density() +
  ggtitle("price after Box-Cox transform")

p3 <- ggplot(data = houses_norm, aes(x = built_up_area)) +
  geom_density() +
  ggtitle("price after normalization")

p4 <- ggplot(data = houses_log, aes(x = built_up_area)) +
  geom_density() +
  ggtitle("price after log transform")

(p1 + p2) / (p3 + p4)

apartments_BoxCox <- recipe(apartments_train, price ~ .) %>%
  step_rm(lon, lat, lon_rotated, lat_rotated) %>%
  step_BoxCox(all_numeric_predictors()) %>%
  prep() %>%
  bake(new_data = NULL)

apartments_log <- recipe(apartments_train, price ~ .) %>%
  step_rm(lon, lat, lon_rotated, lat_rotated) %>%
  step_log(all_numeric_predictors()) %>%
  prep() %>%
  bake(new_data = NULL)

apartments_norm <- recipe(apartments_train, price ~ .) %>%
  step_rm(lon, lat, lon_rotated, lat_rotated) %>%
  step_normalize(all_numeric_predictors()) %>%
  prep() %>%
  bake(new_data = NULL)

# Plot distribution of usable_area before and after transformations
p5 <- ggplot(data = apartments_cleaned, aes(x = usable_area)) +
  geom_density() +
  ggtitle("price before transformations")

p6 <- ggplot(data = apartments_BoxCox, aes(x = usable_area)) +
  geom_density() +
  ggtitle("price after Box-Cox transform")

p7 <- ggplot(data = apartments_norm, aes(x = usable_area)) +
  geom_density() +
  ggtitle("price after normalization")

p8 <- ggplot(data = apartments_log, aes(x = usable_area)) +
  geom_density() +
  ggtitle("price after log transform")

(p5 + p6) / (p7 + p8)

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
houses_xgboost_recipe <- recipe(houses_train, price ~ .) %>%
  step_rm(lat, lon, municipality) %>%
  step_BoxCox(usable_area, built_up_area, land_area) %>%
  #  step_normalize(usable_area, built_up_area, land_area) %>%
  step_other(all_nominal(), threshold = 0.01) %>%
  step_dummy(all_nominal())

apartments_xgboost_recipe <- recipe(apartments_train, price ~ .) %>%
  step_rm(lat, lon, municipality) %>%
  step_impute_knn(all_nominal_predictors()) %>%
  step_BoxCox(usable_area) %>%
#  step_normalize(usable_area) %>%
  step_other(all_nominal(), threshold = 0.01) %>%
  step_dummy(all_nominal())

# models
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
houses_workflow <- workflow() %>%
  add_recipe(houses_xgboost_recipe) %>%
  add_model(xgb_model)
apartments_workflow <- workflow() %>%
  add_recipe(apartments_xgboost_recipe) %>%
  add_model(xgb_model)

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

houses_best_model <- select_best(houses_xgboost_res, "rmse")
apartments_best_model <- select_best(apartments_xgboost_res, "rmse")

# finalize models
houses_final_model <- finalize_model(xgb_model, houses_best_model)
houses_workflow <- houses_workflow %>% update_model(houses_final_model)
houses_xgb_fit <- fit(houses_workflow, data = houses_train)

apartments_final_model <- finalize_model(xgb_model, apartments_best_model)
apartments_workflow <- apartments_workflow %>% update_model(apartments_final_model)
apartments_xgb_fit <- fit(apartments_workflow, data = apartments_train)

houses_xgb_fit %>%
  fit(data = houses_train) %>%
  pull_workflow_fit() %>%
  vip(geom = "point")

apartments_xgb_fit %>%
  fit(data = apartments_train) %>%
  pull_workflow_fit() %>%
  vip(geom = "point")

# evaluate
houses_pred <-
  predict(houses_xgb_fit, houses_test) %>%
  bind_cols(houses_test)

houses_plot1 <-
  houses_pred %>%
  ggplot(aes(x = .pred, y = price)) +
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
  ggplot(aes(x = .pred, y = price)) +
  geom_point() +
  geom_abline(intercept = 0, col = "red")


apartments_plot2 <-
  apartments_pred %>%
  select(.pred, price) %>%
  gather(key, value) %>%
  ggplot(aes(x = value, volor = key, fill = key)) +
  geom_density(alpha = .2) +
  labs(x = "", y = "")

apartments_plot1 / apartments_plot2

# to do
# check for julia silge video on many factor levels

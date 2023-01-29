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
         land_area = as.numeric(land_area)
  )

apartments_cleaned <- apartments %>% 
  mutate(usable_area = as.numeric(usable_area),
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

# to do
# factor character vectors, repalce NA with not provided
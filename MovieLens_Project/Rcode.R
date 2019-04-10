rm(list = ls(all.names = TRUE))
gc()
memory.limit()
memory.size()

library(tidyverse)
library(ranger)
library(rsample)
library(tictoc)
library(Metrics)

tic()

load("tidy_edx.RData")

set.seed(1234)

sample <- sample_n(tidy_edx, 0.004 * nrow(tidy_edx))

rm(tidy_edx)
gc()

cv_split <- vfold_cv(sample, v = 5) #v: how many times the data should split
memory.size()

cv_data <- cv_split %>%
  mutate(train = map(splits, ~training(.x)),
         validate = map(splits, ~testing(.x)))

cv_data <- cv_data %>% mutate(validate_actual = map(validate, ~.x$rating))

cv_tune <- cv_data %>% crossing(mtry = 1:6)

cv_models_tunerf <- cv_tune %>%
  mutate(model = map2(.x = train, .y = mtry, ~ranger(formula = rating ~.,
                                                     data = .x, mtry = .y,
                                                     num.trees = 100, seed = 42)))
# Generate validate predictions for each model
cv_prep_tunerf <- cv_models_tunerf %>% 
  mutate(validate_predicted = map2(.x = model, .y = validate, ~predict(.x, .y)$predictions))

cv_eval_tunerf <- cv_prep_tunerf %>% 
  mutate(validate_rmse = map2_dbl(.x = validate_actual, .y = validate_predicted, ~ rmse(.x, .y)))

# Calculate the mean validate_mae for each mtry used  
cv_eval_tunerf %>% 
  group_by(mtry) %>% 
  summarise(mean_rmse = mean(validate_rmse))

toc()

rm(cv_prep_tunerf, cv_eval_tunerf)
gc()

load("validate_tidy_edx.RData")
test_actual <- validate_tidy_edx$rating

test_predict <- predict(cv_models_tunerf, validate_tidy_edx)
rmse(test_actual, test_predict)

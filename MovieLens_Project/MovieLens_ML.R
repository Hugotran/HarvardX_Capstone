
plot_releasedyear <- tidy_edx %>% group_by(released_year) %>% 
  summarize(year_count = n()) %>% 
  mutate(released_year = reorder(released_year, year_count)) %>% ggplot(aes(released_year, year_count)) + 
  geom_point() +
  coord_flip()
plot_releasedyear

plot_movieid <- tidy_edx %>% group_by(movieId) %>% 
  summarize(movieid_count = n()) %>% 
  mutate(movieId = reorder(movieId, movieid_count)) %>% ggplot(aes(movieId, movieid_count)) + 
  geom_point() +
  coord_flip()  
plot_movieid  

# How can I visualize my data in a specific pathern?

# What are main issues I need to beware of to clean up my data?


# MACHINE LEARNING 
## Loss function 
## https://rafalab.github.io/dsbook/large-datasets.html#loss-function

RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

## Create train set and test set for dat
set.seed(2)
test_index2 <- createDataPartition(y = tidy_edx$movieId, times = 1, p = 0.5, list = FALSE)

train_set <- tidy_edx[test_index2,]
test_set <- tidy_edx[-test_index2,]


dim(train_set)
dim(test_set)
head(train_set)

# APPLY technique in datacamp course "Machine learning in the Tidyverse"
nest_train_set <- train_set %>% group_by(userId) %>% nest()
head(nest_train_set)

map(.x = nest_train_set$data, .f = ~mean(.x$rating))
train_set <- nest_train_set %>%
  mutate(rating_mean = map(data, ~mean(.x$rating))) %>%
  unnest(rating_mean)
head(train_set)

lm_model <- nest_train_set %>% mutate(model = map(data, ~lm(formula = rating ~ movieId, data = .x)))
user8_lmmodel <- lm_model$model[[8]]
summary(user8_lmmodel)

# Using Broom Toolkit
#tidy() : returns the statistical findings of the model (such as coefficients)
# glance(): returns a concise one-row summary of the model
# augement(): adds prediction columns to the data being modeled

library(broom)
tidy(user8_lmmodel)
glance(user8_lmmodel)
augment(user8_lmmodel)

augment(user8_lmmodel) %>%
  ggplot(mapping = aes(x = movieId)) +
  geom_point(mapping = aes(y = rating)) +
  geom_line(mapping = aes(y = .fitted), color = "red")

#Exploring coefficients across models
# To know the effect power of our predictor
coef_lmmodels <- lm_model %>%
  mutate(coef = map(model, ~tidy(.x))) %>%
  unnest(coef)
# Plot a histogram of the coefficient estimates for year         
coef_lmmodels %>% 
  filter(term == "movieId") %>% 
  ggplot(aes(x = estimate)) +
  geom_histogram()

#Evaluating the fit of many models
#Using glance to calculate how well the linear models fit the data for each user
perf_lmmodels <- lm_model %>%
  mutate(fit = map(model, ~glance(.x))) %>%
  unnest(fit)

# Overall, how well do your models fit your data?
# Which are the best fitting models?
# Which models do not fit the data well?
perf_lmmodels %>% 
  ggplot(aes(x = r.squared)) + 
  geom_histogram()

best2_lmmodels <- perf_lmmodels %>%
  top_n(n = 2, wt = r.squared) #best 2 fitting models

worst2_lmmodels <- perf_lmmodels %>%
  top_n(n = 2, wt = -r.squared) #worst 2 fitting models

# Visually inspect the fit of many models
best2_augmented <- best2_lmmodels %>%
  mutate(augmented = map(model, ~augment(.x))) %>%
  unnest(augmented)

best2_augmented %>%
  ggplot(aes(x = movieId, y = rating)) +
  geom_point() +
  geom_line(aes(y= .fitted), color = "red") +
  facet_wrap(~userId, scales = "free_y")

worst2_augmented <- best2_lmmodels %>%
  mutate(augmented = map(model, ~augment(.x))) %>%
  unnest(augmented)

worst2_augmented %>%
  ggplot(aes(x = movieId, y = rating)) +
  geom_point() +
  geom_line(aes(y= .fitted), color = "red") +
  facet_wrap(~userId, scales = "free_y")

# Bingo! While the adjusted R2 does tell us how well the model fit our data, it does not give any 
#indication on how it would perform on new data. In the upcomming chapter you will learn how to estimate
#model performance using data withheld from building the model.


# Logistic regression models
cv_models_lr <- cv_data %>%
  mutate(model = map(train, ~glm(formula = Attrition ~.,
                                 data = .x, family = "binomial")))

# Binary

# Confirm that your data was not modified  
identical(nest_train_set, train_set)


# Extract the first model and validate 
model <- cv_models_lr$model[[1]]
validate <- cv_models_lr$validate[[1]]

# Prepare binary vector of actual Attrition values in validate
validate_actual <-  validate$Attrition== "Yes"

# Predict the probabilities for the observations in validate
validate_prob <- predict(model, validate, type = "response")

# Prepare binary vector of predicted Attrition values for validate
validate_predicted <- validate_prob > 0.5

#3. A metric to compare 1. & 2.
library(Metrics)

table(validate_actual, validate_predicted)

recall(validate_actual, validate_predicted)


#Prepare for cross validated performance
cv_prep_lr <- cv_models_lr %>% 
  mutate(
    # Prepare binary vector of actual Attrition values in validate
    validate_actual = map(validate, ~.x$Attrition == "Yes"),
    # Prepare binary vector of predicted Attrition values for validate
    validate_predicted = map2(.x = model, .y = validate, ~predict(.x, .y, type = "response") > 0.5)
  )

# Calculate the validate recall for each cross validation fold
cv_perf_recall <- cv_prep_lr %>% 
  mutate(validate_recall = map2_dbl(validate_actual, validate_predicted, 
                                    ~recall(actual = .x, predicted = .y)))

# Print the validate_recall column
cv_perf_recall$validate_recall

# Calculate the average of the validate_recall column
mean(cv_perf_recall$validate_recall)


# Random forest for classification
#ranger() for Classification
library(ranger)
library(ranger)

# Prepare for tuning your cross validation folds by varying mtry
cv_tune <- cv_data %>%
  crossing(mtry = c(2, 4, 8, 16)) 

# Build a cross validation model for each fold & mtry combination
cv_models_rf <- cv_tune %>% 
  mutate(model = map2(train, mtry, ~ranger(formula = Attrition~., 
                                           data = .x, mtry = .y,
                                           num.trees = 100, seed = 42)))

cv_prep_rf <- cv_models_rf %>% 
  mutate(
    # Prepare binary vector of actual Attrition values in validate
    validate_actual = map(validate, ~.x$Attrition == "Yes"),
    # Prepare binary vector of predicted Attrition values for validate
    validate_predicted = map2(.x = model, .y = validate, ~predict(.x, .y, type = "response")$predictions == "Yes")
  )

# Calculate the validate recall for each cross validation fold
cv_perf_recall <- cv_prep_rf %>% 
  mutate(recall = map2_dbl(.x = validate_actual, .y = validate_predicted, ~recall(actual = .x, predicted = .y)))

# Calculate the mean recall for each mtry used  
cv_perf_recall %>% 
  group_by(mtry) %>% 
  summarise(mean_recall = mean(recall))


#Build final classification model
# Build the logistic regression model using all training data
best_model <- glm(formula = Attrition ~., 
                  data = training_data, family = "binomial")


# Prepare binary vector of actual Attrition values for testing_data
test_actual <- testing_data$Attrition == "Yes"

# Prepare binary vector of predicted Attrition values for testing_data
test_predicted <- predict(best_model, testing_data, type = "response") > 0.5

# Compare the actual & predicted performance visually using a table
table(test_actual, test_predicted)

# Calculate the test accuracy
accuracy(test_actual, test_predicted)

# Calculate the test precision
precision(test_actual, test_predicted)

# Calculate the test recall
recall(test_actual, test_predicted)


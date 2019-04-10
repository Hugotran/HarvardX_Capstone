
##########################################################################################################################
# HarvardX PH125.9x Data Science Capstone 
# Movielens Project
#
# Tran Phu Hoa
# https://github.com/Hugotran/
#
##########################################################################################################################


##########################################################################################################################
# Create edx set, validation set, and submission file
##########################################################################################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- read.table(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                      col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")


# Validation set will be 10% of MovieLens data

set.seed(1)
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set

validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set

removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)


#The "edx" subset will be the main dataset for this analysis and modelling, while the "validation" subset will be used to calculate RMSE for final algorithm.
#Save "edx.RData" and "validation.RData" to the local system, so that it saves the loading times for further analysis.

save(edx, file = "edx.RData")

save(validation, file = "validation.RData")

##########################################################################################################################
#
# Explore the dataset
#
##########################################################################################################################

# Loading useful packages

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")


# Take a look at the dataset using "glimpse" function in "tidyverse" package, and it shows that the "edx" dataset has 9,000,055 observations and 6 variables. There are three types of data including integer, double and character. In fact, "userId" and "timestamp" are coded in integer, "movieId" and "rating" are double, and "title" and "genres" are character. 

glimpse(edx)


# Look for NA as the missing value in the dataset using "summary" function. The result indicates that there is no missing value in the "edx" dataset.

summary(edx)


# Show the unique values of each variable

edx %>% summarize(n_users = n_distinct(userId),
                  n_movies = n_distinct(movieId),
                  n_rating = n_distinct(rating),
                  n_title = n_distinct(title),
                  n_genres = n_distinct(genres),
                  n_timestamp = n_distinct(timestamp))


##########################################################################################################################
#
#The following activities will tidy up the "edx" dataset.
#
##########################################################################################################################

#Transform "timestamp" to date variable using "lubridate" package
#Separate variables in "genres", and put them into "genres" column gain

tidy_edx_sep_genres <- edx %>% separate_rows(genres, sep = "\\|")


#Parse date to "timestamp" variable

tidy_edx_date <- tidy_edx_sep_genres %>% 
  mutate(date_of_rating = as_datetime(timestamp)) %>%
  select(c("userId", "movieId", "rating", "title", "genres", "date_of_rating"))


# Separate "title" column into "title" and "movie_year"
# Select object "year" in the "title" column, and create "movie_year" variable
# Detect "year" in the title column

tidy_edx <- tidy_edx_date %>% 
  mutate(movie_year = str_extract(tidy_edx_date$title, regex("\\(\\d{4}\\)"))) 

tidy_edx <- tidy_edx %>%
  mutate(movie_year = str_extract(tidy_edx$movie_year, regex("\\d{4}")))

# Change the type of "movie_year" and "movieId" to integer  

tidy_edx <- tidy_edx %>%
  mutate(movie_year = as.integer(movie_year), movieId = as.integer(movieId))


# Save tidy_edx to the local system

save(tidy_edx, file = "tidy_edx.RData")


# Remove unnecessary dataset "tidy_edx_date", "tidy_edx_sep_genres"

rm("tidy_edx_date", "tidy_edx_sep_genres")
gc()


##########################################################################################################################
#
# Explore tidy_edx
#
##########################################################################################################################

# Take a look at the dataset

glimpse(tidy_edx)


# Identify any missing values 

summary(tidy_edx)


# Count the number of unique data in each variable

tidy_edx %>% summarize(n_users = n_distinct(userId),
                       n_movies = n_distinct(movieId),
                       n_rating = n_distinct(rating),
                       n_title = n_distinct(title),
                       n_genres = n_distinct(genres),
                       n_date = n_distinct(date_of_rating),
                       n_movieyear = n_distinct(movie_year))


# Plot the histogram of "rating" 

tidy_edx %>%
  ggplot(aes(rating)) + 
  geom_histogram(binwidth = 0.05, color = "blue") +
  scale_x_discrete(limits = c(seq(0.5,5,0.5))) + 
  scale_y_continuous(breaks = c(seq(0, 7000000, 500000))) +
  xlab("Ratings") +
  ylab("Number of ratings") + 
  ggtitle("Histogram of ratings")


# Calculate the number of rates for each rating

tidy_edx %>% 
  group_by(rating) %>% 
  summarize(n_rating = n()) %>% 
  arrange(desc(n_rating))


# Summary statistic of "rating"

tidy_edx %>% summarize(mean_rating = mean(rating), 
                       median_rating = median(rating), 
                       sd_rating = sd(rating))



tidy_edx %>% group_by(movieId, title) %>% 
  summarize(n_movie = n()) %>% 
  arrange(desc(n_movie))


# Compute the number of ratings for each movie and then plot it against the year the movie came out. 
# Use the square root transformation on the counts.

tidy_edx %>% 
  group_by(movieId) %>%
  summarize(n_ratings = n(), 
            year = as.character(first(movie_year))) %>%
  qplot(year, n_ratings, data = ., geom = "boxplot") +
  coord_trans(y = "sqrt") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 6)) +
  labs(title = "Boxplot of number of ratings and released year of movies", 
       x = "Released year of movies", 
       y = "Number of ratings")


# Boxplot of ratings and released year of movies

tidy_edx %>% group_by(movieId) %>%
  summarize(mean_rating = mean(rating), 
            year = as.character(first(movie_year))) %>%
  qplot(year, mean_rating, data = ., geom = "boxplot") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 6)) +
  labs(title = "Boxplot of ratings and released year of movies", 
       x = "Released year of movies", 
       y = "Ratings")


# Average ratings in quarters of years

tidy_edx %>% mutate(date = round_date(date_of_rating, unit = "quarter")) %>%
  group_by(date) %>%
  summarize(mean_rating = mean(rating), n_rating = n()) %>%
  ggplot(aes(date, mean_rating, size = n_rating)) +
  geom_point(alpha = 0.1) +
  scale_size(name  = "Number of ratings") +
  geom_smooth() + 
  theme_light() +
  labs(title = "Average ratings in quarters of years", 
       x = "Year", 
       y = "Average of ratings")


# Movie genres and the average of rating

tidy_edx %>% group_by(genres) %>% 
  summarize(n_rating = n(), 
            avg_rating = mean(rating), 
            se_rating = sd(rating)) %>%
  filter(n_rating >= 1000) %>%
  arrange(desc(n_rating)) %>% 
  ggplot (aes(x=genres, y=avg_rating)) +
  geom_point() +
  geom_errorbar(aes(ymin=avg_rating - se_rating, 
                    ymax=avg_rating + se_rating), 
                width=0.4, colour="red", 
                alpha=0.8, size=1.3) +
  geom_bar(stat="identity", fill="skyblue", alpha=0.7) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  labs(title = "Movie genres and the average of rating",
       x = "Genres", 
       y = "Average of rating")


# Other view at movie genres and the average of rating

tidy_edx %>% group_by(genres) %>%
  summarize(n = n(), 
            avg = mean(rating), 
            se = sd(rating)/sqrt(n())) %>%
  filter(n >= 1000) %>% 
  mutate(genres = reorder(genres, avg)) %>%
  ggplot(aes(x = genres, 
             y = avg, ymin = avg - 2*se, 
             ymax = avg + 2*se)) + 
  geom_point() +
  geom_errorbar() + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  labs(title = "Relationship between genres of movies and the average of rating", 
       x = "Genres", 
       y = "Average of rating")


# Number of movies produced per year

tidy_edx %>%
  select(movie_year, movieId) %>%
  group_by(movie_year) %>%
  summarise(count = n_distinct(movieId)) %>%
  arrange(desc(count)) %>%
  ggplot(aes(x = movie_year, y = count)) +
  geom_bar(stat = "identity") +
  labs(title = "Movies Production",
       x = "Released year of movies", 
       y = "Number of movies produced") 


# Plot The relationship between movie genres and number of ratings

tidy_edx %>%
  group_by(genres) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  ggplot(aes(x = reorder(genres, -count), y = count)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(breaks = c(seq(0, 7000000, 500000))) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 0)) +
  labs(title = "Number of ratings and Movies' genres",
       x = "Movies' genres", 
       y = "Number of ratings")


# Plot The relationship between movie genres and average of ratings

tidy_edx %>%
  group_by(genres) %>%
  summarise(mean_rating = mean(rating)) %>%
  arrange(desc(mean_rating)) %>%
  ggplot(aes(x = reorder(genres, -mean_rating), y = mean_rating)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 0)) +
  labs(title = "The average of rating and the movie's genres", 
       x = "Movies' genres", 
       y = "Average of ratings")


# Relationship between average rating and number of movie per week

weekly_rating <- tidy_edx %>% 
  mutate(date = round_date(date_of_rating, unit = "week")) %>%
  group_by(date) %>% 
  nest() %>% 
  mutate(n_movie = map(data, ~n_distinct(.x$movieId)),
         mean_rating = map(data, ~mean(.x$rating)))

weekly_rating <- weekly_rating %>% unnest(n_movie,mean_rating)

weekly_rating %>% ggplot(aes(date, mean_rating, size = n_movie)) +
  geom_point(alpha = 0.1) +
  scale_size(name  = "Number of movies") +
  geom_smooth() + 
  theme_light() +
  labs(title = "Relationship between average rating and number of movie per week", 
       x = "Week", 
       y = "Average of ratings")

##########################################################################################################################
#
# Modellling
#
##########################################################################################################################

# RMSE
# "Metrics" package will be used to calculate RMSE in this project.
library(Metrics)

# "caret" package will help to create train and test datasets.
library(caret)

# Create training data and validation data for machine learning

set.seed(2019)
test_index <- createDataPartition(tidy_edx$rating, time = 1, p = 0.8, list = FALSE)

train_data <- tidy_edx %>% slice(test_index)
test_data <- tidy_edx %>% slice(-test_index)


#Use semi_join() to ensure that all users and movies in the testing data are also in the training data

test_data <- test_data %>% 
  semi_join(train_data, by = "movieId") %>%
  semi_join(train_data, by = "userId")

###################################################################################
#
# Matrix factorization approach
#
###################################################################################

# Load useful packages

library(tidyverse)
library(tictoc)
library(Metrics)

############################################
# Benchmark model
############################################

# The average of rating

mean_rating <- mean(train_data$rating)
mean_rating


# RMSE of the Bechmark model

benchmark_rmse <- rmse(test_data$rating, mean_rating)

benchmark_rmse

save(benchmark_rmse, file = "benchmark.RData")


############################################
# Movie Effect Model
############################################

# Calculate b_i

movie_effect_avgs <- train_data %>%
  group_by(movieId) %>%
  summarize(b_i = mean(rating - mean_rating))



# Plot histogram of b_i

movie_effect_avgs %>% 
  qplot(b_i, 
        geom ="histogram",
        bins = 10, data = .,
        color = I("blue"),
        ylab = "Number of movies",
        main = "Number of movies with the computed b_i")


# Predicted values of ratings in test_date using Movie effect model

tic()

movie_effect_predicted_ratings <- mean_rating + test_data %>%
  left_join(movie_effect_avgs, by = "movieId") %>%
  .$b_i

toc()


# Calculate RMSE of the Movie effect model

movie_effect_model_rmse <- rmse(movie_effect_predicted_ratings, test_data$rating)

movie_effect_model_rmse

save(movie_effect_model_rmse, file = "movie_effect_rmse.RData")


############################################
# Movie-User effect model
############################################

# Plot the histogram of b_u with users who rated more than 100 times

train_data %>% 
  left_join(movie_effect_avgs, by='movieId') %>%
  group_by(userId) %>%
  filter(n() >= 100) %>%
  summarize(b_u = mean(rating - mean_rating - b_i)) %>%
  qplot(b_u, 
        geom ="histogram", 
        bins = 30, data = ., 
        color = I("black"))


# Movie-User effect model

tic()

# Calculate b_u

user_effect_avgs <- train_data %>%
  left_join(movie_effect_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mean_rating - b_i))


# Predict the rating in test_data using Movie-user effect model

movie_user_effect_predicted_ratings <- test_data %>%
  left_join(movie_effect_avgs, by='movieId') %>%
  left_join(user_effect_avgs, by='userId') %>%
  mutate(pred = mean_rating + b_i + b_u) %>%
  .$pred

toc()

# Calculate RMSE of Movie-user effect model

movie_user_effect_rmse <- rmse(movie_user_effect_predicted_ratings, test_data$rating)

movie_user_effect_rmse

save(movie_user_effect_rmse, file = "movie_user_effect_rmse.RData")


############################################
# Regularized Movie-user effect model
############################################

# Set up a Lambdas vector 

lambdas <- seq(0, 10, 0.25)

# Build a function to run Movie-user effect model with different values of lambdas, and calculate their RMSE 

tic()
rmses <- sapply(lambdas, function(l){
  
  mean_rating <- mean(train_data$rating)
  
  b_i <- train_data %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mean_rating)/(n()+l))
  
  b_u <- train_data %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mean_rating)/(n()+l))
  
  movie_user_effect_predicted_ratings <- 
    test_data %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mean_rating + b_i + b_u) %>%
    .$pred
  
  return(rmse(movie_user_effect_predicted_ratings, test_data$rating))
})

toc()


# Visualize the rmse and lambdas to observe the optimal value of lambda

qplot(lambdas, rmses)  


# Pick the optimal lambda
  
lambda <- lambdas[which.min(rmses)]
lambda

save(lambda, file = "lambda.RData")

# Report the RMSE of the Movie-User effect model with the optimal lambda
  
regularized_movie_user_effect_rmse <- min(rmses)

regularized_movie_user_effect_rmse

save(regularized_movie_user_effect_rmse, file = "regularized_movie_user_effect_rmse.RData")


###################################################################################
#
# Random forest approach using "Ranger" package
#
###################################################################################


#########################################################
# Random forest with 300 trees using a subset of tidy_edx
#########################################################

# Remove and free up working space
rm(list = ls(all.names = TRUE))

gc()


# Load necessary packages
library(tidyverse)
library(ranger)
library(rsample)
library(tictoc)
library(Metrics)
library(caret)


# Load the datasets
tic()
load("tidy_edx.RData")


# Create a subset of tidy_edx dataset
set.seed(2019)

sample <- sample_n(tidy_edx, 0.001 * nrow(tidy_edx))

rm(tidy_edx)

gc()


# Create validation set
cv_split <- vfold_cv(sample, v = 5) #v: how many times the data should split


# Mapping train & validate
cv_data <- cv_split %>%
  mutate(train = map(splits, ~training(.x)),
         validate = map(splits, ~testing(.x)))


#Tune the Hyper-Parameters
cv_tune <- cv_data %>% crossing(mtry = 1:6)


# Random forest model with 300 trees
cv_models_tunerf <- cv_tune %>%
  mutate(model = map2(train, 
                      mtry, 
                      ~ ranger(formula = 
                                 rating ~.,
                               data = .x, 
                               mtry = .y,
                               num.trees = 300)))


# Generate validate predictions for each model
cv_prep_tunerf <- cv_models_tunerf %>% 
  mutate(validate_actual = map(validate, ~.x$rating),
         validate_predicted = map2(.x = model,
                                   .y = validate,
                                   ~predict(.x, .y)$predictions))


# Calculate validate RMSE for each fold and mtry combination
cv_eval_tunerf <- cv_prep_tunerf %>% 
  mutate(validate_rmse = map2_dbl(.x = validate_actual, 
                                  .y = validate_predicted, 
                                  ~rmse(actual = .x, predicted = .y)))


# Calculate the mean validate_mae for each mtry used  
mean_rmse_by_mtry <- cv_eval_tunerf %>% 
  group_by(mtry) %>% 
  summarise(mean_rmse = mean(validate_rmse))

min_mean_rmse <- min(mean_rmse_by_mtry$mean_rmse)


# Result of Random Forest with 300 trees 

mean_rmse_by_mtry


# Save the minimum of rmse to conduct Table of results
rf1_rmse <- min(mean_rmse_by_mtry$mean_rmse)

save(rf1_rmse, file = "rf1_rmse.RData")


# Memory usage of running Random Forest with 300 trees
memory.size()

# Time spending in running Random Forest with 300 trees
toc()


#############################################################
# Run random forest with 500 trees using a subset of tidy_edx
#############################################################

# Remove and free up working space
rm(list = ls(all.names = TRUE))

gc()


# Load necessary packages
library(tidyverse)
library(ranger)
library(rsample)
library(tictoc)
library(Metrics)
library(caret)


# Load the datasets
tic()
load("tidy_edx.RData")
load("tidy_validation.RData")


# Create a subset of tidy_edx dataset
set.seed(2019)

sample <- sample_n(tidy_edx, 0.001 * nrow(tidy_edx))

rm(tidy_edx)

gc()


# Create validation set
cv_split <- vfold_cv(sample, v = 5) #v: how many times the data should split


# Mapping train & validate
cv_data <- cv_split %>%
  mutate(train = map(splits, ~training(.x)),
         validate = map(splits, ~testing(.x)))


# Tune the Hyper-Parameters
cv_tune <- cv_data %>% crossing(mtry = 1:6)


# Random forest model with 500 trees
cv_models_tunerf <- cv_tune %>%
  mutate(model = map2(train, 
                      mtry, 
                      ~ ranger(formula = 
                                 rating ~.,
                               data = .x, 
                               mtry = .y,
                               num.trees = 500)))


# Generate validate predictions for each model
cv_prep_tunerf <- cv_models_tunerf %>% 
  mutate(validate_actual = map(validate, ~.x$rating),
         validate_predicted = map2(.x = model,
                                   .y = validate,
                                   ~predict(.x, .y)$predictions))


# Calculate validate RMSE for each fold and mtry combination
cv_eval_tunerf <- cv_prep_tunerf %>% 
  mutate(validate_rmse = map2_dbl(.x = validate_actual, 
                                  .y = validate_predicted, 
                                  ~rmse(actual = .x, predicted = .y)))


# Calculate the mean validate_mae for each mtry used  
mean_rmse_by_mtry <- cv_eval_tunerf %>% 
  group_by(mtry) %>% 
  summarise(mean_rmse = mean(validate_rmse))


# Result of Random Forest with 500 trees 

mean_rmse_by_mtry


# Save the minimum of rmse to conduct Table of results
rf2_rmse <- min(mean_rmse_by_mtry$mean_rmse)

save(rf2_rmse, file = "rf2_rmse.RData")


# Memory usage of running Random Forest with 500 trees
memory.size()

# Time spending in running Random Forest with 500 trees
toc()


#################################################################
# Run random forest with better subset of tidy_edx with 300 trees
#################################################################


# Remove and free up working space
rm(list = ls(all.names = TRUE))

gc()


# Load necessary packages
library(tidyverse)
library(ranger)
library(rsample)
library(tictoc)
library(Metrics)
library(caret)


# Load the datasets
tic()
load("tidy_edx.RData")


# Create a subset of tidy_edx dataset
set.seed(2019)

subset_train_data <- tidy_edx %>% 
  group_by(movieId) %>% 
  mutate(count = n()) %>%
  filter(count >= 1000) %>% 
  ungroup(movieId) %>% 
  select(-count) %>%
  group_by(userId) %>% 
  mutate(count = n()) %>% 
  filter(count >= 100) %>% 
  ungroup(userId) %>% 
  select(-count)

sample <- sample_n(subset_train_data, 0.001 * nrow(subset_train_data))

rm(tidy_edx)

gc()


# Create validation set
cv_split <- vfold_cv(sample, v = 5) #v: how many times the data should split


# Mapping train & validate
cv_data <- cv_split %>%
  mutate(train = map(splits, ~training(.x)),
         validate = map(splits, ~testing(.x)))


# Tune the Hyper-Parameters
cv_tune <- cv_data %>% crossing(mtry = 1:6)

# Random forest model with 300 trees
cv_models_tunerf <- cv_tune %>%
  mutate(model = map2(train, 
                      mtry, 
                      ~ ranger(formula = rating ~.,
                               data = .x, 
                               mtry = .y, 
                               num.trees = 300)))

# Generate validate predictions for each model
cv_prep_tunerf <- cv_models_tunerf %>% 
  mutate(validate_actual = map(validate, ~.x$rating),
         validate_predicted = map2(.x = model, 
                                   .y = validate, 
                                   ~predict(.x, .y)$predictions))

# Calculate validate RMSE for each fold and mtry combination
cv_eval_tunerf <- cv_prep_tunerf %>% 
  mutate(validate_rmse = map2_dbl(.x = validate_actual, 
                                  .y = validate_predicted, 
                                  ~rmse(actual = .x, predicted = .y)))

# Calculate the mean validate_mae for each mtry used  
mean_rmse_by_mtry <- cv_eval_tunerf %>% 
  group_by(mtry) %>% 
  summarise(mean_rmse = mean(validate_rmse))


# Result of Random Forest using better dataset with 300 trees 

mean_rmse_by_mtry


# Save the minimum of rmse to conduct Table of results
rf3_rmse <- min(mean_rmse_by_mtry$mean_rmse)

save(rf3_rmse, file = "rf3_rmse.RData")


# Memory usage of running Random Forest using better dataset with 300 trees
memory.size()

# Time spending in running Random Forest using better dataset with 300 trees
toc()


###################################################################################
#
# k-Nearest Neighbors approach
#
###################################################################################

# Remove and free up working space
rm(list = ls(all.names = TRUE))

gc()


# Load necessary packages
library(tidyverse)
library(rsample)
library(tictoc)
library(Metrics)
library(caret)


# Load the datasets
tic()
load("tidy_edx.RData")


# Create a subset of tidy_edx dataset
set.seed(2019)

sample <- sample_n(tidy_edx, 0.001 * nrow(tidy_edx))

rm(tidy_edx)

gc()

# Set up cross validation method for the knn model 
fitControl <- trainControl(method = "cv",
                           number = 5,
                           p = 0.8)

# Run the knn model
model_knn <- train(rating ~ movieId + userId + genres + date_of_rating,
                   data = sample,
                   method = "knn",
                   trControl = fitControl,
                   verbose= FALSE)


# Result of K-nearest neighbors approach

model_knn$results[2]

# Save the minimum of rmse to conduct Table of results
knn_rmse <- min(model_knn$results[2])

save(knn_rmse, file = "knn_rmse.RData")


# Memory usage of running Knn model
memory.size()

# Time spending in running Knn model
toc()


##########################################################################################################################
#
# Choosing a model for validation
#
##########################################################################################################################


load("benchmark.RData")
load("movie_effect_rmse.RData")
load("movie_user_effect_rmse.RData")
load("regularized_movie_user_effect_rmse.RData")
load("rf1_rmse.RData")
load("rf2_rmse.RData")
load("rf3_rmse.RData")
load("knn_rmse.RData")


# Table of RMSEs of different machine learning approaches
RMSE_results <- data_frame(Model = "Benchmark", RMSE = benchmark_rmse)

RMSE_results <- bind_rows(RMSE_results,
                          data_frame(Model = "Movie effect",  
                                     RMSE = movie_effect_model_rmse))

RMSE_results <- bind_rows(RMSE_results,
                          data_frame(Model = "Movie-User effect",  
                                     RMSE = movie_user_effect_rmse))

RMSE_results <- bind_rows(RMSE_results,
                          data_frame(Model = "Regularized Movie-User effect",  
                                     RMSE = regularized_movie_user_effect_rmse))

RMSE_results <- bind_rows(RMSE_results,
                          data_frame(Model = "Random Forest with 300 trees",  
                                     RMSE = rf1_rmse))

RMSE_results <- bind_rows(RMSE_results,
                          data_frame(Model = "Random Forest with 500 trees",  
                                     RMSE = rf2_rmse))

RMSE_results <- bind_rows(RMSE_results,
                          data_frame(Model = "Random Forest in better dataset",  
                                     RMSE = rf3_rmse))

RMSE_results <- bind_rows(RMSE_results,
                          data_frame(Model = "K-nearest neighbors",  
                                     RMSE = knn_rmse))

RMSE_results %>% knitr::kable()


##########################################################################################################################
#
# Validate the models
#
##########################################################################################################################

## Remove and free up working space

rm(list = ls(all.names = TRUE))
gc()
memory.limit()
memory.size()

## Load necessary packages

if(!require(tidyverse)) 
  install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) 
  install.packages("lubridate", repos = "http://cran.us.r-project.org")


## Load "validation" dataset

load("validation.RData")


## Tidy up the "validation" dataset.

## Separate variables in "genres", and put them into "genres" column gain
validation_sep_genres <- validation %>% separate_rows(genres, sep = "\\|")

## Parse date to "timestamp" variable
validation_date <- validation_sep_genres %>% mutate(date_of_rating = as_datetime(timestamp)) %>%
  select(c("userId", "movieId", "rating", "title", "genres", "date_of_rating"))

## Separate "title" column into "title" and "movie_year"

## Select object "year" in the "title" column, and create "movie_year" variable
## Detect "year" in the title column

tidy_validation <- validation_date %>% 
  mutate(movie_year = str_extract(validation_date$title, regex("\\(\\d{4}\\)"))) 

tidy_validation <- tidy_validation %>%
  mutate(movie_year = str_extract(tidy_validation$movie_year, regex("\\d{4}")))

tidy_validation <- tidy_validation %>%
  mutate(movie_year = as.integer(movie_year), movieId = as.integer(movieId))

# Save tidy_edx to the local system.

save(tidy_validation, file = "tidy_validation.RData")

# Remove unnecessary dataset "tidy_edx_date", "tidy_edx_sep_genres"

rm("validation_date", "validation_sep_genres")

gc()


## Validate three best models that have the lowest RMSEs

tic()

load("tidy_edx.RData")
load("lambda.RData")

mean_rating <- mean(tidy_edx$rating)

b_i <- tidy_edx %>% 
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mean_rating)/(n()+lambda))

b_u <- tidy_edx %>% 
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mean_rating)/(n()+lambda))

movie_user_effect_predicted_ratings <- 
  tidy_validation %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  mutate(pred = mean_rating + b_i + b_u) %>%
  .$pred

# Memory usage
memory.size()

# Time spending in running the model
toc()



# RMSE of the best model in the validation step

movie_user_effect_rmse <- rmse(movie_user_effect_predicted_ratings, tidy_validation$rating)

Final_result <- data_frame(Model = "Movie-User effect", RMSE = movie_user_effect_rmse)

Final_result %>% knitr::kable()


##########################################################################################################################
##########################################################################################################################

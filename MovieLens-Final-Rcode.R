
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

# Take a look to the dataset

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


tidy_edx %>% 
  ggplot(aes(userId)) +
  geom_histogram(binwidth = 400) +
  labs(x = "userId", y = "count")


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


tidy_edx %>% group_by(movieId) %>%
  summarize(mean_rating = mean(rating), 
            year = as.character(first(movie_year))) %>%
  qplot(year, mean_rating, data = ., geom = "boxplot") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 6)) +
  labs(title = "Boxplot of ratings and released year of movies", 
       x = "Released year of movies", 
       y = "Ratings")



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


# "Genres" seems to effect how users rate, for example, "Film-noir" was scored the highest rate (4.2 points) on average. "Horror" movies had smallest score which was about 0.8.

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
  theme(axis.text.x = element_text(angle = 30, hjust = 1))



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
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# want to see what movies are rating in the highest and the bottom.

edx %>% filter(rating == 0.5) %>%
  group_by(title) %>% 
  summarize(n_rating = n()) %>%
  arrange(desc(n_rating))


# Plot the number of movies produced per year

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


# What are the most rated movie genres?
# Note: This process could take several minutes

# Plot the relationship between the number of ratings and the movies' genres

tidy_edx %>%
  group_by(genres) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  ggplot(aes(x = reorder(genres, -count), y = count)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(breaks = c(seq(0, 7000000, 500000))) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0)) +
  labs(title = "Number of ratings and Movies' genres",
       x = "Movies' genres", 
       y = "Number of ratings")


# Plot the relationship between the average of rating and the movie's genres

tidy_edx %>%
  group_by(genres) %>%
  summarise(mean_rating = mean(rating)) %>%
  arrange(desc(mean_rating)) %>%
  ggplot(aes(x = reorder(genres, -mean_rating), y = mean_rating)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0)) +
  labs(title = "The average of rating and the movie's genres", 
       x = "Movies' genres", 
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

############################################
# Benchmark model
############################################

# The average of rating

mean_rating <- mean(train_data$rating)
mean_rating


# Model-based approach RMSE

model_based_rmse <- rmse(test_data$rating, mean_rating)
model_based_rmse


# Create table of results to report RMSE values of different models in the project.
# Table of results

RMSE_results <- data_frame(Model = "Model-based", RMSE = model_based_rmse)
RMSE_results %>% knitr::kable()

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


# Calculate b_i in test_data

movie_effect_predicted_ratings <- mean_rating + test_data %>%
  left_join(movie_effect_avgs, by = "movieId") %>%
  .$b_i


# Calculate RMSE of the Movie effect model

movie_effect_model_rmse <- rmse(movie_effect_predicted_ratings, test_data$rating)
movie_effect_model_rmse


# Table of results

RMSE_results <- bind_rows(RMSE_results,
                          data_frame(Model = "Movie effect model",
                                     RMSE = movie_effect_model_rmse))

RMSE_results %>% knitr::kable()

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

# Calculate RMSE of Movie-user effect model

movie_user_effect_rmse <- rmse(movie_user_effect_predicted_ratings, test_data$rating)
movie_user_effect_rmse

# Report the RMSE of Movie-User effect model to Table of results

RMSE_results <- bind_rows(RMSE_results,
                          data_frame(Model = "Movie-user effect model",  
                                     RMSE = movie_user_effect_rmse))

RMSE_results %>% knitr::kable()



############################################
# Regularized Movie-user effect model
############################################

# Set up a Lambdas vector 

lambdas <- seq(0, 10, 0.25)

# Build a function to run Movie-user effect model with different values of lambdas, and calculate their RMSE 

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


# Visualize the rmse and lambdas to observe the optimal value of lambda

qplot(lambdas, rmses)  


# Pick the optimal lambda
  
lambda <- lambdas[which.min(rmses)]
lambda


# Report the RMSE of the Movie-User effect model with the optimal lambda
  
RMSE_results <- bind_rows(RMSE_results,
                          data_frame(Model ="Regularized movie-user effect model",  
                                     RMSE = min(rmses)))
RMSE_results %>% knitr::kable()


############################################
############################################
##########################


```{r}
rm(list = ls(all.names = TRUE))
gc()
memory.limit()
memory.size()

library(tidyverse)
library(ranger)
library(rsample)
library(tictoc)
library(Metrics)
library(caret)
library(Rborist)

tic()

load("tidy_edx.RData")
load("tidy_validation.RData")

set.seed(1234)


subset_tidy_edx <- tidy_edx %>% group_by(userId) %>% mutate(count = n()) %>%
  filter(count >= 100) %>% ungroup(userId) %>% select(-count)


sample <- sample_n(subset_tidy_edx, 0.001 * nrow(tidy_edx))

rm(tidy_edx)
gc()

# set up 10-fold cross validation procedure
train_control <- trainControl(
  method = "cv", 
  number = 3
)

# train model
model_lm <- train(rating ~ movieId + userId + date_of_rating + genres + movie_year,
                  data = sample,
                  method = "brnn",
                  trControl = train_control
)



actual_rating <- tidy_validation$rating
predict_rating <- predict(model_lm, tidy_validation)

rmse(actual_rating, predict_rating)
#1.1015
#1405.8 sec elapsed
toc()

```


rm(list = ls(all.names = TRUE))
gc()
memory.limit()
memory.size()

library(tidyverse)
library(ranger)
library(rsample)
library(tictoc)
library(Metrics)
library(caret)
library(Rborist)

tic()

load("tidy_edx.RData")
load("tidy_validation.RData")

set.seed(1234)


subset_tidy_edx <- tidy_edx %>% group_by(userId) %>% mutate(count = n()) %>%
  filter(count >= 100) %>% ungroup(userId) %>% select(-count)


sample <- sample_n(subset_tidy_edx, 0.1 * nrow(tidy_edx))

rm(tidy_edx)
gc()

model_ranging <- ranger(formula = rating ~ movieId + userId + genres + date_of_rating,
                        data = sample,
                        mtry = 1, num.trees = 100, seed = 42)

actual_rating <- tidy_validation$rating

predict_rating <- predict(model_ranging, tidy_validation)$predictions

rmse(actual_rating, predict_rating)


toc()
#1.020131


```



## Random forest using "Ranger" package

```{r}
rm(list = ls(all.names = TRUE))
gc()
memory.limit()
memory.size()

library(tidyverse)
library(ranger)
library(rsample)
library(tictoc)
library(Metrics)
library(caret)
library(Rborist)

tic()

load("tidy_edx.RData")
load("tidy_validation.RData")

set.seed(1234)

subset_tidy_edx <- tidy_edx %>% group_by(userId) %>% mutate(count = n()) %>%
  filter(count >= 1000) %>% ungroup(userId) %>% select(-count)

sample <- sample_n(subset_tidy_edx, 0.1 * nrow(tidy_edx))
rm(subset_tidy_edx, tidy_edx)

model_ranging <- ranger(formula = rating ~.,
                        data = sample,
                        mtry = 4, num.trees = 100, seed = 42)


actual_rating <- tidy_validation$rating

predict_rating <- predict(model_ranging, tidy_validation)$predictions

rmse(actual_rating, predict_rating)

toc()
#1.052774
#2911.89 sec elapsed
```


```{r}
rm(list = ls(all.names = TRUE))
gc()
memory.limit()
memory.size()

library(tidyverse)
library(ranger)
library(rsample)
library(tictoc)
library(Metrics)
library(caret)
library(Rborist)

tic()

load("tidy_edx.RData")
load("tidy_validation.RData")

set.seed(1234)


subset_tidy_edx <- tidy_edx %>% group_by(userId) %>% mutate(count = n()) %>%
  filter(count >= 100) %>% ungroup(userId) %>% select(-count)


sample <- sample_n(subset_tidy_edx, 0.0001 * nrow(tidy_edx))

rm(tidy_edx)
gc()

# set up 10-fold cross validation procedure
train_control <- trainControl(
  method = "cv", 
  number = 3
)

# train model
model_lm <- train(rating ~ movieId + userId + date_of_rating + genres + movie_year,
                  data = sample,
                  method = "knn",
                  trControl = train_control
)



actual_rating <- tidy_validation$rating
predict_rating <- predict(model_lm, tidy_validation)

rmse(actual_rating, predict_rating)
#1.106196
#207.67 sec elapsed
toc()
```
```{r}
rm(list = ls(all.names = TRUE))
gc()
memory.limit()
memory.size()

library(tidyverse)
library(ranger)
library(rsample)
library(tictoc)
library(Metrics)
library(caret)
library(Rborist)

tic()

load("tidy_edx.RData")
load("tidy_validation.RData")

set.seed(1234)


subset_tidy_edx <- tidy_edx %>% group_by(movieId) %>% mutate(count = n()) %>%
  filter(count >= 1000) %>% ungroup(movieId) %>% select(-count)

test_index <- createDataPartition(subset_tidy_edx$rating, time = 1, p = 0.001, list = FALSE)

sample <- subset_tidy_edx %>% slice(test_index)


fitControl <- trainControl(method = "cv",
                           number = 5,
                           p = 0.8)


a <- train(rating ~ movieId + userId + genres + date_of_rating,
           data = sample,
           method = "brnn",
           trControl = fitControl,
           verbose= FALSE)

actual_rating <- tidy_validation$rating

predict_rating <- predict(a, tidy_validation)

rmse(actual_rating, predict_rating)


toc()
#1.045755
#85.42 sec elapsed
```


```{r}
rm(list = ls(all.names = TRUE))
gc()
memory.limit()
memory.size()

library(tidyverse)
library(ranger)
library(rsample)
library(tictoc)
library(Metrics)
library(caret)
library(Rborist)

tic()

load("tidy_edx.RData")
load("tidy_validation.RData")

set.seed(1234)


subset_tidy_edx <- tidy_edx %>% group_by(userId) %>% mutate(count = n()) %>%
  filter(count >= 100) %>% ungroup(userId) %>% select(-count)


sample <- sample_n(subset_tidy_edx, 0.001 * nrow(tidy_edx))

rm(tidy_edx)
gc()

# set up 10-fold cross validation procedure
train_control <- trainControl(
  method = "cv", 
  number = 3
)

# train model
model_lm <- train(rating ~ movieId + userId + date_of_rating + genres + movie_year,
                  data = sample,
                  method = "brnn",
                  trControl = train_control
)



actual_rating <- tidy_validation$rating
predict_rating <- predict(model_lm, tidy_validation)

rmse(actual_rating, predict_rating)
#1.034702
#67.05 sec elapsed
toc()

```

```{r}
rm(list = ls(all.names = TRUE))
gc()
memory.limit()
memory.size()

library(tidyverse)
library(ranger)
library(rsample)
library(tictoc)
library(Metrics)
library(caret)
library(Rborist)

tic()

load("tidy_edx.RData")
load("tidy_validation.RData")

set.seed(1234)


subset_tidy_edx <- tidy_edx %>% group_by(userId) %>% mutate(count = n()) %>%
  filter(count >= 100) %>% ungroup(userId) %>% select(-count)


sample <- sample_n(subset_tidy_edx, 0.01 * nrow(tidy_edx))

rm(tidy_edx)
gc()

# set up 10-fold cross validation procedure
train_control <- trainControl(
  method = "cv", 
  number = 3
)

# train model
model_lm <- train(rating ~ movieId + userId + date_of_rating + genres + movie_year,
                  data = sample,
                  method = "brnn",
                  trControl = train_control
)



actual_rating <- tidy_validation$rating
predict_rating <- predict(model_lm, tidy_validation)

rmse(actual_rating, predict_rating)


toc()
#1.03176
#1342.01
```

```{r}
rm(list = ls(all.names = TRUE))
gc()
memory.limit()
memory.size()

library(tidyverse)
library(ranger)
library(rsample)
library(tictoc)
library(Metrics)
library(caret)
library(Rborist)

tic()

load("tidy_edx.RData")
load("tidy_validation.RData")

set.seed(1234)


subset_tidy_edx <- tidy_edx %>% group_by(userId) %>% mutate(count = n()) %>%
  filter(count >= 100) %>% ungroup(userId) %>% select(-count)


sample <- sample_n(subset_tidy_edx, 0.0001 * nrow(tidy_edx))

rm(tidy_edx)
gc()

# set up 10-fold cross validation procedure
train_control <- trainControl(
  method = "cv", 
  number = 3
)

# train model
model_lm <- train(rating ~ movieId + userId + date_of_rating + genres + movie_year,
                  data = sample,
                  method = "qrnn",
                  trControl = train_control
)



actual_rating <- tidy_validation$rating
predict_rating <- predict(model_lm, tidy_validation)

rmse(actual_rating, predict_rating)
#1.055515
#632.33 sec elapsed

toc()

```


```{r Remove and free up working space, echo = FALSE}
rm(list = ls(all.names = TRUE))
gc()
memory.limit()
memory.size()
```

```{r}

sample <- sample_n(tidy_edx, 0.05 * nrow(tidy_edx))
```
### Create training and cross validation data sets
```{r}
#Create validation set
cv_split <- vfold_cv(sample, v = 5) #v: how many times the data should split
memory.size()
```

```{r Mapping train & validate}
#Mapping train & validate
cv_data <- cv_split %>%
  mutate(train = map(splits, ~training(.x)),
         validate = map(splits, ~testing(.x)))
memory.size()
```

```{r}
cv_data <- cv_data %>% mutate(validate_actual = map(validate, ~.x$rating))
```

```{r TUne the Hyper-Parameters}
#Tune the Hyper-Parameters
cv_tune <- cv_data %>% crossing(mtry = 1:6)
```

```{r Random forest model}
cv_models_tunerf <- cv_tune %>%
  mutate(model = map2(.x = train, .y = mtry, ~ranger(formula = rating ~.,
                                                     data = .x, mtry = .y,
                                                     num.trees = 100, seed = 42)))
# Generate validate predictions for each model
cv_prep_tunerf <- cv_models_tunerf %>% 
  mutate(validate_predicted = map2(.x = model, .y = validate, ~predict(.x, .y)$predictions))
```

```{r}

# Calculate validate RMSE for each fold and mtry combination
cv_eval_tunerf <- cv_prep_tunerf %>% 
  mutate(validate_rmse = map2_dbl(.x = validate_actual, .y = validate_predicted, ~ rmse(.x, .y)))

# Calculate the mean validate_mae for each mtry used  
cv_eval_tunerf %>% 
  group_by(mtry) %>% 
  summarise(mean_rmse = mean(validate_rmse))
```

```{r}
rm(list = ls(all.names = TRUE))
gc()
memory.limit()
memory.size()

library(tidyverse)
library(ranger)
library(rsample)
library(tictoc)
library(Metrics)
library(caret)
library(Rborist)

tic()

load("tidy_edx.RData")
load("tidy_validation.RData")

set.seed(1234)


subset_tidy_edx <- tidy_edx %>% group_by(userId) %>% mutate(count = n()) %>%
  filter(count >= 1000) %>% ungroup(userId) %>% select(-count)


sample <- sample_n(subset_tidy_edx, 0.01 * nrow(tidy_edx))

sample <- sample %>% mutate(rating = as.factor(rating))

# Create the upsampled training set

up_train <- upSample(x = select(sample, -rating),
                     y = sample$rating,
                     yname = "rating") %>% as_tibble()

# Count the number of each type of Remote employee
up_train %>%
  count(rating)

up_train <- up_train %>% mutate(rating = as.double(rating))

rm(subset_tidy_edx, tidy_edx)

model_ranging <- ranger(formula = rating ~.,
                        data = up_train,
                        mtry = 4, num.trees = 100, seed = 42)


actual_rating <- tidy_validation$rating

predict_rating <- predict(model_ranging, tidy_validation)$predictions

rmse(actual_rating, predict_rating)

toc()
#3.250245
#364.72 sec elapsed
```

```{r}
rm(list = ls(all.names = TRUE))
gc()
memory.limit()
memory.size()

library(tidyverse)
library(ranger)
library(rsample)
library(tictoc)
library(Metrics)
library(caret)
library(Rborist)

tic()

load("tidy_edx.RData")
load("tidy_validation.RData")

set.seed(1234)


subset_tidy_edx <- tidy_edx %>% group_by(userId) %>% mutate(count = n()) %>%
  filter(count >= 1000) %>% ungroup(userId) %>% select(-count)


sample <- sample_n(subset_tidy_edx, 0.001 * nrow(tidy_edx))

sample <- sample %>% mutate(rating = as.factor(rating))

# Create the upsampled training set

up_train <- upSample(x = select(sample, -rating),
                     y = sample$rating,
                     yname = "rating") %>% as_tibble()

# Count the number of each type of Remote employee
up_train %>%
  count(rating)

up_train <- up_train %>% mutate(rating = as.double(rating))

rm(subset_tidy_edx, tidy_edx)

model_ranging <- ranger(formula = rating ~.,
                        data = up_train,
                        mtry = 4, num.trees = 500, seed = 42)


actual_rating <- tidy_validation$rating

predict_rating <- predict(model_ranging, tidy_validation)$predictions

rmse(actual_rating, predict_rating)

toc()
#3.169494
#351.3 sec elapsed
```



```{r}
rm(list = ls(all.names = TRUE))
gc()
memory.limit()
memory.size()

library(tidyverse)
library(ranger)
library(rsample)
library(tictoc)
library(Metrics)
library(caret)
library(Rborist)

tic()

load("tidy_edx.RData")
load("tidy_validation.RData")

set.seed(1234)


subset_tidy_edx <- tidy_edx %>% group_by(userId) %>% mutate(count = n()) %>%
  filter(count >= 100) %>% ungroup(userId) %>% select(-count)


sample <- sample_n(subset_tidy_edx, 0.001 * nrow(tidy_edx))

rm(tidy_edx)
gc()

#Create validation set
cv_split <- vfold_cv(sample, v = 5) #v: how many times the data should split

#Mapping train & validate
cv_data <- cv_split %>%
  mutate(train = map(splits, ~training(.x)),
         validate = map(splits, ~testing(.x)))

#Tune the Hyper-Parameters
cv_tune <- cv_data %>% crossing(mtry = 1:6)

cv_models_tunerf <- cv_tune %>%
  mutate(model = map2(train, mtry, ~ ranger(formula = rating ~.,
                                            data = .x, mtry = .y, seed = 42)))

# Generate validate predictions for each model
cv_prep_tunerf <- cv_models_tunerf %>% 
  mutate(validate_actual = map(validate, ~.x$rating),
         validate_predicted = map2(.x = model, .y = validate, ~predict(.x, .y)$predictions))

# Calculate validate RMSE for each fold and mtry combination
cv_eval_tunerf <- cv_prep_tunerf %>% 
  mutate(validate_rmse = map2_dbl(.x = validate_actual, .y = validate_predicted, ~rmse(actual = .x, predicted = .y)))

# Calculate the mean validate_mae for each mtry used  
cv_eval_tunerf %>% 
  group_by(mtry) %>% 
  summarise(mean_rmse = mean(validate_rmse))




toc()
#482.6 sec elapsed
# 1.03
```


```{r}
rm(list = ls(all.names = TRUE))
gc()
memory.limit()
memory.size()

library(tidyverse)
library(ranger)
library(rsample)
library(tictoc)
library(Metrics)
library(caret)
library(Rborist)

tic()

load("tidy_edx.RData")
load("tidy_validation.RData")

set.seed(1234)


subset_tidy_edx <- tidy_edx %>% group_by(userId) %>% mutate(count = n()) %>%
  filter(count >= 100) %>% ungroup(userId) %>% select(-count)


sample <- sample_n(subset_tidy_edx, 0.1 * nrow(tidy_edx))

rm(tidy_edx)
gc()

model_ranging <- ranger(formula = rating ~.,
                        data = sample,
                        mtry = 1, num.trees = 100, seed = 42)

actual_rating <- tidy_validation$rating

predict_rating <- predict(model_ranging, tidy_validation)$predictions

rmse(actual_rating, predict_rating)


toc()

#[1] 0.9937927


#931.42 sec elapsed
```

```{r}
rm(list = ls(all.names = TRUE))
gc()
memory.limit()
memory.size()

library(tidyverse)
library(ranger)
library(rsample)
library(tictoc)
library(Metrics)
library(caret)
library(Rborist)

tic()

load("tidy_edx.RData")
load("tidy_validation.RData")

set.seed(1234)


subset_tidy_edx <- tidy_edx %>% group_by()
group_by(movieId) %>% mutate(count = n()) %>%
  filter(count >= 1000) %>% ungroup(movieId) %>% select(-count) %>%
  group_by(userId) %>% mutate(count = n()) %>% filter(count >= 100) %>% ungroup(userId) %>% select(-count)

test_index <- createDataPartition(subset_tidy_edx$rating, time = 1, p = 0.1, list = FALSE)

sample <- subset_tidy_edx %>% slice(test_index)


model_ranging <- ranger(formula = rating ~.,
                        data = sample,
                        mtry = 1, num.trees = 100, seed = 42)

actual_rating <- tidy_validation$rating

predict_rating <- predict(model_ranging, tidy_validation)$predictions

rmse(actual_rating, predict_rating)



toc()
#1.02059
#709.98 sec elapsed
```

```{r}
rm(list = ls(all.names = TRUE))
gc()
memory.limit()
memory.size()

library(tidyverse)
library(ranger)
library(rsample)
library(tictoc)
library(Metrics)
library(caret)
library(Rborist)

tic()

load("tidy_edx.RData")
load("tidy_validation.RData")

set.seed(1234)


subset_tidy_edx <- tidy_edx %>% group_by(movieId) %>% mutate(count = n()) %>%
  filter(count >= 1000) %>% ungroup(movieId) %>% select(-count)

test_index <- createDataPartition(subset_tidy_edx$rating, time = 1, p = 0.01, list = FALSE)

sample <- subset_tidy_edx %>% slice(test_index)


model_ranging <- ranger(formula = rating ~.,
                        data = sample,
                        mtry = 1, num.trees = 300, seed = 42)

actual_rating <- tidy_validation$rating

predict_rating <- predict(model_ranging, tidy_validation)$predictions

rmse(actual_rating, predict_rating)


toc()
#1.02266
#247.42 sec elapsed
```

```{r}
rm(list = ls(all.names = TRUE))
gc()
memory.limit()
memory.size()

library(tidyverse)
library(ranger)
library(rsample)
library(tictoc)
library(Metrics)
library(caret)
library(Rborist)

tic()

load("tidy_edx.RData")
load("tidy_validation.RData")

set.seed(1234)


subset_tidy_edx <- tidy_edx %>% group_by(movieId) %>% mutate(count = n()) %>%
  filter(count >= 1000) %>% ungroup(movieId) %>% select(-count)

test_index <- createDataPartition(subset_tidy_edx$rating, time = 1, p = 0.001, list = FALSE)

sample <- subset_tidy_edx %>% slice(test_index)


#Create validation set
cv_split <- vfold_cv(sample, v = 5) #v: how many times the data should split

#Mapping train & validate
cv_data <- cv_split %>%
  mutate(train = map(splits, ~training(.x)),
         validate = map(splits, ~testing(.x)))

#Tune the Hyper-Parameters
cv_tune <- cv_data %>% crossing(mtry = 1:6)

cv_models_tunerf <- cv_tune %>%
  mutate(model = map2(train, mtry, ~ ranger(formula = rating ~.,
                                            data = .x,
                                            mtry = .y,
                                            num.trees = 300,
                                            seed = 42)))

# Generate validate predictions for each model
cv_prep_tunerf <- cv_models_tunerf %>% 
  mutate(validate_actual = map(validate, ~.x$rating),
         validate_predicted = map2(.x = model, .y = validate, ~predict(.x, .y)$predictions))

# Calculate validate RMSE for each fold and mtry combination
cv_eval_tunerf <- cv_prep_tunerf %>% 
  mutate(validate_rmse = map2_dbl(.x = validate_actual, .y = validate_predicted, ~rmse(actual = .x, predicted = .y)))

# Calculate the mean validate_mae for each mtry used  
cv_eval_tunerf %>% 
  group_by(mtry) %>% 
  summarise(mean_rmse = mean(validate_rmse))

#   mtry mean_rmse
#  <int>     <dbl>
#1     1      1.01
#2     2      1.02
#3     3      1.02
#4     4      1.02
#5     5      1.02
#6     6      1.02

#> toc()
#235.86 sec elapsed
toc()

```


```{r}
rm(list = ls(all.names = TRUE))
gc()
memory.limit()
memory.size()

library(tidyverse)
library(ranger)
library(rsample)
library(tictoc)
library(Metrics)
library(caret)
library(Rborist)

tic()

load("tidy_edx.RData")
load("tidy_validation.RData")

set.seed(1234)


subset_tidy_edx <- tidy_edx %>% group_by(movieId) %>% mutate(count = n()) %>%
  filter(count >= 1000) %>% ungroup(movieId) %>% select(-count)

test_index <- createDataPartition(subset_tidy_edx$rating, time = 1, p = 0.001, list = FALSE)

sample <- subset_tidy_edx %>% slice(test_index)


fitControl <- trainControl(method = "cv",
                           number = 5,
                           p = 0.8)


a <- train(rating ~ movieId + userId + genres + date_of_rating,
           data = sample,
           method = "knn",
           trControl = fitControl,
           verbose= FALSE)

actual_rating <- tidy_validation$rating

predict_rating <- predict(a, tidy_validation)

rmse(actual_rating, predict_rating)


toc()
#1.103557
#1435.5 sec elapsed
```

```{r}
rm(list = ls(all.names = TRUE))
gc()
memory.limit()
memory.size()

library(tidyverse)
library(ranger)
library(rsample)
library(tictoc)
library(Metrics)
library(caret)
library(Rborist)

tic()

load("edx.RData")
load("validation.RData")

load("tidy_edx.RData")
load("tidy_validation.RData")

set.seed(1234)


subset_tidy_edx <- tidy_edx %>% group_by(movieId) %>% mutate(count = n()) %>%
  filter(count >= 1000) %>% ungroup(movieId) %>% select(-count) %>%
  group_by(userId) %>% mutate(count = n()) %>% filter(count >= 100) %>% ungroup(userId) %>% select(-count)

test_index <- createDataPartition(subset_tidy_edx$rating, time = 1, p = 0.1, list = FALSE)

sample <- subset_tidy_edx %>% slice(test_index)

sample1 <- sample %>% group_by(movieId) %>% summarize(n_genres = n_distinct(genres))

sample2 <- sample %>% group_by(movieId) %>% 
  mutate(n_genres = n_distinct(genres)) %>% 
  ungroup(movieId)

model_ranging <- ranger(formula = rating ~ userId + movieId + date_of_rating + movie_year + n_genres,
                        data = sample2,
                        mtry = 1, num.trees = 100, seed = 42)

tidy_validation <- tidy_validation %>% group_by(movieId) %>% 
  mutate(n_genres = n_distinct(genres)) %>% 
  ungroup(movieId)

actual_rating <- tidy_validation$rating


predict_rating <- predict(model_ranging, tidy_validation)$predictions

rmse(actual_rating, predict_rating)



toc()
0.9903912
```


Exploring the affect of number of movies in a single period on average rating of that interval.
```{r}

test <- tidy_edx %>% mutate(date = round_date(date_of_rating, unit = "year")) %>%
  group_by(date) %>% nest() %>% mutate(n_movie = map(data, ~n_distinct(.x$movieId)), 
                                       mean_rating = map(data, ~mean(.x$rating)))
a <- test %>% unnest(n_movie,mean_rating)

a %>% ggplot(aes(date, mean_rating, size = n_movie)) +
  geom_point(alpha = 0.1) +
  scale_size(name  = "Number of movies") +
  geom_smooth() + 
  theme_light()
```

```{r}

test <- tidy_edx %>% mutate(date = round_date(date_of_rating, unit = "week")) %>%
  group_by(date) %>% nest() %>% mutate(n_movie = map(data, ~n_distinct(.x$movieId)), 
                                       mean_rating = map(data, ~mean(.x$rating)))
a <- test %>% unnest(n_movie,mean_rating)

a %>% ggplot(aes(date, mean_rating, size = n_movie)) +
  geom_point(alpha = 0.1) +
  scale_size(name  = "Number of movies") +
  geom_smooth() + 
  theme_light()

```



```{r}
a %>% ggplot(aes(y= mean_rating,x =  n_movie)) +
  geom_point(alpha = 0.1) +
  geom_smooth() + 
  theme_light()
```

# Validate the dataset
## Remove and free up working space

```{r Remove and free up working space, echo = FALSE}

rm(list = ls(all.names = TRUE))
gc()
memory.limit()
memory.size()

```

## Load necessary packages

```{r Necessary packages, echo = FALSE}

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")

```

## Load "validation" dataset

```{r}
load("validation.RData")
```

## Tidy up the "validation" dataset.

```{r Transform "timestamp" to date variable using "lubridate" package, echo = FALSE}

#Separate variables in "genres", and put them into "genres" column gain
validation_sep_genres <- validation %>% separate_rows(genres, sep = "\\|")

#Parse date to "timestamp" variable
validation_date <- validation_sep_genres %>% mutate(date_of_rating = as_datetime(timestamp)) %>%
  select(c("userId", "movieId", "rating", "title", "genres", "date_of_rating"))

```

```{r Separate "title" column into "title" and "movie_year", echo = FALSE}

#Select object "year" in the "title" column, and create "movie_year" variable
#Detect "year" in the title column
tidy_validation <- validation_date %>% 
  mutate(movie_year = str_extract(validation_date$title, regex("\\(\\d{4}\\)"))) 

tidy_validation <- tidy_validation %>%
  mutate(movie_year = str_extract(tidy_validation$movie_year, regex("\\d{4}")))

```

```{r}

tidy_validation <- tidy_validation %>%
  mutate(movie_year = as.integer(movie_year), movieId = as.integer(movieId))

```

Save tidy_edx to the local system.
```{r Save "tidy_validation", echo = FALSE}

save(tidy_validation, file = "tidy_validation.RData")

```

Remove unnecessary dataset "tidy_edx_date", "tidy_edx_sep_genres"

```{r}
rm("validation_date", "validation_sep_genres")

gc()
```

library(tidyverse)
library(caret)

set.seed(1)
n <- 100
Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))
y <- dat$y
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
train_set <- dat %>% sclice(test_index)
test_set <- dat %>% sclice(-test_index)


# Create Test set and Train set for the learning
rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects
gc() #free up memory and report the memory usage

library(tidyverse)
library(caret)

set.seed(1)
n <- 100
Sigma <- 9*matrix(c(1.0, 0.95, 0.95, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))
y <- dat$y


set.seed(1)
rmse_vals<-replicate(100, {
  test_index <- createDataPartition(dat$y, p = .5,
                                    list = FALSE,
                                    times = 1)
  train <- dat%>%slice(-test_index)
  test  <- dat%>%slice(test_index)
  fit<-lm(y~x, data = train)
  y_hat<-predict(fit, newdata = test)
  RMSE(y_hat, test$y)
})

head(rmse_vals)
mean(rmse_vals)
sd(rmse_vals)

#---------
rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects
gc() #free up memory and report the memory usage

library(tidyverse)
library(caret)

set.seed(1)
n <- c(100, 500, 1000, 5000, 10000)
Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = n, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

dat <- map(n, (function(n) {MASS::mvrnorm(n,c(69, 69), Sigma) %>% data.frame() %>% setNames(c("x", "y"))}))
dat100 <- dat[[1]]
dat500 <- dat[[2]]
dat1000 <- dat[[3]]
dat5000 <- dat[[4]]
dat10000 <- dat[[5]]
dattotal <- c(dat100, dat500, dat1000, dat5000, dat10000)

set.seed(1)
rmse_vals <- replicate(100, {
  test_index <- createDataPartition(dat100$y, p = .5,
                                    list = FALSE,
                                    times = 1)
  train <- dat100%>%slice(-test_index)
  test  <- dat100%>%slice(test_index)
  fit<-lm(y~x, data = train)
  y_hat<-predict(fit, newdata = test)
  RMSE(y_hat, test$y)
})

head(rmse_vals)
mean(rmse_vals)
sd(rmse_vals)

set.seed(1)
rmse_vals <-map(dattotal, function(dat_total) {
  replicate(100, {
    test_index <- createDataPartition(dat_total$y, p = .5,
                                      list = FALSE,
                                      times = 1)
    train <- dat_total%>%slice(-test_index)
    test  <- dat_total%>%slice(test_index)
    fit<-lm(y~x, data = train)
    y_hat<-predict(fit, newdata = test)
    RMSE(y_hat, test$y)
  })}
)

#-----------------------
rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects
gc() #free up memory and report the memory usage

myRMSE <- function(n) {
  Sigma <- 9*matrix(c(1.0,0.5,0.5,1),2,2)
    dat<- MASS::mvrnorm(n = n,c(69,69),Sigma) %>%
    data.frame() %>% setNames(c("x","y"))
    
    RMSE<- replicate(n=100, {
    test_index<-createDataPartition(dat$y,times=1,p=0.5,list=FALSE)
    train_set<-dat%>%slice(-test_index)
    test_set<-dat%>%slice(test_index)
    fit<-lm(y~x,data=train_set)
    y_hat<-predict(fit,test_set)
    sqrt(mean((y_hat-test_set$y)^2))
    })
  list(mean(RMSE),sd(RMSE))
  
}
n <- c(100,500,1000,5000,10000)

set.seed(1)
map(n,myRMSE)

#------------------
rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects
gc() #free up memory and report the memory usage

library(tidyverse)
library(caret)

set.seed(1)
n <- 1000
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))

test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
train_set  <- dat %>% slice(test_index)
test_set <- dat %>% slice(-test_index)

fit1 <- train_set %>% lm(y ~ x_1, data = .)
y_hat1 <- predict(fit1, test_set)
RMSE1 <- sqrt(mean((y_hat1 - test_set$y)^2))
RMSE1

fit2 <- train_set %>% lm(y ~ x_2, data = .)
y_hat2 <- predict(fit2, test_set)
RMSE2 <- sqrt(mean((y_hat2 - test_set$y)^2))
RMSE2

fit12 <- train_set %>% lm(y ~ x_1 + x_2, data = .)
y_hat12 <- predict(fit12, test_set)
RMSE12 <- sqrt(mean((y_hat12 - test_set$y)^2))
RMSE12

#-----------------------
rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects
gc() #free up memory and report the memory usage

library(tidyverse)
library(caret)

set.seed(1)
n <- 1000
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.95, 0.75, 0.95, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))

test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
train_set  <- dat %>% slice(test_index)
test_set <- dat %>% slice(-test_index)

fit1 <- train_set %>% lm(y ~ x_1, data = .)
y_hat1 <- predict(fit1, test_set)
RMSE1 <- sqrt(mean((y_hat1 - test_set$y)^2))
RMSE1

fit2 <- train_set %>% lm(y ~ x_2, data = .)
y_hat2 <- predict(fit2, test_set)
RMSE2 <- sqrt(mean((y_hat2 - test_set$y)^2))
RMSE2

fit12 <- train_set %>% lm(y ~ x_1 + x_2, data = .)
y_hat12 <- predict(fit12, test_set)
RMSE12 <- sqrt(mean((y_hat12 - test_set$y)^2))
RMSE12

#----------------------
rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects
gc() #free up memory and report the memory usage

library(tidyverse)
library(caret)

set.seed(2)
make_data <- function(n = 1000, p = 0.5, 
                      mu_0 = 0, mu_1 = 2, 
                      sigma_0 = 1,  sigma_1 = 1){
  
  y <- rbinom(n, 1, p)
  f_0 <- rnorm(n, mu_0, sigma_0)
  f_1 <- rnorm(n, mu_1, sigma_1)
  x <- ifelse(y == 1, f_1, f_0)
  
  test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
  
  list(train = data.frame(x = x, y = as.factor(y)) %>% slice(-test_index),
       test = data.frame(x = x, y = as.factor(y)) %>% slice(test_index))
}
dat <- make_data()

dat$train %>% ggplot(aes(x, color = y)) + geom_density()

set.seed(1)
delta <- seq(0, 3, len = 25)
res <- sapply(delta, function(d){
  dat <- make_data(mu_1 = d)
  fit_glm <- dat$train %>% glm(y ~ x, family = "binomial", data = .)
  y_hat_glm <- ifelse(predict(fit_glm, dat$test) > 0.5, 1, 0) %>% factor(levels = c(0, 1))
  mean(y_hat_glm == dat$test$y)
})
qplot(delta, res)


fit <- dat[[1]] %>% glm(y ~ x, data = ., family ="binomial")
p_hat <- predict(fit, newdata = dat[[2]])
y_hat <- factor(ifelse(p_hat > 0.5, 1, 0))
confusionMatrix(data = y_hat, reference = dat$test$y)

#------------------------
rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects
gc() #free up memory and report the memory usage

library(tidyverse)
library(purrr)
library(pdftools)
library(lubridate)

fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf", package="dslabs")

dat <- map_df(str_split(pdf_text(fn), "\n"), function(s){
  s <- str_trim(s)
  header_index <- str_which(s, "2015")[1]
  tmp <- str_split(s[header_index], "\\s+", simplify = TRUE)
  month <- tmp[1]
  header <- tmp[-1]
  tail_index  <- str_which(s, "Total")
  n <- str_count(s, "\\d+")
  out <- c(1:header_index, which(n==1), which(n>=28), tail_index:length(s))
  s[-out] %>%
    str_remove_all("[^\\d\\s]") %>%
    str_trim() %>%
    str_split_fixed("\\s+", n = 6) %>%
    .[,1:5] %>%
    as_data_frame() %>% 
    setNames(c("day", header)) %>%
    mutate(month = month,
           day = as.numeric(day)) %>%
    gather(year, deaths, -c(day, month)) %>%
    mutate(deaths = as.numeric(deaths))
}) %>%
  mutate(month = recode(month, "JAN" = 1, "FEB" = 2, "MAR" = 3, "APR" = 4, "MAY" = 5, "JUN" = 6, 
                        "JUL" = 7, "AGO" = 8, "SEP" = 9, "OCT" = 10, "NOV" = 11, "DEC" = 12)) %>%
  mutate(date = make_date(year, month, day)) %>%
  filter(date <= "2018-05-01" & !is.na(deaths))
names(dat)
head(dat)
tail(dat)

total_days <- diff(range(as.numeric(dat$date)))
typeof(total_days)
total_days

span = 60/total_days
span


fit <- loess(deaths ~ as.numeric(date), degree = 1, span = span, data = dat)

dat %>% mutate(smooth = fit$fitted) %>%
  ggplot(aes(date, deaths)) +
  geom_point(size = 3, alpha = .5, color = "grey") +
  geom_line(aes(date, smooth), lwd = 2, color = "red")


dat %>% 
  mutate(smooth = predict(fit, as.numeric(date)), day = yday(date), year = as.character(year(date))) %>%
  ggplot(aes(day, smooth, col = year)) +
  geom_line(lwd = 2)

#----------------------------
rm(list = ls(all.names = TRUE))
gc()

library(tidyverse)
library(dslabs)
library(broom)

mnist_27$train %>% glm(y ~ x_2, family = "binomial", data = .) %>%
  tidy()

qplot(x_2, y, data = mnist_27$train)

head(mnist_27$train)

fit <- mnist_27$train %>% loess(as.numeric(y) ~ x_2, degree = 1, data = .)

mnist_27$train %>% mutate(smooth = fit$fitted) %>%
  ggplot(aes(x_2, y)) +
  geom_point(size = 3, alpha = .5, color = "grey") +
  geom_line(aes(x_2, smooth), lwd = 2, color = "red")

mnist_27$train %>% 
  mutate(y = ifelse(y=="7", 1, 0)) %>%
  ggplot(aes(x_2, y)) + 
  geom_smooth(method = "loess")

grey_area <- x > 50 & x < 205
grey_area/nrow(mnist)

a <- rowMeans(dat$train$images>50 & dat$train$images<205)
mean(a)
b <- dat$train$images>50 & dat$train$images<205
mean(b)

boxplot(rowMeans(dat$train$images)~dat$train$labels)
qplot(as.factor(dat$train$labels), a, geom = "boxplot") # This one was generated by ggplot which was better than the base in the previous one


########################
library(dslabs)
library(tidyverse)
library(caret)
library(lubridate)

data("movielens")
head(movielens) 
#Compute the number of ratings for each movie and then plot it against the year the movie came out. Use the square root transformation on the counts.
#What year has the highest median number of ratings?
movielens %>% group_by(year) %>% summarize(n_rating = sqrt(sum(rating))) %>% arrange(desc(n_rating)) + ggplot(aes(x = year, y = n_rating)) +
  geom_point()

movielens %>% group_by(movieId) %>%
  summarize(n = n(), year = as.character(first(year))) %>%
  qplot(year, n, data = ., geom = "boxplot") +
  coord_trans(y = "sqrt") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Question 2: We see that, on average, movies that came out after 1993 get more ratings. We also see that with newer movies, starting in 1993, the number of ratings decreases with year: the more recent a movie is, the less time users have had to rate it.

# Among movies that came out in 1993 or later, what are the 25 movies with the most ratings per year, and what is the average rating of each of the top 25 movies?

#  What is the average rating for the movie The Shawshank Redemption?
a <- movielens %>% filter(year >= 1993) %>% group_by(userId) %>% summarize(n_rating = sum(rating), title = first(title)) %>% arrange(desc(n_rating, 25)) 
movielens %>% filter(year >= 1993) %>% group_by(title) %>% summarize(count = n()) %>% arrange(desc(count))

movielens %>% filter(year >= 1993 & title == "Shawshank Redemption, The") %>% group_by(title) %>% summarize(avg_rating = sum(rating)/n())
movielens %>% mutate(date = year(timestamp))

last_year <- movielens %>% 
  filter(title == unique(grep('Forrest Gump', movielens$title, ignore.case = TRUE, value= TRUE))) %>% 
  arrange(desc(timestamp)) %>% 
  slice(1) %>% 
  select(timestamp) %>% 
  as.numeric() %>% 
  as.POSIXct(origin = "1970-01-01") %>% 
  year()
b <- movielens %>% mutate(date = as.numeric(timestamp)) %>% mutate(date = as.POSIXct(date, origin = "1970-01-01")) %>%
  mutate(date = year(date))
c <- b %>% filter(title == unique(grep('Forrest Gump', movielens$title, ignore.case = TRUE, value= TRUE))) 
n_distinct(c$date)

movielens %>% 
  filter(year >= 1993) %>%
  group_by(movieId) %>%
  summarize(n = n(), years = 2018 - first(year),
            title = title[1],
            rating = mean(rating)) %>%
  mutate(rate = n/years) %>%
  top_n(25, rate) %>%
  arrange(desc(rate)) %>%
  ggplot(aes(x = rate, y = rating)) +
  geom_point() +
  coord_flip()

movielens %>% 
  filter(year >= 1993) %>%
  group_by(movieId) %>%
  summarize(n = n(), years = 2018 - first(year),
            title = title[1],
            rating = mean(rating)) %>%
  mutate(rate = n/years) %>%
  ggplot(aes(rate, rating)) +
  geom_point() +
  geom_smooth()

movielens <- movielens %>% mutate(date = as_datetime(timestamp)) %>% 
  select(movieId, title, year, genres, userId, rating, date) 

movielens %>% mutate(rounddate = round_date(movielens$date, "week")) %>% group_by(rounddate) %>%
  summarize(avg_rating = sqrt(mean(rating))) %>% ggplot(aes(x = rounddate, y = avg_rating)) +
  geom_point()

movielens %>% mutate(date = round_date(date, unit = "week")) %>%
  group_by(date) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(date, rating)) +
  geom_point() +
  geom_smooth()

movielens %>% group_by(genres) %>% summarize(n_rating = n(), avg_rating = mean(rating), se_rating = sd(rating)) %>%
  filter(n_rating >= 1000) %>%
  arrange(avg_rating)

movielens %>% group_by(genres) %>% 
  summarize(n_rating = n(), avg_rating = mean(rating), se_rating = sd(rating)) %>%
  filter(n_rating >= 1000) %>% arrange(desc(n_rating)) %>% 
  ggplot (aes(x=genres, y=avg_rating)) +
  geom_point() +
  geom_errorbar(aes(ymin=avg_rating - se_rating, ymax=avg_rating + se_rating), width=0.4, colour="red", alpha=0.8, size=1.3) +
  geom_bar(stat="identity", fill="skyblue", alpha=0.7) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

movielens %>% group_by(genres) %>%
  summarize(n = n(), avg = mean(rating), se = sd(rating)/sqrt(n())) %>%
  filter(n >= 1000) %>% 
  mutate(genres = reorder(genres, avg)) %>%
  ggplot(aes(x = genres, y = avg, ymin = avg - 2*se, ymax = avg + 2*se)) + 
  geom_point() +
  geom_errorbar() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

############# REGULATION ########################
# The exercises in Q1-Q8 work with a simulated dataset for 100 schools. This pre-exercise setup walks you through the code needed to simulate the dataset.
# An education expert is advocating for smaller schools. The expert bases this recommendation on the fact that among the best performing schools, many are small schools. Let's simulate a dataset for 100 schools. First, let's simulate the number of students in each school, using the following code:

set.seed(1986)
n <- round(2^rnorm(1000, 8, 1))

# Now let's assign a true quality for each school that is completely independent from size. This is the parameter we want to estimate in our analysis. The true quality can be assigned using the following code:
set.seed(1)
mu <- round(80 + 2*rt(1000, 5))
range(mu)
schools <- data.frame(id = paste("PS",1:100),
                      size = n,
                      quality = mu,
                      rank = rank(-mu))

# We can see the top 10 schools using this code: 

schools %>% top_n(10, quality) %>% arrange(desc(quality))

# Now let's have the students in the school take a test. There is random variability in test taking, so we will simulate the test scores as normally distributed with the average determined by the school quality with a standard deviation of 30 percentage points. This code will simulate the test scores:

set.seed(1)
scores <- sapply(1:nrow(schools), function(i){
  scores <- rnorm(schools$size[i], schools$quality[i], 30)
  scores
})
schools <- schools %>% mutate(score = sapply(scores, mean))

# Question 1: What are the top schools based on the average score? Show just the ID, size, and the average score.
# Report the ID of the top school and average score of the 10th school.
# What is the ID of the top school?

schools %>% group_by(id) %>% summarize(size = size[1], avg_score = sum(score)/n()) %>%
  top_n(10, avg_score) %>% arrange(desc(avg_score))

schools %>% group_by(id) %>% summarize(size = size[1], med_score = median(score)) %>% arrange(desc(med_score))
schools %>% group_by(id) %>% mutate(med_size = median(size)) %>% top_n(10, med_score) %>% arrange(desc(med_score))
schools %>% top_n(10, score) %>% arrange(desc(score))
median(schools$size)                          
schools %>% top_n(10, score) %>% summarize( med = median(size))
median(schools$size)
schools %>% top_n(-10, score) %>% .$size %>% median()

schools %>% ggplot(aes(size, score)) +
  geom_point(alpha = 0.5) +
  geom_point(data = filter(schools, rank<=10), col = 2) +
  geom_smooth()

schools %>% ggplot(aes(size, score)) +
  geom_point(alpha = 0.5) +
  scale_x_log10() +
  geom_point(data = filter(schools, rank<=10), col = 2) +
  geom_smooth()

overall <- mean(sapply(scores, mean))
schools %>% group_by(id) %>% summarize(reg = (mean(score) - overall)/sqrt(mean(size)+25)) %>% arrange(desc(reg))
schools %>% mutate( reg_score = overall + ( (score - overall) * size / (size + 25) ) ) %>% arrange(desc(reg_score))

rmse <- function(quality, estimate){1/100 * sum(quality - estimate)^2}

a <- seq(1:100)
ab <- apply(a, function(z){
  c <- schools %>% mutate( reg = overall + ( (score - overall) * size / (size + z) ) ) 
  b <- rmse(a$quality, a$reg)
})

b %>% ggplot(aes(a, rmse)) +
  geom_point()

schools %>% mutate( reg = overall + ( (score - overall) * size / (size + 25) ) ) %>% 
  select(reg) %>%
  arrange(desc(reg))

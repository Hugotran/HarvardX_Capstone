rm(list = ls(all.names = TRUE))
gc()


library(dslabs)
data("tissue_gene_expression")
dim(tissue_gene_expression$x)
table(tissue_gene_expression$y)
class(tissue_gene_expression)
summary(tissue_gene_expression)
head(tissue_gene_expression$y)
d <- dist(tissue_gene_expression)
head(d)
class(d)

ind <- c(1, 2, 39, 40, 73, 74)
as.matrix(d)[ind,ind]

image(as.matrix(d))
image(d[ind])
?tissue_gene_expression
class(tissue_gene_expression$y)
head(tissue_gene_expression$y)

#--------------------------
rm(list = ls(all.names = TRUE))
gc()


library(dslabs)
library(tidyverse)
library(caret)
library(e1071)


data("heights")
dim(heights)
class(heights)
class(heights$sex)
ks <- seq(1, 251, 3)

F_1 <- map_df(ks, function(k){

    set.seed(1)
  test_index <- createDataPartition(heights$sex, time = 1, p = 0.5, list = FALSE)
  train_set <- heights %>% slice(test_index)
  test_set <- heights %>% slice(-test_index)
  fit <- knn3(sex ~., data = train_set, k = k)
  y_hat <- predict(fit, test_set, type = "class")
  test_error <- confusionMatrix(data = y_hat,
                                reference = test_set$sex)$byClass["F1"]
  y_hat <- predict(fit, train_set, type = "class")
  train_error <- confusionMatrix(data = y_hat, reference =  train_set$sex)$byClass["F1"]
  
  list(k = k, test = test_error, train = train_error)
  
})
F_1 %>% arrange(desc(test))


#--------------------------
rm(list = ls(all.names = TRUE))
gc()

using_package <- c("dslabs", "tidyverse", "caret", "e1071")
lapply(using_package, library, character.only = TRUE)

data("tissue_gene_expression")

k_test <- seq(1, 11, 2)
acc_knn <- map_df(k_test, function(k){
  set.seed(1)
  test_index <- createDataPartition(tissue_gene_expression$y, time = 1, p = 0.5, list = FALSE)
  train_set_x <- tissue_gene_expression$x[test_index,]
  train_set_y <- tissue_gene_expression$y[test_index]
  test_set_x <- tissue_gene_expression$x[-test_index,]
  test_set_y <- tissue_gene_expression$y[-test_index]
  
  fit_knn <- knn3(train_set_x, train_set_y, k = k)
  y_hat_knn <- predict(fit_knn, test_set_x, type = "class")
  y_hat_knn
  cm <- confusionMatrix(y_hat_knn, test_set_y)$overall["Accuracy"]
  list(k = k, acc = cm)
})
acc_knn


#--------------------- OR
set.seed(1)
library(caret)
y <- tissue_gene_expression$y
x <- tissue_gene_expression$x
train_index <- createDataPartition(y, list = FALSE)
sapply(seq(1, 11, 2), function(k){
  fit <- knn3(x[train_index,], y[train_index], k = k)
  y_hat <- predict(fit, newdata = data.frame(x=x[-train_index,]),
                   type = "class")
  mean(y_hat == y[-train_index])
})
##-----------------------CROSS VALIDATION
rm(list = ls(all.names = TRUE))
gc()

using_package <- c("tidyverse", "caret")
lapply(using_package, library, character.only = TRUE)

set.seed(1996)
n <- 1000
p <- 10000
x <- matrix(rnorm(n*p), n, p)
colnames(x) <- paste("x", 1:ncol(x), sep = "_")
y <- rbinom(n, 1, 0.5) %>% factor()

x_subset <- x[ ,sample(p, 100)]

library(devtools)
if (!requireNamespace("BiocManager"))
  install.packages("BiocManager")
BiocManager::install()
BiocManager::install("genefilter")

library(genefilter)
tt <- colttests(x, y)
head(tt)
wide(tt)

pvals <- tt$p.value
pvals
class(pvals)

ind <- pvals %>% as_tibble() %>% filter(pvals <= 0.01)
ind
ind <- which(pvals <= 0.01)

x_subset <- x[,ind]
dim(x_subset)
fit <- train(x_subset, y, method = "glm")
fit$results

fit <- train(x_subset, y, method = "knn", tuneGrid = data.frame(k = seq(101, 301, 25)))
ggplot(fit)

library(dslabs)
data("tissue_gene_expression")

train(tissue_gene_expression$x, tissue_gene_expression$y, method = "knn", tuneGrid = data.frame(k = seq(5,205,2)))


#--------------- OR
pred <- sapply(fits, function(object) 
  predict(object, newdata = mnist_27$test)
  
)
dim(pred)

acc <- colMeans(pred == mnist_27$test$y)
acc
mean(acc)

#-------------------

names(which.max(table(yhats[num, ])))
set.seed(1995)
indexes <- createResample(mnist_27$train$y, 10)
indexes[["Resample01"]]

#-----------
set.seed(1995)
indexes <- createResample(mnist_27$train$y, 10)

names(indexes)
sum(indexes[[1]] == 3)
sum(indexes[[1]] == 7)
sum(indexes[[1]] == 4)

k <- c(1:10)
a <- map_dfr(indexes, function(k){
  sum(k == 3)
})

a <- map_dbl(k, function(x){
  sum(indexes[[x]] == 3)
})
sum(a)

rm(list = ls(all.names = TRUE))
gc()

set.seed(1)
y <- rnorm(100, 0, 1)
mean(y)
sd(y)

B <- 10000
sim <- replicate(B, {
  y <- rnorm(100,0,1)
  quantile(y, 0.75)})
mean(sim)
sd(sim)
sim

set.seed(1)
indexes <- createResample(y,10)
quantile_boot <- map_dbl(indexes, ~ quantile(., 0.75))
mean(quantile_boot)

sd_boot <- map_dbl(indexes, sd)
mean(sd_boot)

rm(list = ls(all.names =  TRUE))
gc()

library(tidyverse)
set.seed(1)
y <- rnorm(100,0,1)
set.seed(1)
indexes <- createResample(y, 10000)
q_75_star <- map_dbl(indexes, function(ind){
  y_star <- y[ind]
  quantile(y_star, 0.75)
})
mean(q_75_star)
sd(q_75_star)

#-------------------------------------------
rm(list = ls(all.names =  TRUE))
gc()

library(tidyverse)
library(rpart)
library(caret)
n <- 1000
sigma <- 0.25
x <- rnorm(n, 0, 1)
names(x) <- "predictor"
y <- 0.75 * x + rnorm(n, 0, sigma) %>% as_tibble()
dat <- data.frame(x = x, y = y)
fit <- rpart(y ~ ., data = dat)                                              
fit
summary(fit)
plot(fit)
text(fit)
dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col=2)


library(randomForest)
library(Rborist)
minNode <- seq(25, 100, 25)
fit <- train(y ~ ., method = "Rborist",   
             tuneGrid = data.frame(predFixed = 1, 
                                   minNode = seq(25, 100, 25)),
             data = dat)
ggplot(fit) 



dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col = 2)
  
set.seed(1991)
data("tissue_gene_expression")
fit_rpart <- train(tissue_gene_expression$x, tissue_gene_expression$y, method = "rf",   
                   tuneGrid = data.frame(mtry = seq(50, 200, 25)), nodesize = 1)
ggplot(fit_rpart)
imp <- varImp(fit_rpart)
imp
tree_terms <- as.character(unique(fit_rpart$finalModel$frame$var[!(fit_rpart$finalModel$frame$var == "<leaf>")]))
tree_terms
a <- data_frame(term = rownames(imp$importance), 
           importance = imp$importance$Overall) %>%
  mutate(rank = rank(-importance)) %>% arrange(desc(importance)) %>%
  filter(term %in% tree_terms)


fit <- with(tissue_gene_expression, 
            train(x, y, method = "rpart",control = rpart.control(minsplit = 0),
                  tuneGrid = data.frame(cp = seq(0, 0.1, 0.01))))
tree_terms <- as.character(unique(fit$finalModel$frame$var[!(fit$finalModel$frame$var == "CFHR4")]))
tree_terms
varImp(fit)
ggplot(fit) 
y_hat <- predict(fit, tissue_gene_expression$x)
confusionMatrix(fit_rpart)
confusionMatrix(y_hat,tissue_gene_expression$y)$overall["Accuracy"]
confusionMatrix(fit)
fit <- randomForest(y ~ x, data = dat, nodesize = 50, maxnodes = 25)  
plot(fit)
plot(fit$finalModel)
text(fit$finalModel)

y_hat = predict(fit)
ggplot() +
geom_point(aes(tissue_gene_expression$x, tissue_gene_expression$y)) +
geom_step(aes(tissue_gene_expression$x, y_hat), col = 2)
  
#---------------------------
rm(list = ls(all.names = TRUE))
gc()

library(tidyverse)
library(caret)
library(dslabs)

set.seed(1993)
data("tissue_gene_expression")
ind <- which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))
y <- droplevels(tissue_gene_expression$y[ind])
x <- tissue_gene_expression$x[ind, ]
x <- x[, sample(ncol(x), 10)]

train_lda <- train(x, y, method = "lda")
train_lda$finalModel
train_lda$results

train_qda <- train(x, y, method = "qda")
train_qda$results
train_qda$finalModel
colMeans(x)

d <- apply(train_lda$finalModel$means, 2, diff)
ind <- order(abs(d), decreasing = TRUE)[1:2]
plot(x[, ind], col = y)

t(train_lda$finalModel$means) %>% data.frame() %>%
  mutate(predictor_name = rownames(.)) %>%
  ggplot(aes(cerebellum, hippocampus, label = predictor_name)) +
  geom_point() +
  geom_text() +
  geom_abline()

t(train_qda$finalModel$means) %>% data.frame() %>%
  mutate(predictor_name = rownames(.)) %>%
  ggplot(aes(cerebellum, hippocampus, label = predictor_name)) +
  geom_point() +
  geom_text() +
  geom_abline()


set.seed(1993)
data("tissue_gene_expression")
y <- tissue_gene_expression$y
x <- tissue_gene_expression$x
x <- x[, sample(ncol(x), 10)]

train_lda <- train(x, y, method = "lda", preProcess = "center")
train_lda$finalModel
train_lda$results
####---------------------------------------------------
####---------------------------------------------------
rm(list = ls(all.names = TRUE))
gc()

library(tidyverse)
library(caret)
library(dslabs)

models <- c("glm", "lda",  "naive_bayes",  "svmLinear", 
            "gamboost",  "gamLoess", "qda", 
            "knn", "kknn", "loclda", "gam",
            "rf", "ranger",  "wsrf", "Rborist", 
            "avNNet", "mlp", "monmlp",
            "adaboost", "gbm",
            "svmRadial", "svmRadialCost", "svmRadialSigma")

set.seed(1)
data("mnist_27")
names(mnist_27)

fits <- lapply(models, function(model){ 
  print(model)
  train(y ~ ., method = model, data = mnist_27$train)
}) 

names(fits) <- models





matrix_predicts <- map_dbl(fits, function(fit){
  predict(fit, mnist_27$test, type = "raw")
})
dim(matrix_predicts)
matrix_predicts

acc_fit <- map_dbl(fits, function(object){
  fit <- predict(object, mnist_27$test, type = "raw")
  test_error <- confusionMatrix(fit, mnist_27$test$y)$overall["Accuracy"]
})

mean(acc_fit)


y_hat <- map_df(fits, function(object){
  fit <- predict(object, mnist_27$test, type = "raw")
})
y_hat
class(y_hat)
head(y_hat)

df <- data.frame(matrix(unlist(y_hat), nrow=23, byrow=T))
colnames(df) <- seq(1:200)
rownames(df) <- models
class(df)
head(df)

ifelse(test = sum(df[,1] == 7) > 12, yes = 7, no = 2)

col_index <- seq(1,ncol(df), 1)
predict_vote <- map_df(col_index, function(j){
  vote <- ifelse(test = sum(df[,j] == 7) > 12, yes = 7, no = 2)
  return(data_frame(vote = vote))
})

predict_vote
predict_vote <- as.factor(predict_vote$vote) #  as factor

acc_predict_vote <- confusionMatrix(predict_vote,  mnist_27$test$y)$overall["Accuracy"]
sum(acc_predict_vote > acc_fit)


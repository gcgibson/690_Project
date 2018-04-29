# packages used
library(MASS)
library(Hmisc)
library(tidyverse)
library(purrr)
library(broom)
library(haven)
library(stringr)
library(caret)
library(randomForest)
library(e1071)
library(tree)
library(glmnet)
library(class)
source("helpers.R")

nhanes3 <- read.csv("nhanes2.csv") %>%
  mutate(agecat = factor(agecat, ordered = TRUE),
         pircat = factor(pircat, ordered = TRUE),
         race = factor(race),
         diabetes = factor(diabetes)) %>%
  mutate_at(vars(gender, smoke100, chd:stroke, modrecract),
            .funs = function(x) 2 - x) %>%
  select(gender, agecat, race, bmi, hdl, ldl, triglyceride,
         totchol, diabetes, modrecract, smoke100, phq,
         sbp, dbp, heartattack, stroke, chd) %>%
  filter(complete.cases(.))

nhanes3$race <- relevel(nhanes3$race, ref = 3)


test.size <- floor(nrow(nhanes3)/6)
set.seed(1)
test.inds <- sample(1:nrow(nhanes3), test.size, replace = FALSE)
test <- nhanes3[test.inds,]

train <- nhanes3[-test.inds,]

ha.obs <- test$heartattack
str.obs <- test$stroke
chd.obs <- test$chd

expvars <- names(nhanes3)[1:14]

# run logreg lasso

grid <- c(0,10^seq(10,-2,length=100))

# x_tr <- model.matrix(heartattack~0+.,train[c("heartattack",expvars)])
# y_tr <- train$heartattack
# lr.lasso <- cv.glmnet(x_tr,y_tr,lambda=grid,nfolds=5,family="binomial",type.measure="mse")
# minlam <- lr.lasso$lambda.min
# x_te <- model.matrix(heartattack~0+.,test[c("heartattack",expvars)])
# y_te <- test$heartattack

# lr.pred <- 1*(predict(lr.lasso, newx = x_te, s = minlam, type = "response") > 0.50)
# table(pred=lr.pred,obs=y_te)
# mean(lr.pred == y_te)

get_lr_acc <- function(resp, train, test, alpha, grid = grid, expvars = expvars){
  y_tr <- as.numeric(unlist(train[resp]))
  x_tr <- model.matrix(as.formula(paste0(resp,"~0+.")),train[c(resp,expvars)])
  cv.fit <- cv.glmnet(x_tr, y_tr , alpha = alpha, lambda = grid, nfolds = 5,
                      family = "binomial", type.measure = "mse")
  lambda <- cv.fit$lambda.min
  x_te <- model.matrix(as.formula(paste0(resp,"~0+.")), test[c(resp, expvars)])
  y_te <- as.numeric(unlist(test[resp]))
  preds <- 1*(predict(cv.fit, newx = x_te, s = lambda, type = "response") > 0.50)
  acc <- mean(preds == y_te)
  list(acc = acc, preds = preds,lambda = lambda)
}

ha.lr <- get_lr_acc("heartattack", train, test, 1, grid)
str.lr <- get_lr_acc("str", train, test, 1, grid)
chd.lr <- get_lr_acc("chd", train, test, 1, grid)

grid <- 1:20

get_knn_acc <- function(resp, train, test, grid, K = 5, expvars = expvars){
  accs <- numeric(length = length(grid))
  for(i in 1:length(grid)){
    accs[i] <- get_kfold_acc(resp,train,grid[i],K)
  }
  nn <- which.max(accs)
  train.cl <- factor(as.numeric(unlist(train[resp])))
  y_te <- as.numeric(unlist(test[resp]))
  preds <- knn(train[expvars], test[expvars], train.cl, k = nn)
  acc <- mean(preds == y_te)
  list(acc = acc, preds = preds, nn = nn)
}

ha.knn <- get_knn_acc("heartattack", train, test, grid)
str.knn <- get_knn_acc("str", train, test, grid)
chd.knn <- get_knn_acc("chd", train, test, grid)

grid <- list(cost = c(0.01, 0.02, 0.05, 0.1, 0.2, 0.5, 1),
             epsilon = c(0.01, 0.1, 1))

get_svm_acc <- function(resp, train, test, kernel, grid, expvars = expvars, seed = 123){
  train[resp] <- factor(ifelse(train[resp] == 0, -1, 1))
  set.seed(seed)
  tc <- tune.control(cross = 5)
  cv.fit <- tune(svm, as.formula(paste0(resp,"~.")), data = train,
                 kernel = kernel, tunecontrol = tc,
                 ranges = grid)
  bestmod <- cv.fit$best.model
  test[resp] <- factor(ifelse(test[resp] == 0, -1, 1))
  preds <- predict(bestmod,test[c(resp,expvars)])
  acc <- mean(preds == unlist(test[resp]))
  cost <- bestmod$cost
  epislon <- bestmod$epsilon
  list(acc = acc, preds = preds, cost = cost, epsilon = epsilon)
}

ha.svm <- get_svm_acc("heartattack",train,test,"linear",grid)
str.svm <- get_svm_acc("stroke",train,test,"linear",grid)
chd.svm <- get_svm_acc("chd",train,test,"linear",grid)


# DTs

resp <- "heartattack"
tr <- train
te <- test
tr[resp] <- factor(as.numeric(unlist(tr[resp])))
te[resp] <- factor(as.numeric(unlist(te[resp])))
tree.nhanes <- tree(as.formula(paste0(resp,"~.")), tr[c(resp,expvars)])
cv.fit <- cv.tree(tree.nhanes, FUN = prune.misclass)
devmin <- which.min(cv.fit$dev)
bestsize <- cv.fit$size[devmin]
prune.nhanes <- prune.misclass(tree.nhanes, best = bestsize)
preds <- predict(prune.nhanes, te[expvars], type = "class")
acc <- mean(preds == unlist(te[resp]))
ha.dt <- list(acc = acc, preds = preds, bestsize = bestsize)

resp <- "stroke"
tr <- train
te <- test
tr[resp] <- factor(as.numeric(unlist(tr[resp])))
te[resp] <- factor(as.numeric(unlist(te[resp])))
tree.nhanes <- tree(as.formula(paste0(resp,"~.")), tr[c(resp,expvars)])
cv.fit <- cv.tree(tree.nhanes, FUN = prune.misclass)
devmin <- which.min(cv.fit$dev)
bestsize <- cv.fit$size[devmin]
prune.nhanes <- prune.misclass(tree.nhanes, best = bestsize)
preds <- predict(prune.nhanes, te[expvars], type = "class")
acc <- mean(preds == unlist(te[resp]))
str.dt <- list(acc = acc, preds = preds, bestsize = bestsize)

resp <- "chd"
tr <- train
te <- test
tr[resp] <- factor(as.numeric(unlist(tr[resp])))
te[resp] <- factor(as.numeric(unlist(te[resp])))
tree.nhanes <- tree(as.formula(paste0(resp,"~.")), tr[c(resp,expvars)])
cv.fit <- cv.tree(tree.nhanes, FUN = prune.misclass)
devmin <- which.min(cv.fit$dev)
bestsize <- cv.fit$size[devmin]
prune.nhanes <- prune.misclass(tree.nhanes, best = bestsize)
preds <- predict(prune.nhanes, te[expvars], type = "class")
acc <- mean(preds == unlist(te[resp]))
chd.dt <- list(acc = acc, preds = preds, bestsize = bestsize)


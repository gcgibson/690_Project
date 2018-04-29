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
library(pROC)
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

# create sets for individual outputs
ha.train <- train[c("heartattack",expvars)]
str.train <- train[c("stroke",expvars)]
chd.train <- train[c("chd",expvars)]

ha.test <- test[c("heartattack",expvars)]
str.test <- test[c("stroke",expvars)]
chd.test <- test[c("chd",expvars)]

# run logreg lasso
grid <- c(0,10^seq(10,-2,length=100))

get_lr_acc <- function(resp, train, test, alpha, grid = grid, expvars = expvars){
  y_tr <- as.numeric(unlist(train[resp]))
  x_tr <- model.matrix(as.formula(paste0(resp,"~0+.")),train)
  cv.fit <- cv.glmnet(x_tr, y_tr , alpha = alpha, lambda = grid, nfolds = 5,
                      family = "binomial", type.measure = "mse")
  lambda <- cv.fit$lambda.min
  x_te <- model.matrix(as.formula(paste0(resp,"~0+.")), test)
  y_te <- as.numeric(unlist(test[resp]))
  preds <- 1*(predict(cv.fit, newx = x_te, s = lambda, type = "response") > 0.50)
  acc <- mean(preds == y_te)
  list(acc = acc, preds = preds,lambda = lambda)
}

ha.lr <- get_lr_acc("heartattack", ha.train, ha.test, 1, grid)
str.lr <- get_lr_acc("stroke", str.train, str.test, 1, grid)
chd.lr <- get_lr_acc("chd", chd.train, chd.test, 1, grid)

# run knn
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
str.knn <- get_knn_acc("stroke", train, test, grid)
chd.knn <- get_knn_acc("chd", train, test, grid)


# run svm
grid <- list(cost = c(0.01, 0.02, 0.05, 0.1, 0.2, 0.5, 1),
             epsilon = c(0.01, 0.1, 1))

get_svm_acc <- function(resp, tr, te, kernel, grid, seed = 123){
  tr[resp] <- factor(ifelse(tr[resp] == 0, -1, 1))
  set.seed(seed)
  tc <- tune.control(cross = 5)
  cv.fit <- tune(svm, as.formula(paste0(resp,"~.")), data = tr,
                 kernel = kernel, tunecontrol = tc,
                 ranges = grid)
  bestmod <- cv.fit$best.model
  te[resp] <- factor(ifelse(te[resp] == 0, -1, 1))
  preds <- predict(bestmod,te)
  acc <- mean(preds == unlist(te[resp]))
  cost <- bestmod$cost
  epsilon <- bestmod$epsilon
  list(acc = acc, preds = preds, cost = cost, epsilon = epsilon)
}

ha.lin.svm <- get_svm_acc("heartattack",ha.train,ha.test,"linear",grid)
str.lin.svm <- get_svm_acc("stroke",str.train,str.test,"linear",grid)
chd.lin.svm <- get_svm_acc("chd",chd.train,chd.test,"linear",grid)

grid <- list(cost = c(0.01, 0.02, 0.05, 0.1, 0.2, 0.5, 1),
             epsilon = c(0.01, 0.1, 1))

# run DTs
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


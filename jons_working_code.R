# packages used
library(MASS)
library(Hmisc)
library(tidyverse)
library(purrr)
library(dummies)
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
  # mutate(agecat = factor(agecat, ordered = TRUE),
  #        pircat = factor(pircat, ordered = TRUE),
  #        education = factor(education, ordered = TRUE),
  #        marital = factor(marital),
  #        race = factor(race),
  #        diabetes = factor(diabetes),
  #        pulsetype = pulsetype - 1) %>%
  mutate(lightsleep = if_else(sleephrs < 7, 1, 0)) %>%
  mutate_at(vars(gender, smoke100, chd:modrecract, sleepdisorder),
            .funs = function(x) 2 - x) %>%
  mutate(cvd = if_else(chd == 1 | stroke == 1 | heartattack == 1, 1, 0)) %>%
  # mutate_at(vars(bmi,waistcirc,hdl,totchol,sleephrs,))
  # select(-seqn,-dpqna) %>%
  select(-seqn,-dpqna,-limited,-smoker,-drinks,-pir,-bmicat,-age,-sleephrs) %>%
  # select(-seqn,-drinks,-limited,-smoker,-bmicat,-pir,-age,-height,-weight,-gh,-dpqna) %>%
  select(chd,heartattack,stroke,cvd,everything()) %>%
  filter(complete.cases(.))

nhanes3 <- dummy.data.frame(nhanes3,names = c("race","marital"))

#nhanes3$race <- relevel(nhanes3$race, ref = 3)

test.size <- floor(nrow(nhanes3)/6)
set.seed(1)
test.inds <- sample(1:nrow(nhanes3), test.size, replace = FALSE)
test <- nhanes3[test.inds,]

train <- nhanes3[-test.inds,]

obs <- list(ha=test$heartattack,
            str=test$stroke,
            chd=test$chd,
            cvd=test$cvd)

obs <- lapply(obs,ordered)

#expvars <- names(nhanes3)[1:32]
expvars <- names(nhanes3)[5:ncol(nhanes3)]

# create sets for individual outputs
ha.train <- train[c("heartattack",expvars)]
str.train <- train[c("stroke",expvars)]
chd.train <- train[c("chd",expvars)]
cvd.train <- train[c("cvd",expvars)]

ha.test <- test[c("heartattack",expvars)]
str.test <- test[c("stroke",expvars)]
chd.test <- test[c("chd",expvars)]
cvd.test <- test[c("cvd",expvars)]

# run logreg lasso
grid <- c(0,0.01,0.1,1,10,100,100)

get_lr_acc <- function(resp, tr, te, alpha, grid = grid, expvars = expvars){
  y_tr <- as.numeric(unlist(tr[resp]))
  x_tr <- model.matrix(as.formula(paste0(resp,"~0+.")),tr)
  cv.fit <- cv.glmnet(x_tr, y_tr , alpha = alpha, lambda = grid, nfolds = 5,
                      family = "binomial", type.measure = "class")
  lambda <- cv.fit$lambda.min
  x_te <- model.matrix(as.formula(paste0(resp,"~0+.")), te)
  y_te <- as.numeric(unlist(te[resp]))
  preds <- 1*(predict(cv.fit, newx = x_te, s = lambda, type = "response") > 0.50)
  acc <- mean(preds == y_te)
  list(acc = acc, preds = factor(preds,levels =0:1,ordered = TRUE),lambda = lambda)
}

ha.lr.lasso <- get_lr_acc("heartattack", ha.train, ha.test, 1, grid)
confusionMatrix(factor(ha.lr.lasso$preds), obs$ha, positive = "1")
ha.lr.lasso.roc <- roc(obs$ha,ha.lr.lasso$preds)

str.lr.lasso <- get_lr_acc("stroke", str.train, str.test, 1, grid)
confusionMatrix(factor(str.lr.lasso$preds), obs$str, positive = "1")
str.lr.lasso.roc <- roc(obs$str,str.lr.lasso$preds)

chd.lr.lasso <- get_lr_acc("chd", chd.train, chd.test, 1, grid)
confusionMatrix(factor(chd.lr.lasso$preds), obs$chd, positive = "1")
chd.lr.lasso.roc <- roc(obs$chd,chd.lr.lasso$preds)

cvd.lr.lasso <- get_lr_acc("cvd", cvd.train, cvd.test, 1, grid)
confusionMatrix(factor(cvd.lr.lasso$preds), obs$cvd, positive = "1")
cvd.lr.lasso.roc <- roc(obs$cvd,cvd.lr.lasso$preds)

# lr ridge
ha.lr.ridge <- get_lr_acc("heartattack", ha.train, ha.test, 0, grid)
confusionMatrix(factor(ha.lr.ridge$preds), obs$ha, positive = "1")
ha.lr.ridge.roc <- roc(obs$ha,ha.lr.ridge$preds)

str.lr.ridge <- get_lr_acc("stroke", str.train, str.test, 0, grid)
confusionMatrix(factor(str.lr.ridge$preds), obs$str, positive = "1")
str.lr.ridge.roc <- roc(obs$str,str.lr.ridge$preds)

chd.lr.ridge <- get_lr_acc("chd", chd.train, chd.test, 0, grid)
confusionMatrix(factor(chd.lr.ridge$preds), obs$chd, positive = "1")
chd.lr.ridge.roc <- roc(obs$chd,chd.lr.ridge$preds)

cvd.lr.ridge <- get_lr_acc("cvd", cvd.train, cvd.test, 0, grid)
confusionMatrix(factor(cvd.lr.ridge$preds), obs$cvd, positive = "1")
cvd.lr.ridge.roc <- roc(obs$cvd,cvd.lr.ridge$preds)

# run knn
grid <- 1:20

get_knn_acc <- function(resp, tr, te, grid, K = 5, expvars = expvars){
  accs <- numeric(length = length(grid))
  for(i in 1:length(grid)){
    accs[i] <- get_kfold_acc(resp,tr,grid[i],K)
  }
  nn <- which.max(accs)
  tr.cl <- factor(as.numeric(unlist(tr[resp])))
  y_te <- as.numeric(unlist(te[resp]))
  preds <- knn(tr[expvars], te[expvars], tr.cl, k = nn)
  acc <- mean(preds == y_te)
  list(acc = acc, preds = factor(preds, levels =0:1, ordered = TRUE), nn = nn)
}

ha.knn <- get_knn_acc("heartattack", train, test, grid)
confusionMatrix(factor(ha.knn$preds), obs$ha, positive = "1")
ha.knn.roc <- roc(obs$ha,ha.knn$preds)

str.knn <- get_knn_acc("stroke", train, test, grid)
confusionMatrix(factor(str.knn$preds), obs$str, positive = "1")
str.knn.roc <- roc(obs$str,str.knn$preds)

chd.knn <- get_knn_acc("chd", train, test, grid)
confusionMatrix(factor(chd.knn$preds), obs$chd, positive = "1")
chd.knn.roc <- roc(obs$chd,chd.knn$preds)

cvd.knn <- get_knn_acc("cvd", train, test, grid)
confusionMatrix(factor(cvd.knn$preds), obs$cvd, positive = "1")
cvd.knn.roc <- roc(obs$cvd,cvd.knn$preds)

# run linear svm
grid <- list(cost = c(0.01, 0.1, 1, 10),
             epsilon = c(0.01, 0.1, 1))

get_svm_acc <- function(resp, tr, te, kern, grid, seed = 123){
  tr[resp] <- factor(ifelse(tr[resp] == 0, -1, 1))
  set.seed(seed)
  tc <- tune.control(cross = 5)
  cv.fit <- tune(svm, as.formula(paste0(resp,"~.")), data = tr,
                 kern = kern, tunecontrol = tc,
                 ranges = grid)
  bestmod <- cv.fit$best.model
  te[resp] <- factor(ifelse(te[resp] == 0, -1, 1))
  preds <- ifelse(predict(bestmod,te) == -1, 0, 1)
  acc <- mean(preds == unlist(te[resp]))
  cost <- bestmod$cost
  epsilon <- bestmod$epsilon
  gamma <- bestmod$gamma
  list(acc = acc, preds = factor(preds, levels = 0:1, ordered = TRUE),
       cost = cost, epsilon = epsilon, gamma = gamma)
}

ha.lin.svm <- get_svm_acc("heartattack",ha.train,ha.test,"linear",grid)
confusionMatrix(factor(ha.lin.svm$preds), obs$ha, positive = "1")
ha.lin.svm.roc <- roc(obs$ha,ha.lin.svm$preds)

str.lin.svm <- get_svm_acc("stroke",str.train,str.test,"linear",grid)
confusionMatrix(factor(str.lin.svm$preds), obs$str, positive = "1")
str.lin.svm.roc <- roc(obs$str,str.lin.svm$preds)

chd.lin.svm <- get_svm_acc("chd",chd.train,chd.test,"linear",grid)
confusionMatrix(factor(chd.lin.svm$preds), obs$chd, positive = "1")
chd.lin.svm.roc <- roc(obs$chd,chd.lin.svm$preds)

cvd.lin.svm <- get_svm_acc("cvd",cvd.train,cvd.test,"linear",grid)
confusionMatrix(factor(cvd.lin.svm$preds), obs$cvd, positive = "1")
cvd.lin.svm.roc <- roc(obs$cvd,cvd.lin.svm$preds)


# run radial svm
grid <- list(cost = c(0.01, 0.1, 1, 10),
             epsilon = c(0.01, 0.1, 1),
             gamma = c(0.01, 0.1, 1, 10))

ha.rad.svm <- get_svm_acc("heartattack",ha.train,ha.test,"radial",grid)
confusionMatrix(factor(ha.rad.svm$preds), obs$ha, positive = "1")
ha.rad.svm.roc <- roc(obs$ha,ha.rad.svm$preds)

str.rad.svm <- get_svm_acc("stroke",str.train,str.test,"radial",grid)
confusionMatrix(factor(str.rad.svm$preds), obs$str, positive = "1")
str.rad.svm.roc <- roc(obs$str,str.rad.svm$preds)

chd.rad.svm <- get_svm_acc("chd",chd.train,chd.test,"radial",grid)
confusionMatrix(factor(chd.rad.svm$preds), obs$chd, positive = "1")
chd.rad.svm.roc <- roc(obs$chd,chd.rad.svm$preds)

cvd.rad.svm <- get_svm_acc("cvd",cvd.train,cvd.test,"radial",grid)
confusionMatrix(factor(cvd.rad.svm$preds), obs$cvd, positive = "1")
cvd.rad.svm.roc <- roc(obs$cvd,cvd.rad.svm$preds)

# run polynomial svm
grid <- list(cost = c(0.01, 0.1, 1, 10),
             epsilon = c(0.01, 0.1, 1),
             gamma = c(0.01, 0.1, 1, 10))

ha.poly.svm <- get_svm_acc("heartattack",ha.train,ha.test,"polynomial",grid)
confusionMatrix(factor(ha.poly.svm$preds), obs$ha, positive = "1")
ha.poly.svm.roc <- roc(obs$ha,ha.poly.svm$preds)

str.poly.svm <- get_svm_acc("stroke",str.train,str.test,"polynomial",grid)
confusionMatrix(factor(str.poly.svm$preds), obs$str, positive = "1")
str.poly.svm.roc <- roc(obs$str,str.poly.svm$preds)

chd.poly.svm <- get_svm_acc("chd",chd.train,chd.test,"polynomial",grid)
confusionMatrix(factor(chd.poly.svm$preds), obs$chd, positive = "1")
chd.poly.svm.roc <- roc(obs$chd,chd.poly.svm$preds)

cvd.poly.svm <- get_svm_acc("cvd",cvd.train,cvd.test,"polynomial",grid)
confusionMatrix(factor(cvd.poly.svm$preds), obs$cvd, positive = "1")
cvd.poly.svm.roc <- roc(obs$cvd,cvd.poly.svm$preds)

# get_dt_acc <- function(resp,tr,te,K = 5){
#   tr[resp] <- factor(as.numeric(unlist(tr[resp])))
#   te[resp] <- factor(as.numeric(unlist(te[resp])))
#   tree.nhanes <- tree(as.formula(paste0(resp,"~.")), tr)
#   cv.fit <- cv.tree(tree.nhanes, FUN = prune.misclass, K = K)
#   devmin <- which.min(cv.fit$dev)
#   bestsize <- cv.fit$size[devmin]
#   prune.nhanes <- prune.misclass(tree.nhanes, best = bestsize)
#   preds <- predict(prune.nhanes, te[expvars], type = "class")
#   acc <- mean(preds == unlist(te[resp]))
#   list(acc = acc, preds = factor(preds,levels=0:1,ordered=TRUE), bestsize = bestsize)
# }
# 
# ha.dt <- get_dt_acc("heartattack",ha.train,ha.test)
# confusionMatrix(factor(ha.dt$preds), obs$ha, positive = "1")
# ha.dt.roc <- roc(obs$ha,ha.dt$preds)
# 
# str.dt <- get_dt_acc("stroke",str.train,str.test)
# confusionMatrix(factor(str.dt$preds), obs$str, positive = "1")
# str.dt.roc <- roc(obs$str,str.dt$preds)
# 
# chd.dt <- get_dt_acc("chd",chd.train,chd.test)
# confusionMatrix(factor(chd.dt$preds), obs$chd, positive = "1")
# chd.dt.roc <- roc(obs$chd,chd.dt$preds)
# 
# cvd.dt <- get_dt_acc("cvd",cvd.train,cvd.test)
# confusionMatrix(factor(cvd.dt$preds), obs$cvd, positive = "1")
# cvd.dt.roc <- roc(obs$cvd,cvd.dt$preds)

# run DTs
resp <- "heartattack"
tr <- ha.train
te <- ha.test
tr[resp] <- factor(as.numeric(unlist(tr[resp])))
te[resp] <- factor(as.numeric(unlist(te[resp])))
tree.nhanes <- tree(as.formula(paste0(resp,"~.")), tr)
cv.fit <- cv.tree(tree.nhanes, FUN = prune.misclass, K = 5)
devmin <- which.min(cv.fit$dev)
bestsize <- cv.fit$size[devmin]
prune.nhanes <- prune.misclass(tree.nhanes, best = bestsize)
preds <- predict(prune.nhanes, te[expvars], type = "class")
acc <- mean(preds == unlist(te[resp]))
ha.dt <- list(acc = acc, preds = preds, bestsize = bestsize)
confusionMatrix(ha.dt$preds, obs$ha, positive = "1")
ha.dt.roc <- roc(obs$ha,ordered(ha.dt$preds))


resp <- "stroke"
tr <- str.train
te <- str.test
tr[resp] <- factor(as.numeric(unlist(tr[resp])))
te[resp] <- factor(as.numeric(unlist(te[resp])))
tree.nhanes <- tree(as.formula(paste0(resp,"~.")), tr)
cv.fit <- cv.tree(tree.nhanes, FUN = prune.misclass, K = 5)
devmin <- which.min(cv.fit$dev)
bestsize <- cv.fit$size[devmin]
prune.nhanes <- prune.misclass(tree.nhanes, best = bestsize)
preds <- predict(prune.nhanes, te[expvars], type = "class")
acc <- mean(preds == unlist(te[resp]))
str.dt <- list(acc = acc, preds = preds, bestsize = bestsize)
confusionMatrix(str.dt$preds, obs$str, positive = "1")
str.dt.roc <- roc(obs$str,ordered(str.dt$preds))


resp <- "chd"
tr <- chd.train
te <- chd.test
tr[resp] <- factor(as.numeric(unlist(tr[resp])))
te[resp] <- factor(as.numeric(unlist(te[resp])))
tree.nhanes <- tree(as.formula(paste0(resp,"~.")), tr)
cv.fit <- cv.tree(tree.nhanes, FUN = prune.misclass, K = 5)
devmin <- which.min(cv.fit$dev)
bestsize <- cv.fit$size[devmin]
prune.nhanes <- prune.misclass(tree.nhanes, best = bestsize)
preds <- predict(prune.nhanes, te[expvars], type = "class")
acc <- mean(preds == unlist(te[resp]))
chd.dt <- list(acc = acc, preds = preds, bestsize = bestsize)
confusionMatrix(chd.dt$preds, obs$chd, positive = "1")
chd.dt.roc <- roc(obs$chd,ordered(chd.dt$preds))


resp <- "cvd"
tr <- cvd.train
te <- cvd.test
tr[resp] <- factor(as.numeric(unlist(tr[resp])))
te[resp] <- factor(as.numeric(unlist(te[resp])))
tree.nhanes <- tree(as.formula(paste0(resp,"~.")), tr)
cv.fit <- cv.tree(tree.nhanes, FUN = prune.misclass, K = 5)
devmin <- which.min(cv.fit$dev)
bestsize <- cv.fit$size[devmin]
prune.nhanes <- prune.misclass(tree.nhanes, best = bestsize)
preds <- predict(prune.nhanes, te[expvars], type = "class")
acc <- mean(preds == unlist(te[resp]))
cvd.dt <- list(acc = acc, preds = preds, bestsize = bestsize)
confusionMatrix(cvd.dt$preds, obs$cvd, positive = "1")
cvd.dt.roc <- roc(obs$cvd,ordered(cvd.dt$preds))

res.rda <- list(chd.dt = chd.dt,
                chd.knn = chd.knn,
                chd.lin.svm = chd.lin.svm,
                chd.lr.lasso = chd.lr.lasso,
                chd.lr.ridge = chd.lr.ridge,
                chd.poly.svm = chd.poly.svm,
                chd.rad.svm = chd.rad.svm,
                cvd.dt = cvd.dt,
                cvd.knn = cvd.knn,
                cvd.lin.svm = cvd.lin.svm,
                cvd.lr.lasso = cvd.lr.lasso,
                cvd.lr.ridge = cvd.lr.ridge,
                cvd.poly.svm = cvd.poly.svm,
                cvd.rad.svm = cvd.rad.svm,
                ha.dt = ha.dt,
                ha.knn = ha.knn,
                ha.lin.svm = ha.lin.svm,
                ha.lr.lasso = ha.lr.lasso,
                ha.lr.ridge = ha.lr.ridge,
                ha.poly.svm = ha.poly.svm,
                ha.rad.svm = ha.rad.svm,
                str.dt = str.dt,
                str.knn = str.knn,
                str.lin.svm = str.lin.svm,
                str.lr.lasso = str.lr.lasso,
                str.lr.ridge = str.lr.ridge,
                str.poly.svm = str.poly.svm,
                str.rad.svm = str.rad.svm)

save(res.rda, file="res.rda")

load("res.rda")
list2env(res.rda, .GlobalEnv)

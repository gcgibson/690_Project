# packages used
library(MASS)
library(Hmisc)
library(tidyverse)
library(purrr)
library(broom)
library(haven)
library(stringr)
library(caret)
library(glmnet)

# helper functions

# convert NAs coded with one of 2 int values
convert_to_na <- function(x, nas = c(7,9)){
  x <- na_if(x, nas[1])
  na_if(x, nas[2])
}

# join two data frames on id
join_by_id <- function(df1, df2, id= "seqn"){
  inner_join(df1, df2, by = id)
}

# read data from NHANES website
get_data <- function(survey, letter){
  demo <- sasxport.get(paste(url,survey,"/DEMO",letter,".XPT",sep=""))
  bmx <- sasxport.get(paste(url,survey,"/BMX",letter,".XPT",sep=""))
  bpx <- sasxport.get(paste(url,survey,"/BPX",letter,".XPT",sep=""))
  ghb <- sasxport.get(paste(url,survey,"/GHB",letter,".XPT",sep=""))
  hdl <- sasxport.get(paste(url,survey,"/HDL",letter,".XPT",sep=""))
  tchol <- sasxport.get(paste(url,survey,"/TCHOL",letter,".XPT",sep=""))
  bpq <- sasxport.get(paste(url,survey,"/BPQ",letter,".XPT",sep=""))
  diq <- sasxport.get(paste(url,survey,"/DIQ",letter,".XPT",sep=""))
  alq <- sasxport.get(paste(url,survey,"/ALQ",letter,".XPT",sep=""))
  mcq <- sasxport.get(paste(url,survey,"/MCQ",letter,".XPT",sep=""))
  # rxq_rx <- sasxport.get(paste(url,survey,"/RXQ_RX",letter,".XPT",sep=""))
  # we may not need dpq? it's for depression
  dpq <- sasxport.get(paste(url,survey,"/DPQ",letter,".XPT",sep=""))
  inq <- sasxport.get(paste(url,survey,"/INQ",letter,".XPT",sep=""))
  paq <- sasxport.get(paste(url,survey,"/PAQ",letter,".XPT",sep=""))
  pfq <- sasxport.get(paste(url,survey,"/PFQ",letter,".XPT",sep=""))
  slq <- sasxport.get(paste(url,survey,"/SLQ",letter,".XPT",sep=""))
  smq <- sasxport.get(paste(url,survey,"/SMQ",letter,".XPT",sep=""))
  trigly <-  sasxport.get(paste(url,survey,"/TRIGLY",letter,".XPT",sep=""))
  datalist <- list(demo, bmx, bpx, ghb, hdl, tchol, bpq, diq, alq,
                   mcq, dpq, inq, paq, pfq, slq, smq, trigly) # rxq_rx, 
  Reduce(join_by_id, datalist) %>% mutate(survey = survey)
}


get_kfold_acc <- function(resp, train, nn, K = 5, seed = 123){
  
  # sort data frame
  row_inds <- 1:nrow(train)
  set.seed(seed)
  train_sort <- train[sample(row_inds),]
  
  # split into k blocks
  train_split <- split(train_sort, 1:K)
  
  # initialize vector to hold k acc measurements
  acc <- numeric(K)
  
  # for each, create a training set with cases not in the block
  # also create a test set with cases in the block
  for(k in 1:K){
    
    # get row indices for test set
    test_inds <- as.numeric(row.names(train_split[[k]]))
    
    # get logical indices for training set
    train_lgl <- !(row_inds %in% test_inds)
    
    # extract training set, fit model
    train_k <- train_sort[train_lgl,]
    
    train.cl <- factor(as.numeric(unlist(train_k[resp])))
    
    # extract validation set, make predictions using learned model
    test_k <- train_split[[k]]
    preds <- knn(train_k[expvars], test_k[expvars], train.cl, k = nn)
    
    # calculate mse for this block
    acc[k] <- sum(factor(as.numeric(unlist(test_k[resp]))) == preds)/nrow(test_k)
  }
  
  # return mean mse of the k blocks
  mean(acc)
}

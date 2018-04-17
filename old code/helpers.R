# packages used
library(MASS)
library(Hmisc)
library(tidyverse)
library(purrr)
library(broom)
library(haven)
library(stringr)
library(caret)

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

# bivariate models with depressed
make_bivariate_table <- function(xvar, yvar = "depressed", dat = nhanes){
  f <- as.formula(paste(yvar,"~",xvar,sep = ""))
  m <- glm(f, data = dat, family = binomial)
  mt <- tidy(m)[2,]
  mt <- mt %>% mutate(OR = exp(estimate)) %>%
    select(term, B = estimate, "SE(B)" = std.error,
           "exp(B)" = OR, "p-value" = p.value,-statistic)
  mt[,-1] <- round(mt[,-1], 4)
  mt$term <- if_else(mt[5] <= 0.05, paste(mt$term,"*",sep=""),mt$term)
  mt
}

# make multiviate table from tidy mlr mod
make_multivariate_table <- function(xvec){
  rhs <- paste(xvec, collapse = "+")
  f2 <- paste("depressed~",rhs,sep="")
  m <- glm(f2, data = nhanes, family = binomial)
  mt <- tidy(m)
  # mt <- mt[2:nrow(mt),]
  mt <- mt %>% mutate(OR = exp(estimate)) %>%
    select(term, B = estimate, "SE(B)" = std.error,
           "exp(B)" = OR, "p-value" = p.value,-statistic)
  mt[,-1] <- round(mt[,-1], 4)
  mt$term <- if_else(mt[5] <= 0.05, paste(mt$term,"*",sep=""),mt$term)
  mt
}

make_univariate_table <- function(x, dat = nhanes){
  rhs <- paste("~", x, sep="")
  tab <- as.vector(xtabs(as.formula(rhs), data = dat))
  ptab <- as.vector(prop.table(tab))
  col <- paste(tab, " (", round(ptab*100, 1), ")", sep = "")
  d <- data.frame(term = x, col[1],col[2])
  names(d) <- c("term", "0","1")
  d
}


make_twoway_table <- function(x, y = "depressed", dat = nhanes){
  f <- as.formula(paste("~",x,"+",y,sep=""))
  tab <- xtabs(f, data = nhanes)
  tabdf <- tidy(tab) %>% spread_(y, "Freq")
  names(tabdf) <- c("value","no","yes")
  ptab <- round(prop.table(tab,1),3)*100
  ptabdf <- tidy(ptab) %>% spread(depressed, "Freq")
  names(ptabdf) <- c("value","nopct","yespct")
  twoway <- inner_join(tabdf,ptabdf,by = "value") 
  twoway$xvar <- x
  twoway %>% select_("xvar","value","yes", "yespct","no","nopct")
}


get_glance <- function(mod, dat, fam) {
  m <- glm(mod, data = dat, family = fam)
  glance(m)
}

# read data from NHANES website
get_data <- function(survey, letter){
  
  demo <- sasxport.get(paste(url,survey,"/DEMO",letter,".XPT",sep="")) %>%
    select(seqn, gender = riagendr, age = ridageyr, race = ridreth1,
           education = dmdeduc2, marital = dmdmartl, pir = indfmpir)
  
  bmx <- sasxport.get(paste(url,survey,"/BMX",letter,".XPT",sep="")) %>%
    select(seqn, bmi = bmxbmi, height = bmxht, weight = bmxwt, waist = bmxwaist)
  
  bpx <- sasxport.get(paste(url,survey,"/BPX",letter,".XPT",sep="")) %>%
    select(seqn, starts_with("bpxsy"))
  
  glu <- sasxport.get(paste(url,survey,"/GLU",letter,".XPT",sep="")) %>%
    select(seqn, glucose = lbxglu)
  
  tchol <- sasxport.get(paste(url,survey,"/TCHOL",letter,".XPT",sep="")) %>%
    select(seqn, cholest = lbxtc)
  
  alq <- sasxport.get(paste(url,survey,"/ALQ",letter,".XPT",sep="")) %>%
    select(seqn, drinks = alq130)
  
  mcq <- sasxport.get(paste(url,survey,"/MCQ",letter,".XPT",sep="")) %>%
    select(seqn, heartattack = mcq160e, stroke = mcq160f, cancer = mcq220)
  
  rxq_rx <- sasxport.get(paste(url,survey,"/RXQ_RX",letter,".XPT",sep="")) %>%
    select(seqn, rxduse)
  
  dpq <- sasxport.get(paste(url,survey,"/DPQ",letter,".XPT",sep="")) %>%
    select(seqn, dpq010:dpq090)
  
  paq <- sasxport.get(paste(url,survey,"/PAQ",letter,".XPT",sep="")) %>%
    select(seqn, modwork = paq620, modrec = paq665)
  
  pfq <- sasxport.get(paste(url,survey,"/PFQ",letter,".XPT",sep="")) %>%
    select(seqn, limited = pfq059)
  
  slq <- sasxport.get(paste(url,survey,"/SLQ",letter,".XPT",sep="")) %>%
    select(seqn, sleep = slq060)
  
  smq <- sasxport.get(paste(url,survey,"/SMQ",letter,".XPT",sep="")) %>%
    select(seqn, smoke100 = smq020)
  
  
  datalist <- list(demo, bmx, bpx, glu, tchol, alq, mcq, rxq_rx, dpq, paq,
                   pfq, slq, smq)
  
  Reduce(join_by_id, datalist) %>% mutate(survey = survey)
}

# perform k-fold cross validation
get_kfold_acc <- function(seed, rhs, y, train, k){

  # sort data frame
  row_inds <- 1:nrow(train)
  set.seed(seed)
  train_sort <- train[sample(row_inds),]

  # split into k blocks
  train_split <- split(train_sort, 1:k)

  # initialize vector to hold k mse measurements
  acc <- numeric(k)

  # for each, create a training set with cases not in the block
  # also create a test set with cases in the block
  for(block in 1:k){

    # get row indices for test set
    test_inds <- as.numeric(row.names(train_split[[block]]))

    # get logical indices for training set
    train_lgl <- !(row_inds %in% test_inds)

    # extract training set, fit model
    train <- train_sort[train_lgl,]
    mod <- glm(paste(y, "~" , rhs, sep = ""), data = train)

    # extract test set, make predictions using learned model
    test <- train_split[[block]]
    probs <- predict(mod, newdata = test)
    
    set.seed(k)
    runifs <- runif(length(probs),0,1)
    preds <- (probs > runifs)*1

    # calculate mse for this block
    acc[block] <- sum(test[y] == preds)/nrow(test)
  }

  # return mean mse of the k blocks
  mean(acc)
}
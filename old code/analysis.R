# analysis document
source("readdata.R")
source("helpers.R")

nhanes <- read.csv("nhanes.csv")

# refactor, relevel the following variables
nhanes$agecat <- factor(nhanes$agecat)
nhanes$bmicat <- factor(nhanes$bmicat)
nhanes$education <- factor(nhanes$education)
nhanes$race <- factor(nhanes$race)

nhanes$bmicat <- relevel(nhanes$bmicat, ref = 2)
nhanes$education <- relevel(nhanes$education, ref = 5)
nhanes$race <- relevel(nhanes$race, ref = 3)

# list of explanatory vars
xlist <- list("college","overwt","poverty","rxduse","limited","modwork",
              "modrec","diabetic","highbp","highdrink","smoke100","disease",
              "female","race","married","elderly")

xlist2 <- list("education","bmicat","poverty","rxduse","limited","modwork",
              "modrec","diabetic","highbp","highdrink","smoke100","disease",
              "female","race","married","agecat")

# univariate table
univariate_table <- map_df(c("depressed",xlist), make_univariate_table, dat = nhanes)

# twoway tables
two_way_table <- map_df(xlist2, make_twoway_table)
# get chisq tests
xvecA <- unlist(xlist2)
xtabsf <- paste("~",xvecA,"+depressed",sep="")
xtabslist <- map(xtabsf, xtabs, data = nhanes)
chisqlist <- map_df(xtabslist, compose(glance, chisq.test))[1]
chisqdf <- cbind(xvar = xvecA,chisqlist)
chisqdf[2] <- round(chisqdf[2],4)

# bivariate table
bivariate_table <- map_df(xlist, make_bivariate_table, dat = nhanes)

# multivariate model
xvec <- unlist(xlist)
rhs <- paste(xvec, collapse = "+")
multivariate_table <- map_df(rhs, make_multivariate_table)

# multivariate table with confints
xvec <- unlist(xlist2)
rhs <- paste(xvec, collapse = "+")
f <- paste("depressed~",rhs,sep="")
fit <- glm(as.formula(f), data = nhanes, family = binomial)
confints <- confint(fit)
fit_tidy <- tidy(fit)
confints_tidy <- tidy(confints) %>% select(term = .rownames, lower = X2.5.., upper = X97.5..)
#confints_tidy[2:3] <- round(confints_tidy[2:3],4)
multivariate_table2A <- inner_join(fit_tidy, confints_tidy, by = "term") %>%
  mutate(OR = round(exp(estimate),2),
         Lower = round(exp(lower),2),
         Upper = round(exp(upper),2),
         CI = paste("(",Lower,", ",Upper,")",sep="")) %>%
  select(Term = term, OR, CI)

# perform backward selection
fit_back <- stepAIC(fit, direction = "backward",trace = 0)
confints_back <- confint(fit_back)
fit_back_tidy <- tidy(fit_back)
confints_back_tidy <- tidy(confints_back) %>% select(term = .rownames, lower = X2.5.., upper = X97.5..)
multivariate_table2B <- inner_join(fit_back_tidy, confints_back_tidy, by = "term") %>%
  mutate(OR = round(exp(estimate),2),
         Lower = round(exp(lower),2),
         Upper = round(exp(upper),2),
         CI = paste("(",Lower,", ",Upper,")",sep="")) %>%
  select(Term = term, OR, CI)

# join full model with reduced model
multivariate_table2AB <- left_join(multivariate_table2A, multivariate_table2B, by = "Term")

# test accuracy
seed_list <- 100:200
rhs2 <- c("disease","female","highdrink","limited","married","modrec","poverty",
          "rxduse","smoke100","agecat","bmicat","education","race")
rhs2 <- paste(rhs2, collapse = "+")
acc_list <- map_dbl(seed_list, get_kfold_acc, rhs = rhs2, y = "depressed", train = nhanes, k = 5)
mean(acc_list)

# test model on held out test set
test_len <- floor(nrow(nhanes)/2)
inds <- rep(c(0,1), times = c(nrow(nhanes)-test_len,test_len))
set.seed(33)
inds <- sample(inds)

test <- nhanes[inds == 1,]
train <- nhanes[inds == 0,]

# train glm, get probs, get preds, make confusion matrix
train.glm <- glm(paste("depressed~",rhs2,sep=""),data=train,family=binomial)
probs <- predict(train.glm, newdata=test,type = "response")
set.seed(34)
runifs <- runif(length(probs), 0, 1)
preds <- (probs > runifs)*1

confusionMatrix(data=preds, test$depressed, positive = "1")

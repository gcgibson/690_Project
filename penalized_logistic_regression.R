library(glmnet)
library(caret)
library(rstanarm)
## We need an outer for loop

lr_lasso_mse_results_matrix <- matrix(NA,nrow=100,ncol=5)
lr_ridge_mse_results_matrix <- matrix(NA,nrow=100,ncol=5)
lr_none_mse_results_matrix <- matrix(NA,nrow=100,ncol=5)
lr_bayes_results_matrix <- matrix(NA,nrow=100,ncol=5)

row_count <- 1

pos <- nhanes2[nhanes2$stroke == 1,]
neg <- nhanes2[nhanes2$stroke == 2,]

pos_test  <- pos[1:100,]
pos  <- pos[100:743,]

neg_test <- neg[1:100,]


test_data <- rbind(pos_test,neg_test)


for (num_sim in 1:100){
  colIndex <- 1
  for (i in seq(1,14,3)){
  
  
    ### Get all positive and negative classes
    
    ### start with 50/50 class balance 
    neg_sample <- neg[sample(nrow(neg), i*nrow(pos)), ]
    
    nhanes3 <- rbind(pos,neg_sample)
    nhanes3 <- nhanes3[sample(nrow(nhanes3)),]
    
    
    folds <- cut(seq(1,nrow(nhanes3)),breaks=5,labels=FALSE)
    #Perform 10 fold cross validation
      # Segment data for cross validation
      trainData <- nhanes3
      x_train <- trainData[ , ! colnames(trainData) %in% c("seqn","stroke") ]
      y_train <- trainData[,colnames(trainData) %in% c("stroke") ]
      x_test <- testData[ , ! colnames(testData) %in% c("seqn","stroke") ]
      y_test <- testData[,colnames(trainData) %in% c("stroke") ]
      
      #get data into correct format
      x_train <- data.matrix(x_train)
      y_train <- data.matrix(y_train) -1
      
      x_test <- data.matrix(x_test)
      y_test <- data.matrix(y_test)-1
      
      x_train[is.na(x_train)] <- 0
      y_train[is.na(y_train)] <- 0
      
      x_test[is.na(x_test)] <- 0
      y_test[is.na(y_test)] <- 0 
      
      df_train <- data.frame(x_train=x_train,y_train=y_train)
      df_test <- data.frame(x_train = x_test, y_train = y_test)
      
      cv.lasso <- cv.glmnet(x_train, y_train, alpha = 1, family = "binomial")
      logistic_regression_lasso_penalty_preds <- round(predict(cv.lasso,x_test,type="response"))
      
      model <- glm (stroke ~ ., data = df_train, family = binomial)
      logistic_regression_no_penalty_preds <- round(predict(model,df_test, type = 'response'))
      
      cv.ridge <- cv.glmnet(x_train, y_train, alpha = 0, family = "binomial")
      logistic_regression_ridge_penalty_preds <- round(predict(cv.ridge,x_test,type="response"))
      
     # t_prior <- student_t(df = 7, location = 0, scale = 2.5)
     # post1 <- stan_glm(stroke ~ ., data = df_train,
      #                 family = binomial(link = "logit"), 
       #                 prior = t_prior, prior_intercept = t_prior,
        #                seed = 1)
      
      
     # ppd <- posterior_predict(post1,newdata=df_test,type="response")
      #bayesian_logistic_regression_preds <- round(colMeans(ppd))
      
      lr_none_mse_results_matrix[num_sim,colIndex] <- mean(logistic_regression_no_penalty_preds-y_test)^2
      lr_ridge_mse_results_matrix[num_sim,colIndex] <- mean(logistic_regression_ridge_penalty_preds-y_test)^2
     # lr_bayes_results_matrix[row_count,j] <- mean(bayesian_logistic_regression_preds-y_test)^2
      lr_lasso_mse_results_matrix[num_sim,colIndex] <- mean(logistic_regression_lasso_penalty_preds-y_test)^2
      
    
    #plot.roc(as.numeric(y_test),as.numeric(logistic_regression_no_penalty_preds),percent=TRUE,col="#1c61b6",  print.auc=TRUE)
    #plot.roc(as.numeric(y_test),as.numeric(logistic_regression_ridge_penalty_preds),percent=TRUE,col="#1c61b6",  print.auc=TRUE)
    #plot.roc(as.numeric(y_test),as.numeric(logistic_regression_lasso_penalty_preds),percent=TRUE,col="#1c61b6",  print.auc=TRUE)
  #  plot.roc(as.numeric(y_test),as.numeric(bayesian_logistic_regression_preds),percent=TRUE,col="#1c61b6",  print.auc=TRUE)
    colIndex <- colIndex + 1
  }
}


fullLassoLr <- colMeans(lr_lasso_mse_results_matrix)
fullRidgeLr <- colMeans(lr_ridge_mse_results_matrix)
fullBayesLr <- colMeans(lr_none_mse_results_matrix)
fullNoneLr <- colMeans(lr_none_mse_results_matrix)

library(matrixStats)
ciLassoLr <- colQuantiles(lr_lasso_mse_results_matrix,probs=c(.025,.975))
ciRidgeLr <- colQuantiles(lr_ridge_mse_results_matrix,probs=c(.025,.975))
ciBayesLr <- colQuantiles(lr_none_mse_results_matrix,probs=c(.025,.975))
ciNoneLr <- colQuantiles(lr_none_mse_results_matrix,probs=c(.025,.975))

plot(fullNoneLr,type='l',ylim=c(0,.015),col="black",xaxt = "n",xlab="Ratio of +:-",ylab="Error",main="None")
axis(1, at=1:5, labels=c("1:1","1:4","1:8","1:12","1:16"))
polygon(c(1:5,rev(1:5)),c(ciNoneLr[,1],rev(ciNoneLr[,2])),col = "grey75", border = FALSE)
lines(1:5,fullNoneLr)

lines(fullRidgeLr,col="blue")
lines(ciRidgeLr[,1],col='blue',lty='dashed')
lines(ciRidgeLr[,2],col='blue',lty='dashed')

lines(fullBayesLr,col="orange")
lines(ciRidgeLr[,1],col='orange',lty='dashed')
lines(ciRidgeLr[,2],col='orange',lty='dashed')

lines(fullNoneLr)
lines(fullNoneLr[,1],col='black',lty='dashed')
lines(fullNoneLr[,2],col='black',lty='dashed')

legend("topright", legend=c("Lasso", "Ridge","Bayesian","None"),
       col=c("red", "blue","orange","black"), lty=1, cex=0.8)



library(glmnet)
library(caret)
library(rstanarm)
## We need an outer for loop

for (val_size in 1:5){
  num_sims <- 100
  nprops <- 6
  lr_lasso_mse_results_matrix <- matrix(NA,nrow=num_sims,ncol=nprops)
  lr_ridge_mse_results_matrix <- matrix(NA,nrow=num_sims,ncol=nprops)
  lr_none_mse_results_matrix <- matrix(NA,nrow=num_sims,ncol=nprops)
  lr_bayes_results_matrix <- matrix(NA,nrow=num_sims,ncol=nprops)
  
  row_count <- 1
  
  pos <- nhanes2[nhanes2$stroke == 1,]
  neg <- nhanes2[nhanes2$stroke == 2,]
  
  ### maintain validation set test size at 100
  if (val_size == 1){
    pos_test_index <- sample(1:100,50)
    neg_test_index <- setdiff(1:100,pos_test_index)
  }
  
  if (val_size == 2){
    pos_test_index <- sample(1:100,25)
    neg_test_index <- setdiff(1:100,pos_test_index)
  }
    
  if (val_size == 3){
    pos_test_index <- sample(1:100,10)
    neg_test_index <- setdiff(1:100,pos_test_index)
  }
  if (val_size == 4){
    pos_test_index <- sample(1:100,5)
    neg_test_index <- setdiff(1:100,pos_test_index)
  }
  
  if (val_size == 5){
    pos_test_index <- sample(1:100,1)
    neg_test_index <- setdiff(1:100,pos_test_index)
  }
  pos_train_index <- setdiff(1:nrow(pos),pos_test_index)
    neg_train_index <- setdiff(1:nrow(neg),neg_test_index)
    

    pos_test  <- pos[pos_test_index,]
    neg_test <- neg[neg_test_index,]
    
    pos  <- pos[pos_train_index,]
    neg <- neg[neg_train_index,]
    
    test_data <- rbind(pos_test,neg_test)
  
  for (num_sim in 1:num_sims){
    colIndex <- 1
    for (i in c(1,2,4,8,10,14)){
    
    
      ### Get all positive and negative classes
      
      ### start with 1/2 class balance 
      neg_sample <- neg[sample(nrow(neg), i*nrow(pos)), ]
      
      nhanes3 <- rbind(pos,neg_sample)
      nhanes3 <- nhanes3[sample(nrow(nhanes3)),]
      
      
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
  
  
  plot(fullRidgeLr,type='l',ylim=c(0,.015),col="black",xaxt = "n",xlab="Ratio of +:-",ylab="Error",main="Ridge")
  axis(1, at=1:nprops,cex.axis=.75, labels=c("1:1","1:2","1:4","1:8","1:12","1:14"))
  polygon(c(1:nprops,rev(1:nprops)),c(ciRidgeLr[,1],rev(ciRidgeLr[,2])),col = "grey75", border = FALSE)
  lines(1:nprops,fullRidgeLr)
  
  
  plot(fullLassoLr,type='l',ylim=c(0,.015),col="black",xaxt = "n",xlab="Ratio of +:-",ylab="Error",main="Lasso")
  axis(1, at=1:nprops,cex.axis=.75, labels=c("1:1","1:2","1:4","1:8","1:12","1:14"))
  polygon(c(1:nprops,rev(1:nprops)),c(ciLassoLr[,1],rev(ciLassoLr[,2])),col = "grey75", border = FALSE)
  lines(1:nprops,fullLassoLr)
  
  plot(fullNoneLr,type='l',ylim=c(0,.015),col="black",xaxt = "n",xlab="Ratio of +:-",ylab="Error",main="None")
  axis(1, at=1:nprops,cex.axis=.75,labels=c("1:1","1:2","1:4","1:8","1:12","1:14"))
  polygon(c(1:nprops,rev(1:nprops)),c(ciNoneLr[,1],rev(ciNoneLr[,2])),col = "grey75", border = FALSE)
  lines(1:nprops,fullNoneLr)
}
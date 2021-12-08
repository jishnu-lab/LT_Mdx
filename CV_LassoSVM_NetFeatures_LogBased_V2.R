
rm(list = ls())
cat("\014")

setwd("/Users/sar210/Box/MDx_project/")
# setwd("/Users/syedashiqurrahman/Box/MDx_project/")

library(e1071)
library(caret)
library(glmnet)
library(randomForest)
library(tree)
library(gbm)
library(matrixStats)
library(readxl)
library(cvAUC)
library(pROC)


# data <- read.delim("preCombinedFinal.txt")
data <- read.delim("earlyCombinedFinal.txt")
# data <- read.delim("lateCombinedFinal.txt")
data <- data[, -2]
Y <- data$Y

# df <- read_excel("Fig_3_4_E_ModulesMatrices_PID_Thymo information added.xlsx", 2)
df <- read_excel("Fig_3_4_E_ModulesMatrices_PID_Thymo information added.xlsx", 1)
Y <- df$TY
data$Y <- df$TY

modules <- read.csv("OutputClusterOne_s_5_d_6.csv")
tempModules <- modules[-c(32, 81, 269, 302, 305, 422, 492),]
# tempModules <- modules

# M <- matrix(0, nrow=555, ncol=nrow(data))

K <- 10
# k <- nrow(data) - 1

aucRF <- c()
aucSVM <- c()

dF <- c(1:nrow(data))

# replicateCV <- 1
for (replicateCV in 1:10)
{
  
  folds <- createFolds(y = data$Y, k=K, list = FALSE, returnTrain = FALSE)
  myData <- cbind(data, folds)
  
  Y <- c()
  append.RF <- c()
  append.SVM <- c()
  selectedVars <- c()
  
  # NoF <- 1
  for (NoF in 1:K)
  {
    
      fold = which(folds == NoF)
      
      train <- myData[-fold, ]
      test <- myData[fold, ]
      
      train <- train[, -ncol(train)]
      test <- test[, -ncol(test)]
      
      testM <- matrix(0, nrow=555, ncol=nrow(test))
      trainM <- matrix(0, nrow=555, ncol=nrow(train))
      
      # begin feature construction outer loop
      # i <- 1
      for (i in 1:nrow(tempModules))
      {
        
        module <- tempModules[i, ]
        module <- unique(module[module != ""])
        
        # subDF <- data[ , names(data) %in% module]
        # subDF <- cbind.data.frame(Y, subDF)
        
        trainSubDF <- train[ , names(train) %in% module]
        Y <- train$Y
        trainSubDF <- cbind.data.frame(Y, trainSubDF)
        testSubDF <- test[ , names(test) %in% module]
        Y <- test$Y
        testSubDF <- cbind.data.frame(Y, testSubDF)
        
        Zr <- subset(trainSubDF, Y==1)
        Znr <- subset(trainSubDF, Y==0)
        muZr <- colMeans(Zr)
        muZnr <- colMeans(Znr)
        muZr <- muZr[c(2:length(muZr))]
        muZnr <- muZnr[c(2:length(muZnr))]
        
        # test feature construction
        tempSubDF <- testSubDF[,-1]
        m <- c()
        N <- c()
        SN <- c()
        for (k in 1:nrow(as.matrix(tempSubDF))) {
          for (j in 1:ncol(as.matrix(tempSubDF))) {
            # m[j] <- log1p(((tempSubDF[k, j])^2)/(muZr[j]*muZnr[j]))
            m[j] <- log2((tempSubDF[k, j]^2)/(muZr[j]*muZnr[j]))
            SN[j] <- sign(m[j])
          }
          m[!is.finite(m)] <- 0.0001    
          
          posSN <- 0
          negSN <- 0
          posSN <- sum(SN > 0, na.rm = T)
          negSN <- sum(SN < 0, na.rm = T)
          
          m  <- abs(m)
          
          if (posSN > negSN){
            N[k] <- sum(m)*1
          } else{
            N[k] <- sum(m)*(-1)
          }
        }
        testM[i, ] <- N
        
        # train feature construction
        tempSubDF <- trainSubDF[,-1]
        m <- c()
        N <- c()
        SN <- c()
        for (k in 1:nrow(as.matrix(tempSubDF))) {
          for (j in 1:ncol(as.matrix(tempSubDF))) {
            # m[j] <- log1p(((tempSubDF[k, j])^2)/(muZr[j]*muZnr[j]))
            m[j] <- log2((tempSubDF[k, j]^2)/(muZr[j]*muZnr[j]))
            SN[j] <- sign(m[j])
          }
          
          m[!is.finite(m)] <- 0.0001    
          
          posSN <- 0
          negSN <- 0
          posSN <- sum(SN > 0, na.rm = T)
          negSN <- sum(SN < 0, na.rm = T)
          
          m  <- abs(m)
          
          if (posSN > negSN){
            N[k] <- sum(m)*1
          } else{
            N[k] <- sum(m)*(-1)
          }
        }
        trainM[i, ] <- N
      } # end of feature construction outer-loop
      
      testModules <- t(testM)
      Y <- test$Y
      testModules <- cbind.data.frame(Y, testModules)
      
      trainModules <- t(trainM)
      Y <- train$Y
      trainModules <- cbind.data.frame(Y, trainModules)
      
      for(i in 2:ncol(trainModules)){
        trainModules[is.na(trainModules[,i]), i] <- 0.001
        trainModules[!is.finite(trainModules[,i]), i] <- 0.001
        
        testModules[is.na(testModules[,i]), i] <- 0.001
        testModules[!is.finite(testModules[,i]), i] <- 0.001
        
      }
      
      X = as.matrix(trainModules[, -1])
      y = trainModules$Y
      
      glmnet1 <- cv.glmnet(X, y=trainModules$Y, alpha=1, nfolds = K)
      
      lambda <- glmnet1$lambda.min
      # lambda <- glmnet1$lambda.1se
      lambda <- lambda*0.70
      
      glmnet2 <- glmnet(X, y=trainModules$Y, alpha=1, lambda = lambda)
      c <- coef(glmnet2)
      
      inds<-which(c!=0)
      variables<-row.names(c)[inds]
      len <- length(variables)
      
      if (len == 1)
      {
        randomSelect <- sample(ncol(data), 3)
        variables <- row.names(c)[randomSelect]
      } else
      {
        variables<-row.names(c)[inds]
        variables <- variables[2:len]
      }
      selectedVars <- append(selectedVars, len)
      
      tempTr <- trainModules[, (names(trainModules) %in% variables)]
      tempTrain <- cbind.data.frame(trainModules$Y, tempTr)
      tempTr <- testModules[, (names(testModules) %in% variables)]
      tempTest <- cbind.data.frame(testModules$Y, tempTr)
      
      colnames(tempTrain)[1] <- "Y"
      colnames(tempTest)[1] <- "Y"
      
      Y <- append(Y, tempTest$Y)
      
      # svm
      svmfit = svm(Y~ ., data = tempTrain , kernel="linear", cost=10, scale=FALSE)
      yhat.SVM = predict(svmfit, newdata = tempTest[, -1])
      append.SVM <- append(append.SVM, yhat.SVM)
      
      # # RF
      # RFfit <- randomForest(Y~ ., data = tempTrain, importance=TRUE, ntree = 100)
      # yhat.RF = predict(RFfit, newdata = tempTest[, -1])
      # append.RF <- append(append.RF, yhat.RF)
      
  } # end of K-fold CV loop

  # aucRF[replicateCV] <- auc(Y, append.RF)
  aucSVM[replicateCV] <- auc(Y, append.SVM)
}
selectedVars

# aucRF
# median(aucRF)
aucSVM
median(aucSVM)


# write.csv(aucSVM, "AUC_Lasso_SVM_NetFeatures_LogBased_withSign_V2_Pre.csv")
# write.csv(aucSVM, "AUC_Lasso_SVM_NetFeatures_LogBased_withSign_V2_Early.csv")
# write.csv(aucSVM, "AUC_Lasso_SVM_NetFeatures_LogBased_withSign_V2_Late.csv")

# write.csv(aucRF, "AUCLasso_RF_NetFeatures_LogBased_V2_Pre.csv")
# write.csv(aucSVM, "AUC_Lasso_SVM_NetFeatures_LogBased_V2_Pre.csv")

# write.csv(aucSVM, "AUC_pre_Thymo_Lasso_SVM.csv")
# write.csv(aucSVM, "AUC_early_Thymo_Lasso_SVM.csv")

# write.csv(aucSVM, "AUC_pre_Thymo_Lasso_SVM_Permuted.csv")
# write.csv(aucSVM, "AUC_early_Thymo_Lasso_SVM_Permuted.csv")


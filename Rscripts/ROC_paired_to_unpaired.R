
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
library(ROCR)
library(ggplot2)
library(plotROC)
library(ROCit)


train <- read.delim("Modules_Pre_32_Variation2_New.txt")
test <- read.delim("Modules_Pre_43_Unmatched_Variation2_New.txt")
features <- c("Y","X351","X389","X391", "X401","X521")

# train <- read.delim("Modules_Early_32_Variation2_New.txt")
# test <- read.delim("Modules_Early_23_Unmatched_Variation2_New.txt")
# features <- c("Y","X93","X112","X178","X200","X278","X327","X362","X443","X449","X478","X486","X500","X514")


tempTrain <- train[, features]
tempTest <- test[, features]

# svm
svmfit = svm(Y~ ., data = tempTrain , kernel="linear", cost=10, scale=T)
yhat.SVM = predict(svmfit, newdata = tempTest[, -1])
yhat.SVM.train = predict(svmfit, newdata = tempTrain[, -1])

# RF
RFfit <- randomForest(Y~ ., data = tempTrain, importance=TRUE, ntree = 100)
yhat.RF = predict(RFfit, newdata = tempTest[, -1])
yhat.RF.train = predict(RFfit, newdata = tempTrain[, -1])

aucRF <- auc(tempTest$Y, yhat.RF)
aucSVM <- auc(tempTest$Y, yhat.SVM)
aucSVM.train <- auc(tempTrain$Y, yhat.SVM.train)
aucRF.train <- auc(tempTrain$Y, yhat.RF.train)

rocSVM.train <- roc(tempTrain$Y, yhat.SVM.train)

rocSVM <- roc(tempTest$Y, yhat.SVM)
rocSVM_obj <- rocit(score = yhat.SVM, class=tempTest$Y)
plot(rocSVM_obj)

rocSVMTrain_obj <- rocit(score = yhat.SVM.train, class=tempTrain$Y)
plot(rocSVMTrain_obj)

pdf(file = "Pre_paired_to_unpaired.pdf", width = 4, height = 4)
plot(rocSVM_obj, col = c(1,"gray50"), legend = FALSE, YIndex = FALSE, grid = FALSE)
lines(rocSVMTrain_obj$TPR ~ rocSVMTrain_obj$FPR, col = 2, lwd = 2)
legend("bottomright", col = c(1,2), c("test", "train"), lwd = 2)
dev.off()

rocRF_obj <- rocit(score = yhat.RF, class=tempTest$Y)
plot(rocRF_obj)
rocRFTrain_obj <- rocit(score = yhat.RF.train, class=tempTrain$Y)
plot(rocRFTrain_obj)

pdf(file = "Pre_paired_to_unpaired.pdf", width = 4, height = 4)
plot(rocRF_obj, col = c(1,"gray50"), legend = FALSE, YIndex = FALSE, grid = FALSE)
lines(rocRFTrain_obj$TPR ~ rocRFTrain_obj$FPR, col = 2, lwd = 2)
legend("bottomright", col = c(1,2), c("test", "train"), lwd = 2)
dev.off()
# rocRF <- roc(tempTest$Y, yhat.RF)
p <- ggroc(rocSVM)
# p <- ggroc(rocRF)
q <- ggroc(rocSVM.train)
p + theme_classic()

p <- rocSVM
q <- rocSVM.train
plot <- ggroc(list(PittCohort = q, validationCohort = p))
plot + theme_classic()


#  the other way
tempTest <- preSubCommonDF[, -ncol(preSubCommonDF)]
tempTrain <- preSubOthersDF[, -ncol(preSubOthersDF)]

# svm
svmfit = svm(Y~ ., data = tempTrain , kernel="linear", cost=10, scale=T)
yhat.SVM = predict(svmfit, newdata = tempTest[, -1])
yhat.SVM.train = predict(svmfit, newdata = tempTrain[, -1])

rocSVM <- roc(tempTest$Y, yhat.SVM)
rocSVM_obj <- rocit(score = yhat.SVM, class=tempTest$Y)
plot(rocSVM_obj)

rocSVMTrain_obj <- rocit(score = yhat.SVM.train, class=tempTrain$Y)
plot(rocSVMTrain_obj)

pdf(file = "Pre_unpaired_to_paired.pdf", width = 4, height = 4)
plot(rocSVM_obj, col = c(1,"gray50"), legend = FALSE, YIndex = FALSE, grid = FALSE)
lines(rocSVMTrain_obj$TPR ~ rocSVMTrain_obj$FPR, col = 2, lwd = 2)
legend("bottomright", col = c(1,2), c("test", "train"), lwd = 2)
# ggsave("test.pdf")
dev.off()

# RF
RFfit <- randomForest(Y~ ., data = tempTrain, importance=TRUE, ntree = 100)
yhat.RF = predict(RFfit, newdata = tempTest[, -1])

aucRF <- auc(tempTest$Y, yhat.RF)
aucSVM <- auc(tempTest$Y, yhat.SVM)
aucSVM.train <- auc(tempTrain$Y, yhat.SVM.train)


rocSVM.train <- roc(tempTrain$Y, yhat.SVM.train)
rocSVM <- roc(tempTest$Y, yhat.SVM)
# rocRF <- roc(tempTest$Y, yhat.RF)
p <- ggroc(rocSVM)
# p <- ggroc(rocRF)
q <- ggroc(rocSVM.train)
p + theme_classic()
# ggsave("AUC_rocSVM_.pdf")

p <- rocSVM
q <- rocSVM.train
plot <- ggroc(list(PittCohort = q, validationCohort = p))
plot + theme_classic()



train <- read.delim("Modules_Early_32_Variation2_New.txt")
test <- read.delim("Modules_Early_23_Unmatched_Variation2_New.txt")
features <- c("Y","X93","X112","X178","X200","X278","X327","X362","X443","X449","X478","X486","X500","X514")


tempTrain <- train[, features]
tempTest <- test[, features]

# svm
svmfit = svm(Y~ ., data = tempTrain , kernel="linear", cost=10, scale=T)
yhat.SVM = predict(svmfit, newdata = tempTest[, -1])
yhat.SVM.train = predict(svmfit, newdata = tempTrain[, -1])

RFfit <- randomForest(Y~ ., data = tempTrain, importance=TRUE, ntree = 100)
yhat.RF = predict(RFfit, newdata = tempTest[, -1])
yhat.RF.train = predict(RFfit, newdata = tempTrain[, -1])
aucRF <- auc(tempTest$Y, yhat.RF)

rocSVM_obj <- rocit(score = yhat.SVM, class=tempTest$Y)
plot(rocSVM_obj)
rocSVMTrain_obj <- rocit(score = yhat.SVM.train, class=tempTrain$Y)
plot(rocSVMTrain_obj)

# pdf(file = "Early_unpaired_to_paired.pdf", width = 4, height = 4)
# plot(rocSVM_obj, col = c(1,"gray50"), legend = FALSE, YIndex = FALSE, grid = FALSE)
# lines(rocSVMTrain_obj$TPR ~ rocSVMTrain_obj$FPR, col = 2, lwd = 2)
# legend("bottomright", col = c(1,2), c("test", "train"), lwd = 2)
# dev.off()

rocRF_obj <- rocit(score = yhat.RF, class=tempTest$Y)
plot(rocRF_obj)
rocRFTrain_obj <- rocit(score = yhat.RF.train, class=tempTrain$Y)
plot(rocRFTrain_obj)

# pdf(file = "Early_unpaired_to_paired.pdf", width = 4, height = 4)
# plot(rocRF_obj, col = c(1,"gray50"), legend = FALSE, YIndex = FALSE, grid = FALSE)
# lines(rocRFTrain_obj$TPR ~ rocRFTrain_obj$FPR, col = 2, lwd = 2)
# legend("bottomright", col = c(1,2), c("test", "train"), lwd = 2)
# dev.off()


# # RF
# RFfit <- randomForest(Y~ ., data = tempTrain, importance=TRUE, ntree = 100)
# yhat.RF = predict(RFfit, newdata = tempTest[, -1])
# 
# aucRF <- auc(tempTest$Y, yhat.RF)
# aucSVM <- auc(tempTest$Y, yhat.SVM)
# aucSVM.train <- auc(tempTrain$Y, yhat.SVM.train)
# 
# 
# rocSVM.train <- roc(tempTrain$Y, yhat.SVM.train)
# rocSVM <- roc(tempTest$Y, yhat.SVM)
# # rocRF <- roc(tempTest$Y, yhat.RF)
# p <- ggroc(rocSVM) 
# # p <- ggroc(rocRF)
# q <- ggroc(rocSVM.train)
# p + theme_classic()
# # ggsave("AUC_rocSVM_.pdf")
# 
# p <- rocSVM
# q <- rocSVM.train
# plot <- ggroc(list(PittCohort = q, validationCohort = p))
# plot + theme_classic() 


# the other way
tempTest <- earlySubCommonDF[, -ncol(earlySubCommonDF)]
tempTrain <- earlySubOthersDF[, -ncol(earlySubOthersDF)]

svmfit = svm(Y~ ., data = tempTrain , kernel="linear", cost=10, scale=T)
yhat.SVM = predict(svmfit, newdata = tempTest[, -1])
yhat.SVM.train = predict(svmfit, newdata = tempTrain[, -1])

RFfit <- randomForest(Y~ ., data = tempTrain, importance=TRUE, ntree = 100)
yhat.RF = predict(RFfit, newdata = tempTest[, -1])
yhat.RF.train = predict(RFfit, newdata = tempTrain[, -1])

aucRF <- auc(tempTest$Y, yhat.RF)
aucSVM <- auc(tempTest$Y, yhat.SVM)
aucSVM.train <- auc(tempTrain$Y, yhat.SVM.train)
aucRF.train <- auc(tempTrain$Y, yhat.RF.train)

rocSVM.train <- roc(tempTrain$Y, yhat.SVM.train)

rocSVM_obj <- rocit(score = yhat.SVM, class=tempTest$Y)
plot(rocSVM_obj)
rocSVMTrain_obj <- rocit(score = yhat.SVM.train, class=tempTrain$Y)
plot(rocSVMTrain_obj)

pdf(file = "Early_paired_to_unpaired.pdf", width = 4, height = 4)
plot(rocSVM_obj, col = c(1,"gray50"), legend = FALSE, YIndex = FALSE, grid = FALSE)
lines(rocSVMTrain_obj$TPR ~ rocSVMTrain_obj$FPR, col = 2, lwd = 2)
legend("bottomright", col = c(1,2), c("test", "train"), lwd = 2)
dev.off()


rocRF_obj <- rocit(score = yhat.RF, class=tempTest$Y)
plot(rocRF_obj)
rocRFTrain_obj <- rocit(score = yhat.RF.train, class=tempTrain$Y)
plot(rocRFTrain_obj)

pdf(file = "Early_paired_to_unpaired.pdf", width = 4, height = 4)
plot(rocRF_obj, col = c(1,"gray50"), legend = FALSE, YIndex = FALSE, grid = FALSE)
lines(rocRFTrain_obj$TPR ~ rocRFTrain_obj$FPR, col = 2, lwd = 2)
legend("bottomright", col = c(1,2), c("test", "train"), lwd = 2)
dev.off()



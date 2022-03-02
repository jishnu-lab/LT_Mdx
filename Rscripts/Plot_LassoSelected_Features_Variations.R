
rm(list = ls())
cat("\014")

setwd("/Users/sar210/Box/MDx_project/")
# setwd("/Users/syedashiqurrahman/Box/MDx_project/")

library(e1071)
library(caret)
library(glmnet)
library(randomForest)
library(matrixStats)
library(readxl)
library(cvAUC)
library(pROC)
library(RColorBrewer)
library(pheatmap)


# variation_1 no absolute
data <- read.delim("trans_Modules_LogBased_Pre_Variation1.txt")
# data <- read.delim("trans_Modules_LogBased_Early_Variation1.txt")

# # variation_2 with considering sign
# data <- read.delim("trans_Modules_LogBased_Pre_Variation2.txt")
# data <- read.delim("trans_Modules_LogBased_Early_Variation2.txt")

# # variation_2 with considering sign
# data <- read.delim("trans_Modules_LogBased_Pre_Variation2_New.txt")
# data <- read.delim("trans_Modules_LogBased_Early_Variation2_New.txt")

Y <- data$Y
# loc1 <- which(Y == 1)
# loc0 <- which(Y == 0)
# Y[loc1] <- "R"
# Y[loc0] <- "NR"

features <- c("X69","X75","X136","X271","X333","X407","X423","X428","X431","X435","X441","X448","X451","X467","X468","X500")

df <- data[, features]
df <- cbind(Y, df)

R <- subset(df, Y == 1)
RMe = apply(R, 2, mean)
NR <- subset(df, Y == 0)
NRMe = apply(NR, 2, mean) 
fc <- log2(RMe/NRMe)

tempDF <- rbind(R, NR)
# tempDF <- tempDF[,-1]

# Taqman Genes sort based on fc
newX <- rbind.data.frame(fc, tempDF)
newX[1,1] <- "fc"
tempX <- newX
newX <- newX[,-1]
sorted <- newX[, order(newX[1,]) ]
Y <- tempX$Y
sorted <- cbind.data.frame(Y, sorted)

tempDF <- sorted[-1,]

# sort within R and NR
sortedR <- sorted[c(2:31), ]
sortedNR <- sorted[c(32:76), ]
sortedR <- sortedR[order(sortedR[, 2]), ]
sortedNR <- sortedNR[order(sortedNR[, 2]), ]


tempDF <- rbind.data.frame(sortedR, sortedNR)

# pheatmap(as.matrix(scale(tempDF[,-1])),  main = "", color = colorRampPalette(rev(brewer.pal(n = 6, name ="RdYlBu")))(10), breaks=c(-5,-4,-3,-2,-1,0,1,2,3,4,5), cluster_rows = F, cluster_cols = F)
pheatmap(as.matrix(scale(tempDF[,-1])),  main = "", color = colorRampPalette(rev(brewer.pal(n = 6, name ="PRGn")))(10), breaks=c(-5,-4,-3,-2,-1,0,1,2,3,4,5), cluster_rows = F, cluster_cols = F)
# pheatmap(as.matrix(scale(tempDF[,-1])),  main = "", color = colorRampPalette(rev(brewer.pal(n = 6, name ="Spectral")))(10), breaks=c(-5,-4,-3,-2,-1,0,1,2,3,4,5), cluster_rows = F, cluster_cols = F)
# write.csv(tempDF, "Pre_Modules_test.csv")
# ggsave("Pre_Modules_heatmap.pdf")


# Early
# # variation_2 with considering sign
# data <- read.delim("trans_Modules_LogBased_Early_Variation1.txt")
data <- read.delim("trans_Modules_LogBased_Early_Variation2.txt")
# data <- read.delim("trans_Modules_LogBased_Early_Variation2_New.txt")

Y <- data$Y
# loc1 <- which(Y == 1)
# loc0 <- which(Y == 0)
# Y[loc1] <- "R"
# Y[loc0] <- "NR"

features <- c("X130","X213","X372")

df <- data[, features]
df <- cbind(Y, df)

R <- subset(df, Y == 1)
RMe = apply(R, 2, mean)
NR <- subset(df, Y == 0)
NRMe = apply(NR, 2, mean) 
fc <- log2(RMe/NRMe)

tempDF <- rbind(R, NR)
# tempDF <- tempDF[,-1]

# Taqman Genes sort based on fc
newX <- rbind.data.frame(fc, tempDF)
newX[1,1] <- "fc"
tempX <- newX
newX <- newX[,-1]
sorted <- newX[, order(newX[1,]) ]
Y <- tempX$Y
sorted <- cbind.data.frame(Y, sorted)

tempDF <- sorted[-1,]


# sort within R and NR
sortedR <- sorted[c(2:24), ]
sortedNR <- sorted[c(25:56), ]

sortedR <- sortedR[order(-sortedR[, 2]), ]
sortedNR <- sortedNR[order(-sortedNR[, 2]), ]


tempDF <- rbind.data.frame(sortedR, sortedNR)

# pheatmap(as.matrix(scale(tempDF[,-1])),  main = "", color = colorRampPalette(rev(brewer.pal(n = 6, name ="RdYlBu")))(10), breaks=c(-5,-4,-3,-2,-1,0,1,2,3,4,5), cluster_rows = F)
pheatmap(as.matrix(scale(tempDF[,-1])),  main = "", color = colorRampPalette(rev(brewer.pal(n = 5, name ="PRGn")))(10), breaks=c(-5,-4,-3,-2,-1,0,1,2,3,4,5), cluster_rows = F, cluster_cols = F)
# pheatmap(as.matrix(scale(tempDF[,-1])),  main = "", color = colorRampPalette(rev(brewer.pal(n = 6, name ="BrBG")))(10), breaks=c(-5,-4,-3,-2,-1,0,1,2,3,4,5), cluster_rows = F)

# ggsave("Early_Modules_heatmap.pdf")

AUC <- c()
for (i in 2:ncol(tempDF)) {
  AUC[i] <- auc(tempDF[,1], tempDF[,i])
}
Modules <- names(tempDF)
ModulesDF <- rbind(Modules, AUC)
ModulesDF <- ModulesDF[,-1]

# write.csv(tempDF, "Early_Modules_with_Sign.csv")



# variation_1 no absolute
# data <- read.delim("trans_Modules_LogBased_Pre_Variation1.txt")
# data <- read.delim("trans_Modules_LogBased_Early_Variation1.txt")

# # variation_2 with considering sign
# data <- read.delim("trans_Modules_LogBased_Pre_Variation2.txt")
# data <- read.delim("trans_Modules_LogBased_Early_Variation2.txt")

# # variation_2 with considering sign
data <- read.delim("trans_Modules_LogBased_Late_Variation2_New.txt")


Y <- data$Y
# loc1 <- which(Y == 1)
# loc0 <- which(Y == 0)
# Y[loc1] <- "R"
# Y[loc0] <- "NR"

features <- c("X55","X128","X248","X251")

df <- data[, features]
df <- cbind(Y, df)

R <- subset(df, Y == 1)
RMe = apply(R, 2, mean)
NR <- subset(df, Y == 0)
NRMe = apply(NR, 2, mean) 
fc <- log2(RMe/NRMe)

tempDF <- rbind(R, NR)
# tempDF <- tempDF[,-1]

# Taqman Genes sort based on fc
newX <- rbind.data.frame(fc, tempDF)
newX[1,1] <- "fc"
tempX <- newX
newX <- newX[,-1]
sorted <- newX[, order(newX[1,]) ]
Y <- tempX$Y
sorted <- cbind.data.frame(Y, sorted)

tempDF <- sorted[-1,]

# sort within R and NR
sortedR <- sorted[c(2:31), ]
sortedNR <- sorted[c(32:56), ]
sortedR <- sortedR[order(sortedR[, 2]), ]
sortedNR <- sortedNR[order(sortedNR[, 2]), ]


tempDF <- rbind.data.frame(sortedR, sortedNR)

# pheatmap(as.matrix(scale(tempDF[,-1])),  main = "", color = colorRampPalette(rev(brewer.pal(n = 6, name ="RdYlBu")))(10), breaks=c(-5,-4,-3,-2,-1,0,1,2,3,4,5), cluster_rows = F, cluster_cols = F)
pheatmap(as.matrix(scale(tempDF[,-1])),  main = "", color = colorRampPalette(rev(brewer.pal(n = 6, name ="PRGn")))(10), breaks=c(-5,-4,-3,-2,-1,0,1,2,3,4,5), cluster_rows = F, cluster_cols = F)
# ggsave("Late_Modules_heatmap.pdf")
# ggsave("Late_Modules_heatmap_Final.pdf")
# write.csv(tempDF, "Late_Modules_test.csv")


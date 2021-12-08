
rm(list = ls())
cat("\014")

library(pls)
library(ggplot2)
library(pROC)
library(cvAUC)
# library(plot3D)

setwd("/Users/sar210/Box/MDx_project/")

# # variation_2 with considering sign
Dataset <- read.delim("trans_Modules_LogBased_Pre_Variation2.txt")

# Y <- data$Y
# loc1 <- which(Y == 1)
# loc0 <- which(Y == 0)
# Y[loc1] <- "R"
# Y[loc0] <- "NR"

# features <- c("X69","X75","X136","X271","X333","X407","X423","X428","X431","X435","X441","X448","X451","X467","X468","X500","X548")
features <- c("X69","X75","X136","X271","X333","X407","X423","X428","X431","X435","X441","X448","X451","X467","X468","X500")


Y <- Dataset$Y
Outcome <- Dataset$Y
loc1 <- which(Outcome == 1)
loc0 <- which(Outcome == 0)
Outcome[loc1] <- "R"
Outcome[loc0] <- "TR"

data <- Dataset[, features]

# Partial Least Squares

data <- cbind.data.frame(Y, data)
# write.csv(data, "pre_Modules.csv")

pls.fit = plsr(Y~., data = data, scale=TRUE)

# pls.fit = plsr(Y~., data = data, scale=TRUE, validation="CV")

summary(pls.fit)
pls.fit$loadings

AUC(pls.fit$scores[,1], Dataset$Y)

# validationplot(pls.fit, val.type="MSEP")

PC1 <- pls.fit$scores[,1]
PC2 <- pls.fit$scores[,2]
dF <- cbind.data.frame(Outcome, PC1, PC2)
dF$Outcome <- as.factor(dF$Outcome)
p <- ggplot(dF, aes(x=PC1, y=PC2, colour = Outcome)) + geom_point()
# p <- ggplot(dF, aes(x=PC1, y=PC2), colour = c("red", "green")) + geom_point()
p + theme_classic()  + xlab("LV1") + ylab("LV2")
# ggsave("PLS_LassoFeatures_pre.pdf")

selectedDF <- subset(dF, (PC1 > 1.8 | PC2 > 2) & Outcome == "R")

# dF$Name = c(1:nrow(dF))
# p <- ggplot(dF, aes(x=PC1, y=PC2, colour = Outcome)) + geom_point()+ theme_classic()  + xlab("LV1") + ylab("LV2") + geom_text(aes(label=Name),hjust=0, vjust=0)
# p
# ggsave("PLS_LassoFeatures_pre_Annotated.pdf")

# pr.out=prcomp(data[,-1], scale = TRUE)
# PC1 <- pr.out$x[,1]
# PC2 <- pr.out$x[,2]
# dF <- cbind.data.frame(Outcome, PC1, PC2)
# dF$Y <- as.factor(dF$Y)
# p <- ggplot(dF, aes(x=PC1, y=PC2, colour = Outcome)) + geom_point()
# p + theme_classic()  + xlab("pc1") + ylab("pc2")
# # ggsave("PCA_LassoFeatures_pre.pdf")




Dataset <- read.delim("trans_Modules_LogBased_Early_Variation2.txt")

Y <- Dataset$Y
Outcome <- Dataset$Y
loc1 <- which(Outcome == 1)
loc0 <- which(Outcome == 0)
Outcome[loc1] <- "R"
Outcome[loc0] <- "TR"

features <- c("X130","X213","X372")

data <- Dataset[, features]
# data <- cbind.data.frame(Y, data)

# Partial Least Squares

data <- cbind.data.frame(Y, data)

# write.csv(data, "early_Modules.csv")
pls.fit = plsr(Y~., data = data, scale=TRUE)
# pls.fit = plsr(Y~., data = data)

# pls.fit = plsr(Y~., data = data, scale=TRUE, validation="CV")

summary(pls.fit)
pls.fit$loadings

AUC(pls.fit$scores[,1], Dataset$Y)

# validationplot(pls.fit, val.type="MSEP")

PC1 <- pls.fit$scores[,1]
PC2 <- pls.fit$scores[,2]
dF <- cbind.data.frame(Outcome, PC1, PC2)
dF$Outcome <- as.factor(dF$Outcome)
p <- ggplot(dF, aes(x=PC1, y=PC2, colour = Outcome)) + geom_point()
# p <- ggplot(dF, aes(x=PC1, y=PC2), colour = c("red", "green")) + geom_point()
p + theme_classic()  + xlab("LV1") + ylab("LV2")
# ggsave("PLS_LassoFeatures_early.pdf")

# dF$Name = c(1:nrow(dF))
# p <- ggplot(dF, aes(x=PC1, y=PC2, colour = Outcome)) + geom_point()+ theme_classic()  + xlab("LV1") + ylab("LV2") + geom_text(aes(label=Name),hjust=0, vjust=0)
# p
# ggsave("PLS_LassoFeatures_early_Annotated.pdf")
# write.csv()

# pr.out=prcomp(data[,-1], scale = TRUE)
# PC1 <- pr.out$x[,1]
# PC2 <- pr.out$x[,2]
# dF <- cbind.data.frame(Outcome, PC1, PC2)
# dF$Y <- as.factor(dF$Y)
# p <- ggplot(dF, aes(x=PC1, y=PC2, colour = Outcome)) + geom_point()
# p + theme_classic()  + xlab("pc1") + ylab("pc2")
# # ggsave("PCA_LassoFeatures_pre.pdf")


Dataset <- read.delim("trans_Modules_LogBased_Late_Variation2_New.txt")

Y <- Dataset$Y
Outcome <- Dataset$Y
loc1 <- which(Outcome == 1)
loc0 <- which(Outcome == 0)
Outcome[loc1] <- "R"
Outcome[loc0] <- "NR"

features <- c("X55","X128","X248","X251")

data <- Dataset[, features]
# data <- cbind.data.frame(Y, data)

# Partial Least Squares

data <- cbind.data.frame(Y, data)

pls.fit = plsr(Y~., data = data, scale=TRUE)
# pls.fit = plsr(Y~., data = data)

# pls.fit = plsr(Y~., data = data, scale=TRUE, validation="CV")

summary(pls.fit)
pls.fit$loadings

AUC(pls.fit$scores[,1], Dataset$Y)

# validationplot(pls.fit, val.type="MSEP")

PC1 <- pls.fit$scores[,1]
PC2 <- pls.fit$scores[,2]
dF <- cbind.data.frame(Outcome, PC1, PC2)
dF$Outcome <- as.factor(dF$Outcome)
p <- ggplot(dF, aes(x=PC1, y=PC2, colour = Outcome)) + geom_point()
# p <- ggplot(dF, aes(x=PC1, y=PC2), colour = c("red", "green")) + geom_point()
p + theme_classic()  + xlab("LV1") + ylab("LV2")
# ggsave("Late_module_PLS.pdf")



# # EARLY - PRE
# df1 <- read.delim("Modules_Pre_32_Variation2_New.txt")
# df2 <- read.delim("Modules_Early_32_Variation2_New.txt")
# 
# features <- c("Y","X60","X178","X253","X277","X421","X500")
# 
# df1 <- df1[, features]
# df2 <- df2[, features]
# 
# Y <- df1$Y
# Dataset <- df2 - df1
# Dataset$Y <- Y
# 
# Outcome <- Dataset$Y
# loc1 <- which(Outcome == 1)
# loc0 <- which(Outcome == 0)
# Outcome[loc1] <- "R"
# Outcome[loc0] <- "TR"
# 
# features <- c("X60","X178","X253","X277","X421","X500")
# 
# data <- Dataset[, features]
# # data <- cbind.data.frame(Y, data)
# 
# # Partial Least Squares
# 
# data <- cbind.data.frame(Y, data)
# 
# pls.fit = plsr(Y~., data = data, scale=TRUE)
# 
# summary(pls.fit)
# pls.fit$loadings
# 
# AUC(pls.fit$scores[,1], Dataset$Y)
# 
# 
# PC1 <- pls.fit$scores[,1]
# PC2 <- pls.fit$scores[,2]
# dF <- cbind.data.frame(Outcome, PC1, PC2)
# dF$Outcome <- as.factor(dF$Outcome)
# p <- ggplot(dF, aes(x=PC1, y=PC2, colour = Outcome)) + geom_point()
# # p <- ggplot(dF, aes(x=PC1, y=PC2), colour = c("red", "green")) + geom_point()
# p + theme_classic()  + xlab("LV1") + ylab("LV2")
# # ggsave("Late_module_PLS.pdf")

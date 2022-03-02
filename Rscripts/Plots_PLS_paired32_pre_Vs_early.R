
rm(list = ls())
cat("\014")

setwd("/Users/sar210/Box/MDx_project/")
# setwd("/Users/syedashiqurrahman/Box/MDx_project/")

library(matrixStats)
library(readxl)
library(stringr)
library(rapport)

df <- read_excel("All_Samples_with_outcome.xlsx", 1)
pre <- subset(df, Cohort == "Pre-Tx")
early <- subset(df, Cohort == "Early Post Tx")

commonSubjects <- intersect(pre$`Subject ID`, early$`Subject ID`)
# pre <- subset(pre, `Subject ID` %in% commonSubjects)
# early <- subset(early, `Subject ID` %in% commonSubjects)


preDF <- read.delim("trans_Modules_LogBased_Pre_Variation2.txt")
preDF <- cbind.data.frame(pre, preDF[,-1])

earlyDF <- read.delim("trans_Modules_LogBased_Early_Variation2.txt")
earlyDF <- cbind.data.frame(early, earlyDF[,-1])

pre <- subset(preDF, `Subject ID` %in% commonSubjects)
early <- subset(earlyDF, `Subject ID` %in% commonSubjects)


features <- c("Y","X60","X178","X253","X277","X421","X500")

data <- pre[, features]
# Dataset <- pre[, features]

Y <- data$Y
Outcome <- Y
loc1 <- which(Outcome == 1)
loc0 <- which(Outcome == 0)
Outcome[loc1] <- "R"
Outcome[loc0] <- "TR"

pls.fit = plsr(Y~., data = data, scale=TRUE)

PC1 <- pls.fit$scores[,1]
PC2 <- pls.fit$scores[,2]
dF <- cbind.data.frame(Outcome, PC1, PC2)
dF$Outcome <- as.factor(dF$Outcome)
p <- ggplot(dF, aes(x=PC1, y=PC2, colour = Outcome)) + geom_point()
p + theme_classic()  + xlab("LV1") + ylab("LV2")
# ggsave("PLS_LassoFeatures_pre_common32.pdf")


data <- early[, features]

Y <- data$Y
Outcome <- Y
loc1 <- which(Outcome == 1)
loc0 <- which(Outcome == 0)
Outcome[loc1] <- "R"
Outcome[loc0] <- "TR"

pls.fit = plsr(Y~., data = data, scale=TRUE)

PC1 <- pls.fit$scores[,1]
PC2 <- pls.fit$scores[,2]
dF <- cbind.data.frame(Outcome, PC1, PC2)
dF$Outcome <- as.factor(dF$Outcome)
p <- ggplot(dF, aes(x=PC1, y=PC2, colour = Outcome)) + geom_point()
p + theme_classic()  + xlab("LV1") + ylab("LV2")
# ggsave("PLS_LassoFeatures_early_common32.pdf")



# preDF <- read.delim("preCombinedFinal.txt")
# preSubCommonDF <- subset(preDF, PID %in% pre$ID)
# preSubOthersDF <- subset(preDF, !(PID %in% pre$ID))
# 
# earlyDF <- read.delim("earlyCombinedFinal.txt")
# earlySubCommonDF <- subset(earlyDF, PID %in% early$ID)
# earlySubOthersDF <- subset(earlyDF, !(PID %in% early$ID))



rm(list = ls())
cat("\014")

setwd("/Users/sar210/Box/MDx_project/")

# EARLY - PRE
df1 <- read.delim("Modules_Pre_32_Variation2_New.txt")
df2 <- read.delim("Modules_Early_32_Variation2_New.txt")

features <- c("Y","X60","X178","X253","X277","X421","X500")

df1 <- df1[, features]
df2 <- df2[, features]

Y <- df1$Y
# Dataset <- df2 - df1
Dataset <- df1

Outcome <- Dataset$Y
loc1 <- which(Outcome == 1)
loc0 <- which(Outcome == 0)
Outcome[loc1] <- "R"
Outcome[loc0] <- "TR"

features <- c("X60","X178","X253","X277","X421","X500")

data <- Dataset[, features]
data <- cbind.data.frame(Y, data)

pls.fit = plsr(Y~., data = data, scale=TRUE)

PC1 <- pls.fit$scores[,1]
PC2 <- pls.fit$scores[,2]
dF <- cbind.data.frame(Outcome, PC1, PC2)
dF$Outcome <- as.factor(dF$Outcome)
p <- ggplot(dF, aes(x=PC1, y=PC2, colour = Outcome)) + geom_point()
p + theme_classic()  + xlab("LV1") + ylab("LV2")
ggsave("pre_module_PLS_common32.pdf")


Dataset <- df2
Outcome <- Dataset$Y
loc1 <- which(Outcome == 1)
loc0 <- which(Outcome == 0)
Outcome[loc1] <- "R"
Outcome[loc0] <- "TR"

features <- c("X60","X178","X253","X277","X421","X500")

data <- Dataset[, features]
data <- cbind.data.frame(Y, data)

pls.fit = plsr(Y~., data = data, scale=TRUE)

PC1 <- pls.fit$scores[,1]
PC2 <- pls.fit$scores[,2]
dF <- cbind.data.frame(Outcome, PC1, PC2)
dF$Outcome <- as.factor(dF$Outcome)
p <- ggplot(dF, aes(x=PC1, y=PC2, colour = Outcome)) + geom_point()
p + theme_classic()  + xlab("LV1") + ylab("LV2")
# ggsave("early_module_PLS_common32.pdf")


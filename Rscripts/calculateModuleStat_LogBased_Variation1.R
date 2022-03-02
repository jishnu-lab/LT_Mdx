
rm(list = ls())
cat("\014")

setwd("/Users/sar210/Box/MDx_project/")
# setwd("/Users/syedashiqurrahman/Box/MDx_project/")

library(matrixStats)
library(readxl)
library(stringr)
library(rapport)

# data <- read.delim("preCombinedFinal.txt")
data <- read.delim("earlyCombinedFinal.txt")
# data <- read.delim("lateCombinedFinal.txt")

Y <- data$Y
# Zr <- subset(data, Y==1)
# Znr <- subset(data, Y==0)

# data <- read.csv("preCombinedFinal.csv")
# data <- read.csv("earlyCombinedFinal.csv")
# data <- read.csv("lateCombinedFinal.csv")


modules <- read.csv("OutputClusterOne_s_5_d_6.csv")
# modules <- read.csv("OutputClusterOne_s_6_d_5.csv")

modules <- modules[-c(32, 81, 269, 302, 305, 422, 492),]

tempModules <- modules

M <- matrix(0, nrow=555, ncol=nrow(data))
# i <- 1
i <- 122
for (i in 1:nrow(tempModules))
{
  
  module <- tempModules[i, ]
  # if(unique(module[module != ""]))
  module <- unique(module[module != ""])
  
  # module <- module[2:length(module)]
  subDF <- data[ , names(data) %in% module]
  # # removing features with 0's
  # limit <- 0.1 * nrow(subDF)
  # subDF <- subDF[, which(apply(subDF, 2, function(col) !any(table(col) > limit)))]
  subDF <- cbind.data.frame(Y, subDF)
  
  Zr <- subset(subDF, Y==1)
  Znr <- subset(subDF, Y==0)
  muZr <- colMeans(Zr)
  muZnr <- colMeans(Znr)
  muZr <- muZr[c(2:length(muZr))]
  muZnr <- muZnr[c(2:length(muZnr))]
  
  # m <- log((subDF)^2/(muZr*muZnr))
  # m <- log1p(((tempSubDF)^2)/(muZr*muZnr))

  # m <- c()
  # tempSubDF <- subDF[,-1]
  # for (j in 1:ncol(tempSubDF)) {
  #   m[j] <- log1p(((tempSubDF[i, j])^2)/(muZr[j]*muZnr[j]))
  # }
  tempSubDF <- subDF[,-1]
  m <- c()
  N <- c()
  for (k in 1:nrow(as.matrix(tempSubDF))) {
    for (j in 1:ncol(as.matrix(tempSubDF))) {
      m[j] <- log1p(((tempSubDF[k, j])^2)/(muZr[j]*muZnr[j]))
    }
    # m  <- abs(m)
    N[k] <- sum(m)  
  }
  M[i, ] <- N
}

modulePA <- t(M)
Y <- data$Y
modulePA <- cbind.data.frame(Y, modulePA)


# write.table(modulePA, file = "trans_Modules_LogBased_Pre_Variation1.txt", sep = "\t", row.names=F, quote = F)
# write.table(modulePA, file = "trans_Modules_LogBased_Early_Variation1.txt", sep = "\t", row.names=F, quote = F)
# write.table(modulePA, file = "NetFeatures_LogBased_Late_Variation1.txt", sep = "\t", row.names=F, quote = F)



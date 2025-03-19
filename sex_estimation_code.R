#setwd("C:/Users/Nefeli/Desktop/Lab/Διπλωματικές/Διπλωματικές 2023-2024/Κωνσταντίνος/Analysis/Sex estimation")

library(readr)
library(caret) # for the partition of the dataset
library(e1071)
library(tidyverse)
library(readxl)
library(corrplot)
library(RColorBrewer)
library(xtable)
library(Hmisc)

# corstars function 
# source: http://www.sthda.com/english/wiki/elegant-correlation-table-using-xtable-r-package
corstars <-function(x, method=c("pearson", "spearman"), 
                    removeTriangle=c("upper", "lower"),
                    result=c("none", "html", "latex")){
  #Compute correlation matrix
  require(Hmisc)
  x <- as.matrix(x)
  correlation_matrix<-rcorr(x, type=method[1])
  R <- correlation_matrix$r # Matrix of correlation coeficients
  p <- correlation_matrix$P # Matrix of p-value 
  
  ## Define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .0001, "****", ifelse(p < .001, "*** ", ifelse(p < .01, "**  ", ifelse(p < .05, "*   ", "    "))))
  
  ## trunctuate the correlation matrix to two decimal
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1]
  
  ## build a new matrix that includes the correlations with their apropriate stars
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x))
  diag(Rnew) <- paste(diag(R), " ", sep="")
  rownames(Rnew) <- colnames(x)
  colnames(Rnew) <- paste(colnames(x), "", sep="")
  
  ## remove upper triangle of correlation matrix
  if(removeTriangle[1]=="upper"){
    Rnew <- as.matrix(Rnew)
    Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }
  
  ## remove lower triangle of correlation matrix
  else if(removeTriangle[1]=="lower"){
    Rnew <- as.matrix(Rnew)
    Rnew[lower.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }
  
  ## remove last column and return the correlation matrix
  Rnew <- cbind(Rnew[1:length(Rnew)-1])
  if (result[1]=="none") return(Rnew)
  else{
    if(result[1]=="html") print(xtable(Rnew), type="html")
    else print(xtable(Rnew), type="latex") 
  }
} 

data <- read.csv("data_full.csv", sep = ",")
data <- data[, -c(1, 5:20, 29)]

vars = c("ML_MT1", "DPP_MT1", "ML_MT2", "DPP_MT2", "ML_MC1", "DPP_MC1", 
         "ML_MC2", "DPP_MC2")

names(data)[4:11] <- vars

data_m <- data[data$Sex == "M", ]
data_f <- data[data$Sex == "F", ]


# Correlation analysis for pooled
cc <- cor(data[ , 4:11], method = "spearman", use = "complete.obs")
  
png("Correlation plot - Pooled.png", 
    width=1500, height=1500, res=300)
corrplot(cc, method="circle",type="upper",
         title="Correlation plot - Pooled", mar=c(1,1,2,1),
         col=COL2('BrBG', 10), tl.col="black",
         cl.ratio = 0.1, tl.srt = 0, tl.cex = 0.7)
dev.off()
  
# Using the corstars function, save the correlation matrix with ss levels
cc_tab <- corstars(data[ , 4:11])
write.csv(cc_tab, paste0("Correlation table - Pooled.csv"))


remove(cc, cc_tab)

# Correlation analysis for female sample
cc <- cor(data_f[ , 4:11], method = "spearman", use = "complete.obs")

png("Correlation plot - Fem.png", 
    width=1500, height=1500, res=300)
corrplot(cc, method="circle",type="upper",
         title="Correlation plot - Female sample", mar=c(1,1,2,1),
         col=COL2('BrBG', 10), tl.col="black",
         cl.ratio = 0.1, tl.srt = 0, tl.cex = 0.7)
dev.off()

# Using the corstars function, save the correlation matrix with ss levels
cc_tab <- corstars(data_f[ , 4:11])
write.csv(cc_tab, paste0("Correlation table - Fem.csv"))


remove(cc, cc_tab)

# Correlation analysis for male sample
cc <- cor(data_m[ , 4:11], method = "spearman", use = "complete.obs")

png("Correlation plot - Male.png", 
    width=1500, height=1500, res=300)
corrplot(cc, method="circle",type="upper",
         title="Correlation plot - Male sample", mar=c(1,1,2,1),
         col=COL2('BrBG', 10), tl.col="black",
         cl.ratio = 0.1, tl.srt = 0, tl.cex = 0.7)
dev.off()

# Using the corstars function, save the correlation matrix with ss levels
cc_tab <- corstars(data_m[ , 4:11])
write.csv(cc_tab, paste0("Correlation table - Male.csv"))


remove(cc, cc_tab)


### SVM classification
data <- read.csv("data_full.csv", sep = ",")
data <- data[, -c(1, 5:20, 29)]

set.seed(1997)
dataset <- data
dataset$Sex <- as.factor(dataset$Sex)
partition = createDataPartition(dataset$Sex, times = 5, p=0.8)


results_mt1 <- matrix(NA, nrow = 5, ncol =6)
results_mt2 <- matrix(NA, nrow = 5, ncol =6)
results_mt <- matrix(NA, nrow = 5, ncol =6)
results_mc1 <- matrix(NA, nrow = 5, ncol =6)
results_mc2 <- matrix(NA, nrow = 5, ncol =6)
results_mc <- matrix(NA, nrow = 5, ncol =6)
results_com1 <- matrix(NA, nrow = 5, ncol =6)
results_com2 <- matrix(NA, nrow = 5, ncol =6)
results_com <- matrix(NA, nrow = 5, ncol =6)

## In the following segment, the SVM parameters (eg. "kernel", "cost") were
## changed accordingly each time a new set was examined. In its current form,
## the scripts will compute 3rd degree polynomial SVMs, with cost = 100.

for (i in 1:5)
{
  
  idx <- partition[[i]]
  train <- dataset[idx, ]
  
  test <- dataset[-idx, ]
  vars <- colnames(train)[4:11]
  
  # MTs
  
  ml_mt1 <- svm(formula = Sex ~ MLMT1 + MT1, 
                data = train, kernel = "polynomial", cost=10, degree=3, #gamma=0.001,
                scale = TRUE)
  
  pred_mt1 <- predict(ml_mt1, newdata = test[ , c(4,5)])
  
  ref <- test[,c(2,4,5)]
  ref <- na.omit(ref)
  
  mt1 <- confusionMatrix(data=pred_mt1, reference = ref[,1])
  
  results_mt1[i,] <- c(mt1$overall[c(1,3,4)], mt1$byClass[c(1,2,11)]) 
  
  ml_mt2 <- svm(formula = Sex ~ MLMT2 + MT2, 
                 data = train, kernel = "polynomial", cost=10, degree=3, #gamma=0.001,
                 scale = TRUE)
  
  pred_mt2 <- predict(ml_mt2, newdata = test[ , c(6,7)])
  
  ref <- test[,c(2,6,7)]
  ref <- na.omit(ref)
  
  mt2 <- confusionMatrix(data=pred_mt2, reference = ref[,1])
  
  results_mt2[i,] <- c(mt2$overall[c(1,3,4)], mt2$byClass[c(1,2,11)])
  
  ml_mt <- svm(formula = Sex ~ MLMT1 + MT1 + MLMT2 + MT2, 
              data = train, kernel = "polynomial", cost=10, degree=3, #gamma=0.001,
              scale = TRUE)

  pred_mt <- predict(ml_mt, newdata = test[ , c(4:7)])
  
  ref <- test[,c(2,4:7)]
  ref <- na.omit(ref)
  
  mt <- confusionMatrix(data=pred_mt, reference = ref[,1])
  
  results_mt[i,] <- c(mt$overall[c(1,3,4)], mt$byClass[c(1,2,11)])
  
  # MCs
  
  ml_mc1 <- svm(formula = Sex ~ MLMC1 + MC1, 
                data = train, kernel = "polynomial", cost=10, degree=3, #gamma=0.001,
                scale = TRUE)
  
  pred_mc1 <- predict(ml_mc1, newdata = test[ , c(8,9)])
  
  ref <- test[,c(2,8,9)]
  ref <- na.omit(ref)
  
  mc1 <- confusionMatrix(data=pred_mc1, reference = ref[,1])
  
  results_mc1[i,] <- c(mc1$overall[c(1,3,4)], mc1$byClass[c(1,2,11)])
  
  ml_mc2 <- svm(formula = Sex ~ MLMC2 + MC2 , 
                 data = train, kernel = "polynomial", cost=10, degree=3, #gamma=0.001,
                 scale = TRUE)
  
  pred_mc2 <- predict(ml_mc2, newdata = test[ , c(10,11)])
  
  ref <- test[,c(2,10,11)]
  ref <- na.omit(ref)
  
  mc2 <- confusionMatrix(data=pred_mc2, reference = ref[,1])
  
  results_mc2[i,] <- c(mc2$overall[c(1,3,4)], mc2$byClass[c(1,2,11)])
  
  ml_mc <- svm(formula = Sex ~ MLMC1 + MC1 + MLMC2 + MC2 , 
               data = train, kernel = "polynomial", cost=10, degree=3, #gamma=0.001,
               scale = TRUE)
  
  pred_mc <- predict(ml_mc, newdata = test[ , c(8:11)])
  
  ref <- test[,c(2,8:11)]
  ref <- na.omit(ref)
  
  mc <- confusionMatrix(data=pred_mc, reference = ref[,1])
  
  results_mc[i,] <- c(mc$overall[c(1,3,4)], mc$byClass[c(1,2,11)])
  
  # Combo
  
  ml_1 <- svm(formula = Sex ~ MLMC1 + MC1 + MLMT1 + MT1, 
              data = train, kernel = "polynomial", cost=10, degree=3, #gamma=0.001,
              scale = TRUE)
  
  pred_1 <- predict(ml_1, newdata = test[ , c(4,5,8,9)])
  
  ref <- test[,c(2,4,5,8,9)]
  ref <- na.omit(ref)
  
  com1 <- confusionMatrix(data=pred_1, reference = ref[,1])
  
  results_com1[i,] <- c(com1$overall[c(1,3,4)], com1$byClass[c(1,2,11)])
  
  
  ml_2 <- svm(formula = Sex ~ MLMC2 + MC2 + MLMT2 + MT2, 
              data = train, kernel = "polynomial", cost=10, degree=3, #gamma=0.001,
              scale = TRUE)
  
  pred_2 <- predict(ml_2, newdata = test[ , c(6,7,10,11)])
  
  ref <- test[,c(2,6,7,10,11)]
  ref <- na.omit(ref)
  
  com2 <- confusionMatrix(data=pred_2, reference = ref[,1])
  
  results_com2[i,] <- c(com2$overall[c(1,3,4)], com2$byClass[c(1,2,11)])
  
  ml <- svm(formula = Sex ~ MLMC1 + MC1 + MLMT1 + MT1 + MLMC2 + MC2 + MLMT2
            + MT2, 
            data = train, kernel = "polynomial", cost=10, degree=3, #gamma=0.001,
            scale = TRUE)
  
  pred <- predict(ml, newdata = test[ , c(4:11)])
  
  ref <- test[,c(2,4:11)]
  ref <- na.omit(ref)
  
  com <- confusionMatrix(data=pred, reference = ref[,1])
  
  results_com[i,] <- c(com$overall[c(1,3,4)], com$byClass[c(1,2,11)])
  
}

results_mc <- rbind(results_mc, apply(results_mc, 2, mean))
results_mc1 <- rbind(results_mc1, apply(results_mc1, 2, mean))
results_mc2 <- rbind(results_mc2, apply(results_mc2, 2, mean))
results_mt <- rbind(results_mt, apply(results_mt, 2, mean))
results_mt1 <- rbind(results_mt1, apply(results_mt1, 2, mean))
results_mt2 <- rbind(results_mt2, apply(results_mt2, 2, mean))
results_com <- rbind(results_com, apply(results_com, 2, mean))
results_com1 <- rbind(results_com1, apply(results_com1, 2, mean))
results_com2 <- rbind(results_com2, apply(results_com2, 2, mean))


metacarpals <- rbind(results_mc[6,], results_mc1[6,], results_mc2[6,])
metatarsals <- rbind(results_mt[6,], results_mt1[6,], results_mt2[6,])
combination <- rbind(results_com[6,], results_com1[6,], results_com2[6,])

colnames(metacarpals) <- c("Accuracy", "Upper CI", "Lower CI",
                           "Sensitivity", "Specificity", "Balanced Accuracy")
write.csv(metacarpals, "CV_Metrics_MCs.csv")

colnames(metatarsals) <- c("Accuracy", "Upper CI", "Lower CI",
                           "Sensitivity", "Specificity", "Balanced Accuracy")
write.csv(metatarsals, "CV_Metrics_MTs.csv")

colnames(combination) <- c("Accuracy", "Upper CI", "Lower CI",
                           "Sensitivity", "Specificity", "Balanced Accuracy")
write.csv(combination, "CV_Metrics_Combo.csv")

rm(list=ls())

metacarpals <- read.csv("CV_Metrics_MCs.csv")
metacarpals <- metacarpals[,-1]
metacarpals <- round(metacarpals, 3)
rownames(metacarpals)[1] <- "Both MCs"
rownames(metacarpals)[2] <- "MC1"
rownames(metacarpals)[3] <- "MC2"

metatarsals <- read.csv("CV_Metrics_MTs.csv")
metatarsals <- metatarsals[,-1]
metatarsals <- round(metatarsals, 3)
rownames(metatarsals)[1] <- "Both MTs"
rownames(metatarsals)[2] <- "MT1"
rownames(metatarsals)[3] <- "MT2"

combination <- read.csv("CV_Metrics_Combo.csv")
combination <- combination[,-1]
combination <- round(combination, 3)
rownames(combination)[1] <- "All 4 bones"
rownames(combination)[2] <- "MC1, MT1"
rownames(combination)[3] <- "MC2, MT2"

results <- rbind(combination, metacarpals, metatarsals)
write.table(results, "Results_polynomial_C10.csv", sep=";") #the csv with the avg metrics


rm(list=ls())

### SVM classification for Left & Right MC2 (DPP_MC2)
data <- read.csv("data_full.csv", sep = ",")
data <- data[, -c(1, 5:15, 17:19, 21:27, 29)]

set.seed(1997)
dataset <- data
dataset$Sex <- as.factor(dataset$Sex)
partition = createDataPartition(dataset$Sex, times = 5, p=0.8)


results_mcl <- matrix(NA, nrow = 5, ncol =6)
results_mcr <- matrix(NA, nrow = 5, ncol =6)
results_mc <- matrix(NA, nrow = 5, ncol =6)

for (i in 1:5)
{
  
  idx <- partition[[i]]
  train <- dataset[idx, ]
  
  test <- dataset[-idx, ]
  vars <- colnames(train)[4:6]
  
  # Right Side
  ml_mcr <- svm(formula = Sex ~ MCR2, 
                data = train, kernel = "polynomial", cost=100, degree=3,
                scale = TRUE)
  
  pred_mcr <- predict(ml_mcr, newdata = test[ , 4])
  
  ref <- test[,c(2,4)]
  ref <- na.omit(ref)
  
  mcr <- confusionMatrix(data=pred_mcr, reference = ref[,1])
  
  results_mcr[i,] <- c(mcr$overall[c(1,3,4)], mcr$byClass[c(1,2,11)]) 
  
  # Left Side
  ml_mcl <- svm(formula = Sex ~ MCL2, 
                data = train, kernel = "polynomial", cost=100, degree=3,
                scale = TRUE)
  
  pred_mcl <- predict(ml_mcl, newdata = test[ , 5])
  
  ref <- test[,c(2,5)]
  ref <- na.omit(ref)
  
  mcl <- confusionMatrix(data=pred_mcl, reference = ref[,1])
  
  results_mcl[i,] <- c(mcl$overall[c(1,3,4)], mcl$byClass[c(1,2,11)])
  
  # Average
  ml_mc <- svm(formula = Sex ~ MC2, 
               data = train, kernel = "polynomial", cost=100, degree=3,
               scale = TRUE)
  
  pred_mc <- predict(ml_mc, newdata = test[ , 6])
  
  ref <- test[,c(2,6)]
  ref <- na.omit(ref)
  
  mc <- confusionMatrix(data=pred_mc, reference = ref[,1])
  
  results_mc[i,] <- c(mc$overall[c(1,3,4)], mc$byClass[c(1,2,11)])
  
}

results_mcr <- rbind(results_mcr, apply(results_mcr, 2, mean))
results_mcl <- rbind(results_mcl, apply(results_mcl, 2, mean))
results_mc <- rbind(results_mc, apply(results_mc, 2, mean))


two_sides <- rbind(results_mcr[6,], results_mcl[6,], results_mc[6,])

colnames(two_sides) <- c("Accuracy", "Upper CI", "Lower CI",
                         "Sensitivity", "Specificity", "Balanced Accuracy")
rownames(two_sides) <- vars
write.csv(two_sides, "CV_Metrics_BA_MCs.csv")

rm(list=ls())

### PCA

library('corrr')
library('ggfortify')


data <- read.csv("data_full.csv", sep = ",")
data <- data[, -c(1, 5:20, 29)]


  # Both MCs
  
  data_mcs <- scale(data[,8:10])
  data_num <- data_mcs
  
  data_mcs <- as.data.frame(cbind(data[,2], data_mcs))
  colnames(data_mcs)[1] <- "Sex"
  
  data_mcs <- na.omit(data_mcs)
  data_num <- na.omit(data_num)
  
  mcs_pca <- princomp(data_num)
  summary(mcs_pca)
  
  png("PCA plot - MCs.png", 
      width=1500, height=1500, res=300)
  autoplot(mcs_pca, data=data_mcs, colour='Sex', shape='Sex', 
           loadings = TRUE, loadings.colour = 'blue',
           loadings.label = TRUE, loadings.label.colour='black',
           loadings.label.size = 3.5) + 
    ggtitle("Metacarpals PCA") + theme_classic()
  dev.off()
  
  
  # MTs
  
  data_mts <- scale(data[,4:7])
  data_num <- data_mts
  
  data_mts <- as.data.frame(cbind(data[,2], data_mts))
  colnames(data_mts)[1] <- "Sex"
  
  data_mts <- na.omit(data_mts)
  data_num <- na.omit(data_num)
  
  mts_pca <- princomp(data_num)
  summary(mts_pca)
  
  png("PCA plot - MTs.png", 
      width=1500, height=1500, res=300)
  autoplot(mts_pca, data=data_mts, colour='Sex', shape='Sex',
           loadings = TRUE, loadings.colour = 'blue',
           loadings.label = TRUE,
           loadings.label.colour='black',
           loadings.label.size = 3.5) + 
    ggtitle("Metatarsals PCA") + theme_classic()
  dev.off()
  
  
  # All 4 bones
  
  data_four <- scale(data[,4:10])
  data_num <- data_four
  
  data_four <- as.data.frame(cbind(data[,2], data_four))
  colnames(data_four)[1] <- "Sex"
  
  data_four <- na.omit(data_four)
  data_num <- na.omit(data_num)
  
  four_pca <- princomp(data_num)
  summary(four_pca)
  
  png("PCA plot - All Bones.png", 
      width=1500, height=1500, res=300)
  autoplot(four_pca, data=data_four, colour='Sex', shape='Sex',
           loadings = TRUE, loadings.colour = 'blue',
           loadings.label = TRUE,
           loadings.label.colour='black',
           loadings.label.size = 3.5) + 
    ggtitle("All Bones PCA") + theme_classic()
  dev.off()


rm(list=ls())


  

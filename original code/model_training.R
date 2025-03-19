setwd("C:/Users/Nefeli/Desktop/Lab/Διπλωματικές/Διπλωματικές 2023-2024/Κωνσταντίνος/Analysis/Sex estimation")

library(readr)
library(e1071)
library(tidyverse)
library(readxl)

### SVM model training
data <- read.csv("data_full.csv", sep = ",")
data <- data[, -c(1, 5:20, 29)]

set.seed(1993)
dataset <- data
dataset$Sex <- as.factor(dataset$Sex)

# MTs

ml_mt1 <- svm(formula = Sex ~ MLMT1 + MT1, 
              data = dataset, kernel = "linear", cost=100,
              scale = TRUE, probability = TRUE)
saveRDS(ml_mt1, "./models/MT1_SVM.rds")


ml_mt2 <- svm(formula = Sex ~ MLMT2 + MT2, 
              data = dataset, kernel = "polynomial", cost=100, degree=3, 
              scale = TRUE, probability = TRUE)

saveRDS(ml_mt2, "./models/MT2_SVM.rds")


ml_mt <- svm(formula = Sex ~ MLMT1 + MT1 + MLMT2 + MT2, 
             data = dataset, kernel = "linear", cost=10,
             scale = TRUE, probability = TRUE)

saveRDS(ml_mt, "./models/MTs_SVM.rds")


# MCs

ml_mc1 <- svm(formula = Sex ~ MLMC1 + MC1, 
              data = dataset, kernel = "polynomial", cost=10, degree=3, 
              scale = TRUE, probability = TRUE)

saveRDS(ml_mc1, "./models/MC1_SVM.rds")

ml_mc2 <- svm(formula = Sex ~ MLMC2 + MC2 , 
              data = dataset, kernel = "polynomial", cost=100, degree=3, 
              scale = TRUE, probability = TRUE)

saveRDS(ml_mc2, "./models/MC2_SVM.rds")


ml_mc <- svm(formula = Sex ~ MLMC1 + MC1 + MLMC2 + MC2 , 
             data = dataset, kernel = "polynomial", cost=100, degree=3, 
             scale = TRUE, probability = TRUE)

saveRDS(ml_mc, "./models/MCs_SVM.rds")


# Combo

ml_1 <- svm(formula = Sex ~ MLMC1 + MC1 + MLMT1 + MT1, 
            data = dataset, kernel = "polynomial", cost=10, degree=3,
            scale = TRUE, probability = TRUE)

saveRDS(ml_1, "./models/MC1_MT1_SVM.rds")



ml_2 <- svm(formula = Sex ~ MLMC2 + MC2 + MLMT2 + MT2, 
            data = dataset, kernel = "linear", cost=10,
            scale = TRUE, probability = TRUE)

saveRDS(ml_2, "./models/MC2_MT2_c10_SVM.rds")

ml_2 <- svm(formula = Sex ~ MLMC2 + MC2 + MLMT2 + MT2, 
            data = dataset, kernel = "linear", cost=100,
            scale = TRUE, probability = TRUE)

saveRDS(ml_2, "./models/MC2_MT2_c100_SVM.rds")

ml <- svm(formula = Sex ~ MLMC1 + MC1 + MLMT1 + MT1 + MLMC2 + MC2 + MLMT2
          + MT2, 
          data = dataset, kernel = "polynomial", cost=10, degree=3,
          scale = TRUE, probability = TRUE)

saveRDS(ml, "./models/All_4_bones_SVM.rds")

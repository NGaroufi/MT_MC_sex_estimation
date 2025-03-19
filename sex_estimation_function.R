## Copyright (C) 2023 Nefeli Garoufi <nefeligar@biol.uoa.gr>


# Package installing and library loading
list.of.packages <- c("readr", "readxl", "caret", "e1071", "dplyr")
install.packages(list.of.packages, quiet = TRUE)

suppressMessages(suppressWarnings(invisible(sapply(list.of.packages, require, 
                                                   character.only = TRUE))))

MT_MC_sex_estimation <- function(bone_model, validation)
{
  
  # Setting working director
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))
  
  # Data loading and prep
  data <- read.csv(file.choose(new=TRUE))
  
  data$Sex <- as.factor(data$Sex)
  
  # Welcoming message
  print("Hello!")
  print(paste0("You are working with ", nrow(data), 
               " samples and have loaded the ", bone_model, " SVM model."))
  
  class <- readRDS(paste0("./models/", bone_model, "_SVM.rds"))
  
  if (bone_model == "MC1")
  {test <- data[,c(2,3,4)]} else if (bone_model == "MC2")
  {test <- data[ , c(2,5,6)]} else if (bone_model == "MCs")
  {test <- data[ , c(2,3:6)]} else if (bone_model == "MT1")
  {test <- data[ , c(2,7,8)]} else if (bone_model == "MT2")
  {test <- data[ , c(2,9,10)]} else if (bone_model == "MTs")
  {test <- data[ , c(2,7:10)]} else if (bone_model == "MC1_MT1")
  {test <- data[ , c(2,3,4,7,8)]} else if (bone_model == "MC2_MT2_c10" || bone_model == "MC2_MT2_c100")
  {test <- data[ , c(2,5,6,9,10)]} else if (bone_model == "All_4_bones")
  {test <- data[ , c(2,3:10)]}
  
  pred <- predict(class, newdata = test, probability = TRUE)
  
  if (validation == TRUE)
    
  {
    ref <- test
    ref[,1] <- as.factor(ref[,1])
    
    ref <- na.omit(ref)
  
    res <- confusionMatrix(data=pred, reference = ref[,1])
    
    results <- c(res$overall[c(1,3,4)], res$byClass[c(1,2,11)])
    names(results) <- c("Accuracy", "Upper CI", "Lower CI",
                        "Sensitivity", "Specificity", "Balanced Accuracy")
    
    write.csv(results, paste0("Validation metrics ", bone_model, ".csv"))
    
    print("Metrics of the validation have been saved. In more detail:")
    print(res)
    
  } 
  
  print("The resutls of the sex estimation for your sample have been saved in your working directory.")
  
  pred_fac <- 0
  for (i in 1:length(pred))
  {pred_fac[i] <- ifelse(pred[i]=="F", "Female", "Male")}
  
  predictions <- cbind(data[,1], pred_fac, attr(pred, "probabilities"))  
  colnames(predictions) <- c("Sample ID", "Estimated Sex", "Probability for Male",
                             "Probability for Female")
  
  write.csv(predictions, paste0("Sex Estimation Results ", bone_model, ".csv"))
    
  

}
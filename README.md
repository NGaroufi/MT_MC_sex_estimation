# MT_MC_sex_estimation
A repository for an R function utilizing SVM models derived from the first and second metacarpal and metatarsal bones for sex estimation, as well as the code behind the initial analysis.

The present function allows the user to utilize metacarpal and metatarsal skeletal elements for sex estimation. The only requirement for the `MT_MC_sex_estimation` function is a CSV file containing the proposed measurements for every individual of the desired sample (as shown in the testing dataset provided as an example). In case of missing measurements, please still indicate the columns as presented and use "NA" instead of a measurement.

In order to use the function, the user must first download this repository from GitHub. Then, the downloaded folder must be unzipped and the CSV data file must be copied inside the MT_MC_sex_estimation folder. All necessary libraries are installed and loaded directly by the function.

First, the user needs to load the function in the R workspace. This is achieved with the command:
```
source("sex_estimation_function.R")
```

A message showing the successful installation of the necessary packages will show on the R console upon loading the function. Additionally, once the function is properly loaded, it can be called from the R console as:
```
MT_MC_sex_estimation(bone_model, validation)
```
The two inputs the `MT_MC_sex_estimation` requires are: 
1. bone: decides which bone model the user wants to utilize. It can be one of "MC1", "MC2", "MCs", "MT1", "MT2", "MTs", "MC1_MT1", "MC2_MT2_c10", "MC2_MT2_c100", "All_4_bones"
2. validation: decides whether to calculate a confusion matrix and accuracy metrics, in case the ground truth is known. Must be a logical argument of either TRUE or FALSE
   
This will open a window of the working directory, where the user can choose the CSV file containing the measurements. The function will then display a message reporting the number of samples from the data file, as well as the model being used. The results of the analysis are saved in .csv files. In case of validation results, they are saved in the corresponding .csv file and printed in the console in more detail.

A testing dataset is provided as a use case.

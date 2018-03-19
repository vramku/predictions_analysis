library(readr)      #required for faster file reading command
library(stringr)
library(dplyr)

Prediction_Data <- read_csv("./cleaned_data/Pred_Data_Cleaned.csv")
Prediction_Data_Express <- read_csv("./cleaned_data/Pred_Data_Express.csv")
Prediction_data_Local <- read_csv("./cleaned_data/Pred_Data_Local.csv")

#A problem with R^2 , though, is that it doesn’t follow a distribution.
#We can’t compare the R^2 ’s in two models and know when one is
#meaningfully better.
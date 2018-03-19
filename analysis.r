library(readr)      #required for faster file reading command
library(stringr)
library(dplyr)

Prediction_Data_Complete <- read_csv("./cleaned_data/Valid_Complete_Data.csv")
Prediction_Data_Express <- read_csv("./cleaned_data/Valid_Express_Data.csv")
Prediction_Data_Local <- read_csv("./cleaned_data/Valid_Local_Data.csv")

#A problem with R^2 , though, is that it doesn’t follow a distribution.
#We can’t compare the R^2 ’s in two models and know when one is
#meaningfully better. Also, look for at variance of the residual distribution. 
#If it is not constant, reevaluate the model.
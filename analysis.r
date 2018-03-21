library(readr)      #required for faster file reading command
library(stringr)
library(dplyr)

Prediction_Data_Complete <- read_csv("./cleaned_data/Valid_Complete_Data.csv")
Prediction_Data_Express <- read_csv("./cleaned_data/Valid_Express_Data.csv")
Prediction_Data_Local <- read_csv("./cleaned_data/Valid_Local_Data.csv")

#A problem with R^2 , though, is that it doesn’t follow a distribution.
#We can’t compare the R^2 ’s in two models and know when one is
#meaningfully better. Also, look at the variance of the residual distribution. 
#If it is not constant, reevaluate the model. 
#ALSM pg. 77 the farther we are from the sample mean the more Y-hat will wobble from sample to sample
#Difference between E[Y] (expected value) and Y-hat (predicted value): https://stats.stackexchange.com/questions/328807/difference-between-predicted-value-and-expected-value-for-binary-model?utm_medium=organic&utm_source=google_rich_qa&utm_campaign=google_rich_qa
#Remember, we assume that epsilon (error term), is normally distributed thus making Y a normal random variable 

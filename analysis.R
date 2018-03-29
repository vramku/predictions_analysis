library(readr)      #required for faster file reading command
library(stringr)
library(dplyr)
#Load cleaned data 
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
#our X's are not fixed. Do they vary and, if yes, by how much? ALSM pg. 66 effect on point estimator b1
#The slope, B1, should be normally distributed with the regression line always going through point (X-bar, Y-bar) or (Sample X Mean, Sample Y mean) or mean of the fitted values?
#The farther from (X-Bar, Y-Bar), the higher will be the variance of B1
#MSE stands for error mean square or residual mean square Sum(Yi - Y-hat)^2/(n-2). SSE stands for error sum of squares or residual sum of squares Sum(Yi - Y-hat)^2
#Linear regression models perfrom well within a range of predictor variables (Xi); outside this range, the relationship may not be linear
#How do we establish this range for our predictors?
#Even if the distriputions of Y are far from normal, the estimators b0 and b1
#generally have the property of asymptotic normality - their distributions approach normality
#under very general conditions as the sample size increases. CLT? Spacing of X affects the variance of b1 and b0. The larger the spacing the smaller this var of b1.
#When we make inferences, our confidence bands widen, this is due to the fact that besides sample to sample variablity, 
#we must account for variability between predictor variables (X)
#High R^2 (coef. of determination) does NOT mean our predictions will be useful; for instance the confidence band around the prediction can be so wide as to make impractical 
#The value taken by R2 in a given sample tends to be affected by the spacing of the X observations.
#Correlation vs Causation: Regression models do not by themselves imply causation, even when R^2 is close to 1. Our analysis is observational. 
#Be cautios when predicting outside the range of X variables obtained from data. Close is ok, far may be inappropriate. 
#Our predictors are subject to measurement errors and are, hence, biased. Consult Ch. 4 of ALSM
#Do our X and Y values come from a bivariate normal distribution?

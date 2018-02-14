#Rewrite times as POSIXct R nutshell pg. 95
#Consider using time series objects where applicable 
#Use connections for network I/O in R pg. 96
#To access variables outside the function, use the calling stack pg. 106
#Look for parts of the code that can be reused and condense as needed 
#read_csv is a great idea. It is much faster at reading large csv files than the native read.csv
#!!page 150 for reading fixed width files without resorting to SAS 
#for ODBC, try rows_at_a_time=1024 pg. 166 Don't forget to close the connection with odbcClose()
#anytime library for converting epoch to PosixCt don't forget to / by 1000
#Package 'readr' required for faster file reading command:
library(readr)
#install package for performing operations on data type string:
library(stringr)
#Eliminate groups of records having one or more time components as Zero:
#for grouping, we require package 'dplyr':
library(dplyr)
#package for exporting data:
library(rio)
library(rJava)
library(xlsx)
#for epoch to posix_ct conversion
library(anytime)

setwd("/home/smertmashina/Downloads/MTA/R_Data_Analysis/") 
#Read in the .csv file
Arrivals <- read_csv("csv_files/split_aa.csv")
#Make column names consistent (underscore-separated naming convention) 
names(Arrivals)[names(Arrivals) == 'tailStopArrivalTime'] <- 'tail_stop_arrival_time'
#view entire numerical values without a scientific format
options(scipen=999)

#Convert epoch time (ms since 1/1/1970) to POSIXct 
#Arrivals$tail_stop_arrival_time <- anytime(Arrivals$tail_stop_arrival_time/1000)
#Arrivals$time_of_sample <- anytime(Arrivals$time_of_sample/1000)
#Arrivals$predicted_arrival <- anytime(Arrivals$predicted_arrival/1000)
Arrivals <- transform(Arrivals, tail_stop_arrival_time=anytime(tail_stop_arrival_time/1000),
                                time_of_sample=anytime(time_of_sample/1000),
                                predicted_arrival=anytime(predicted_arrival/1000),
                                service_date=anytime(service_date/1000))
#Calculate Measured, Predicted and Residual times
Arrivals$time_to_arrival = (Arrivals$tail_stop_arrival_time - Arrivals$time_of_sample)
Arrivals$prediction = (Arrivals$predicted_arrival - Arrivals$time_of_sample)
Arrivals$residual = (Arrivals$tail_stop_arrival_time - Arrivals$predicted_arrival) #Possibly redundant. Consider taking abs of residual

#Take the absolute values of the Residual Time:
Arrivals$abs_residual = abs(Arrivals$residual)

#creating buckets for AbsResidual and storing in new column 'timeRes':
Arrivals$time_res = cut(as.numeric(Arrivals$abs_residual), c(-Inf,60,120,240,360,Inf),labels=c("0-1","1-2","2-4","4-6","6+"))  

#Classify Measured Time from seconds to minutes and check for errors: ??? >30 or 30-60 
Arrivals$time_period = cut(as.numeric(Arrivals$time_to_arrival), c(-Inf,0,300,600,1200,1800,3600,Inf),
                          labels=c("err","0--5","5--10","10--20","20--30",">30",">60"))

#Eliminate records classified as Errors
Arrivals <- subset(Arrivals, time_period != "err")

#df <- read.table(text=Arrivals$trip, sep="_")
#cbind(Arrivals,(read.table(text=Arrivals$trip, sep="_", col.names = c("bc_or_nyct", "depot"))))
#Arrivals <- subset(Arrivals, select = -trip)

#Extract depot information from attribute 'trip':
Arrivals$depot = sapply(str_split(Arrivals$trip, "_"),function(x){x[2]})
print(Arrivals$depot)
Arrivals$depot = sapply(str_split(Arrivals$depot, "-"),function(x){x[length(x)]})
print(Arrivals$depot)







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

#Read the file, enter the exact file name and store it in a data frame named 'Raw_file':
Arrivals <- read_csv("csv_files/split_aa.csv")
#Make column names consistent 
names(Arrivals)[names(Arrivals) == 'tailStopArrivalTime'] <- 'tail_stop_arrival_time'
#view entire numerical values without a scientific format:
options(scipen=999)
#Convert epoch to POSIXct 
Arrivals$tail_stop_arrival_time <- anytime(Arrivals$tail_stop_arrival_time/1000)
Arrivals$time_of_sample <- anytime(Arrivals$time_of_sample/1000)

#View the Raw_file by clicking on it in the Environment window towards the right end of the screen:
#Go to View >> Show Environment




#Rewrite times as POSIXct R nutshell pg. 95
#Consider using time series objects where applicable 
#Use connections for network I/O in R pg. 96
#To access variables outside the function, use the calling stack pg. 106
#Look for parts of the code that can be reused and condense as needed 
#read_csv is a great idea. It is much faster at reading large csv files than the native read.csv
#!!page 150 for reading fixed width files without resorting to SAS 
#for ODBC, try rows_at_a_time=1024 pg. 166 Don't forget to close the connection with odbcClose()
#anytime library for converting epoch to`` PosixCt don't forget to / by 1000
#library(parallel) to speed up processing boxplot.stats(x)$out to detect outliers 
#editrules for consistency checking (igraph package not supported on 3.4 yet)
#look at various bin cuts; consider changing values to NA, instead of deleting rows 

library(readr)      #required for faster file reading command
library(stringr)    #for performing operations on data type string
library(plyr)       #namespace conflicts b/w plyr and dplyr
library(dplyr)      #grouping
library(rio)        #packages for exporting data
library(rJava)
library(xlsx)
library(anytime)    #epoch to posix_ct conversion
library(tidyr)      #data extraction and cleanup 
library(doParallel) #multicore processing for the plyr::ddply step

setwd(".") 
#Read in the .csv file. Add a vector of column names as a function argument to speed up processing. 
#tryCatch constructions will help deal with any errors due to erroneous data. Will need to check for NAs
Arrivals <- read_csv("raw_data/split_aa.csv")

#Make column names short and consistent (underscore-separated naming convention) 
names(Arrivals)[names(Arrivals) == 'tailStopArrivalTime'] <- 'tail_stop_arr_time'
names(Arrivals)[names(Arrivals) == 'stop_gtfs_sequence'] <- 'stop_gtfs_seq'
names(Arrivals)[names(Arrivals) == 'distance_along_trip'] <- 'dist_covered'
names(Arrivals)[names(Arrivals) == 'stop_distance_along_trip'] <- 'dist_left_trip'
names(Arrivals)[names(Arrivals) == 'distance_of_trip'] <- 'dist_left_total'

#view entire numerical values without a scientific format
options(scipen=999)

#Convert epoch time (ms since 1/1/1970) to POSIXct 
Arrivals <- transform(Arrivals, tail_stop_arr_time = anytime(tail_stop_arr_time / 1000),
                                time_of_sample = anytime(time_of_sample / 1000),
                                predicted_arrival = anytime(predicted_arrival / 1000),
                                service_date = anytime(service_date / 1000))

#Calculate Measured, Predicted and Residual times (timeToArrival and prediction in old code)
Arrivals$measured_t <- as.integer(Arrivals$tail_stop_arr_time - Arrivals$time_of_sample)
Arrivals$predicted_t <- as.integer(Arrivals$predicted_arrival - Arrivals$time_of_sample)
Arrivals$residual <- as.integer(Arrivals$tail_stop_arr_time - Arrivals$predicted_arrival) #Possibly redundant. Consider taking abs of residual

#Take the absolute values of the Residual Time:
Arrivals$abs_residual <- abs(Arrivals$residual)

#Round off floating point numbers for data consistency. Consider using int instead of double due to GPS error & significance of the measurement
Arrivals$dist_covered <- round(Arrivals$dist_covered, digits = 2)
Arrivals$dist_left_trip <- round(Arrivals$dist_left_trip, digits = 2)
Arrivals$dist_left_total <- round(Arrivals$dist_left_total, digits = 2)
#Arrivals$measured_t <- round(Arrivals$measured_t, digits = 4)
#Arrivals$predicted_t <- round(Arrivals$predicted_t, digits = 4)
#Arrivals$abs_residual <- round(Arrivals$abs_residual, digits = 4)
#Arrivals$residual <- round(Arrivals$residual, digits = 4)


#creating buckets for abs_residuals 
Arrivals$abs_res_bins <- cut(as.numeric(Arrivals$abs_residual), c(-Inf, 60, 120, 240, 360, Inf), 
                              labels = c("0-1","1-2","2-4","4-6","6+")) 

#Classify Measured Time from seconds to minutes and check for errors: ??? >30 or 30-60 
Arrivals$measured_bins <- cut(as.numeric(Arrivals$measured_t), c(-Inf, 0, 300, 600, 1200, 1800, 3600, Inf),
                              labels = c("err","0--5","5--10","10--20","20--30",">30",">60"))

#Eliminate records classified as errors
Arrivals <- subset(Arrivals, measured_bins != "err")

#Divide the original predictions into various buckets with units as minutes:
Arrivals$predicted_bins <- cut(as.numeric(Arrivals$predicted_t), c(0, 120, 240, 360, 600, 900, 1200, Inf),
                           labels=c("0--2","2--4","4--6","6--10","10--15","15--20","20+"))  

#Extract depot information from attribute 'trip'. Optimized to 1.2 vs 7.1 secs:
Arrivals <- tidyr::extract(Arrivals, trip, 'depot', "(?:MTA)(?: NYCT_|BC_[0-9]+-)([A-Z]{2})", remove=TRUE, perl=TRUE, useBytes=TRUE)


#Extract historical, recent, and scheduled times in ms from the colon_delimited_db_components column. Performs 3.3 vs 23.1
Arrivals <- tidyr::extract(Arrivals, colon_delimited_db_components, 
                           c('historical', 'recent', 'schedule'), 
                           "(?:HISTORICAL_([0-9]+))(?::RECENT_([0-9]+))(?::SCHEDULE_([0-9]+))", 
                           remove=TRUE, convert = TRUE, perl=TRUE, useBytes=TRUE) 

#convert miliseconds to seconds 
Arrivals <- transform(Arrivals, historical = as.integer(historical / 1000), 
                                recent = as.integer(recent / 1000), 
                                schedule = as.integer(schedule / 1000))

#Running this step before removing invalid 'stop' rows risks losing valid data 
#filter out rows with zeros in historical, recent, or schedule. Check after filtering rows by gtfs_stops | vs *
#Arrivals <- Arrivals %>% group_by(time_of_sample, vehicle) %>% filter(!any(historical == 0 | schedule == 0 | recent == 0) && !is.na())

#used to substitute invalid gtfs stop numbers after a skipped projected stop 
del_miss_stops <- function (df, column = 'stop_gtfs_seq') {
  first_stop <- df[1, column]
  for (row in seq_len(nrow(df))) {
    ifelse (row == df[row, column] - first_stop + 1 , next, df[row, column] <- NA)
  }
  return (df)
}

#Set up a parallel backend for plyr::ddply
registerDoParallel(cores = 3)      

#Mark invalid gtfs values in every bus report as NA
ArrivalsNA <- plyr::ddply(Arrivals, .(time_of_sample, vehicle), del_miss_stops, .parallel = TRUE)


#for testing gtfs sequence skips 
#df <- head(Arrivals, n = 10000)
#df2 %>% group_by(time_of_sample, route, vehicle) %>% summarize(stop_gtfs_sequence = paste(sort(unique(stop_gtfs_sequence)),collapse=", "))  

#Create columns showing to accumulated historical, recent and scheduled times for each report 
ArrivalsNA <- ArrivalsNA %>% group_by(time_of_sample, vehicle) %>% mutate(hist_cum = cumsum(historical),
                                                                          rece_cum = cumsum(recent),
                                                                          sche_cum = cumsum(schedule))

ArrivalsNA <- bus_data_organized_cleaned <- ArrivalsNA[c("vehicle", "time_of_sample", "route", "historical", "recent", "schedule",
                                           "hist_cum", "rece_cum", "sche_cum", "predicted_t", "measured_t", "residual",
                                           "abs_residual", "stop_gtfs_seq", "measured_bins", "predicted_bins", 
                                           "dist_covered", "dist_left_trip", "dist_left_total", "depot",
                                           "phase", "direction", "shape", "stop_id", "block", "service_date", "predicted_arrival",
                                           "colon_delimited_used_components")] 

#filter groups that have zeros in either historical, recent, or schedule 
ArrivalsNA <- ArrivalsNA %>% group_by(time_of_sample, vehicle) %>% 
                             filter(!any((historical == 0 | schedule == 0 | recent == 0) & stop_gtfs_seq != "NA")) %>%
                             arrange(time_of_sample, vehicle)


#primary key check
#ArrivalsNA %>% count(time_of_sample, vehicle, stop_gtfs_seq) %>% filter(n > 1)
#Release memory by deleting the redundant Arrivals object.
write_csv(ArrivalsNA, "./cleaned_data/ArrivalsNA.csv")



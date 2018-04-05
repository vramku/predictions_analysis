#Rewrite times as POSIXct R nutshell pg. 95
#Consider using time series objects where applicable 
#Use connections for network I/O in R pg. 96
#To access variables outside the function, use the calling stack pg. 106
#Look for parts of the code that can be reused and condense as needed 
#read_csv is a great idea. It is much faster at reading large csv files than the native read.csv
#!!page 150 for reading fixed width files without resorting to SAS 
#for ODBC, try rows_at_a_time=1024 pg. 166 Don't forget to close the connection with odbcClose()
#lubridate library for converting epoch to`` PosixCt don't forget to / by 1000
#library(parallel) to speed up processing boxplot.stats(x)$out to detect outliers 
#editrules for consistency checking (igraph package not supported on 3.4 yet)
#look at various bin cuts; consider changing values to NA, instead of deleting rows 
#this code cleans a 700MB prediction file in about 5 minutes 

tstart <- Sys.time()
library(readr)      #required for faster file reading command
library(stringr)    #for performing operations on data type string
library(plyr)       #namespace conflicts b/w plyr and dplyr
library(dplyr)      #grouping
library(rio)        #packages for exporting data
library(rJava)
library(xlsx)
library(lubridate)  #epoch to posix_ct conversion
library(tidyr)      #data extraction and cleanup 
library(doParallel) #multicore processing for the plyr::ddply step
library(RSQLite)    #DBI framework for db access 

#set the timezone using IANA convention 
timezone <- "America/New_York"
#Read in the .csv file. 
#tryCatch constructions will help deal with any errors due to erroneous data. Will need to check for NAs
Arrivals <- read_csv("raw_data/split_aa.csv")

#Make column names short and consistent (underscore-separated naming convention) 
names(Arrivals)[names(Arrivals) == 'tailStopArrivalTime'] <- 'tail_stop_arr_time'
names(Arrivals)[names(Arrivals) == 'stop_gtfs_sequence'] <- 'stop_gtfs_seq'
names(Arrivals)[names(Arrivals) == 'distance_along_trip'] <- 'dist_covered'
names(Arrivals)[names(Arrivals) == 'stop_distance_along_trip'] <- 'dist_from_origin'
names(Arrivals)[names(Arrivals) == 'distance_of_trip'] <- 'total_trip_dist'
names(Arrivals)[names(Arrivals) == 'time_of_sample'] <- 'time_stamp'

#view entire numerical values without a scientific format
options(scipen = 999)

#Convert epoch time (ms since 1/1/1970) to POSIXct; issues when using SQLite 
Arrivals <- transform(Arrivals, tail_stop_arr_time = as_datetime(tail_stop_arr_time / 1000, tz = timezone),
                                time_stamp = as_datetime(time_stamp / 1000, tz = timezone),
                                predicted_arrival = as_datetime(predicted_arrival / 1000, tz = timezone),
                                service_date = as_datetime(service_date / 1000, tz = timezone))

#Calculate Measured, Predicted and Residual times (timeToArrival and prediction in the old code)
Arrivals$measured_t <- (as.double(Arrivals$tail_stop_arr_time - Arrivals$time_stamp))
Arrivals$predicted_t <-(as.double(Arrivals$predicted_arrival - Arrivals$time_stamp))
Arrivals$residual <- (as.double(Arrivals$tail_stop_arr_time - Arrivals$predicted_arrival)) #Possibly redundant. Consider taking abs of residual

#Take the absolute values of the corresponding residual times:
Arrivals$abs_residual <- abs(Arrivals$residual)

#Round off floating point numbers for data consistency. Consider using int instead of double due to GPS error & significance of the measurement
Arrivals$dist_covered <- round(Arrivals$dist_covered, digits = 2)
Arrivals$dist_from_origin <- round(Arrivals$dist_from_origin, digits = 2)
Arrivals$total_trip_dist <- round(Arrivals$total_trip_dist, digits = 2)
Arrivals$measured_t <- round(Arrivals$measured_t, digits = 3)
Arrivals$predicted_t <- round(Arrivals$predicted_t, digits = 3)
Arrivals$abs_residual <- round(Arrivals$abs_residual, digits = 3)
Arrivals$residual <- round(Arrivals$residual, digits = 3)

#Extract depot information from attribute 'trip'. Optimized to 1.2 vs 7.1 secs:
Arrivals <- tidyr::extract(Arrivals, trip, 'depot', "(?:MTA)(?: NYCT_|BC_[0-9]+-)([A-Z]{2})", remove=TRUE, perl=TRUE, useBytes=TRUE)

#Extract historical, recent, and scheduled times in ms from the colon_delimited_db_components column. Performs 3.3 vs 23.1
Arrivals <- tidyr::extract(Arrivals, colon_delimited_db_components, 
                           c('historical', 'recent', 'schedule'), 
                           "(?:HISTORICAL_([0-9]+))(?::RECENT_([0-9]+))(?::SCHEDULE_([0-9]+))", 
                           remove=TRUE, convert = TRUE, perl=TRUE, useBytes=TRUE) 

#convert miliseconds to seconds 
Arrivals <- transform(Arrivals, historical = (as.double(historical / 1000)), 
                                recent = (as.double(recent / 1000)), 
                                schedule = (as.double(schedule / 1000)))

#remove unnecessary columns
Arrivals <- subset(Arrivals, select = -c(colon_delimited_used_components))

#used == in this function, which is vectorized; better to compare using identical(arg1, arg2)
#used to mark invalid gtfs stop numbers after a skipped projected stop or an express "jump", as well as express buses
mark_invalid_stops <- function (df, column = 'stop_gtfs_seq') {
  rows_in_df <- nrow(df)
  #edge cases and express bus detection
  if (rows_in_df == 0) {return(df)}
  rexpr <- "(([BQ]{1}X?[M]{1}[1-4]?[0-9]{1}A?)|([^B_]?X[0-9]{1,2}[A-Z]{0,1})$)"
  is_express <- str_detect(df[1, 'route'], rexpr)
  if (rows_in_df == 1) {(df$is_invalid[1] <- TRUE) & (df$is_express <- is_express) & return(df)}
  
  #adjust the predicted_t of gtfs_seq 1 by prorating the first prediction time using the distance to the first stop
  scalar <- (df$dist_from_origin[2] - df$dist_covered[2]) / (df$dist_from_origin[2] - df$dist_from_origin[1])
  df$historical[2] <- round(df$historical[2] <- df$historical[2] * scalar, digits = 3) 
  df$recent[2]     <- round(df$recent[2]     <- df$recent[2]     * scalar, digits = 3)
  df$schedule[2]   <- round(df$schedule[2]   <- df$schedule[2]   * scalar, digits = 3)
  
  #eliminate the first row from the data frame; row 2 becomes row 1 in the resultant df; update rows_in_df
  df <- df[2:rows_in_df,]
  rows_in_df <- rows_in_df - 1
  rownames(df) <- seq(length = rows_in_df)
  
  #iterate through the group looking for skipped stops 
  first_stop <- df[1, column]
  
  for (row in seq_len(rows_in_df)) {
    df$is_express[row] <- is_express
    ifelse (row == df[row, column] - first_stop + 1 , df$is_invalid[row] <- FALSE, df$is_invalid[row] <- TRUE)
  }
  
  #mark first record as invalid for our computational purposes and adjust component values based on predicted time
  #since this is a weighted average, setting each component to the value of predicted_t ensures consistency of the following rows
  #df$is_invalid[1] <- TRUE
  #df$historical[1] <- df$predicted_t[1] 
  #df$recent[1]     <- df$predicted_t[1]
  #df$schedule[1]   <- df$predicted_t[1]

  #continue checking for jumps if the bus is an express bus, else return
  if (!is_express) {return(df)}
  borough_id <- NA
  ifelse (is_express, borough_id <- str_match(df$stop_id[1], '(?:MTA_)([0-9]{1})')[2], return(df))
  jump_detected <- FALSE 
  jump_row <- NA
  
  for (row in seq_len(rows_in_df - 1)) {
    next_stop <- str_match(df$stop_id[row + 1], '(?:MTA_)([0-9]{1})')[2]
    if (borough_id != next_stop) { 
      jump_detected <- TRUE 
      jump_row <- row
      borough_id <- next_stop } else {
        next  
      }
  }
  
  #mark rows after the jump as invalid
  if (jump_detected) {
    for (rows in seq_len(jump_row)) {
      df$is_invalid[rows_in_df - jump_row + 1] <- TRUE
    }
  }
  return(df)
}

#Set up a parallel backend for plyr::ddply
registerDoParallel(cores = 3)      

#Mark rows with invalid gtfs values and express routes
Pred_Data_Cleaned <- plyr::ddply(Arrivals, .(time_stamp, vehicle), mark_invalid_stops, .parallel = TRUE)

#Eliminate records classified as errors
#Arrivals <- subset(Arrivals, measured_bins != "err")
#Arrivals <- Arrivals %>% filter(measured_t >= 0)

#for testing gtfs sequence skips 
#df <- head(Arrivals, n = 10000)
#df2 %>% group_by(time_stamp, route, vehicle) %>% summarize(stop_gtfs_sequence = paste(sort(unique(stop_gtfs_sequence)),collapse=", "))  

#Create columns showing to accumulated historical, recent and scheduled times for each report 
Pred_Data_Cleaned <- Pred_Data_Cleaned %>% group_by(time_stamp, vehicle) %>% mutate(hist_cum = cumsum(historical),
                                                                                   rece_cum = cumsum(recent),
                                                                                   sche_cum = cumsum(schedule))

Pred_Data_Cleaned <- Pred_Data_Cleaned[c("vehicle", "time_stamp", "route", "historical", "recent", "schedule",
                           "hist_cum", "rece_cum", "sche_cum", "predicted_t", "measured_t", "residual",
                           "abs_residual", "stop_gtfs_seq", "phase", "direction",
                           "dist_covered", "dist_from_origin", "total_trip_dist", "depot", "is_express", "is_invalid",
                           "shape", "stop_id", "block", "service_date", "predicted_arrival", "tail_stop_arr_time")] 


#filter groups that have zeros in either historical, recent, or sched times in valid rows or rows with measured_t < 0
Pred_Data_Cleaned <- Pred_Data_Cleaned %>% group_by(time_stamp, vehicle) %>% 
                     filter(!any((near(historical, 0) && !is_invalid) | 
                                 (near(schedule, 0)   && !is_invalid) | 
                                 (near(recent, 0)     && !is_invalid))) %>%
                     filter(measured_t >= 0) %>%
                     arrange(time_stamp, vehicle)

#remove anomalous values that have predicted_t values, which deviate by more than 0.5 from expected comp. calculation
#the % of occurence of these anomalies is low and their removal should not significantly alter the data
Pred_Data_Cleaned <- Pred_Data_Cleaned %>% 
                     mutate(cpred = 0.4*hist_cum + 0.4*rece_cum + 0.2*sche_cum, delta = cpred - predicted_t) %>% 
                     filter(abs(cpred - predicted_t) < 0.5) %>% subset(select = -c(cpred, delta))

#primary key check
#Pred_Data_Cleaned %>% count(time_stamp, vehicle, stop_gtfs_seq) %>% filter(n > 1)
#Release memory by deleting the redundant Arrivals object.
#Save valid results to external .csv files
#Pred_Data_Cleaned %>% filter(is_invalid == F) %>% 
#                      write_csv(., "./cleaned_data/Valid_Complete_Data.csv")
#Pred_Data_Cleaned %>% filter((is_express == T) & (is_invalid == F)) %>% 
#                      write_csv(., "./cleaned_data/Valid_Express_Data.csv")
#Pred_Data_Cleaned %>% filter((is_express == F) & (is_invalid == F)) %>% 
#                      write_csv(., "./cleaned_data/Valid_Local_Data.csv")


#connect to an external database and append to it the cleaned data
drv <- dbDriver("SQLite")
con <- dbConnect(drv, 'test.db', flags = SQLITE_RW)
dbWriteTable(con, 'mta_bus_data', Pred_Data_Cleaned, append = TRUE)
dbDisconnect(con)
#clean up
rm(c(Arrivals, Pred_Data_Cleaned))
#time the data collection stage
tend <- Sys.time()
print(totalt <- tend - tstart)

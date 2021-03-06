library(tidyverse)
library(RSQLite)
library(rJava)
library(openxlsx)
library(lubridate)
source("BusData.R") 
source("Visualizer.R")

setwd("~/Documents/MTA/R_Data_Analysis/")

drv <- dbDriver("SQLite")
con <- dbConnect(drv, 'nov9_bus_data.db', flags = SQLITE_RO)
data <- as.data.frame(dbGetQuery(con, "SELECT * FROM mta_bus_data WHERE route = 'MTABC_Q10'"))
dbDisconnect(con)

data <- transform(data, t_stamp = as_datetime(as.double(t_stamp)))

drip_removal <- function (data) {
  #filter the data to locate "drips" by grouping by drip_cols
  data <- as.data.table(data)
  data[, ':=' (day = day(t_stamp))]
  drip_cols <- c("vehicle", "t_predicted", "stop_gtfs_seq", "day")
  drips <- data[, if(.N > 3) .SD[-(1:3)], by = drip_cols]
  data[, c("day") := NULL]
  
  #perform an antijoin of the data and drips tables
  setkey(data, vehicle, t_predicted, t_stamp, stop_gtfs_seq)
  setkey(drips, vehicle, t_predicted, t_stamp, stop_gtfs_seq)
  data_wo_drips <- data[!drips]
}
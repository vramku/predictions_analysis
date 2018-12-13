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

#filter the data to locate "drips" 
drip_cols <- c("vehicle", "t_predicted", "stop_gtfs_seq")
drips <- as.data.table(data)[, if(.N > 3) .SD, by = drip_cols]
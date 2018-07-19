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
data <- as.data.frame(dbGetQuery(con, "SELECT * FROM mta_bus_data WHERE route = 'MTA NYCT_B36';"))
dbDisconnect(con)

data <- transform(data, t_stamp = as_datetime(as.double(t_stamp)))
#drip_track <- data_5388[block == unique(drip_sample$block),][date(t_stamp) == "2017-11-11" & shape == "MTA_Q100466" & hour(t_stamp) %in% c(6,7)]

#pass a cut based on vehicle and block
drip_cut <- function(df) {
  max_gtfs <- max(drip_samp$stop_gtfs_seq)
  stop_slices <- vector("list", length = max_gtfs)
  
  for (stop in 1:max_gtfs) {
    print(stop_slices[[stop]] <- filter(df, stop_gtfs_seq == stop))
  }
  
  print(stop_slices)
  drip_stamp <- vector("list")
  rows_in_gtfs_one <- nrow(stop_slices[[1]])
  if(rows_in_gtfs_one < 4) return()
  lag_inx <- 1 
  lead_inx <- 4 
  while (!(lead_inx == rows_in_gtfs_one)) {
    #bus has not moved more than 30m for the last 4 signals
    dist_advanced <- stop_slices[[1]]$dist_covered[lead_inx] - stop_slices[[1]]$dist_covered[lag_inx]
    print(dist_advanced)
    if (dist_advanced < 30) {
      append(drip_stamp, stop_slices[[1]]$t_stamp[lag_inx])
      print(stop_slices[[1]]$t_stamp[lag_inx])
    }
    lead_inx <- lead_inx + 1
    lag_inx <- lag_inx + 1
  }
}


Pred_Data_Cleaned <- plyr::ddply(Arrivals, .(vehicle, block), drip_cut, .parallel = TRUE)

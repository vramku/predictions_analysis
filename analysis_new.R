library(tidyverse)
library(RSQLite)
library(rJava)
library(openxlsx)
source("BusData.R")

drv <- dbDriver("SQLite")
con <- dbConnect(drv, 'test.db', flags = SQLITE_RO)

locobj <- BusData$new(con, 0)
expobj <- BusData$new(con, 1)

locdata <- locobj$get_mod_data()[[1]]

stop_cut <- function(data) {
  min_stop <- min(data$stop_gtfs_seq)
  max_stop <- max(data$stop_gtfs_seq)
  analyzed_stop_grps <- vector('list', length = (max_stop - min_stop + 1))
  for (stop in min_stop:max_stop) {
    stop_data <- locdata %>% filter(stop_gtfs_seq == stop)
    analyzed_stop_grps[[stop - min_stop + 1]] <- locdata %>% filter(stop_gtfs_seq == stop)
  }
  analyzed_stop_grps 
}

stop_grp_analysis <- stop_cut(locdata)
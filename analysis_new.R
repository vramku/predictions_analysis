library(tidyverse)
library(RSQLite)
library(rJava)
library(openxlsx)
source("BusData.R")
source("Visualizer.R")

drv <- dbDriver("SQLite")
con <- dbConnect(drv, 'test.db', flags = SQLITE_RO)

locobj <- BusData$new(con, 0)
expobj <- BusData$new(con, 1)

locdata <- locobj$get_mod_data()[[1]]

#analyze gtfs stops 
stop_cut <- function(db_con) {
  min_stop <- as.numeric(dbGetQuery(db_con, "SELECT MIN(stop_gtfs_seq) FROM mta_bus_data"))
  max_stop <- as.numeric(dbGetQuery(db_con, "SELECT MAX(stop_gtfs_seq) FROM mta_bus_data"))
  analyzed_stop_grps <- vector('list', length = (max_stop - min_stop + 1))
  for (stop in min_stop:max_stop) {
    analyzed_stop_grps[[stop - min_stop + 1]] <- BusData$new(db_con, 1, stop_gtfs_seq = stop)
  }
  analyzed_stop_grps 
}
stop_grps_analyzed <- stop_cut(con)


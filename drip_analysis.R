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

drip_cut <- function(df) {
  rows_in_df <- nrow(df)
  
}


Pred_Data_Cleaned <- plyr::ddply(Arrivals, .(vehicle, block), drip_cut, .parallel = TRUE)

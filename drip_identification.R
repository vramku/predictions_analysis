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

drip_times <- data %>% group_by(t_predicted) %>% do(count(.)) %>% filter(any(n > 3))
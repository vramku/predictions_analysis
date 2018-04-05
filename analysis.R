library(readr)      #required for faster file reading command
library(stringr)
library(dplyr)
library(RSQLite)
library(tidyr)
library(lubridate)  #epoch to posix_ct conversion

#set the timezone using IANA convention 
timezone <- "America/New_York"
#sig figures 
options(scipen = 999)

drv <- dbDriver("SQLite")
con <- dbConnect(drv, 'test.db', flags = SQLITE_RO)
Pred_Data_Complt <- dbGetQuery(con, "SELECT vehicle, time_stamp, stop_gtfs_seq, hist_cum, rece_cum, sche_cum,
                                            predicted_t, measured_t
                                     FROM mta_bus_data")
dbDisconnect(con)

Pred_Data_Complt <- transform(Pred_Data_Complt, time_stamp = as_datetime(as.double(time_stamp), tz = timezone))
#bin and residual cutoffs in seconds 
bin_cutoffs <- c(0, 120, 240, 360, 600, 900, 1200, Inf)
res_cutoffs <- c(0, 60, 120, 240, 360, Inf)
model_form <- formula(Pred_Data_Complt$measured_t ~ Pred_Data_Complt$hist_cum + Pred_Data_Complt$rece_cum + Pred_Data_Complt$sche_cum + 0)

#use biglm if you run out of memory; lmtest library to test the model 
Pred_Data_Complt$pred_bin <- cut(Pred_Data_Complt$predicted_t, cutoffs,  dig.lab = 5 )

#calculate linear models for every subgroup 
bin_lvl <- levels(Pred_Data_Complt$pred_bin)
bin_names <- paste0((cutoffs[-length(cutoffs)]) / 60, " to ", (cutoffs[-1]) / 60)
bin_metrics <- vector("list", length(bin_names))
bin_metrics <- setNames(bins, bin_names)

#takes a vector of coefficients and returns a vector of normalized coefficients 
normalize_coef <- function(x) {
  coef_sum <- sum(x)
  x <- unlist(lapply(x, function(coef) {coef / coef_sum}))
}

bin_analyzer <- function(pred_bin) {
  
}



for (bin in seq_along(bin_lvl)) {
  bin_metrics[bin] <- vector("list", length = )
  bin_metrics[bin] <- lm(model_form, subset(Pred_Data_Complt, pred_bin == bin_lvl[bin]))
}










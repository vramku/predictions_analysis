library(readr)      #required for faster file reading command
library(stringr)
library(plyr)       #namespace conflicts b/w plyr and dplyr
library(dplyr)
library(RSQLite)
library(tidyr)
library(lubridate)  #epoch to posix_ct conversion
library(doParallel) #multicore processing for the plyr::ddply step

#set the timezone using IANA convention 
timezone <- "America/New_York"
#sig figures 
options(scipen = 999)
#Original Coefficients
org_coef = c(0.4, 0.4, 0.2)

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

#use biglm if you run out of memory; lmtest library to test the model 
Pred_Data_Complt$pred_bin <- cut(Pred_Data_Complt$predicted_t, bin_cutoffs,  dig.lab = 5 )

#calculate linear models for every subgroup 
bin_lvl <- levels(Pred_Data_Complt$pred_bin)

bin_names <- paste0((bin_cutoffs[-length(bin_cutoffs)]) / 60, " to ", (bin_cutoffs[-1]) / 60, " mins")
res_names <- c(paste0((res_cutoffs[-length(res_cutoffs)]) / 60,  " to ", (res_cutoffs[-1]) / 60, " mins"), "Total")
metric_names <- c("Bin_Str", "Coef_Matrix", "Optim_Model", "Metric_Matrix", "AbsRes_Matrix")
mod_names <- c("Original", "Optimized", "Normalized")

bin_metrics <- vector("list", length(bin_lvl))
bin_metrics <- setNames(bin_metrics, bin_lvl)

#takes a vector of coefficients and returns a vector of normalized coefficients. might require mod to avoid name stripping
normalize_coef <- function(x) {
  coef_sum <- sum(x)
  x <- unlist(lapply(x, function(coef) {coef / coef_sum}))
}

for (bin in seq_along(bin_metrics)) {
  bin_metrics[[bin]] <- vector("list", length = 5)
  bin_metrics[[bin]] <- setNames(bin_metrics[[bin]], metric_names)
  bin_metrics[[bin]][["Bin_Str"]] <- bin_names[bin]
  bin_metrics[[bin]][["Optim_Model"]] <- vector("list", length = 12)
  bin_metrics[[bin]][["Coef_Matrix"]] <- matrix(nrow = 3, ncol = 3, dimnames = (list(mod_names, c("Historical", "Recent", "Schedule"))))
  bin_metrics[[bin]][["Coef_Matrix"]]["Original",] <- org_coef
  bin_metrics[[bin]][["Metric_Matrix"]] <- matrix(nrow = 3, ncol = 4, dimnames = (list(mod_names, c("R2 (Pearson)", "SD", "Mean", "Median"))))
  bin_metrics[[bin]][["AbsRes_Matrix"]] <- matrix(nrow = 3, ncol = length(res_names), dimnames = list(mod_names, res_names))
}

calc_model <- function(bin_df) {
  bin_inx <- match(bin_df$pred_bin[1], names(bin_metrics))
  mod_formula <- formula(bin_df$measured_t ~ bin_df$hist_cum + bin_df$rece_cum + bin_df$sche_cum + 0)
  optimized_model <- lm(mod_formula, bin_df)
  bin_metrics[[bin_inx]][["Optim_Model"]] <<- optimized_model
  optim_coef <- coefficients(optimized_model)
  bin_metrics[[bin_inx]][["Coef_Matrix"]]["Optimized",] <<- optim_coef
  bin_metrics[[bin_inx]][["Coef_Matrix"]]["Normalized",] <<- normalize_coef(optim_coef)
  return(bin_df)
}

#may rewrite as parMap with bin_metrics as return df and split Pred_Data as exported variable. 
invisible(plyr::ddply(Pred_Data_Complt, .(pred_bin), calc_model))

#redundant calculation of optim_residuals, the data is part of bin_metrics$Optim_Model; use it as a consistency check
calc_new_pred <- function(bin_df) {
  bin_inx <- match(bin_df$pred_bin[1], names(bin_metrics))
  coef_matrix <- bin_metrics[[bin_inx]][["Coef_Matrix"]]
  calc_new_t <- function(h, r, s, type) {
    return(h * coef_matrix[type, "Historical"] + r * coef_matrix[type, "Recent"] + s * coef_matrix[type, "Schedule"])
  }
  bin_df <- mutate(bin_df, a_res_orig = abs(bin_df$measured_t - bin_df$predicted_t),
                           pred_t_optim = calc_new_t(bin_df$hist_cum, bin_df$rece_cum, bin_df$sche_cum, "Optimized"))
  bin_df <- mutate(bin_df, a_res_optim = abs(bin_df$measured_t - bin_df$pred_t_optim),
                           pred_t_norm = calc_new_t(bin_df$hist_cum, bin_df$rece_cum, bin_df$sche_cum, "Normalized"))
  bin_df <- mutate(bin_df, a_res_norm = abs(bin_df$measured_t - bin_df$pred_t_norm))
}
  
doParallel::registerDoParallel(detectCores() - 1)
Pred_Data_Analyzed <- plyr::ddply(Pred_Data_Complt, .(pred_bin), calc_new_pred, .parallel = T)

res_funs <- list(sd = function(x) sd(x), mean = function(x) mean(x), median = function(x) median(x))
calc_metr <- function(bin_df) {
  rows <- c("predicted_t", "pred_t_optim", "pred_t_norm")
  rsds <- c("a_res_orig", "a_res_optim", "a_res_norm")
  dim_names = (list(mod_names, c("R2 (Pearson)", "SD", "Mean", "Median")))
  metr_mat <- matrix(nrow = 3, ncol = 4, dimnames = dim_names)
  for (row in seq_along(rows)) { 
    metr_mat[row, 1] <- cor(bin_df$measured_t, bin_df[rows[row]])^2 
    for (cell in seq_along(metr_mat[row, 2:4])) {
      metr_mat[row, (cell + 1)] <- res_funs[[cell]](bin_df[[rsds[row]]])
    }
  }
  return(metr_mat)
}

doParallel::registerDoParallel(detectCores() - 1, "FORK")
metr_mats <- vector('list', length = length(bin_lvl))
bins <- split(Pred_Data_Analyzed, Pred_Data_Analyzed$pred_bin)
metr_mats <- foreach(bin = seq_along(bins)) %dopar% {
  calc_metr(bins[[bin]])
}
  
for (bin in seq_along(bin_metrics)) {
  bin_metrics[[bin]][["Metric_Matrix"]] <- metr_mats[[bin]]
  
}

count_residuals <- function(bin_df) {
  res_type <- c("a_res_orig", "a_res_optim", "a_res_norm")
  res_matrix <- matrix(nrow = 3, ncol = length(res_names))
  for (i in seq_along(res_type)) {
    res_row <- unlist(table(cut(bin_df[[res_type[i]]], res_cutoffs)))
    res_matrix <- rbind(c(res_row, sum(res_row)), res_matrix)
  }
  return(res_matrix[1:3,])
}

for (bin in seq_along(bins)) {
  bin_metrics[[bin]][["AbsRes_Matrix"]][1:3,] <- count_residuals(bins[[bin]])
}

  

library(R6)
library(RSQLite)

BusData <- R6Class(
  # Set the name for the class; figure out how to chain method calls for in place data manipulation 
  "BusData",
  # Define the attributes 
  private = list(
    db_con      = "S4",
    route       = "character",
    is_express  = "integer",
    #tables for the basic, fitted and fitted-normalized models
    orig_data   = NULL,
    optm_data   = NULL,
    norm_data   = NULL, 
    org_coef    = c(0.4, 0.4, 0.2), #coefficients used by the BusTime model
    no_coef     = "double",
    optim_model = vector("list", length = 12),
    #bin and residual cutoffs in seconds
    bin_cutoffs = c(0, 120, 240, 360, 600, 900, 1200, Inf),
    res_cutoffs = c(0, 60, 120, 240, 360, Inf),
    #SQLite Query Params
    q_fields = c("vehicle", "t_stamp", "stop_gtfs_seq", "hist_cum", "rece_cum", "sche_cum", 
                 "t_predicted", "t_measured", "route", "depot", "is_express"),
    q_table  = "mta_bus_data",
    #Matrices for Results
    mod_names = c("Original", "Optimized", "Normalized"),
    bin_metric = NULL,
    ################################################################################################
    #Private Functions
    ################################################################################################
    read_from_db = function(db_con, is_express) {
      query <- paste0("SELECT ", str_c(private$q_fields, collapse = ', '), " FROM ", private$q_table)
      if (is.null(is_express)) {
        private$orig_data <- dbGetQuery(db_con, query)
      } else {
        private$orig_data <- dbGetQuery(db_con, paste0(query, " WHERE is_express = ", is_express))
      }
      private$orig_data <- transform(private$orig_data, t_stamp = as_datetime(as.double(t_stamp), tz = "America/New_York"))
      private$orig_data <- as.data.table(private$orig_data)
    },
    #takes a vector of coefficients and returns a vector of normalized coefficients
    normalize_coef = function(x) {
      coef_sum <- sum(x)
      x <- unlist(lapply(x, function(coef) {coef / coef_sum}))
    }
    ),
  
   public = list(
    ############################################################
    #Public Interface 
    ############################################################
    print = function() {
      cat("BusData Elements:\n", private$is_express, private$route, "\n")
      print(private$db_con)
      print(private$no_coef)
      
    },
    #constructor data.table library is used for efficiency reasons; please refer to dt docs for help with syntax
    initialize = function(db_con, is_express = NULL, route = NULL) {
      #save arguments 
      
      private$db_con <- db_con
      private$is_express <- is_express 
      private$route  <- route
      orig_data <- private$read_from_db(db_con, is_express)
    
      #update bin if needed here
      #use biglm if you run out of memory; lmtest library to test the model 
      op_mod_form = formula(private$orig_data$t_measured ~ private$orig_data$hist_cum + 
                              private$orig_data$rece_cum + 
                              private$orig_data$sche_cum + 0)
      private$optim_model <- lm(op_mod_form, private$orig_data)
      #bin original data and calculate absolute values of residuals for each row
      orig_data[,':='(pred_bin = cut(orig_data$t_predicted, private$bin_cutoffs,  dig.lab = 5),
                      abs_res = abs(t_measured - t_predicted))]
      private$orig_data <- orig_data
      private$no_coef <- private$normalize_coef(coefficients(private$optim_model))
      
      
      #calculate new prediction times using optimized and normalized coef. and bin them 1=h 2=r 3=s
      op_coef <- coefficients(private$optim_model)
      optm_data <- private$orig_data[, !c("t_predicted", "pred_bin"), with = FALSE]
      #optimized data init.
      optm_data[,':='(t_predicted = op_coef[[1]] * hist_cum + op_coef[[2]] * rece_cum + op_coef[[3]] * sche_cum)
                ][,':='(pred_bin = cut(t_predicted, private$bin_cutoffs,  dig.lab = 5),
                        abs_res = abs(t_measured - t_predicted))]
      private$optm_data <- optm_data
      
      #normalized data init.
      norm_data <- private$orig_data[, !c("t_predicted", "pred_bin"), with = FALSE]
      norm_data[,':='(t_predicted = private$no_coef[[1]] * hist_cum + 
                                  private$no_coef[[2]] * rece_cum + private$no_coef[[3]] * sche_cum)
                ][,':='(pred_bin = cut(t_predicted, private$bin_cutoffs,  dig.lab = 5),
                        abs_res = abs(t_measured - t_predicted))]
      private$norm_data <- norm_data
      
      #initialize the matrix for storing bin metrics using number of models and number of bins
      num_of_bins = length(private$bin_cutoffs) - 1
      dim_names = (list(private$mod_names, c("R2 (Pearson)", "SD", "Mean", "Median")))
      bin_metric <- matrix(rep(vector('list', num_of_bins), times = num_of_bins * 3), nrow = 3, ncol = num_of_bins)
      res_funs <- list(sd = function(x) sd(x), mean = function(x) mean(x), median = function(x) median(x))
      pred_grp_mat <- matrix(rep(vector('list'), times = num_of_bins * 3), nrow = 3, ncol = num_of_bins)
      mod_data_refs <- list(private$orig_data, private$optm_data, private$norm_data)
      print(bin_metric)
      #print(mod_data_refs[[2]]$pred_bin)
      #print(typeof(mod_data_refs[[2]]))
      #pred_gr_mat[2,] <- split(mod_data_refs[[2]], mod_data_refs[[2]]$pred_bin)
      #print(pred_gr_mat)
      
      for(i in 1:length(mod_data_refs)) {
        pred_grp_mat[i,] <- split(mod_data_refs[[i]], mod_data_refs[[i]]$pred_bin)
      }
      print(pred_grp_mat[3,1])
      print(pred_grp_mat[2,3])
      
      calc_metr <- function(bin_dt) {
        lst_names = c("R2 (Pearson)", "SD", "Mean", "Median")
        metr_lst <- vector('list', length = length(lst_names))
        metr_lst[[1]] <- cor(bin_dt$t_measured, bin_dt$t_predicted)^2
        
        for (e in 2:length(metr_lst)) {
            metr_lst[[e]] <- res_funs[[e - 1]](bin_dt$abs_res)
          }
        
        return(metr_lst)
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
      
      # cr_list <- function() {
      #   met_mat <- 
      # }
      # 
      # 
      # bin_metric <- foreach(col = seq_len(ncol(bin_metric)), .combine='cbind') %:%
      #                   foreach(row = seq_len(nrow(bin_metric)), .combine='c') %do% {
      #                       #metric_lst <- calc_metr(pred_grp_mat[row, col])
      #                       #bin_metric[row, col][1] <- metric_lst
      #                       #bin_metric[row, col]
      #                       print(row)
      #                       print(col)
      #                   }
      for(i in seq_len(nrow(bin_metric))) {
        for(j in seq_len(ncol(bin_metric))) {
          metric_lst <- calc_metr(pred_grp_mat[i, j][[1]])
          print(j)
          print(metric_lst)
          #bin_metric[row, col][1] <- metric_lst
          print(bin_metric[i,j])
        }
      }
      print(bin_metric)
      private$bin_metric <- bin_metric
      
      
     },
    get_q_fields = function() {
      print(private$q_fields)
    },
    get_orig_data = function() {
      private$orig_data
    },
    get_optm_data = function() {
      private$optm_data
    },
    get_norm_data = function() {
      private$norm_data
    },
    get_op_mod = function() {
      private$optim_model
    },
    get_bin_metric = function() {
      private$bin_metric
    }
   
   )
)

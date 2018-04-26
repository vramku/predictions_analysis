library(R6)
library(RSQLite)
library(stringr)
library(data.table)
library(lubridate)
library(MASS)
BusData <- R6Class(
  # Set the name for the class; figure out how to chain method calls for in place data manipulation 
  "BusData",
  # Define the attributes 
  private = list(
    #############################################################################################
    #Private Attribues
    #############################################################################################
    db_con      = "S4",
    route       = "character",
    is_express  = "integer",
    #tables for the basic, fitted and fitted-normalized models
    orig_data   = NULL,
    lsft_data   = NULL,
    norm_data   = NULL,
    rbst_data   = NULL,
    #coefficients and model specific parameters
    coef_list   = vector('list', length = 4),
    lsfit_mod   = vector("list", length = 12),
    robust_mod  = NULL, 
    bin_lvl     = NULL,                           #factor levels for prediction bins
    #bin and residual cutoffs in seconds
    bin_cutoffs = c(0, 120, 240, 360, 600, 900, 1200, Inf),
    res_cutoffs = c(0, 60, 120, 240, 360, Inf),
    #SQLite Query Params
    q_fields = c("vehicle", "t_stamp", "stop_gtfs_seq", "hist_cum", "rece_cum", "sche_cum", 
                 "t_predicted", "t_measured", "route", "depot", "is_express"),
    q_table  = "mta_bus_data",
    #Matrices for Results
    bin_summaries = NULL,
    res_names     = NULL,
    summ_dt       = NULL, 
    form_summ_dt  = NULL, 
    #Name Vectors 
    meas_names = c("R2 (Pearson)", "SD", "Mean", "Median"),
    coef_names = c("Historical", "Recent", "Schedule"),
    mod_names = c("Original", "Optimized", "Normalized"),
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
    },
    create_mod_dt = function(orig_dt, applied_mod = NULL, mcoefs = NULL) {
      if (is.null(mcoefs)) mcoefs <- as.vector(coefficients(applied_mod))
      new_dt <- orig_dt[, !c("t_predicted", "pred_bin"), with = FALSE]
      new_dt[,':='(t_predicted = mcoefs[1] * hist_cum + mcoefs[2] * rece_cum + mcoefs[3] * sche_cum)
             ][,':='(pred_bin = cut(t_predicted, private$bin_cutoffs,  dig.lab = 5),
                     abs_res = abs(t_measured - t_predicted))]
    }
    ),
    
   public = list(
    ############################################################
    #Public Interface 
    ############################################################
    print = function() {
      cat("BusData Elements:\n", private$is_express, private$route, "\n")
      print(private$db_con)
      print(no_coef)
      
    },
    #constructor: data.table library is used for efficiency reasons; please refer to dt docs for help with syntax
    initialize = function(db_con, is_express = NULL, route = NULL) {
      #save arguments 
      private$res_names <- c(paste0((private$res_cutoffs[-length(private$res_cutoffs)]) / 60,  " to ", (private$res_cutoffs[-1]) / 60, " mins"), "Total")
      private$db_con <- db_con
      private$is_express <- is_express 
      private$route  <- route
      orig_data <-  private$read_from_db(db_con, is_express)
      orig_coef <- c(0.4, 0.4, 0.2)
      private$coef_list[[1]] <- orig_coef
      #####################################################################################
      #Data Set Partitioning (by model) and Initialization
      #####################################################################################
      #update bins here if needed 
      #use biglm if you run out of memory; lmtest library to test the model 
      op_mod_form = formula(private$orig_data$t_measured ~ private$orig_data$hist_cum + 
                              private$orig_data$rece_cum + 
                              private$orig_data$sche_cum + 0)
      #Least Squares Linear Model
      private$lsfit_mod <- lm(op_mod_form, private$orig_data)
      private$coef_list[[2]] <- round(as.vector(private$lsfit_mod$coefficients), digits = 3)
      private$coef_list[[3]] <- round(as.vector(private$normalize_coef(coefficients(private$lsfit_mod))), digits = 3)
      #Robust Linear Model
      private$robust_mod <- rlm(op_mod_form, private$orig_data)
      private$coef_list[[4]] <- round(as.vector(private$robust_mod$coefficients), digits = 3)
      
      #bin original data and calculate absolute values of residuals for each row
      orig_data[,':='(pred_bin = cut(orig_data$t_predicted, private$bin_cutoffs,  dig.lab = 5),
                      abs_res = abs(t_measured - t_predicted))]
      private$orig_data <- orig_data
      private$bin_lvl <- levels(private$orig_data$pred_bin)
      
      #calculate new prediction times using optimized, normalized and robust coef. 1=h 2=r 3=s
      private$lsft_data <- private$create_mod_dt(orig_data, private$lsfit_mod)
      private$norm_data <- private$create_mod_dt(orig_data,  mcoefs = private$coef_list[[3]])
      private$rbst_data <- private$create_mod_dt(orig_data, private$robust_mod)
      
      #####################################################################################
      #Bin Group Analysis
      #####################################################################################
      #initialize the matrix for storing bin metrics using number of models and number of bins
      #this is a matrix of lists where elements should be accessed using the [[]] operator
      #for more information consult R's documentation on [] vs [[]]
      num_of_bins = length(private$bin_cutoffs) - 1
      dim_names = (list(private$mod_names, private$meas_names))
      bin_summaries <- matrix(rep(vector('list'), times = num_of_bins * 3), nrow = 3, ncol = num_of_bins, 
                              dimnames = list(private$mod_names, private$bin_names))
      for (i in 1:nrow(bin_summaries)) {
        for (j in 1:ncol(bin_summaries)) {
          named_bmat_lst <- list(Metric_Matrix = NULL, Residual_Matrix = NULL)
          bin_summaries[i,j][[1]] <- named_bmat_lst
        }
      }
      
      #split data into groups by prediction time bins. each matrix element represents a group
      res_funs <- list(sd = function(x) sd(x), mean = function(x) mean(x), median = function(x) median(x))
      pred_grp_mat <- matrix(rep(vector('list'), times = num_of_bins * 3), nrow = 3, ncol = num_of_bins)
      mod_data_refs <- list(private$orig_data, private$lsft_data, private$norm_data)
      for (i in 1:length(mod_data_refs)) {
        pred_grp_mat[i,] <- split(mod_data_refs[[i]], mod_data_refs[[i]]$pred_bin)
      }

      #Function accepts a prediction bin and computes metrics such as the
      #Pearson Correlation Coefficient and Standard Deviation of the residuals
      calc_metr <- function(bin_dt) {
        dim_names = list(c("Residual Metric"), private$meas_names)
        metr_mat <- matrix(nrow = 1, ncol = 4, dimnames = dim_names)
        metr_mat[1,1] <- cor(bin_dt$t_measured, bin_dt$t_predicted)^2
        
        for (inx in 2:length(metr_mat)) {
            metr_mat[1,inx] <- res_funs[[inx - 1]](bin_dt$abs_res)
          }
        
        return(metr_mat)
      }

      #Function accepts a prediction bin, and calculates and bins the residuals
      count_residuals <- function(bin_dt, lvl, percent = FALSE) {
        dim_names <- (list(private$bin_lvl[[lvl]], private$res_names))
        res_matrix <- matrix(nrow = 1, ncol = length(private$res_names))
        res_row <- unlist(table(cut(bin_dt$abs_res, private$res_cutoffs)))
        res_matrix <- rbind(c(res_row, sum(res_row)))
        dimnames(res_matrix) <- dim_names
        return(res_matrix)
      }
      
      for (i in seq_len(nrow(bin_summaries))) {
        for (j in seq_len(ncol(bin_summaries))) {
          pred_bin_for_mod <- pred_grp_mat[i, j][[1]]
          metric_mat <- calc_metr(pred_bin_for_mod)
          resid_mat <- count_residuals(pred_bin_for_mod, j)
          bin_summaries[i,j][[1]][[1]] <- metric_mat
          bin_summaries[i,j][[1]][[2]] <- resid_mat
        }
      }
      #Save the summary matrix for bins. Each cell is a two member list containing metric and residual matrices.
      private$bin_summaries <- bin_summaries
      ######################################################################################################
      #Table Creation
      ######################################################################################################
      make_table <- function(percent = FALSE) { 
        #Create a summary table using information from the summary matrix, the models and the bin factors
        sum_mat_row_names <- (rep(private$mod_names, times = length(private$bin_lvl)))
        sum_mat_col_names <- c("Bin", private$coef_names, private$res_names, private$meas_names)
        summary_matrix <- matrix(nrow = length(bin_summaries), ncol = length(sum_mat_col_names))
        #start the row counter and build the table by looping through the summary matrix
        row_num <- 1
        for (i in 1:ncol(bin_summaries)) {
          for (j in 1:nrow(bin_summaries)) {
            cell_to_insert <- bin_summaries[j,i][[1]]
            get_resids <- function() {
              if (percent) {
                #dim_names <- (list(bin_lvl[[i]], private$res_names))
                num_vec <- as.vector(cell_to_insert[[2]])
                per_vec <- round(purrr::map2_dbl(num_vec[-length(num_vec)], num_vec[length(num_vec)], function(x,y) (x/y * 100)), digits = 2)
                fus_vec <- (str_c(num_vec[-length(num_vec)], " (", per_vec, "%)", sep = ""))
                form_resids <- rbind(c(fus_vec, as.character(num_vec[length(num_vec)])))
                #dimnames(form_resids) <- dim_names
                return(form_resids)
              } else {
                return(as.vector(cell_to_insert[[2]]))
              }
            }
            buf_vec <- c(private$bin_lvl[[i]], private$coef_list[[j]], get_resids(), round(as.vector(cell_to_insert[[1]]), digits = 3))
            summary_matrix[row_num,] <- buf_vec
            row_num <- row_num + 1
          }
        }
        dimnames(summary_matrix) <- list(sum_mat_row_names, sum_mat_col_names)
        return(table <- as.data.table(summary_matrix, keep.rownames = TRUE))
      }
      
      #Summary Table
      private$summ_dt <- make_table()
      #Formatted Summary Table Creation
      private$form_summ_dt <- make_table(percent = TRUE)
    },
    get_q_fields = function() {
      print(private$q_fields)
    },
    get_orig_data = function() {
      private$orig_data
    },
    get_lsft_data = function() {
      private$lsft_data
    },
    get_norm_data = function() {
      private$norm_data
    },
    get_rbst_data = function() {
      private$rbst_dat
    },
    get_ls_mod = function() {
      private$lsfit_mod
    },
    get_ro_mod = function() {
      private$robust_mod
    },
    get_bin_summaries = function() {
      private$bin_summaries
    },
    get_coef_list = function() {
      private$coef_list
    },
    get_bin_lvl = function() {
      private$bin_lvl
    },
    get_summary_table = function() {
      private$summ_dt
    },
    get_formatted_summ_table = function() {
      private$form_summ_dt
    }
   
   )
)

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
      #orig_data$pred_bin <- cut(orig_data$t_predicted, private$bin_cutoffs,  dig.lab = 5)
      orig_data[,':='(pred_bin = cut(orig_data$t_predicted, private$bin_cutoffs,  dig.lab = 5),
                      abs_res = abs(t_measured - t_predicted))]
      private$orig_data <- orig_data
      private$no_coef <- private$normalize_coef(coefficients(private$optim_model))
      
      
      #calculate new prediction times using optimized and normalized coef. and bin them 1=h 2=r 3=s
      op_coef <- coefficients(private$optim_model)
      optm_data <- private$orig_data[, !c("t_predicted", "pred_bin"), with = FALSE]
      #optimized data init.
      optm_data[,':='(t_pred_op = op_coef[[1]] * hist_cum + op_coef[[2]] * rece_cum + op_coef[[3]] * sche_cum)
                ][,':='(pred_op_bin = cut(t_pred_op, private$bin_cutoffs,  dig.lab = 5),
                        abs_op_res = abs(t_measured - t_pred_op))]
      private$optm_data <- optm_data
      
      #normalized data init.
      norm_data <- private$orig_data[, !c("t_predicted", "pred_bin"), with = FALSE]
      #calculate new prediction times using optimized and normalized coef. and bin them 1=h 2=r 3=s
      norm_data[,':='(t_pred_no = private$no_coef[[1]] * hist_cum + 
                                  private$no_coef[[2]] * rece_cum + private$no_coef[[3]] * sche_cum)
                ][,':='(pred_no_bin = cut(t_pred_no, private$bin_cutoffs,  dig.lab = 5),
                        abs_no_res = abs(t_measured - t_pred_no))]
      private$norm_data <- norm_data
      
      
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
    }
   
   )
)

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
    pred_data   = NULL,
    org_coef    = c(0.4, 0.4, 0.2), #coefficients used by the BusTime model
    norm_coef   = "double",
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
    get_pred_data = function(db_con, is_express) {
      query <- paste0("SELECT ", str_c(private$q_fields, collapse = ', '), " FROM ", private$q_table)
      if (is.null(is_express)) {
        pred_data <- dbGetQuery(db_con, query)
      } else {
        private$pred_data <- dbGetQuery(db_con, paste0(query, " WHERE is_express = ", is_express))
      }
      private$pred_data <- transform(private$pred_data, t_stamp = as_datetime(as.double(t_stamp), tz = "America/New_York"))
      private$pred_data <- as.data.table(private$pred_data)
    },
    #takes a vector of coefficients and returns a vector of normalized coefficients
    normalize_coef = function(x) {
      coef_sum <- sum(x)
      x <- unlist(lapply(x, function(coef) {coef / coef_sum}))
    }
    ),
   public = list(
    #Public Interface 
    print = function() {
      cat("BusData Elements:\n", private$is_express, private$route, "\n")
      print(private$db_con)
    },
   initialize = function(db_con, is_express = NULL, route = NULL) {
      private$db_con <- db_con
      private$is_express <- is_express 
      private$route  <- route
      private$pred_data <- private$get_pred_data(db_con, is_express)
      #update bin if needed here
      #use biglm if you run out of memory; lmtest library to test the model 
      op_mod_form = formula(private$pred_data$t_measured ~ private$pred_data$hist_cum + 
                              private$pred_data$rece_cum + 
                              private$pred_data$sche_cum + 0)
      private$optim_model <- lm(op_mod_form, private$pred_data)
      private$pred_data$pred_bin <- cut(private$pred_data$t_predicted, bin_cutoffs,  dig.lab = 5)
      private$norm_coef <- private$normalize_coef(private$org_coef)
      
      
      
      
      
      
      
     },
    get_q_fields = function() {
      print(private$q_fields)
    },
    get_int_data = function() {
      private$pred_data
    },
    get_op_mod = function() {
      private$optim_model
    }
   
   )
)
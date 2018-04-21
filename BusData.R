library(R6)
library(RSQLite)

BusData <- R6Class(
  # Set the name for the class; figure out how to chain method calls for in place data manipulation 
  "BusData",
  # Define the attributes 
  private = list(
    db_con     = "S4",
    route      = "character",
    is_express = "integer",
    pred_data  = NULL,
    #SQLite Query Params
    q_fields = c("vehicle", "t_stamp", "stop_gtfs_seq", "hist_cum", "rece_cum", "sche_cum", 
                 "t_predicted", "t_measured", "route", "depot", "is_express"),
    q_table  = "mta_bus_data"
    ),
  public = list(
   print = function() {
      cat("BusData Elements:\n", private$is_express, private$route, "\n")
      print(private$db_con)
    },
    get_pred_data = function(db_con, is_express) {
     query <- paste0("SELECT ", str_c(private$q_fields, collapse = ', '), " FROM ", private$q_table)
     if (is.null(is_express)) {
       private$pred_data <- dbGetQuery(private$db_con, query)
     } else {
       private$pred_data <- dbGetQuery(db_con, paste0(query, " WHERE is_express = ", is_express))
     }
     private$pred_data <- transform(private$pred_data, t_stamp = as_datetime(as.double(t_stamp), tz = "America/New_York"))
   }, 
   initialize = function(db_con, is_express = NULL, route = NULL) {
      private$db_con <- db_con
      private$is_express <- is_express 
      private$route  <- route
      private$pred_data <- self$get_pred_data(db_con, is_express)
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      },
    get_q_fields = function() {
      print(private$q_fields)
    },
    get_int_data = function() {
      head(private$pred_data, 100)
    }
    
   
   )
)
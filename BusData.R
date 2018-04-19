library(R6)

BusData <- R6Class(
  # Set the name for the class; figure out how to chain method calls for in place data manipulation 
  "BusData",
  # Define the attributes 
  private = list(
    db_con     = "S4",
    route      = "character",
    is_express = "integer",
    pred_data  = "list",
    #SQLite Query Params
    q_fields = c("vehicle", "t_stamp", "stop_gtfs_seq", "hist_cum", "rece_cum", "sche_cum", 
                 "t_predicted", "t_measured", "route, depot", "is_express")
    ),
  public = list(
   print = function() {
      cat("BusData Elements:\n")
      print(private$db_con)
    },
    initialize = function(db_con, is_express = NULL, route = NULL) {
      private$db_con <- db_con
      private$is_express <- is_express 
      private$route  <- route 
      },
    get_q_fields = function() {
      print(private$q_fields)
    },
    get_pred_data <- function(con, is_express) {
     query <- "SELECT vehicle, t_stamp, stop_gtfs_seq, hist_cum, rece_cum, sche_cum, 
                   t_predicted, t_measured, route, depot, is_express
               FROM mta_bus_data"
     if (missing(is_express)) {
       return(dbGetQuery(con, query))
     } else {
       return(dbGetQuery(con, paste0(query, " WHERE is_express = ", is_express)))
     }
   } 
   
   )
)
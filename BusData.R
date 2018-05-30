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
    name          = "character",
    db_con        = "S4",
    route         = NULL,
    is_express    = "integer",
    stop_gtfs_seq = "integer",
    op_arg_lst    = vector('list'),
    num_of_mods   = 4, 
    #list of applied linear models 
    mod_dat_lst = NULL, 
    #coefficients and model specific parameters
    coef_list   = NULL,
    lsfit_mod   = vector("list", length = 12),
    robust_mod  = NULL, 
    #bin and residual cutoffs in seconds
    bin_lvl     = NULL, #factor levels for prediction bins
    bin_cutoffs = c(0, 120, 240, 360, 600, 900, 1200, Inf),
    res_cutoffs = c(0, 60, 120, 240, 360, Inf),
    #SQLite Query Params
    q_fields = c("vehicle", "t_stamp", "stop_gtfs_seq", "hist_cum", "rece_cum", "sche_cum", 
                 "t_predicted", "t_measured", "route", "depot", "is_express"),
    q_table  = "mta_bus_data",
    db_query = NULL, 
    #Matrices for Results
    bin_summaries = NULL,
    summ_dt       = NULL, 
    form_summ_dt  = NULL, 
    whole_mod_mat = NULL, 
    #Name Vectors 
    bin_names  = NULL,
    res_names  = NULL,
    meas_names = c("R2 (Pearson)", "SD", "Mean", "Median"),
    coef_names = c("Historical", "Recent", "Schedule"),
    mod_names  = c("Original", "Optimized", "Normalized", "Robust"),
    ################################################################################################
    #Private Functions
    ################################################################################################
    read_from_db = function(db_con) { 
      db_query <- paste0("SELECT ", str_c(private$q_fields, collapse = ', '), " FROM ", private$q_table)
      print(private$op_arg_lst)
      arg_vec <- unlist(private$op_arg_lst)
      counter <- length(arg_vec)
      if (!is.null(arg_vec)) {
        arg_names <- names(arg_vec)
        db_query <- paste0(db_query, " WHERE")
        for (arg_name in arg_names) {
          arg_val <- arg_vec[[arg_name]]
          if (arg_name == 'route') arg_val <- paste0("'",arg_val,"'")
          db_query <- paste(db_query, arg_name, "=", arg_val, sep = " ")
          if (counter > 1) {
            db_query <- paste(db_query, "AND", sep = " ")
            counter <- counter - 1
          }
        }
      }
      print(db_query)
      private$db_query <- db_query
      private$mod_dat_lst[[1]] <- dbGetQuery(db_con, private$db_query)
      private$mod_dat_lst[[1]] <- transform(private$mod_dat_lst[[1]], t_stamp = as_datetime(as.double(t_stamp), tz = "America/New_York"))
      private$mod_dat_lst[[1]] <- as.data.table(private$mod_dat_lst[[1]])
    },
    #takes a vector of coefficients and returns a vector of normalized coefficients
    normalize_coef = function(x) {
      coef_sum <- sum(x)
      x <- unlist(lapply(x, function(coef) {coef / coef_sum}))
    },
    #calculates new predictions based on supplied coefficiensts and bins the results
    create_mod_dt = function(orig_dt, applied_mod = NULL, mcoefs = NULL) {
      if (is.null(mcoefs)) mcoefs <- as.vector(coefficients(applied_mod))
      new_dt <- orig_dt[, !c("t_predicted", "pred_bin"), with = FALSE]
      new_dt[,':='(t_predicted = mcoefs[1] * hist_cum + mcoefs[2] * rece_cum + mcoefs[3] * sche_cum)
             ][,':='(pred_bin = cut(t_predicted, private$bin_cutoffs,  dig.lab = 5),
                     abs_res = abs(t_measured - t_predicted))]
    },
    create_name = function() {
      t_min <- min(private$mod_dat_lst[[1]]$t_stamp)
      t_str <- round((max(private$mod_dat_lst[[1]]$t_stamp) - t_min), digits = 1)
      name_vec <- c("Route", "Stop GTFS Seq", "Vehicle", "Direction")
      name_ctr <- 1
      is_null_args <- is.null(unlist(private$op_arg_lst))
      exp <- ifelse(is_null_args, "Aggregate Data for", 
                    ifelse(private$mod_dat_lst[[1]]$is_express[1], "Express Data for", "Local Data for"))
      name <- paste(exp, t_min, "Time Interval:", t_str, units(t_str), sep = " ")
      for (arg in private$op_arg_lst[2:length(private$op_arg_lst)]) {
      if (!is.null(arg)) name <- paste(name, name_vec[name_ctr], arg, sep = " ")
      name_ctr <- name_ctr + 1
      }
      return(name)
    }
    ),
    
   public = list(
    ############################################################
    #Public Interface 
    ############################################################
    print = function() {
      print(private$name)
      print(private$db_con)
      print(private$db_query)
      lapply(private$mod_dat_lst, summary)
    },
    #constructor: data.table library is used for efficiency reasons; please refer to dt docs for help with syntax
    #IMPORTANT: When passing arguments to this function that are variables, use do.call() function to ignore R's lazy evaluation rules
    initialize = function(db_con, is_express = NULL, route = NULL, vehicle = NULL, direction = NULL, stop_gtfs_seq = NULL) {
      #initialize private fields and save arguments 
      private$op_arg_lst <- formals()[2:length(formals())]
      op_arg_lst <- (as.list(match.call()))[-c(1:2)]
      #print(op_arg_lst)
      for (arg_name in names(op_arg_lst)) { 
          private$op_arg_lst[arg_name] <- op_arg_lst[arg_name]
      }
      num_of_mods <- private$num_of_mods
      private$mod_dat_lst <- vector('list', length = num_of_mods)
      private$coef_list <- vector('list', length = num_of_mods)
      private$res_names <- c(paste0((private$res_cutoffs[-length(private$res_cutoffs)]) / 60,  " to ", (private$res_cutoffs[-1]) / 60, " mins"), "Total")
      private$bin_names <- paste0((private$bin_cutoffs[-length(private$bin_cutoffs)]) / 60, " to ", (private$bin_cutoffs[-1]) / 60, " mins")
      private$mod_dat_lst <- setNames(private$mod_dat_lst, private$mod_names)
      private$db_con <- db_con
      private$is_express <- is_express 
      private$stop_gtfs_seq <- stop_gtfs_seq
      private$route <- as.character(route)
      orig_data <- private$read_from_db(db_con)
      orig_coef <- c(0.4, 0.4, 0.2)
      private$name <- private$create_name()
      print(is.null(private$is_express))
      #####################################################################################
      #Data Set Partitioning (by model) and Initialization
      #####################################################################################
      #update bins here if needed 
      #use biglm if you run out of memory; lmtest library to test the model 
      op_mod_form = formula(orig_data$t_measured ~ orig_data$hist_cum + 
                              orig_data$rece_cum + 
                              orig_data$sche_cum + 0)
      #Least Squares Linear Model
      private$lsfit_mod <- lm(op_mod_form, orig_data)
      
      #Robust Linear Model
      private$robust_mod <- rlm(op_mod_form, orig_data)
      
      #Save the coefficients
      private$coef_list[[1]] <- orig_coef
      private$coef_list[[2]] <- round(as.vector(private$lsfit_mod$coefficients), digits = 3)
      private$coef_list[[3]] <- round(as.vector(private$normalize_coef(coefficients(private$lsfit_mod))), digits = 3)
      private$coef_list[[4]] <- round(as.vector(private$robust_mod$coefficients), digits = 3)
      
      #bin original data and calculate absolute values of residuals for each row
      orig_data[,':='(pred_bin = cut(orig_data$t_predicted, private$bin_cutoffs,  dig.lab = 5),
                      abs_res = abs(t_measured - t_predicted))]
      private$mod_dat_lst[[1]] <- orig_data
      private$bin_lvl <- levels(orig_data$pred_bin)
      
      #calculate new prediction times using optimized, normalized and robust coef. 1=h 2=r 3=s
      private$mod_dat_lst[[2]] <- private$create_mod_dt(orig_data, private$lsfit_mod)
      private$mod_dat_lst[[3]] <- private$create_mod_dt(orig_data, mcoefs = private$coef_list[[3]])
      private$mod_dat_lst[[4]] <- private$create_mod_dt(orig_data, private$robust_mod)
      
      #####################################################################################
      #Bin Group Analysis
      #####################################################################################
      #initialize the matrix for storing bin metrics using number of models and number of bins
      #this is a matrix of lists where elements should be accessed using the [[]] operator
      #for more information consult R's documentation on [] vs [[]]
      num_of_bins = length(private$bin_cutoffs) - 1
      dim_names = (list(private$mod_names, private$meas_names))
      bin_summaries <- matrix(rep(vector('list'), times = num_of_bins * num_of_mods), nrow = num_of_mods, ncol = num_of_bins, 
                              dimnames = list(private$mod_names, private$bin_names))
      for (i in 1:nrow(bin_summaries)) {
        for (j in 1:ncol(bin_summaries)) {
          named_bmat_lst <- list(Metric_Matrix = NULL, Residual_Matrix = NULL)
          bin_summaries[i,j][[1]] <- named_bmat_lst
        }
      }
      
      #split data into groups by prediction time bins. each matrix element represents a group
      res_funs <- list(sd = function(x) sd(x), mean = function(x) mean(x), median = function(x) median(x))
      pred_grp_mat <- matrix(rep(vector('list'), times = num_of_bins * num_of_mods), nrow = num_of_mods, ncol = num_of_bins)
      for (i in 1:length(private$mod_dat_lst)) {
        pred_grp_mat[i,] <- split(private$mod_dat_lst[[i]], private$mod_dat_lst[[i]]$pred_bin)
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
      
      #Calculate metrics over all of the data for each model 
      private$whole_mod_mat <- matrix(nrow = length(res_funs) + 1, ncol = length(private$mod_dat_lst))
      wmm_names <- list(private$mod_names, private$meas_names)
      for (md_inx in seq_along(private$mod_dat_lst)) {
        wmetr_mat <- matrix(nrow = 1, ncol = length(private$meas_names))
        wmetr_mat[1,1] <- round(cor(private$mod_dat_lst[[md_inx]]$t_measured, private$mod_dat_lst[[md_inx]]$t_predicted)^2, digits = 2)
        for (inx in 2:length(wmetr_mat)) {
          wmetr_mat[1,inx] <- trunc(res_funs[[inx - 1]](private$mod_dat_lst[[md_inx]]$abs_res))
        }
        private$whole_mod_mat[md_inx,] <- wmetr_mat
      }
      dimnames(private$whole_mod_mat) <- wmm_names 
      ######################################################################################################
      #Table Creation
      ######################################################################################################
      make_table <- function(compressed = FALSE) { 
        #Create a summary table using information from the summary matrix, the models and the bin factors
        resid_names <- private$res_names
        if (!compressed) {
          vp <- str_c(c(rep("%", times = length(resid_names) - 1)), " Bin ", c(1:(length(resid_names) - 1)))
          resid_names <- c(unlist(purrr::map2(resid_names[-length(resid_names)], vp, function(x,y) {c(x,y)})), "Total")
        }
        sum_mat_row_names <- (rep(private$mod_names, times = length(private$bin_lvl)))
        sum_mat_col_names <- c("Bin", private$coef_names, resid_names, private$meas_names)
        summary_matrix <- matrix(nrow = length(bin_summaries), ncol = length(sum_mat_col_names))
        #start the row counter and build the table by looping through the summary matrix
        row_num <- 1
        for (i in 1:ncol(bin_summaries)) {
          for (j in 1:nrow(bin_summaries)) {
            cell_to_insert <- bin_summaries[j,i][[1]]
            get_resids <- function() {
              if (compressed) {
                #dim_names <- (list(bin_lvl[[i]], private$res_names))
                num_vec <- as.vector(cell_to_insert[[2]])
                per_vec <- round(purrr::map2_dbl(num_vec[-length(num_vec)], num_vec[length(num_vec)], function(x,y) (x/y * 100)), digits = 2)
                fus_vec <- (str_c(num_vec[-length(num_vec)], " (", per_vec, "%)", sep = ""))
                form_resids <- rbind(c(fus_vec, as.character(num_vec[length(num_vec)])))
                #dimnames(form_resids) <- dim_names
                return(form_resids)
              } else {
                cell <- cell_to_insert[[2]]
                per_vec <- round(purrr::map2_dbl(cell[-length(cell)], cell[length(cell)], function(x,y) (x/y * 100)), digits = 2)
                res_w_perc <- c(rbind(as.vector(rbind(cell[-length(cell)], per_vec))), cell[length(cell)])
                return(res_w_perc)
              }
            }
            
            buf_vec <- c(private$bin_names[[i]], private$coef_list[[j]], get_resids(), round(as.vector(cell_to_insert[[1]]), digits = 3))
            summary_matrix[row_num,] <- buf_vec
            row_num <- row_num + 1
          }
        }
        dimnames(summary_matrix) <- list(sum_mat_row_names, sum_mat_col_names)
        table <- as.data.table(summary_matrix, keep.rownames = TRUE)
        table[,':='(SD = as.integer(SD), Mean = as.integer(Mean), Median = as.integer(Median))]
        names(table)[[1]] <- "Coefficients"
        return(table)
      }
      
      #Summary Table
      private$summ_dt <- make_table()
      #Formatted Summary Table Creation
      private$form_summ_dt <- make_table(compressed = TRUE)
    },
    get_db_query = function() {
      print(private$db_query)
    },
    get_mod_data = function() {
      private$mod_dat_lst
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
    },
    get_bin_names = function() {
      private$bin_names
    },
    get_res_names = function() {
      private$res_names
    },
    get_res_cuts = function() {
      private$res_cutoffs
    },
    get_bin_cuts = function() {
      private$bin_cutoffs
    },
    get_op_args = function() {
      private$op_arg_lst
    },
    get_name = function() {
      private$name
    },
    get_whole_mets = function() {
      private$whole_mod_mat
    },
    write_table = function() {
      hs <- createStyle(fontColour = "#ffffff", fgFill = "#4F80BD",
                        halign = "center", valign = "center", textDecoration = "Bold",
                        border = "TopBottomLeftRight") 
      
    }
   )
)

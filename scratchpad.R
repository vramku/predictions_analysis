# sum_mat_row_names <- (rep(private$mod_names, times = length(private$bin_lvl)))
# sum_mat_col_names <- c("Bin", private$coef_names, private$res_names, private$meas_names)
# summary_matrix <- matrix(ncol = length(sum_mat_col_names), nrow = length(bin_summaries), dimnames = list(rep(mod_names, times = length(bin_summaries)), 
#                                                                                       c(res_names, meas_names, coef_names, "Bin")))
# Map(function(x, y) {x / y * 100}, dts[,3], dts[,8])

calcpercent <- function(...) {
  resid <- list(...)
  for(i in 1:(length(resid) - 1)) {
    resid[[i]] <- resid[[i]] / resid[[length(resid)]] * 100
  }
}

mod_dt <- function(x) {
  print(x/)
}

in_cols = c("0 to 1 mins", "1 to 2 mins")
dts[, .(perc := 0 to 1 mins/Total, by = .(Bin, rn)]


func1 <- function(...) {
  ls <- list(...)
  print(ls)
}

func2 <- function(x, y) {
  per <- x/y * 100
  str_c(x,per,sep = " ",collapse = "")
}
#helpful in building a matrix 
func3 <- function(dt) {
  m <- matrix(nrow = 21, ncol = 1)
  for (i in 3:7) {
    m <- cbind(m, purrr::map2_chr(dt[[i]], dt[[8]], ~func2(.x,.y)))
  }
  print(m)
}

count_residuals <- function(bin_dt, lvl) {
  dim_names <- (list(bin_lvl[[lvl]], res_names))
  res_matrix <- matrix(nrow = 1, ncol = length(res_names))
  res_row <- as.vector(table(cut(bin_dt$a_res_orig, res_cutoffs)))
  per_row <- round(purrr::map2_dbl(res_row, sum(res_row), function(x,y) (x/y * 100)), digits = 2)
  fus_row <- (str_c(res_row, " (", per_row, "%)", sep = ""))
  res_matrix <- rbind(c(fus_row, as.character(sum(res_row))))
  dimnames(res_matrix) <- dim_names
  
  return(res_matrix)
}

for (i in 1:ncol(binsums)) {
  for (j in 1:nrow(binsums)) {
    print(i + j - 1)
    cell_to_insert <- binsums[j,i][[1]]
    buf_vec <- c(bin_lvl[[i]], coefs[[j]], as.vector(cell_to_insert[[2]]), round(as.vector(cell_to_insert[[1]]), digits = 3))
    summ_t[i*j,] <- buf_vec
    #print(rbind(bin_lvl[[i]], coefs[[j]], as.vector(cell_to_insert[[2]]), as.vector(cell_to_insert[[1]])))
    #summary_matrix[i*j, ] <- cbind(bin_lvl[[i]], coef_list[[j]], cell_to_insert[[2]], cell_to_insert[[1]])
  }
}


create_mod_dt <- function(orig_dt, applied_mod, coefs = NULL) {
  if (coefs == NULL) coefs  <- coefficients(applied_mod)
  new_dt <- orig_data[, !c("t_predicted", "pred_bin"), with = FALSE]
  new_dt[,':='(t_predicted = coefs[[1]] * hist_cum + coefs[[2]] * rece_cum + coefs[[3]] * sche_cum)
                ][,':='(pred_bin = cut(t_predicted, private$bin_cutoffs,  dig.lab = 5),
                        abs_res = abs(t_measured - t_predicted))]
}

wb <- loadWorkbook("testwb.xlsx", create = TRUE)
createSheet(wb, "busdata1")
XLConnect::writeWorksheet(wb, dt1, "busdata1", startRow = 2, startCol = 2, header = TRUE)
saveWorkbook(wb)

swb <- loadWorkbook("predtemp.xltx")
dt1 <- locobj$get_summary_table()
c <- getCellStyle (swb, "mystyle")
writeWorksheet (swb , data = dt1 , sheet = 1 , startRow = 2 , startCol = 2 , header = TRUE )
setCellStyle (swb , sheet = 1 , row = 1 , col = 1 , cellstyle = c )
saveWorkbook ( swb )

#openxlsx
writeDataTable(wb2, 1, dt1, startRow = 2, startCol = 2, tableStyle = "TableStyleMedium1")

test_func <- function(dt, ..., ) {
  wrapr::stop_if_dot_args(substitute(list(...)), "f"))
  argls <- substitute(list(...))
  print(argls)
}

create_name = function() {
  t_min <- min(private$mod_dat[[1]]$t_stamp)
  t_str <- max(private$mod_dat_lst[[1]]$t_stamp - t_min)
  exp <- ifelse(is_express, "Express Data", ifelse(is.null(is_express)))
}

create_name_t <- function(e = NULL, r = NULL, s = NULL) {
  #arglist <- as.list(match.call())
  #args <- names(arglist) 
  name <- NULL 
  #print(arglist)
  arglist <- print(as.list(match.call()))
}

    read_from_db2 = function(db_con, is_express, stop_gtfs_seq, route) { 
      db_query <- paste0("SELECT ", str_c(private$q_fields, collapse = ', '), " FROM ", private$q_table)
      arg_vec <- unlist(private$op_args_lst)
      counter <- length(arg_vec)
      
      if (!is.null(arg_vec)) {
        arg_names <- names(arg_vec)
        paste0(db_query, " WHERE ")
        for (arg_name in arg_names) {
          paste(db_query, arg_name, "=", arg_vec[[arg_name]], sep = " ")
          if(counter > 1) {
            paste0(" AND ")
            counter <- counter - 1
          }
        }
      }

      print(db_query)
      private$mod_dat_lst[[1]] <- dbGetQuery(db_con, db_query)
      private$mod_dat_lst[[1]] <- transform(private$mod_dat_lst[[1]], t_stamp = as_datetime(as.double(t_stamp), tz = "America/New_York"))
      private$mod_dat_lst[[1]] <- as.data.table(private$mod_dat_lst[[1]])
      private$db_query <- db_query
    }
    
    
funci <- function(x = NULL, y = NULL, z = NULL) {
  call <- sys.call()
  fcall <- force(call)
  match.call((call = fcall))
}



swb <- loadWorkbook(system.file("summary.xls", package = "openxlsx"))
#load the template 
swb <- loadWorkbook('predtemp1.xlsx')
setColWidths(swb, 1, cols = 3:22, widths = 1.2)


options("openxlsx.borderColour" = "#4F80BD")
options("openxlsx.borderStyle" = "thin")
options("openxlsx.dateFormat" = "mm/dd/yyyy")
options("openxlsx.datetimeFormat" = "yyyy-mm-dd hh:mm:ss")
options("openxlsx.numFmt" = NULL) ## For default style rounding of numeric columns

wb <- createWorkbook()
addWorksheet(wb, 1)
freezePane(wb, 1, firstActiveRow = 3)

writeDataTable(wb, 1, expobj$get_summary_table(), startRow = 2, startCol = 1, tableStyle = "TableStyleMedium1")
insertPlot(wb, 1, xy = c("A",32), width = 9.5, height = 8)
insertPlot(wb, 1, xy = c("K",32), width = 9.5, height = 8)
saveWorkbook(wb, "newbook.xlsx")



wb <- write.xlsx(table_list, file = "writeXLSXTable.xlsx", asTable = TRUE, tableStyle = "TableStyleLight2", headerStyle = hs)
setColWidths(wb, 1, cols = 1:20, widths = 13)
hs <- createStyle(fontColour = "#FFFFFF", fgFill = "#4F80BD",
                  halign = "center", valign = "center", textDecoration = "Bold",
                  border = "TopBottomLeftRight", textRotation = 60, indent = 4, fontSize = 12)


for (inx in seq_len(length(routes_vec[[1]]))) {
  if (exists("wb")) rm(wb)
  wb <- createWorkbook()
  addWorksheet(wb, routes_vec[[1]][inx])
  setColWidths(wb, 1, cols = 1:20, widths = 13)
  freezePane(wb, 1, firstActiveRow = 2)
  writeDataTable(wb, 1, table_list[[inx]], headerStyle = hs, startRow = 1, tableStyle = "TableStyleMedium1")
  print(graph_list[[inx]])
  insertPlot(wb, 1, xy = c("A", 30), width = 9.5, height = 8)
  print(rob_graph_list[[inx]])
  insertPlot(wb, 1, xy = c("I", 30), width = 9.5, height = 8)
  print(bar_list[[inx]])
  insertPlot(wb, 1, xy = c("A", 72), width = 19, height = 10)
  saveWorkbook(wb, file = paste0(routes_vec[[1]][inx], ".xlsx"))

}
saveWorkbook(wb, "newworkbook.xlsx")


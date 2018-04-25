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
  print(str_c(res_row, " (", per_row, "%)" sep = ""))
  res_matrix <- rbind(c(res_row, sum(res_row)))
  dimnames(res_matrix) <- dim_names
  
  return(res_matrix)
}
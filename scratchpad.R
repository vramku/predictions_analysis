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
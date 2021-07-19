Zr <- function(x, r = 0) {
  # takes a vector x and return the Z_r's statistics
  # x : the vector x
  # r : number of contamination
  x <- x[order(x)]
  n <- length(x)
  if (x[n] == 0) {
    warning("the maximum observation is 0")
  } else {
    return((x[n-r] - x[1]) /(sum(x[(n-r+1):n])- r*(x[1])))
  }
}

JD <- function(x,k){
# the function takes a vector x and returns the JD's statistics
# x: the input vector, assumed coming from Pareto disribution
# k: the number of contamination in the sample, it's the parameter of the slippage alternative, has to be gerater or equal to 2
if(k < 2){
    stop("k has to be at last 2")
}
  lnx <- log(x)[order(x)]
  n <- length(x)
  return(1 - (lnx[n-k] - lnx[1])/(lnx[n]- lnx[n-k+1]))
}
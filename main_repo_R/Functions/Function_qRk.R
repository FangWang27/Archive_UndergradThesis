# use bisection method to obtain the quantile of R_k with bisection method
qRk <- function(q, n, k, beta = 1) {
  # n, k, beta: parameter for slippage alternative.
  # q: quantile
  if (q == 1) {
    return(Inf)
  }
  if (q < 0) {
    return(0)
  }
  p <- bisection_method(
    f = function(p) {
      r <- p / (1 - p)
      pRk(r, n, k, beta, lower.tail = TRUE)
    },
    y = q
  )$root
  return(p / (1 - p))
}

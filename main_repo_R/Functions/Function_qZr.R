# use bisection method to obtain the quantile of Z_r with bisection method
qZr <- function(n, r, q) {
  # n,r : parameter for slippage alternative
  # q: quantile
  bisection_method(
    f = function(z) {
      pZr(z, b = 1, n, r, lower.tail = TRUE)
    },
    y = q
  )$root
}

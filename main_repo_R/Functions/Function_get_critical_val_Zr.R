get_critical_val_Zr <- function(n, r, alpha = 0.05) {
    #n,r : parameter for slippage alternative
    #alpha: type1 error threhold 
  bisection_method(
    f = function(z) {
      pZr(z, b =1, n, r, lower.tail = TRUE)
    },
    y = alpha
  )$root
}

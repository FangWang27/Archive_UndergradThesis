bisection_method <- function(f, y = 0, tol = 1e-10, lower = 0, upper = 1, Bmax = 5000) {
  # the function use bisection method to find the root for f(x) - y = 0
  # f : function which root need to be find
  # y : the root it try to find
  # tol : the tolerance of the error
  # lower : lower bound for the root
  # upper bound for the root to exist

  b <- 0
  x_0 <- mean(c(lower, upper))
  g <- function(x) {
    f(x) - y
  }
  while (b < Bmax & abs(g(x_0)) > tol) {
    if (sign(g(x_0)) == sign(g(upper))) {
      upper <- x_0
      x_0 <- mean(c(lower, upper))
    } else {
      lower <- x_0
      x_0 <- mean(c(lower, upper))
    }
    b <- b + 1
  }
  result <- data.frame(
    iteration_number = b,
    root = x_0,
    error = abs(g(x_0))
  )
  return(result)
}
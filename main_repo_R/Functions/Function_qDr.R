require(dplyr)
# functions that get the critical values for Dixon's statistics with Monte Carlo method

qDr <- function(n, r, q = 0.05, B = 1e5, R = 5e2, se.return = TRUE) {
  # B: Monte Carlo simulation times
  # R: Bootstrap simulation times
  # q: quantile
  # n, r : the parameter specified for slippage alternative
  # se.return: should standard error be returned? default is true
  # generate samples
  rexp(B * n) %>%
    matrix(., nrow = n, ncol = B) %>%
    apply(., 2, Dr, r = r) ->
  sample_Dr

  # Monte-Carlo estimate
  theta_hat <- quantile(sample_Dr, q)

  # Monte Carlo  Nonparametric bootstrap standard error estimation
  theta_boot <- rep(NA, R)

  for (i in 1:R) {
    sample_boot <- sample(sample_Dr, size = B, replace = TRUE)
    theta_boot[i] <- quantile(sample_boot, q)
  }
  theta_bias <- 1 / R * (mean(theta_boot) - theta_hat)
  theta_se <- sd(theta_boot)
  if (!se.return) {
    return(theta_hat - theta_bias)
  } else {
    return(data.frame(estimate = theta_hat - theta_bias, estimate_se = theta_se))
  }
}

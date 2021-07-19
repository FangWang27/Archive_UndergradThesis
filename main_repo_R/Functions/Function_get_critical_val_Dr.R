get_critical_val_Dr <- function(n, r, alpha = 0.05, B = 1e5, R = 5e2) {
  # B: Monte Carlo simulation times
  # R: Bootstrap simulation times
  # alpha: critical value confidence level
  # n, r : the parameter specified for slippage alternative

  #generate samples
  rexp(B * n) %>%
  matrix(., nrow = n, ncol = B) %>%
  apply(., 2, Dr, r = r) ->
  sample_Dr

  # Monte-Carlo estimate 
  theta_hat <- quantile(sample_Dr, 1 - alpha)

  #Monte Carlo  Nonparametric bootstrap standard error estimation
  theta_boot <- rep(NA, R)

  for (i in 1:R) {
    sample_boot <- sample(sample_Dr, size = B, replace = TRUE)
    theta_boot[i] <- quantile(sample_boot, 1 - alpha)
  }
  theta_bias <- 1 / R * (mean(theta_boot) - theta_hat)
  theta_se <- sd(theta_boot)

  return(
    data.frame(estimate = theta_hat - theta_bias, estimate_se = theta_se)
  )
}

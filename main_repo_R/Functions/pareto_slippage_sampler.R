pareto_slippage_sampler <- function(n = 5, r = 0, alpha = 1, beta = 1, B = 10) {
  # the function generate pareto sample under $H_r$.
  # n: number of samples to generate for each set of sample
  # r: how many contaminated observation for each set of sample
  # beta: parameter controling contaminated sample generated
  # alpha: the parameter of the pareto distribution
  # B: how many sets of sample generated
  sample_generated <- matrix(NA, nrow = n, ncol = B) # generate Exp(rate = 1, rate = b )
  for (i in 1:B) {
    accept <- FALSE
    while (!accept) {
      if (r == 0) {
        accept <- TRUE
        sample_generated[, i] <- rexp(n, alpha)
      } else {
        exp_samples_1 <- rexp(n - r, alpha)
        exp_samples_r <- rexp(r, alpha) /beta
        if (max(exp_samples_1) < min(exp_samples_r)) {
          accept <- TRUE
          sample_generated[, i] <- sort(c(exp_samples_1, exp_samples_r))
        }
      }
    }
  }
  return( beta * exp(sample_generated))
}


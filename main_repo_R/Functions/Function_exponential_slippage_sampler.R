exponential_slippage_sampler <- function(n = 5, r = 0, b = 1, B = 10) {
  # the function generate exponential sample under $H_r$.
  # n: number of samples to generate for each set of sample
  # r: how many contaminated observation for each set of sample
  # b: parameter controling contaminated sample generated. Generate Exp(1) and Exp(b) r.v
  # B: how many sets of sample generated
  sample_generated <- matrix(NA, nrow = n, ncol = B)
  for (i in 1:B) {
    accept <- FALSE
    while (!accept) {
      if (r == 0) {
        accept <- TRUE
        sample_generated[, i] <- rexp(n, 1)
      } else {
        exp_samples_1 <- rexp(n - r, 1)
        exp_samples_r <- rexp(r, 1) / b # rate b r.v
        if (max(exp_samples_1) < min(exp_samples_r)) {
          accept <- TRUE
          sample_generated[, i] <- sort(c(exp_samples_1, exp_samples_r))
        }
      }
    }
  }
  return(sample_generated)
}

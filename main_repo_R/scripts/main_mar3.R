# file used to examinant test statistics for Pareto disribution

setwd("C:/Users/wangf/OneDrive - Seneca College of Applied Arts & Technology/Current/MATH4P06/main_repo_R/Functions")
sapply(list.files(), source)
require("dplyr")
require("ggplot2")
# generate Pareto(alpha, theta) samples
n <- 15
k <- 3
B <- 5000
sample_pareto_1 <- pareto_slippage_sampler(n, r = k , alpha = 1, beta = 1, B = B)
sample_pareto_2 <- pareto_slippage_sampler(n, r = k , alpha = 1, beta = 1/2, B = B)
sample_pareto_5 <- pareto_slippage_sampler(n, r = k , alpha = 1, beta = 1/5, B = B)

df_pareto_samples <- data.frame(
    s_1 = apply(sample_pareto_1,2, function(rk){Rk(rk,k = k)}),
    s_2 = apply(sample_pareto_2,2, function(rk){Rk(rk,k = k)}),
    s_5 = apply(sample_pareto_5,2, function(rk){Rk(rk,k = k)})
)

ggplot(data = log(df_pareto_samples), aes(alpha=0.5)) +
geom_histogram(aes(s_1, fill = "s_1")) +
geom_histogram(aes(s_2, fill = "s_2")) +
geom_histogram(aes(s_5, fill = "s_5"))




# generate 100 sample and obtain test statistics
sample_Rk <- apply(sample_pareto_5,2, function(rk){Rk(rk,k = k)})
seq_r  <- seq(0,5,0.01)

df_edf <- data.frame(
    seq_r = seq_r,
    theoretical =  sapply(seq_r, function(r){pRk(r,n,k, beta = 1/5)}),
    empirical = sapply(seq_r, ecdf(sample_Rk))
)
# plot the theoretical cdf and edf
ggplot(df_edf, aes(seq_r)) +
geom_line(aes( y = theoretical, colour  = "Theoretical"),) +
geom_point( aes(y = empirical,colour  = "Empirical"),alpha = 0.5) +
labs(x = "r", y = "P(R_k < r)", title = "CDF of R_k, n = 15, k = 3, beta = 1/5", color = "CDF Type") +
guides(colour=guide_legend(override.aes=list( linetype=c(0,1), shape = c(16,NA))))

# the above code verified that the cdf and simulation method is correct.

# test the power for n = 12, k = 3

# obtain the critical values for Dr

crit_val_Rk <- qRk(q = 0.05, n = 15, k = 2)
pRk(crit_val_Rk, n = 15, k = 2,beta = 1)


seq_beta <- seq(1e-4, 1, 1e-2)

power_Rk <- sapply(seq_beta, function(beta){pRk(crit_val_Rk, n= 15, k =2, beta)})
plot(power_Rk)


qRk(q = 0.1, n = 12, k = 3)

seq_r <- seq(0,100,1)
plot(sapply(seq_r, function(x) pRk(x,n=12,k=3, beta = 1/5)))



# using Monte Carlo method to obtain the critical values for Dixon's statistics for Pareto distribution

set.seed(1234)
crit_val_Dr <- qDr(n = 12, r = 3,q = 0.95, se.return = FALSE)
mat_power_Dr <- matrix(NA, ncol = 2, nrow = length(seq_beta))
B <- 1e4
for (i in seq_along(seq_beta)) {
  beta <- seq_beta[i]
  pareto_slippage_sampler(n = 12, r = 3, beta = beta, B = B) %>%
    apply(., 2, function(x) Dr(log(x), r = 3)) %>%
    {
      . > crit_val_Dr
    } %>%
    {
      c(p.val = mean(.), se = sd(.))
    } ->
  power_Dr
  mat_power_Dr[i, ] <- power_Dr
}

plot(mat_power_Dr[, 1])
# save the file
df_power_pareto <- data.frame(
  betas = seq_beta,
  R_k = power_Rk,
  Dr = mat_power_Dr[, 1],
  Dr_se = mat_power_Dr[, 2]
)
# save(df_power_pareto, file = "pareto_power.RData")
# setwd("C:\\Users\\wangf\\OneDrive - Seneca College of Applied Arts & Technology\\Current\\MATH4P06\\")
# load("midterm_report_Rfiles/data/pareto_power.RData")
ggplot(df_power_pareto, aes(x = betas)) +
geom_line(aes( y = R_k, color = "Rk")) +
geom_point( aes(y = Dr, color = "Dixon's stats")) +
#geom_errorbar(aes(ymin = Dr - Dr_se, ymax = Dr + Dr_se, width = .02)) +
labs(x = "beta", y = "power", title = "power of the test with n = 12, k = 3", color = "test type") +
guides(colour = guide_legend(override.aes = list(linetype=c(0,1), shape = c(16,NA))))


pareto_sample <- pareto_slippage_sampler(n = 12, r = 3, beta = 0.5, B = 1)
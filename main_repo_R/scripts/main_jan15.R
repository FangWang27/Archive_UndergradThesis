# The main file used to write the mid-term report
library(dplyr)
library(magrittr)
# source all functions
#setwd("C:/Users/wangf/OneDrive - Seneca College of Applied Arts & Technology/Current/MATH4P06/midterm_report_Rfiles/Functions")
setwd("~/Documents/MATH4P06/midterm_report_Rfiles/Functions")
sapply(list.files(), source)


# function that calculate the critical values for Dixon's stats

# test the case for n=6, r=4

# functions that get the critical values for Dixon's statistics with Monte Carlo method



# calculate the critical values for Dr

mat_cirtical_val_Dr <- matrix(NA, nrow = 12, ncol = 6)
mat_se_Dr <- matrix(NA, nrow = 12, ncol = 6)
set.seed(1234)
tictoc::tic()
for (n in 6:12) {
  for (r in 1:(min(6, n - 2))) {
    B <- ifelse(r == 1, 1e6, 1e5) # se is larger for the case r = 1
    estimated <- qDr(n, r, B = B)
    mat_cirtical_val_Dr[n, r] <- estimated$estimate
    mat_se_Dr[n, r] <- estimated$estimate_se
  }
}
tictoc::toc()
#save file
save(mat_cirtical_val_Dr,mat_se_Dr, file = "midterm_report_Rfiles/data/Dr_critical.RData" )
# power comparison for n = 12, r = 3 and b vary from 0.01 to 0.1

# get the critical values
set.seed(1234)
crit_val_Dr <- qDr(n = 12, r = 3, se.return = FALSE)

# obtain the power for Dr
seq_b <- seq(0.01, 1/3, 0.001)
mat_power_Dr <- matrix(NA, ncol = 2, nrow = length(seq_b))
B <- 1e3
for (i in seq_along(seq_b)) {
  b <- seq_b[i]
  exponential_slippage_sampler(n = 12, r = 3, b = b, B = B) %>%
    apply(., 2, function(x) Dr(x, r = 3)) %>%
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

# obtain the power for Zr

crit_val_Zr <- qZr(n=12,r=3, q = 0.05)

power_Zr <- sapply(seq_b, function(b) {
pZr(crit_val_Zr, b, n=12, r = 3, lower.tail = TRUE)
})

# plot two power function
plot(power_Zr,type = "l", col = "red")
par(new = TRUE)
plot(mat_power_Dr[, 1])


#plot cdf for of Zr
z_vals <- seq(0, 1/5, 0.001)
p_r <- sapply(z_vals, function(z) {
  pZr(z = z, b = 0.3, n = 10, r = 5)
})
p_0 <- sapply(z_vals, function(z) {
  pZr(z = z, b = 1, n = 10, r = 5)
})
plot(z_vals, p_r, type = "l", col = "red", ann = FALSE, axes = FALSE)
par(new = TRUE)
plot(z_vals, p_0,
     type = "l", col = "green",
     xlab = "x", ylab = "F(x)"
)
# legend(0, 0.4,
#        legend = c("H_r", "H_0"), lty = c(1, 1),
#        col = c("red", "green"), cex = 0.2
# )

pZr(1/3, b =0.01, n=12, r = 3)



# the following are the code I restore from the history file
seq_z <- seq(0,2,length.out = 1e4)
samples_zr <- apply(exponential_slippage_sampler(n=12,r =3, b = 1/2, B = 1e4),2,function(x){Zr(x,r = 3)})

p_zr_wrong <- sapply(seq_z, function(z) {
pZr_wrong(z = z, b = 1/2, n = 12, r = 3)
})
p_zr <- sapply(seq_z, function(z) {
pZr(z = z, b = 1/2, n = 12, r = 3)
})


df_exp_samples_t <- data.frame(sample_zr = samples_zr, p_Zr = p_zr, seq_z = seq_z, pzr_wrong = p_zr_wrong)
#plot the cdf and edf of the sample generated
ggplot(df_exp_samples, aes(seq_z)) +
geom_smooth(aes( y = p_zr, color = "Theoretical")) +
geom_smooth(aes( y = p_zr_wrong, color = "Claimed Theoretical")) +
stat_ecdf(mapping = aes(samples_zr,color = "Empirical"), pad = FALSE) +
labs(title = "cdf of Z_r with n = 12, r = 3, b = 1/2") +
ylab("P(Z_r < z)") +
xlab("z") +
scale_color_discrete("")


set.seed(1234)
crit_val_Dr <- qDr(n = 12, r = 3, q = 0.95, se.return = FALSE)
seq_b <- seq(0.01, 1, 0.001)
mat_power_Dr <- matrix(NA, ncol = 2, nrow = length(seq_b))
B <- 1e3
for (i in seq_along(seq_b)) {
b <- seq_b[i]
exponential_slippage_sampler(n = 12, r = 3, b = b, B = B) %>%
apply(., 2, function(x) Dr(x, r = 3)) %>%
{
. > crit_val_Dr
} %>%
{
c(p.val = mean(.), se = sd(.))
} ->
power_Dr
mat_power_Dr[i, ] <- power_Dr
}

critical_Zr <- qZr(n=12,r = 3,q = 0.05)
critical_Zr_wrong <- 0.2747054

power_Zr <- sapply(seq_b, function(b){pZr(critical_Zr, b =b, r =3, n=12)})

power_Zr_wrong <- sapply(seq_b, function(b){pZr_wrong(critical_Zr, b =b, r =3, n=12,lower.tail = FALSE)})

df_power <- data.frame(power_zr = power_Zr, power_dr = mat_power_Dr[, 1], wrong_power = power_Zr_wrong,b_vals = seq_b)



ggplot(df_power, aes(seq_b)) +
geom_jitter(aes(y = power_dr, color = "power of Dr")) +
geom_line(aes(y = power_zr, color = "power of Zr")) +
geom_line(aes(y = wrong_power, color = "wrong power estimated"))+
labs(title = "power comparision between test with n = 12, r = 3") +
ylab("power") +
xlab("b") +
labs(fill="")


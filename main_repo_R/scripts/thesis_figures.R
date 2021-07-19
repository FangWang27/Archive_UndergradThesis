# this is the R scipts that will be used to produce the figrues and plots
# that will be used for the final report


setwd("C:/Users/wangf/OneDrive - Seneca College of Applied Arts & Technology/Current/MATH4P06/main_repo_R/Functions")
sapply(list.files(), source)
require("dplyr")
require("ggplot2")
require(tidyr)
# Introduction======================================================


n <- 15
r <- 5
B <- 10

# Data simulation under H_1, exp, with b < 1
b_1 <- 1 / 4

# simulate the data and store them in a data.frame to be plot

df_exp_alt <- as.data.frame(exponential_slippage_sampler(n, r = r, b = b_1, B = B))
View(df_exp_alt)
df_plot <- gather(df_exp_alt, trials) %>%
    mutate(order = (row_number() - 1) %% n + 1) %>% # order statistics of the obs in the data
    mutate(contamination = order %in% (n - r):n) # is this a contamination

# illustrative plot for slippage alternative

# make the plot for the illustrative example
ggplot(df_plot, aes(x = order, y = value, color = contamination)) +
    geom_point() +
    facet_grid(rows = vars(trials))





# Simulation Study=================================================
B <- 1000
r <- 3
n <- 12
# Data simulation under H_0=======================================
# ================ Exponential Distribution========================

# samples generation-------------------------------

sample_exp_0 <- exponential_slippage_sampler(n, r = r, b = 1, B = B)

# distribution of each order statistics
View(sample_exp_0)

df_os_exp0 <- sample_exp_0 %>%
    as.data.frame() %>% # change to data.frame
    gather(trials) %>% # put it in the long df
    mutate(order = (row_number() - 1) %% n + 1)%>% # order statistics of the obs in the data
  # spread(key = order,value) %>%
    select(-trials)
View(df_os_exp0)
# plot the simulation sample by order statistics

ggplot(df_os_exp0,aes(x = as.factor(order), y = value)) +
geom_boxplot()



#--------------- Zr statistics----------------------

# distribution of test stats
zr_0 <- apply(sample_exp_0, 2, function(x) Zr(x, r = r))
# distribution of empirical p.values
zr_0_pval <- sapply(zr_0, function(z) pZr(z, b = 1, n = n, r = r))

# estimated type 1 error rate
alpha_hat <- mean(zr_0_pval < .05)



#------Dixon's statistics -----------------

# we use the ecdf of Dixon's statistics
# distribution of the test statistics Dr

dr_0 <- apply(sample_exp_0, 2, function(x) Dr(x, r = r))
hist(dr_0)


# ----------==== Paretod Distribution--------------------

sample_pareto_0 <- pareto_slippage_sampler(n, r = r, alpha = 1, beta = 1, B = B)

# distribution of each order statistics

df_os_pareto0 <- sample_pareto_0 %>%
  as.data.frame() %>% # change to data.frame
  gather(trials) %>% # put it in the long df
  mutate(order = (row_number() - 1) %% n + 1)%>% # order statistics of the obs in the data
  # spread(key = order,value) %>%
  select(-trials)
# plot the simulation sample by order statistics

ggplot(df_os_pareto0,aes(x = as.factor(order), y = value)) +
  geom_boxplot()

# distribution of the test stats

# Rk

rk_0 <- apply(sample_pareto_0, 2, function(x) Rk(x, k = r))
hist(rk_0)

# disribution of P-value of R_k

rk_0_pval <- sapply(rk_0, function(z) pRk(z, n = n, beta = 1, k = r))

hist(rk_0_pval)

# type1 error rate
alpha_hat <- mean(rk_0_pval < .05)


#=========== Performance Under Ha============================

#--------Exponential--------------------------------------

b_1 <- 1/3
alpha <- .05

sample_exp_1 <- exponential_slippage_sampler(n, r = r, b = b_1, B = B)

# distribution of each order statistics
View(sample_exp_0)

df_os_exp1 <- sample_exp_1 %>%
  as.data.frame() %>% # change to data.frame
  gather(trials) %>% # put it in the long df
  mutate(order = (row_number() - 1) %% n + 1)%>% # order statistics of the obs in the data
  # spread(key = order,value) %>%
  select(-trials)
# plot the simulation sample by order statistics

ggplot(df_os_exp1,aes(x = as.factor(order), y = value)) +
  geom_boxplot()

# exam cdf of Zr--------------------------


# obtain the ecdf

sample_Zr <- apply(sample_exp_1,2, function(z){Zr(z,r = r)})
seq_z  <- seq(0,1/r,1e-3)

df_cdfs <- data.frame(
  seq_z = seq_z,
  theoretical =  sapply(seq_z, function(z){pZr(z,b=b_1,n=n,r=r)}),
  empirical = sapply(seq_z, ecdf(sample_Zr))
)

# give CI and se for ecdf, ready for the plot

df_cdfs_pt <- df_cdfs %>% 
  mutate( se = sqrt(empirical *(1- empirical)/B)) %>%
  mutate(CI_l = empirical + qnorm(1-alpha/2) *se , CI_h = empirical - qnorm(1-alpha/2) *se )


#plot the ecdfs and cdfs

ggplot(df_cdfs_pt, aes(x = seq_z)) +
  geom_line(aes( y = theoretical, color = "Theoretical")) +
  geom_line( aes(y = empirical, color = "Empirical")) +
  geom_ribbon(aes(ymin = CI_l, ymax = CI_h), alpha = 0.3) +
  labs(x = "r", y = "P(Z_r < z)", title = "cdf of Z_r")



# distribution of p value

zr_1_pval <- sapply(sample_Zr, function(z) pZr(z, b = 1, n = n, r = r))

hist(zr_1_pval)


# ------Pareto samples--------------------

sample_pareto_1 <- pareto_slippage_sampler(n, r = r, beta = b_1, B = B)



df_os_pareto_1 <- sample_pareto_1 %>%
  as.data.frame() %>% # change to data.frame
  gather(trials) %>% # put it in the long df
  mutate(order = (row_number() - 1) %% n + 1)%>% # order statistics of the obs in the data
  # spread(key = order,value) %>%
  select(-trials)
# plot the simulation sample by order statistics

ggplot(df_os_pareto_1,aes(x = as.factor(order), y = value)) +
  geom_boxplot()

# exam cdf of Zr--------------------------


# obtain the ecdf

sample_Rk <- apply(pareto_1,2, function(z){Rk(z,k = r)})
seq_z  <- seq(0,5,1e-3)

df_cdfs <- data.frame(
  seq_z = seq_z,
  theoretical =  sapply(seq_z, function(z){pRk(z,n=n,k=r, beta = b_1)}),
  empirical = sapply(seq_z, ecdf(sample_Rk))
)

# give CI and se for ecdf, ready for the plot

df_cdfs_pt <- df_cdfs %>% 
  mutate( se = sqrt(empirical *(1- empirical)/B)) %>%
  mutate(CI_l = empirical + qnorm(1-alpha/2) *se , CI_h = empirical - qnorm(1-alpha/2) *se )


#plot the ecdfs and cdfs

ggplot(df_cdfs_pt, aes(x = seq_z)) +
  geom_line(aes( y = theoretical, color = "Theoretical")) +
  geom_line( aes(y = empirical, color = "Empirical")) +
  geom_ribbon(aes(ymin = CI_l, ymax = CI_h), alpha = 0.3) +
  labs(x = "r", y = "P(Z_r < z)", title = "cdf of Z_r")



# distribution of p value

rk_1_pval <- sapply(sample_Rk, function(z) pRk(z,n=n,k=r, beta = b_1))

hist(zr_1_pval)



############## Illustrative power comparision

# Exponential case

# power for Zr
crit_val_Zr <- qZr(n=n,r=r, q = 0.05)

power_Zr <- sapply(seq_z, function(b) {
  pZr(crit_val_Zr, b, n=12, r = 3, lower.tail = TRUE)
})

# Power of Dr-----------------------------------
seq_z  <- seq(0,1/r,1e-2)

crit_val_Dr <- qDr(n = 12, r = 3, se.return = FALSE)
mat_power_Dr <- matrix(NA, ncol = 2, nrow = length(seq_z))
B <- 1e3
for (i in seq_along(seq_z)) {
  b <- seq_z[i]
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

# Power Of Zr
plot(power_Zr)


# Pareto Case----------------------------


set.seed(1234)

seq_beta <- seq(1e-4, 1, 1e-2)
crit_val_Rk <- qRk(q = 0.05, n = n, k = r)
power_Rk <- sapply(seq_beta, function(beta){pRk(crit_val_Rk, n= n, k =r, beta)})
plot(power_Rk)


mat_power_Dr <- matrix(NA, ncol = 2, nrow = length(seq_beta))
B <- 1e4
for (i in seq_along(seq_beta)) {
  beta <- seq_beta[i]
  pareto_slippage_sampler(n = n, r = r, beta = beta, B = B) %>%
    apply(., 2, function(x) Dr(log(x), r = 3)) %>%
    {
      . > crit_val_Dr
    } %>% mean %>%
    power_Dr
  mat_power_Dr[i, ] <- power_Dr
}

#load the critical values
# load("~/Documents/MATH4P06/main_repo_R/data/pareto_power.RData")
# note that the se in the saved file is not usable

df_pareto_power_plt <- df_power_pareto %>% 
  mutate(se = sqrt(Dr * (1-Dr)/B)) %>%
  mutate(CI_l = Dr + qnorm(1-alpha/2) *se , CI_h = Dr - qnorm(1-alpha/2) *se )

# plot the power comparision

ggplot(df_pareto_power_plt, aes(x = betas)) +
  geom_line(aes( y = R_k, color = "Rk")) +
  geom_line( aes(y = Dr, color = "Dixon's stats")) +
  geom_ribbon(aes(ymin = CI_l, ymax = CI_h), alpha = 0.3) +
  labs(x = "beta", y = "power", title = "power of the test with n = 12, k = 3", color = "test type")


# real application of the data
setwd("C:/Users/wangf/OneDrive - Seneca College of Applied Arts & Technology/Current/MATH4P06/main_repo_R/Functions")
sapply(list.files(), source)
require("dplyr")
require("ggplot2")

df_stock <- read.csv("C:/Users/wangf/OneDrive - Seneca College of Applied Arts & Technology/Current/MATH4P06/main_repo_R/data/prices.csv")

names(df_stock)
table(df_stock$Period.Ending)

# data cleaning
df_price <- df_stock %>% 
    select(open, date) %>%
    filter(grepl('2010|2016', date)) %>%
    transmute(price = open, year = 2010 * (grepl("2010", date)) + 2016 * (grepl("2016", date))) %>%
    mutate(weight = sapply(year, function(y){if(y == 2010){10}else{2}}))


View(df_price)
hist(df_price$price)

df_summary <- df_price %>% group_by(year) %>% summarise(mean = mean(price), xm = min(price))

df_price$adj_price <- apply(df_price,1, function(x){
    if(x[2] == 2016) {
        adj_price <- x[1] /1.66
    }  else {
        adj_price <- x[1] / 0.85
    }
    return(adj_price)
})

hist(log(df_price$adj_price))

pareto_samples <- replicate(50, sample(df_price$adj_price, 12, prob = df_price$weight))

View(pareto_samples)
mode(pareto_samples)

x_m <- min(pareto_samples)

# n =12, r = 3

# crtical val for Dr: 0.17010

# critical val for Zr:0.099

# critical val for Rr : 0.17010

df_test <- apply(pareto_samples,2, function(x){
      d_r <- 0.7392545
      z_r <- 0.16729
      r_r <- 0.2516
      r <- 2
      es <- log(x)
      t_d <- Dr(es, r = r) > d_r
      t_z <- Zr(es, r = r) < z_r
      t_r <- Rk(x, k = r) < r_r
      return(
          data.frame(
               Dr = t_d, Zr = t_z, Rr = t_r,
               Dr_test = Dr(es, r = r),
               Zr_test = Zr(es, r = r),
               Rr_test = Rk(x, k = r)
               ))
})

df_result <- (do.call(rbind, df_test))

hist(log(df_result$Rr_test))

hist(df_result$Dr_test)

View(df_result)
table(df_result[,3])


View(pareto_samples)


###################################################

df_cancer <- read.csv("C:/Users/wangf/OneDrive - Seneca College of Applied Arts & Technology/Current/MATH4P06/main_repo_R/data/haberman.csv")

names(df_cancer) <- c("age", "Op_Year","axil_nodes","Surv_status")


df_cancer %>% group_by(Surv_status) %>% summarize(mean = mean(axil_nodes), length = length(axil_nodes))

names(df_cancer)

dim(df_cancer)
 df_cancer_plot <- df_cancer %>%
 select(Surv_status,axil_nodes) %>%
 transmute(axil_nodes,Surv_status = case_when(Surv_status == 2 ~ "Died", Surv_status == 1 ~ "Survived "))


ggplot( data = df_cancer_plot, aes(x = axil_nodes, fill = Surv_status)) +
geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
    # scale_fill_manual(values=c("#69b3a2", "#404080")) +
    labs(fill="Survival status",
     x = "Number of positive axillary nodes detected", 
     y = "Number of patients",
    title = "Histogram of positive axillary nodes")



set.seed(1234)
exp_samples <- replicate(1000, sample(df_cancer$axil_nodes, 12))

hist(exp_samples)

df_test_exp <- apply(exp_samples,2, function(es){
      d_r <- 0.80375
      z_r <- 0.09909
      r_r <- 0.17010
      r <- 3
      t_d <- Dr(es, r = r) > d_r
      t_z <- Zr(es, r = r) < z_r
      t_r <- Rk(exp(es), k = r) < r_r
      return(
          data.frame(
               Dr = t_d, Zr = t_z, Rr = t_r,
               Dr_test = Dr(es, r = r),
               Zr_test = Zr(es, r = r),
               Rr_test = Rk(exp(es), k = r)
               ))
})

df_result_exp <- do.call(rbind, df_test_exp)

table(df_result_exp$Zr)
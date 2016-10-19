
rm(list = ls())
gc()


require(dplyr)
require(lubridate)
require(parallel)
require(tidyr)
require(quantreg)
require(data.table)
require(bit64)
require(rqc)
require(ggplot2)
require(gridExtra)

infile <-  "/mnt/data/r1/input/20160928_r1.csv"
varfile <- "/mnt/data/r1/variable.name"
outfile <- "/mnt/data/r1/20160928_r1_ftc_out.rds"
#save_results <- TRUE
nsample <- 10^6
last_date <- ymd("2016-09-28")
have_insurance <- 0
nrow <-  -1
tau <- c(q_002.5 = 2.5, q_0.25 = 25, q_0.5 = 50, q_0.75 = 75, q_0.975 = 97.5)/100
max_purchases_count <- 23
factors <- c("individual_brand_preferred_site_classification", "mail_available", "insurance", "left_cens")
fill_na_var <-  c(
  # "female_worker_age"
  # ,"male_worker_age"
  # ,"marriedcouple_family_own_children_under_18_years"
  # ,"marriedcouple_family_no_own_children_under_18_years"
  # ,"median_household_income_in_the_past_12_months_in_2009_inflationadjusted_dollars"
  # ,"per_capita_income_in_the_past_12_months_in_2009_inflationadjusted_dollars"
  # ,"train_purchases_count"
  # ,"train_purchase_max"
  # ,"train_purchase_min"
  "avg_no_vehicle"
  # ,"avg_male_age"
  # ,"avg_female_age"
  # ,"avg_male_education"
  #,"avg_female_education"
  )





######################################################
############     scripts sourcing        #############
######################################################

source("~/functions.R")
source("~/clean_data.R")

dim(df)

df <- df %>%
  filter(complete.cases(individual_brand_preferred_site_classification,
                          female_worker_age,
                          male_worker_age,
                          mail_available,
                          marriedcouple_family_own_children_under_18_years,
                          marriedcouple_family_no_own_children_under_18_years,
                          median_household_income_in_the_past_12_months_in_2009_inflationadjusted_dollars,
                          per_capita_income_in_the_past_12_months_in_2009_inflationadjusted_dollars,
                          train_purchases_count,
                          train_purchase_max ,
                          train_purchase_min ,
                          avg_no_vehicle ,
                          avg_male_age ,
                          avg_female_age ,
                          avg_male_education ,
                          avg_female_education ,
                          recency ,
                          left_cens
  ))


ff <- train_total_gross_value ~
  individual_brand_preferred_site_classification +
  female_worker_age +
  male_worker_age +
  mail_available +
  marriedcouple_family_own_children_under_18_years +
  marriedcouple_family_no_own_children_under_18_years +
  median_household_income_in_the_past_12_months_in_2009_inflationadjusted_dollars +
  per_capita_income_in_the_past_12_months_in_2009_inflationadjusted_dollars+
  train_purchases_count+
  train_purchase_max +
  train_purchase_min +
  avg_no_vehicle +
  avg_male_age +
  avg_female_age +
  avg_male_education +
  avg_female_education +
  recency +
  left_cens



model_list <- make_best_model_list (
  formula = ff,
  formula_base = train_total_gross_value ~1,
  formula_null = train_total_gross_value ~1,
  data = df,
  tau = .5, method = "fn",
  mc.cores = 1)

test <- summary_all_rqc (rqc = model_list, best_only = TRUE, digits = 7)

p_null <- ggplot(test ) +
  geom_line(aes(n_model_var, ratio_rho_null) , col = "darkblue") +
  geom_point(aes(n_model_var, ratio_rho_null), col = "darkgray", size  = 3)  +
  xlab("Number of variables in model") + ylab("Ratio Rho of NULL model")


p_base <- ggplot(test ) +
  geom_line(aes(n_model_var, ratio_rho_base) , col = "darkgreen") +
  geom_point(aes(n_model_var, ratio_rho_base), col = "darkorange", size  = 3) +
  xlab("Number of variables in model") + ylab("Ratio Rho of base model")

grid.arrange(p_null, p_base, ncol = 2)

## check coefficients
summary(model_list[[5]]$fm_best)

# prova del nove
bf <- model_list[[18]]$fm_best$formula
bfm <- rq(bf, data = df, tau = .5, method = 'fn')
summary(bfm)


## check coefficients
summary(model_list[[10]]$fm_best)

# prova del nove
bf <- model_list[[18]]$fm_best$formula
bfm <- rq(bf, data = df, tau = .5, method = 'fn')
summary(bfm)


### Mod by Mau
## not the same results
ff <- train_total_gross_value ~
individual_brand_preferred_site_classification+
marriedcouple_family_own_children_under_18_years+
marriedcouple_family_no_own_children_under_18_years+
median_household_income_in_the_past_12_months_in_2009_inflationadjusted_dollars+
per_capita_income_in_the_past_12_months_in_2009_inflationadjusted_dollars+
male_worker_age+
female_worker_age+
train_purchases_count+
train_purchase_max+
train_purchase_min+
avg_no_vehicle+
avg_male_age+
avg_female_age+
avg_male_education+
avg_female_education+
recency

fm_mau <- rq(ff, data = df, tau = .5, method = 'fn')
summary(fm_mau)

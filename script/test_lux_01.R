require(rqc)
require(quantreg)

rm(list = ls())
df<- readRDS("df.RData")

formula <- train_total_gross_value ~
  individual_brand_preferred_site_classification +
  female_worker_age +
  male_worker_age +
  mail_available +
  marriedcouple_family_own_children_under_18_years +
  marriedcouple_family_no_own_children_under_18_years +
  median_household_income_in_the_past_12_months_in_2009_inflationadjusted_dollars +
  per_capita_income_in_the_past_12_months_in_2009_inflationadjusted_dollars +
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
  formula = formula,
  formula_base = train_total_gross_value ~1,
  formula_null = train_total_gross_value ~1,
  data = df,
  tau = .5, method = "fn",
  n_var = 10,
  mc.cores = 4)


# length(model_list[[10]]$fm_all)
#
#
test <- model_list[[6]]
length(test$fm_all)
#
# length(test$fm_base)
#
# debugonce(summary_single_rqc)
test <- summary_single_rqc (i = 6, rqc = model_list)
class(test)
names(test)

class(test$formula_list)

summary_all_rqc (rqc = model_list, best_only = TRUE, digits = 7)


test <- summary_all_rqc (rqc = model_list, best_only = F, digits = 8)
write.table(test, "test.csv", sep = ",", row.names = F, col.names = T)


for ( i in 1:14){
  formula <- fl[i]
  fm <- rq(formula, data = df, tau = .5, method = 'fn')
}

fl

f1 <- train_total_gross_value ~ train_purchase_max + train_purchases_count + marriedcouple_family_own_children_under_18_years
f2 <- train_total_gross_value ~ train_purchase_max + train_purchases_count + avg_male_education

fm1 <- rq(f1, data = df, tau = .5, method = 'fn')
fm2 <- rq(f2, data = df, tau = .5, method = 'fn')

fm1$rho
fm2$rho

x1 <- df$marriedcouple_family_own_children_under_18_years
x2 <- df$avg_male_education

plot(x1, x2)










## ------------------------------------------------------------------------
require(quantreg)
require(rqc)
require(ggplot2)
require(gridExtra)

## ------------------------------------------------------------------------
fm_b <- rq(stack.loss~1, data = stackloss, tau = .5, method = "fn")
mod_lst <- make_model_list (formula =  stack.loss~Air.Flow+Water.Temp+ Acid.Conc.,
                              fm_base = fm_b,
                              data = stackloss, tau = .5, method = "fn", mc.cores = 1)

                 

## ------------------------------------------------------------------------
make_formula_list(mod_lst)

## ------------------------------------------------------------------------
fm_best <- best_model (model_list = mod_lst, fm_base = fm_b)
fm_best

## ------------------------------------------------------------------------
frm(fm_best)

## ------------------------------------------------------------------------
stackloss_model_list <- make_best_model_list (
  formula = stack.loss~Air.Flow+Water.Temp+ Acid.Conc.,
  formula_base = stack.loss~Air.Flow, 
  formula_null = stack.loss~1,
  data = stackloss, 
  tau = .5, method = "fn",
  mc.cores = 1)

## ------------------------------------------------------------------------
summary_all_rqc(stackloss_model_list)

## ------------------------------------------------------------------------
state_x77 <-  as.data.frame(state.x77)  
names(state_x77) <- c("population","income","illiteracy","life_exp","murder","hs_grad","frost","area" )

## ------------------------------------------------------------------------
state_x77_model_list <- make_best_model_list (
  formula = life_exp~population+income+illiteracy+murder+hs_grad+frost+area,
  formula_base = life_exp~murder+illiteracy, 
  formula_null = life_exp~1, 
  data = state_x77, 
  n_var = 4,
  tau = .5, method = "fn",
  mc.cores = 1)

## ------------------------------------------------------------------------
summary_single_rqc (i = 2, rqc = state_x77_model_list)

## ------------------------------------------------------------------------
summary_all_rqc (rqc = state_x77_model_list, exclude_var = c("formula_best", "formula_null"))
summary_all_rqc (rqc = state_x77_model_list, exclude_var = c("formula_best", "best_model", "formula_null"), best_only = F, digits = 4)
summary_all_rqc (rqc = state_x77_model_list, exclude_var = c("formula_best", "best_model", "formula_null"), best_only = T, digits = 4)

## ------------------------------------------------------------------------
state_x77_model_list <- make_best_model_list (
  formula = life_exp~population+income+illiteracy+murder+hs_grad+frost+area,
  formula_base = life_exp~1, 
  formula_null = life_exp~1, 
  data = state_x77, 
  tau = .5, method = "fn",
  mc.cores = 1)

## ------------------------------------------------------------------------
summary <- summary_all_rqc (rqc = state_x77_model_list, best_only = TRUE, digits = 7)


p_null <- ggplot(summary ) + 
  geom_line(aes(n_model_var, ratio_rho_null) , col = "darkblue") + 
  geom_point(aes(n_model_var, ratio_rho_null), col = "darkgray", size  = 3)  +
  xlab("Number of variables in model") + ylab("Ratio Rho of NULL model") 
  

p_base <- ggplot(summary ) + 
  geom_line(aes(n_model_var, ratio_rho_base) , col = "darkgreen") + 
  geom_point(aes(n_model_var, ratio_rho_base), col = "darkorange", size  = 3) +
  xlab("Number of variables in model") + ylab("Ratio Rho of base model") 

grid.arrange(p_null, p_base, ncol = 2)


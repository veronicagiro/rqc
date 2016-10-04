
# library(quantreg)
#
# fm0 <- rq(train_total_gross_value ~ 1, data = df, tau=tau[3], method='fn')
#
# fm1 <- rq(train_total_gross_value ~ train_purchases_count, data = df, tau=tau[3], method='fn')
# fm2 <- rq(train_total_gross_value ~ train_purchase_max, data = df, tau=tau[3], method='fn')
# fm3 <- rq(train_total_gross_value ~ train_purchase_min , data = df, tau=tau[3], method='fn')
#
# rho <- function(u,tau=tau[3])u*(tau - (u < 0))
#
# R10 <- 1- fm1$rho/fm0$rho
# R20 <- 1 - fm2$rho/fm0$rho
# R30 <- 1 - fm3$rho/fm0$rho

# Ciascun test valuta in questo caso il merito di una variabile rispetto al modello nullo (fm0)
# usando la mediana come quantile.

#I risultati vengono poi rankizzati. E prendendo il modello con la variabile più signigicativa come
#modello di base si aggiunge una variabile alla volta e si vede qual'è
#il modello a due var più sginificativo e così via.
########################
#rm(list = ls())
#require(quantreg)
#stackloss$noise <- rnorm(nrow(stackloss), 4, 1)

#############################################
#' @title Compare two quantile regression (rq)  models
#' @description
#' compute model according to  formula and model_base according to formula_base.
#' Return a between models comparison statistics.
#' @param formula Formula for the full model
#' @param formula_base Formula for teh base model
#' @param data Model data frame
#' @param Model quantile for quantile regression.
#' @param method Model method for quantile regression
#' @return double, model comparison indicator
#' @importFrom  quantreg rq
#' @examples
#' rqc2(
#'  formula = stack.loss~Air.Flow+Water.Temp+ Acid.Conc.+noise,
#'  formula_base = stack.loss ~1,
#'  data = stackloss,
#'  tau = .5, method = 'fn')
#' @export
#rqc1
rqc2 <- function(formula, formula_base, data , tau, method) {
  fm <- rq(formula, data = data , tau = tau, method = method)
  fm_base <- rq(formula_base, data = data , tau = tau, method = method)
  1-(fm$rho/fm_base$rho)
}
###########################
#' @title Compare all quantile regression (rq) with i variables
#' @description
#' compute all models starting from formula base and adding all variables one by one
#' Return a between models comparison statistics.
#' @param formula Formula for the full model
#' @param formula_base Formula for teh base model
#' @param data Model data frame
#' @param tau a quantile for quantile regression
#' @param method Model method for quantile regression
#' @return formula object corrensponding to the best model
#' @importFrom  quantreg rq
#' @examples
#'  rqci( i = 2, formula = stack.loss~Air.Flow+Water.Temp+ Acid.Conc.,
#'      formula_base = stack.loss ~1,
#'      data = stackloss,
#'      tau = .5, method = 'fn')
#' @export
rqci <- function(i , formula, formula_base , data ,  tau , method) {

  # response variable
  response <- all.vars(formula)[1]

  # all x in formula
  Xs <- all.vars(formula)[-1]

  # all x in formula base
  Xsb <- all.vars(formula_base)[-1]

  # all x in formula not in formula base
  Xs <- Xs[!(Xs %in% Xsb)]

  # all combination of i as a matrix
  Xs_comb_matrix <- combn(Xs, i)

  # all combination of i as a list
  # must be a simpler way :-)
  Xs_comb_list <- lapply(seq_len(ncol(Xs_comb_matrix)), function(i, x) x[,i] , Xs_comb_matrix)

  # right hand side of the formulas
  rs_formula_list <- unlist(lapply(Xs_comb_list, paste , collapse = "+"))

  # list of formulas
  formula_list <- unlist(lapply(response, paste, rs_formula_list, sep = "~"))

  # models and scores for each formula
  score <- unlist(lapply(formula_list,
                         rqc2,
                         formula_base = formula_base, data = data ,tau = tau, method = method))

  #best formula
  formula_best <- formula_list[score == max(score)]

  # return
  return(formula_best)

}

##############################
#' @title Compare all quantile regression (rq) with i=1:n variables
#' @description
#' Starting from formula base  with i terms  adds variables one by one, computes models
#' and returns the formulae for all best models with number of variables ranging
#' between i and n with n being the number of variables in  formula
#' @param formula Formula for the full model
#' @param formula_base Formula for the base model
#' @param data Model data frame
#' @param tau a quantile for quantile regression
#' @param method Model method for quantile regression
#' @return formula object corrensponding to the best model
#' @importFrom  quantreg rq
#' @examples
#'  rqcn( formula = stack.loss~Air.Flow+Water.Temp+ Acid.Conc.,
#'      formula_base = stack.loss ~1,
#'      data = stackloss,
#'      tau = .5, method = 'fn')
#' @export
rqcn <- function(formula, formula_base, data, tau , method){

  Xs <- all.vars(formula)[-1]
  n_vars <- length(Xs)
  out <- list()
  for ( i in seq_len(n_vars)){
    formula_best <- rqci( i = i,
                    formula = formula, formula_base = formula_base, data = data, tau = tau, method = method)
    out[i] <- formula_best
    formula_base <- formula_best
  }

out
}
#######################################

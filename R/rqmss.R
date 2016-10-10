
#############################################
#' @title Compare two quantile regression (rq)  models
#' @description
#' compute model according to  formula and model_base according to formula_base.
#' Return a between models comparison statistics.
#' @param formula Formula for the full model
#' @param formula_base Formula for teh base model
#' @param data Model data frame
#' @param tau quantile for quantile regression.
#' @param method Model method for quantile regression
#' @return double: model performance indicator
#' @importFrom  quantreg rq
#' @examples
#' rqc2(
#'  formula = stack.loss~Air.Flow+Water.Temp+ Acid.Conc.,
#'  formula_base = stack.loss ~1,
#'  data = stackloss,
#'  tau = .5, method = 'fn')
#' #@export
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
#' @return data frame of five colums:
#' * i Integer:  number of variables tested
#' * formula character: tested formula
#' * formula_base character: base formula
#' * score double: model perfornace indicator
#' * formula_best integer: 0/1
#' @importFrom  quantreg rq
#' @examples
#' rqci(
#'  formula = stack.loss~Air.Flow+Water.Temp+ Acid.Conc.,
#'  formula_base = stack.loss ~1,
#'  data = stackloss,
#'  tau = .5, method = 'fn')
#' ## i = 2
#' rqci(
#'  formula = stack.loss~Air.Flow+Water.Temp+ Acid.Conc.,
#'  formula_base = stack.loss ~Air.Flow,
#'  data = stackloss,
#'  tau = .5, method = 'fn')
#' ## i = 3
#' rqci(
#'  formula = stack.loss~Air.Flow+Water.Temp+ Acid.Conc.,
#'  formula_base = stack.loss~Water.Temp,
#'  data = stackloss,
#'  tau = .5, method = 'fn')
#'  #@export
rqci <- function(formula, formula_base , data ,  tau , method) {

  # response variable
  response <- all.vars(formula)[1]

  # all x in formula
  Xs <- all.vars(formula)[-1]

  # all x in formula base
  Xsb <- all.vars(formula_base)[-1]
  n_Xsb <- length(Xsb)

  # all x in formula not in formula base
  Xs <- Xs[!(Xs %in% Xsb)]

  # paste all base vars together with a #
  Xsb <- paste(c( Xsb, "1"), collapse = "+")

  rs_formula_list <- apply(expand.grid(Xsb, Xs), 1, paste, collapse = "+")

  # list of formulas
  formula_list <- unlist(lapply(response, paste, rs_formula_list, sep = "~"))

  # models and scores for each formula
  score <- unlist(lapply(formula_list,
                         rqc2,
                         formula_base = formula_base, data = data ,tau = tau, method = method))

  #best formula
  formula_best <- formula_list[score == max(score)]

  out <- data.frame(
                    formula = as.character(formula_list),
                    formula_base = Reduce(paste, deparse(formula_base)),
                    score = score,
                    n_var = n_Xsb+1,
                    formula_best = ifelse(score == max(score), 1, 0),
                    stringsAsFactors = FALSE

                    )
  # return
  return(out)
}
#############################
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
#' @importFrom dplyr sample_n
#' @examples
#' # Example 1
#' rqcn( formula = stack.loss~Air.Flow+Water.Temp+ Acid.Conc.,
#' formula_base = stack.loss ~1,
#' data = stackloss,
#' tau = .5, method = 'fn')
#' # Example 2
#' rqcn( formula = stack.loss~Air.Flow+Water.Temp+ Acid.Conc.,
#' formula_base = stack.loss ~1,
#' n_vars = 2,
#' data = stackloss,
#' tau = .5, method = 'fn')
#' @export
rqcn <- function(formula, formula_base, data, tau , method, n_vars = NULL, n_sample = -1){

  Xs <- all.vars(formula)[-1]

  # take into account intercept
  Xsb <- all.vars(formula_base)[-1]
  if (length(Xsb == 0)) {
    Xsb <- 1
  }

  # start from
  n_start <- length(Xsb)

  # if n_vars is not given
  if (is.null(n_vars)){
    n_vars <- length(Xs)
  } else {

  # check n_vars consistency
    if (n_vars > length(Xs)){
      stop("n_vars cannot be larger than number of variables in teh rhs of formula")
    }
  }

  # check n_start vs n_vars consistency
  if (n_start > n_vars ){
    stop("n_start cannot be larger n_vars")
  }

  if(n_sample > 0) {
    data <- sample_n(data, n_sample)
  }


  n_vars <- n_vars-n_start

  out <- list()
  for ( i in 1:n_vars){
    df_out <- rqci(
                    formula = formula, formula_base = formula_base, data = data, tau = tau, method = method)

    out[[i]] <- df_out
    formula_base <- as.formula(df_out$formula[df_out$formula_best == 1])
    }

  # Reduce
  out <- Reduce(rbind , out)

  return(out)
}
#######################################

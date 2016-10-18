

#################################
#' @title names as char of lhs variables in formula
#' @importFrom lazyeval f_lhs
#' @export
f_lhs_var <- function(formula){
  all.vars(f_lhs(formula))
}
#################################
#' @title names as char of rhs variables in formula
#' @importFrom lazyeval f_rhs
#' @export
f_rhs_var <- function(formula){
    all.vars(f_rhs(formula))
}
#################################
#' @title number of rhs variables in formula
#' @export
f_rhs_n <- function(formula){
  length(f_rhs_var(formula))
}
################################
#' @title extract formula from a rq object
#' @export
frm <- function(fm){
  as.formula(fm$formula)
}
################################
#' @title extract formula from a rq object as character
#' @export
frm_char <- function(fm){
 Reduce(paste, deparse(frm(fm)))
}
################################
#' @title extract rho from a rq object
#' @export
rho <- function(fm){
  fm$rho
}
#################################
#' @title compute ratio_tho given two nested models
#' @examples
#' fm <-  rq(formula = stack.loss~Air.Flow, data = stackloss)
#' fm_base <- rq(formula = stack.loss~1, data = stackloss)
#' ratio_rho(fm = fm, fm_base = fm_base)
#' @export
ratio_rho <- function(fm, fm_base) {
  1-(rho(fm)/rho(fm_base))
}
#################################
#' @title compute ratio_rho for a list of models compared to a base model
#' @examples
#' # List of models
#' model_list <- list(
#'    fm1 = rq(formula = stack.loss~Air.Flow, data = stackloss),
#'    fm2 = rq(formula = stack.loss~Water.Temp, data = stackloss),
#'    fm3 = rq(formula = stack.loss~Acid.Conc., data = stackloss))
#' # Base model
# fm_base <- rq(stack.loss~1, data = stackloss)
#'
#' #comparison
#' multi_ratio_rho(model_list, fm_base)
#' @export
multi_ratio_rho <- function(model_list, fm_base){

  ratio_rho_list <- lapply(model_list, ratio_rho, fm_base)

  return(ratio_rho_list)
}
########################################
#' @title select best model given a list of models and a base model
#' @examples
#' ratio_rho_list <-  multi_ratio_rho(model_list, fm_base)
#' best_model(ratio_rho_list, model_list)
#' @export
best_model <- function(model_list, fm_base) {

  ratio_rho_list <- unlist(multi_ratio_rho(model_list, fm_base))

  position_max_ratio_rho <- which(ratio_rho_list == max(ratio_rho_list))[1] # big problem
  #position_max_ratio_rho <- Position(max, unlist(ratio_rho_list))

  best_model <- model_list[[position_max_ratio_rho]]

  return(best_model)
}
########################################
#' @title Make a list of models staring from a formula and a formula_base
#' @importFrom quantreg rq
#' @examples
#' make_model_list (formula =  stack.loss~Air.Flow+Water.Temp+ Acid.Conc.,
#'             formula_base = stack.loss~1,
#'             data = stackloss, tau = .5, method = "fn")
#' @export
make_model_list <- function(formula, fm_base , data, mc.cores , ...){

  # get formula base
  formula_base <- frm(fm_base)

  # response variable
  response <- f_lhs_var(formula)

  # all x in formula
  Xs <- f_rhs_var(formula)

  # all x in formula base
  Xsb <- f_rhs_var(formula_base)
  #n_Xsb <- length(Xsb)

  # all x in formula not in formula base
  Xs <- Xs[!(Xs %in% Xsb)]


  # paste all base vars together with a #
  Xsb <- paste(c( Xsb, "1"), collapse = "+")


  #make formula list
  if (length(Xs) > 0 ) {
    rs_formula_list <- apply(expand.grid(Xsb, Xs), 1, paste, collapse = "+")
  } else {
    rs_formula_list <- list(paste(Xsb, collapse = "+"))
  }

  # list of formulas
  formula_list <- unlist(lapply(response, paste, rs_formula_list, sep = "~"))

  #test
  #fl <<- formula_list

  # list of models
  model_list <- mclapply(formula_list, rq, data=data , mc.cores = mc.cores, ...)

  #return
  return(model_list)

}


#########################################
#' @title extract a list formulae from a list of rq object
#' @export
make_formula_list <- function(model_list) {

  formula_list <- lapply(model_list, frm)

  #return
  return(formula_list)

}
###########################################
#' @title nakes list of models with best selection
#' @importFrom lazyeval f_rhs
#' @importFrom dplyr sample_n
#' @importFrom parallel mclapply
#' @importFrom quantreg rq
#' @export
make_best_model_list <- function(formula, formula_base, formula_null, data , n_rec = -1, n_var = NULL, mc.cores = 1, ...) {



    Xs <- f_rhs_var(formula)

    # take into account intercept
    Xsb <- f_rhs_var(formula_base)

    # start from
    n_start <- f_rhs_n(formula_base)

    # if n_var is not given
    if (is.null(n_var)){
      n_var <- length(Xs)
    } else {

    # check n_vars consistency
      if (n_var > length(Xs)){
        stop("n_vars cannot be larger than number of variables in the rhs of formula")
      }
    }

    # sample data if required
    if(n_rec > 0) {
      data <- sample_n(data, n_rec)
    }


  #null model
  fm_null <- rq(formula_null, data = data, ...)


  #base model
  fm_base <- rq(formula_base, data = data, ...)


  # list of models
  fm_all <-   make_model_list (formula =  formula,
                               fm_base = fm_base,
                               data = data, mc.cores = mc.cores, ...)

  # empty list
  out <- list()

  # initialize list index
  i <- 1

  # looping
  while(n_start < n_var){

  #  cat("n_var = ", n_var, "\n")
    fm_best <- best_model (model_list = fm_all, fm_base = fm_base)

    out[[i]] <- list(fm_all = fm_all, fm_best = fm_best, fm_base = fm_base, fm_null = fm_null)


    fm_base <- fm_best
    fm_best =  NULL

    fm_all <- make_model_list (formula =  formula,
                               fm_base = fm_base,
                               data = data,
                               mc.cores = mc.cores,
                               ...)

    #increase
    n_start <- n_start+1
    i <- i+1
  }

  # set names
  #out_names <- unlist(lapply(lapply(fm_all, frm), f_rhs_n))

  #return
  return(out)
}
#####################################################
#' @title return summary statistcs for single level rqc
#' @importFrom lazyeval f_rhs
#' @importFrom dplyr select one_of mutate filter
#' @export
summary_single_rqc <- function(i, rqc, best_only = F, exclude_var = NULL , digits = 3) {

  fm_all <- rqc[[i]]$fm_all
  fm_best <- rqc[[i]]$fm_best
  fm_base <- rqc[[i]]$fm_base
  fm_null <- rqc[[i]]$fm_null

  out <- data.frame(
    formula_list = unlist(lapply(fm_all, frm_char)) #(result[[1]]$fm_all)
    ,formula_best = frm_char(fm_best)
    ,n_model_var = unlist(lapply(lapply(fm_all, frm), f_rhs_n))
    ,formula_base = frm_char(fm_base)
    ,ratio_rho_base = round(unlist(lapply(fm_all, ratio_rho, fm_base)), digits = digits)
    ,formula_null = frm_char(fm_null)
    ,ratio_rho_null = round(unlist(lapply(fm_all, ratio_rho, fm_null)), digits = digits)
    ,stringsAsFactors = F
  )

  # add best model indicator
  out <- mutate(out, best_model = ifelse(ratio_rho_base  == max(ratio_rho_base ), 1, 0))

  if(best_only){
    out <- filter(out, best_model == 1)
  }

  if (!is.null(exclude_var)) {
    out <- select(out, -(one_of(exclude_var)))
  }



  return(out)
}
######################################################
#' @title return summary statistcs for multi level rqc
#' @export
summary_all_rqc <- function(rqc, ...){
  n <- length(rqc)
  out <- lapply(seq_len(n), summary_single_rqc, rqc, ...)
  out <- Reduce(rbind, out)
  return(out)

}













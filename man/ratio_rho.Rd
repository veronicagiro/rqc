% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rqc.R
\name{ratio_rho}
\alias{ratio_rho}
\title{compute ratio_tho given two nested models}
\usage{
ratio_rho(fm, fm_base)
}
\examples{
fm <-  rq(formula = stack.loss~Air.Flow, data = stackloss)
fm_base <- rq(formula = stack.loss~1, data = stackloss)
ratio_rho(fm = fm, fm_base = fm_base)
}


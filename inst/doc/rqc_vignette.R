## ------------------------------------------------------------------------
require(quantreg)
require(rqc)

## ------------------------------------------------------------------------
rqcn( formula = stack.loss~Air.Flow+Water.Temp+ Acid.Conc.,
formula_base = stack.loss ~1,
data = stackloss,
tau = .5, method = 'fn')

## ------------------------------------------------------------------------
rqcn( formula = stack.loss~Air.Flow+Water.Temp+ Acid.Conc.,
formula_base = stack.loss ~1,
n_vars = 2,
data = stackloss,
tau = .5, method = 'fn')

## ------------------------------------------------------------------------
rqcn( formula = stack.loss~Air.Flow+Water.Temp+ Acid.Conc.,
formula_base = stack.loss ~1,
n_vars = 1,
data = stackloss,
tau = .5, method = 'fn')

## ------------------------------------------------------------------------
rqcn( formula = stack.loss~Air.Flow+Water.Temp+ Acid.Conc.,
formula_base = stack.loss ~Air.Flow,
n_vars = 2,
data = stackloss,
tau = .5, method = 'fn')


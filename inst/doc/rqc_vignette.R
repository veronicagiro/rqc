## ------------------------------------------------------------------------
require(quantreg)
require(rqc)

## ------------------------------------------------------------------------
rqc2(
formula = stack.loss~Air.Flow+Water.Temp+ Acid.Conc.,
formula_base = stack.loss ~1,
data = stackloss,
tau = .5, method = 'fn')

## ------------------------------------------------------------------------
rqci( i = 2, formula = stack.loss~Air.Flow+Water.Temp+ Acid.Conc.,
  formula_base = stack.loss ~1,
 data = stackloss,
 tau = .5, method = 'fn')

## ------------------------------------------------------------------------
rqcn( formula = stack.loss~Air.Flow+Acid.Conc.+Water.Temp,
 formula_base = stack.loss ~1,
 data = stackloss,
 tau = .5, method = 'fn')


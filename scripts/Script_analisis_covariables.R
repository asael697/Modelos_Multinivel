library(brms)
library(bayesplot)

load("~/Documents/Modelos_Multinivel/Datos/Datos2021.RData")
# Modelo en brms con las covariables, region de residencias y noches que pernoctaron
fit <- brm(log(PGastoTotal) ~ P10D+P04_RegionVA + (1|P11_Zona1),
             data = ECV2021NF,chains = 4)
summary(mod)

mcmc_trace(fit)
mcmc_dens(fit)


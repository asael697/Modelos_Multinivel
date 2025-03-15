library(cmdstanr)
library(bayesplot)
library(loo)
library(posterior)
library(xtable)
library(ggplot2)
library(cowplot)
library(ggthemes)

load("~/Documents/Modelos_Multinivel/Datos/Datos2021.RData")

sf <- "~/Documents/Modelos_Multinivel/Stancodes/ML_t.stan"
sm <- cmdstan_model(sf)

# La lista de datos que Stan necesita para hacer mcmc
d1 = list(n = length(LogGTN), J = 6, group = gl, y = LogGTN)
d2 = list(n = length(LogGTN), J = 1, group = rep(1, length(LogGTN)), y = LogGTN)

# mcmc para modelo multinivel
fit <- sm$sample(data = d1, chains = 4, parallel_chains = 4, refresh = 500)
fit2 <- sm$sample(data = d2, chains = 4, parallel_chains = 4, refresh = 500)

# extraer las cadenas de las variables importantes multinivel student_t
fv = fit$draws(variables = c("mu","mu_group","nu_group","sigma"),format = "matrix")
# Resumen de la cadenas
summarise_draws(fv)

# graficos de las posteriors multinivel student_t
mcmc_combo(fv[,1:7],gg_theme = theme(legend.position = "none"))
mcmc_combo(fv[,8:14])

###########################################################
# Posterior predictive checks
###########################################################
sple = sample(1:4000,500)
yrep = fit$draws(variables = c("y_rep"),format = "matrix")

ppc_dens_overlay_grouped(LogGTN, yrep[sple,], group = glevels) + 
  labs(title = "Posterior Predictive checks",
       subtitle = "Modelo student-t multinivel")

# Leave one out modelo multinivel
print(loo_compare(fit$loo(), fit2$loo()),simplify = FALSE)

## result = Hierarchical-zona visitada

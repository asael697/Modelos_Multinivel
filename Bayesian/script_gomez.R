library(cmdstanr)
library(bayesplot)
library(loo)
library(posterior)
library(xtable)
library(ggplot2)
library(cowplot)
library(ggthemes)

load("~/Documents/Modelos_Multinivel/Datos/Datos2021.RData")

# Compilar el codigo Stan del modelo de Gomez
sm <- cmdstan_model("Stancodes/skew_normal.stan")

# La lista de datos que Stan necesita para hacer mcmc
d1 = list(n = length(LogGTN), J = 6, group = gl, y = LogGTN)

# mcmc para modelo de Gomez
fit <- sm$sample(data = d1, chains = 4, parallel_chains = 4,refresh = 500)
fv = fit$draws(variables = c("mu","sigma","alpha"),format = "matrix")

# resumen de las cadenas
summarize_draws(fv)

# Posterior plots
color_scheme_set("blue")
mcmc_combo(fv)

###########################################################
# Posterior predictive checks
###########################################################
sple = sample(1:4000,500)
yrep = fit$draws(variables = c("y_rep"),format = "matrix")

ppc_dens_overlay(LogGTN, yrep) + 
  labs(title = "Posterior Predictive checks",
       subtitle = "Modelo normal asimétrico, de Gomez, et al (2022)")

###########################################################
# Leave one out modelo Gomez
###########################################################

ll = fit$draws(variables = "log_lik",format = "matrix")
r_eff = relative_eff(exp(ll), cores = 2, chain_id = rep(1:4, each = 1000))
loo = loo(ll, r_eff = r_eff, cores = 2)
loo

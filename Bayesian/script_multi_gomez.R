ibrary(cmdstanr)
library(bayesplot)
library(loo)
library(posterior)
library(xtable)
library(ggplot2)
library(cowplot)
library(ggthemes)

load("~/Documents/Modelos_Multinivel/Datos/Datos2021.RData")

# Compilar el codigo Stan del modelo student
sm <- cmdstan_model("Stancodes/ML_skew_normal.stan")

# La lista de datos que Stan necesita para hacer mcmc
d1 = list(n = length(LogGTN), J = 6, group = gl, y = LogGTN)

# mcmc para multinivel stundent_t
fit <- sm$sample(data = d1, chains = 4, parallel_chains = 4,refresh = 500)

# extraer las cadenas de las variables importantes multinivel student_t
fv = fit$draws(variables = c("mu","mu_group","alpha","sigma"),format = "matrix")
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
       subtitle = "Modelo skew-normal multinivel")

# Leave one out modelo multinivel student_t
ll = fit$draws(variables = "log_lik",format = "matrix")
r_eff = relative_eff(exp(ll), cores = 2, chain_id = rep(1:4, each = 1000))
loo = loo(ll,r_eff = r_eff, cores = 2)
loo

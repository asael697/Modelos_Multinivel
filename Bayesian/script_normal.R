library(cmdstanr)
library(bayesplot)
library(loo)
library(posterior)
library(xtable)
library(ggplot2)
library(cowplot)
library(ggthemes)

load("~/Documents/Modelos_Multinivel/Datos/Datos2021.RData")

# Compilar el codigo Stan del modelo multinivel
sm <- cmdstan_model("Stancodes/multi_level.stan")

# La lista de datos que Stan necesita para hacer mcmc
d1 = list(n = length(LogGTN), J = 6, group = gl, y = LogGTN)

# mcmc para modelo multinivel
fit <- sm$sample(data = d1, chains = 4, parallel_chains = 4, refresh = 500)
fv = fit$draws(variables = c("mu_group","sigma"),format = "matrix")
colnames(fv) = c(levels(glevels),'sigma')

# resumen de las cadenas
summarize_draws(fv)
xtable(print(summarize_draws(fv),simplify = FALSE, digits = 2))

# graficos de las posteriors multinivel
color_scheme_set("blue")
g1 = mcmc_combo(fv[,1:4],gg_theme = theme(legend.position = "none"))
g2 = mcmc_combo(fv[,5:7])
cowplot::plot_grid(g1,g2,ncol = 2,rel_widths = c(1.1, 1.2))

###########################################################
# Posterior predictive checks
###########################################################

# modelo multinivel normal
sple = sample(1:4000,500)
yrep = fit$draws(variables = c("y_rep"),format = "matrix")
ppc_dens_overlay_grouped(LogGTN, yrep[sple,], group = glevels)

# Leave one out modelo multinivel
ll = fit$draws(variables = "log_lik",format = "matrix")
r_eff = relative_eff(exp(ll), cores = 2, chain_id = rep(1:4, each = 1000))
loo = loo(ll, r_eff = r_eff, cores = 2)
loo

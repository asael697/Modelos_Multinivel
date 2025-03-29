library(cmdstanr)
library(bayesplot)
library(loo)
library(posterior)
library(xtable)
library(ggplot2)
library(cowplot)
library(ggthemes)

load("~/Documents/Modelos_Multinivel/Datos/Datos2021.RData")

sf <- "~/Documents/Modelos_Multinivel/Stancodes/ML_N.stan"
sm <- cmdstan_model(sf)

# La lista de datos que Stan necesita para hacer mcmc
## Global
d1 = list(n = length(LogGTN), J = 1, group = rep(1, length(LogGTN)), y = LogGTN)
## Zona visitada
d2 = list(n = length(LogGTN), J = 6, group = gl1, y = LogGTN)
## Procedencia
d3 = list(n = length(LogGTN), J = 6, group = gl2, y = LogGTN)
## Procedencia y Zona
d4 = list(n = length(LogGTN), J = 34, group = gl3, y = LogGTN)

# mcmc para modelo multinivel
fit1 <- sm$sample(data = d1, chains = 4, parallel_chains = 4, refresh = 500)
fit2 <- sm$sample(data = d2, chains = 4, parallel_chains = 4, refresh = 500)
fit3 <- sm$sample(data = d3, chains = 4, parallel_chains = 4, refresh = 500)
fit4 <- sm$sample(data = d4, chains = 4, parallel_chains = 4, refresh = 500)

fv = fit2$draws(variables = c("mu","mu_group","sigma"),format = "matrix")
colnames(fv) = c("mu",levels(glevels),'sigma')

# resumen de las cadenas
summarize_draws(fv)
xtable(print(summarize_draws(fv),simplify = FALSE, digits = 2))

# graficos de las posteriors multinivel
color_scheme_set("blue")
g1 = mcmc_combo(fv[,1:4],gg_theme = theme(legend.position = "none"))
g2 = mcmc_combo(fv[,5:8])
cowplot::plot_grid(g1,g2,ncol = 2,rel_widths = c(1.1, 1.2))

###########################################################
# Posterior predictive checks
###########################################################

# modelo multinivel normal
sple = sample(1:4000,500)
yrep = fit4$draws(variables = c("y_rep"),format = "matrix")
ppc_dens_overlay_grouped(LogGTN, yrep[sple,], group = glevels3)

# Leave one out modelo multinivel
print(loo_compare(fit1$loo(), fit2$loo(),fit3$loo(),fit4$loo()),simplify = FALSE)

## result = Hierarchical2-zona visitada y Procedencia modelo fit4
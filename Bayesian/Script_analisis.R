library(cmdstanr)
library(bayesplot)
library(posterior)
library(xtable)
library(ggplot2)
library(cowplot)
library(ggthemes)
library(loo)

load("Datos/datos2016.RData")

# Compilar el codigo Stan del modelo multinivel
sm1 <- cmdstan_model("Stancodes/multi_level.stan")

# Compilar el codigo Stan del modelo de Gomez
sm2 <- cmdstan_model("Stancodes/skew_normal.stan")

# Compilar el codigo Stan del moelo multinivel Student-t
sm3 <- cmdstan_model("Stancodes/ML_student.stan")

# La lista de datos que Stan necesita para hacer mcmc
d1 = list(n = length(LogGFN), J = 6, group = gl, y = LogGFN)

# mcmc para modelo multinivel
fit1 <- sm1$sample(data = d1, chains = 4, parallel_chains = 4, refresh = 500)

# mcmc para modelo de Gomez
fit2 <- sm2$sample(data = d1, chains = 4, parallel_chains = 4,refresh = 500)

#mcmc para modelo multinivel Student
fit3 <- sm3$sample(data = d1, chains = 4, parallel_chains = 4,refresh = 500)

# extraer las cadenas de las variables importantes multinivel
fv1 = fit1$draws(variables = c("mu_group","sigma"),format = "matrix")
colnames(fv1) = c(levels(glevels),'sigma')

# resumen de las cadenas
summarize_draws(fv1)
xtable(print(summarize_draws(fv1),simplify = FALSE, digits = 2))
# graficos de las posteriors multinivel
g1 = mcmc_combo(fv1[,1:4],gg_theme = theme(legend.position = "none"))
g2 = mcmc_combo(fv1[,5:7])
cowplot::plot_grid(g1,g2,ncol = 2,rel_widths = c(1.1, 1.2))

# Leave one out modelo multinivel
ll1 = fit1$draws(variables = "log_lik",format = "matrix")
loo1 = loo(ll1)

# Leave one out modelo Gomez
ll2 = fit2$draws(variables = "log_lik",format = "matrix")
loo2 = loo(ll2)

comp = loo_compare(loo1,loo2)
xtable(print(comp,simplify = FALSE, digits = 2))





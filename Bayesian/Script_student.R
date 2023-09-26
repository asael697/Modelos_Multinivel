library(cmdstanr)
library(bayesplot)
library(loo)
library(rstan)
library(posterior)
library(xtable)
library(ggplot2)
library(cowplot)
library(ggthemes)

# Compilar el codigo Stan del modelo student
sm3 <- cmdstan_model("Stancodes/ML_student.stan")

# mcmc para multinivel stundent_t
fit3 <- sm3$sample(data = d1, chains = 4, parallel_chains = 4,refresh = 500)

# extraer las cadenas de las variables importantes multinivel student_t
fv3 = fit3$draws(variables = c("mu_group","sigma"),format = "matrix")
colnames(fv3) = c(levels(glevels),'sigma')

# Resumen de la cadenas
summarise_draws(fv3)

# graficos de las posteriors multinivel student_t
g1 = mcmc_combo(fv3[,1:4],gg_theme = theme(legend.position = "none"))
g2 = mcmc_combo(fv3[,5:7])
cowplot::plot_grid(g1,g2,ncol = 2,rel_widths = c(1.1, 1.2))

# Leave one out modelo multinivel student_t
ll3 = fit3$draws(variables = "log_lik",format = "matrix")
loo3 = loo(ll3)
#Comparacion de los modelos
comp2 = loo_compare(loo1,loo2,loo3)
xtable(print(comp2,simplify = FALSE, digits = 2))

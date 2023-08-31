library(cmdstanr)
library(bayesplot)
library(loo)
library(rstan)
library(posterior)
library(xtable)
library(ggplot2)
library(cowplot)
library(ggthemes)

# ignora esta funcion
dat_gen = function(N = 32,beta = rnorm(1),K = 4,alphaj = rnorm(K),seed  = NULL){
  
  if (!is.null(seed))
    set.seed(seed)
  
  g =  rep(1:K,N/K)
  
  # Generating the hierarchical model
  x = rnorm(N)
  y = rnorm(N, beta + alphaj, 1)
  
  df = data.frame(g = g,x = x,y = y)
  return(df)
}

# Compilar el codigo Stan del modelo multinivel
sm1 <- cmdstan_model("Stancodes/multi_level.stan")

# Compilar el codigo Stan del modelo de Gomez
sm2 <- cmdstan_model("Stancodes/skew_normal.stan")

# Compilar el codigo Stan del modelo student
sm3 <- cmdstan_model("Stancodes/multi_level_student_t.stan")

# La lista de datos que Stan necesita para hacer mcmc
d1 = list(n = length(LogGFN), J = 6, group = gl, y = LogGFN)

# mcmc para modelo multinivel
fit1 <- sm1$sample(data = d1, chains = 4, parallel_chains = 4, refresh = 500)

# mcmc para modelo de Gomez
fit2 <- sm2$sample(data = d1, chains = 4, parallel_chains = 4,refresh = 500)

# mcmc para multinivel stundent_t
fit3 <- sm3$sample(data = d1, chains = 4, parallel_chains = 4,refresh = 500)

# extraer las cadenas de las variables importantes multinivel
fv1 = fit1$draws(variables = c("mu_group","sigma"),format = "matrix")
colnames(fv1) = c(levels(glevels),'sigma')

# extraer las cadenas de las variables importantes multinivel student_t
fv3 = fit3$draws(variables = c("mu_group","sigma"),format = "matrix")
colnames(fv3) = c(levels(glevels),'sigma')


# resumen de las cadenas
summarize_draws(fv1)
summarise_draws(fv3)

# graficos de las posteriors multinivel
g1 = mcmc_combo(fv1[,1:4],gg_theme = theme(legend.position = "none"))
g2 = mcmc_combo(fv1[,5:7])
cowplot::plot_grid(g1,g2,ncol = 2,rel_widths = c(1.1, 1.2))

# graficos de las posteriors multinivel student_t
g1 = mcmc_combo(fv3[,1:4],gg_theme = theme(legend.position = "none"))
g2 = mcmc_combo(fv3[,5:7])
cowplot::plot_grid(g1,g2,ncol = 2,rel_widths = c(1.1, 1.2))


# Leave one out modelo multinivel
ll1 = fit1$draws(variables = "log_lik",format = "matrix")
loo1 = loo(ll1)

# Leave one out modelo multinivel student_t
ll3 = fit3$draws(variables = "log_lik",format = "matrix")
loo3 = loo(ll3)

# Leave one out modelo Gomez
ll2 = fit2$draws(variables = "log_lik",format = "matrix")
loo2 = loo(ll2)

comp = loo_compare(loo1,loo2)
xtable(print(comp,simplify = FALSE, digits = 2))

comp2 = loo_compare(loo1,loo2,loo3)
xtable(print(comp2,simplify = FALSE, digits = 2))



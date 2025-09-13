library(cmdstanr)
library(bayesplot)
library(loo)
library(posterior)
library(xtable)
library(ggplot2)
library(cowplot)
library(ggthemes)

load("~/Documents/GitHub/Modelos_Multinivel/Datos/Datos2021.RData")

compute_loo <- function(stan_file_path = NULL, data_list = NULL){
  sm <- cmdstan_model(stan_file_path)
  fit <- sm$sample(data = data_list, chains = 4, 
                   parallel_chains = 4, refresh = 500)
  
  ll = fit$draws(variables = "log_lik",format = "matrix")
  r_eff = relative_eff(exp(ll), cores = 2, chain_id = rep(1:4, each = 1000))
  loo_results = loo(ll, r_eff = r_eff, cores = 2)
  
  return(loo_results)
}

# Gamma
sm1 <- cmdstan_model("~/Documents/GitHub/Modelos_Multinivel/Stancodes/ML_gamma.stan")
# Gamma generalizada
sm2 <- cmdstan_model("~/Documents/GitHub/Modelos_Multinivel/Stancodes/ML_gG.stan")
# Inversa Gamma
sm3 <- cmdstan_model("~/Documents/GitHub/Modelos_Multinivel/Stancodes/ML_gamma.stan")
# Normal
sm4 <- cmdstan_model("~/Documents/GitHub/Modelos_Multinivel/Stancodes/ML_N.stan")
# Normal skew
sm5 <- cmdstan_model( "~/Documents/GitHub/Modelos_Multinivel/Stancodes/ML_sN.stan")
# student t
sm6 <- cmdstan_model("~/Documents/GitHub/Modelos_Multinivel/Stancodes/ML_t.stan")

# La lista de datos que Stan necesita para hacer mcmc
## Global
d1 = list(n = length(GastoTotal), J = 1, group = rep(1, length(GastoTotal)), y = GastoTotal)
d1_log = list(n = length(LogGTN), J = 1, group = rep(1, length(LogGTN)), y = LogGTN)
## Zona visitada
d2 = list(n = length(GastoTotal), J = 6, group = gl1, y = GastoTotal)
d2_log = list(n = length(LogGTN), J = 6, group = gl1, y = LogGTN)
## Procedencia
d3 = list(n = length(GastoTotal), J = 6, group = gl2, y = GastoTotal)
d3_log = list(n = length(LogGTN), J = 6, group = gl2, y = LogGTN)
## Procedencia y Zona
d4 = list(n = length(GastoTotal), J = 34, group = gl3, y = GastoTotal)
d4_log = list(n = length(LogGTN), J = 34, group = gl3, y = LogGTN)

## datos para la inversa Gamma
# Gasto Global
d1_inver <- list(n = length(GastoTotal), J = 1, group = rep(1, length(GastoTotal)), y = 1/GastoTotal)
# Zona Visitada
d2_inver <- list(n = length(GastoTotal), J = length(unique(gl1)), group = gl1, y = 1/GastoTotal)
# Procedencia del turista
d3_inver <- list(n = length(GastoTotal), J = length(unique(gl2)), group = gl2, y = 1/GastoTotal)
# Zona -Procedencia
d4_inver <- list(n = length(GastoTotal), J = length(unique(gl3)), group = gl3, y = 1/GastoTotal)

# ajustes de los modelos
## ajustar modelo
fit1 <- sm1$sample(data = d1, chains = 4, parallel_chains = 4, refresh = 500)
fit2 <- sm2$sample(data = d4, chains = 4, parallel_chains = 4,refresh = 500)
fit3 <- sm3$sample(data = d1_inver, chains = 4, parallel_chains = 4,refresh = 500)
fit4 <- sm4$sample(data = d4_log, chains = 4, parallel_chains = 4,refresh = 500)
fit5 <- sm5$sample(data = d4_log, chains = 4, parallel_chains = 4,refresh = 500)
fit5.1 <- sm5$sample(data = d2_log, chains = 4, parallel_chains = 4,refresh = 500) #Zona
fit5.2 <- sm5$sample(data = d3_log, chains = 4, parallel_chains = 4,refresh = 500) #Procedencia
fit5.3 <- sm5$sample(data = d3, chains = 4, parallel_chains = 4,refresh = 500) #Procedencia
fit6 <- sm6$sample(data = d4_log, chains = 4, parallel_chains = 4,refresh = 500)

## fit2, y fit6 tienen mal diagnositcos en loo
fit1$loo()
fit2$loo()
fit3$loo()
fit4$loo()
fit5$loo()
fit5.1$loo()
fit6$loo()

## Comparar modelos con buen LOO
print(loo_compare(fit1$loo(),   # model1: Gamma, Global escala real
                  #fit2$loo(),   # model2: Gen-Gamma, procedencia escala real
                  fit3$loo(),   # model3: inv-Gamma, Zona_y_procedencia escala inv
                  #fit4$loo(),   # model4: normal, Zona_y_procedencia escala log
                  #fit5$loo(),   # model5: normal asymetrica, zona_y_procedencia, escala log
                  #fit5.1$loo(), # model6: normal asymetrica, zona, escala log
                  fit5.2$loo(),  # model7: normal asymetrica, procedencia escala log
                  fit5.3$loo()   # model8: normal asymetrica, procedencia escala real
                  #fit6$loo(),   # model9: student t, zona_y_procedencia, escala log
      ),  simplify = FALSE)

fit5.2$loo()
################################################################
# mejor modelo:  
###############################################################

fit = fit5.2
data = d3_log

rm(fit1, fit2, fit3, fit4, fit5, fit5.1, fit5.2, fit5.3, fit6)
rm(sm1,sm2,sm3,sm4,sm5,sm6)
rm(d1, d2, d3, d4, d1_log, d2_log, d3_log, d4_log)
#################################################################################
##         Analisis de parametros mejor modelo
#################################################################################
# extraer las cadenas de las variables importantes multinivel
fv = fit$draws(variables = c("mu","mu_group","alpha","sigma"), format = "matrix")

# resumen de las cadenas
summarize_draws(fv)
xtable(print(summarize_draws(fv),simplify = FALSE, digits = 2))

# graficos de las posteriors multinivel
color_scheme_set("blue")
g1 = mcmc_combo(fv[,1:7], gg_theme = theme(legend.position = "none"))
g2 = mcmc_combo(fv[,8:14])
cowplot::plot_grid(g1,g2,ncol = 2,rel_widths = c(1.1, 1.2))

###########################################################
# Posterior predictive checks
###########################################################

yrep = fit$draws(variables = c("y_rep"),format = "matrix")

ppc_dens_overlay_grouped(LogGTN, yrep, group = glevels2) + 
  labs(title = "Posterior Predictive checks",
       subtitle = "Modelo skew-normal multinivel")

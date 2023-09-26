library(tidyverse)
library(tidyquant)
library(ggdist)
library(ggthemes)

load("~/Documents/Modelos_Multinivel/Datos/Datos2021.RData")
## Grafico de Caja del Gasto y Densidad
ECV2021NF %>% 
  filter(PGastoTotal>= 200, PGastoTotal <= 10000) %>%
  ggplot(aes(x = "", y=PGastoTotal))+
  # add half-violin from {ggdist} package
  stat_halfeye(
    # adjust bandwidth
    adjust = 0.5,
    # move to the right
    justification = -0.2,
    # remove the slub interval
    .width = 0,
    point_colour = NA,
    alpha=0.7,
    fill="#2297E7"
  )+
  geom_boxplot(
    width = 0.12,
    alpha = 0.7,
    fill="#2297E7",
    outlier.color = "red"
  )+
  theme_tq() +
  labs(
    title = "Gráfico de densidad y caja",
    x = "",
    y = "Gasto",
  ) +
  coord_flip()+
  theme(legend.position ="right",plot.title = element_text(hjust = 0.5))

## Grafico por zonas
ECV2021NF %>% 
  filter(P11_Zona1 %in% c("Zona Centro", "Zona Insular", "Zona Norte","Zona Occidental",
                      "Zona Oriental","Zona Sur"),
         PGastoTotal>=200, PGastoTotal<= 5000) %>% 
  ggplot(aes(x = factor(P11_Zona1), y =log(PGastoTotal), fill = factor(P11_Zona1)))+
  # add half-violin from {ggdist} package
  stat_halfeye(
    # adjust bandwidth
    adjust = 0.5,
    # move to the right
    justification = -0.2,
    # remove the slub interval
    .width = 0,
    point_colour = NA
  )+
  geom_boxplot(
    width = 0.12,
    # removing outliers
    outlier.color = NA,
    alpha = 0.7
  )+
  theme_tq() +
  labs(
    title = "Densidades y Box-plot",
    x = "",
    y = "Gasto ",
    fill = "Zonas"
  )+ 
  coord_flip()+
  theme(legend.position ="right",plot.title = element_text(hjust = 0.5))

library(ggplot2)
library(ggdist)
##Grafico Logaritmico
## Grafico por zonas
ECV2021NF %>% 
  filter(P11_Zona1 %in% c("Zona Centro", "Zona Insular", "Zona Norte","Zona Occidental",
                          "Zona Oriental","Zona Sur"))%>% 
  ggplot(aes(x = factor(P11_Zona1), y =log(PGastoTotal), fill = factor(P11_Zona1)))+
  # add half-violin from {ggdist} package
  stat_halfeye(
    # adjust bandwidth
    adjust = 0.5,
    # move to the right
    justification = -0.2,
    # remove the slub interval
    .width = 0,
    point_colour = NA
  )+
  geom_boxplot(
    width = 0.12,
    # removing outliers
    outlier.color = NA,
    alpha = 0.7
  )+
  theme_tq() +
  labs(
    title = "Densidades y Box-plot",
    x = "",
    y = "Gasto ",
    fill = "Zonas"
  )+ 
  coord_flip()+
  theme(legend.position ="right",plot.title = element_text(hjust = 0.5))

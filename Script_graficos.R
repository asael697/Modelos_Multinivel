
library(tidyverse)
library(tidyquant)
library(ggdist)
library(ggthemes)

load("~/Documents/Modelos_Multinivel/Datos/Datos2016.RData")

## Grafico de Caja del Gasto y Densidad
EGYPV2016TNF2 %>% 
  filter(GastoFin>= 200, GastoFin <= 5000) %>%
  ggplot(aes(x = "", y=GastoFin))+
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
EGYPV2016TNF2 %>% 
  filter(Zona1 %in% c("Zona Centro", "Zona Insular", "Zona Norte","Zona Occidental",
                      "Zona Oriental","Zona Sur"),
         GastoFin>=200, GastoFin<= 3000) %>% 
  ggplot(aes(x = factor(Zona1), y =GastoFin, fill = factor(Zona1)))+
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
##Grafico Logaritmico
EGYPV2016TNF2 %>% 
  filter(Zona1 %in% c("Zona Centro", "Zona Insular", "Zona Norte","Zona Occidental",
                      "Zona Oriental","Zona Sur")) %>% 
  ggplot(aes(x = factor(Zona1), y =log(EGYPV2016TNF2$GastoFin), fill = factor(Zona1)))+
  # add half-violin from {ggdist} package
  stat_halfeye(
    # adjust bandwidth
    adjust = 0.5,
    # move to the right
    justification = -0.2,
    # remove the slub interval
    .width = 0,
    point_colour = NA
  ) +
  geom_boxplot(
    width = 0.12,
    # removing outliers
    outlier.color = NA,
    alpha = 0.5
  )+
  theme_tq() +
  labs(
    title = "Densidades y Box-plot",
    x = "",
    y = "Gasto Logaritmico",
    fill = "Zonas"
  ) +
  coord_flip()+
  theme(legend.position ="right",plot.title = element_text(hjust = 0.5))

### Grafico Por Regiones Logaritmico
EGYPV2016TNF2 %>% 
  filter(RegRes %in% c("Centroamérica", "Europa", "Norteamérica","Resto del Mundo")) %>% 
  ggplot(aes(x = factor(RegRes), y = log(EGYPV2016TNF2$GastoFin), fill = factor(RegRes)))+
  
  # add half-violin from {ggdist} package
  stat_halfeye(
    # adjust bandwidth
    adjust = 0.5,
    # move to the right
    justification = -0.2,
    # remove the slub interval
    .width = 0,
    point_colour = NA
  ) +
  geom_boxplot(
    width = 0.12,
    # removing outliers
    outlier.color = NA,
    alpha = 1
  ) +
  # Themes and Labels
  scale_fill_tq() +
  theme_tq() +
  labs(
    title = "Densidades y Box-plots",
    x = "",
    y = "Gasto Logaritmico",
    fill = "Región"
  ) +
  coord_flip()+
  theme(legend.position ="right",plot.title = element_text(hjust = 0.5))
###Grafico por region gasto normal
EGYPV2016TNF2 %>% 
  filter(RegRes %in% c("Centroamérica", "Europa", "Norteamérica","Resto del Mundo"),
         GastoFin>=200, GastoFin<= 3000) %>% 
  ggplot(aes(x = factor(RegRes), y = GastoFin, fill = factor(RegRes)))+
  
  # add half-violin from {ggdist} package
  stat_halfeye(
    # adjust bandwidth
    adjust = 0.5,
    # move to the right
    justification = -0.2,
    # remove the slub interval
    .width = 0,
    point_colour = NA
  ) +
  geom_boxplot(
    width = 0.12,
    # removing outliers
    outlier.color = NA,
    alpha = 1
  ) +
  # Themes and Labels
  scale_fill_tq() +
  theme_tq() +
  labs(
    title = "Densidades y Box-plots",
    x = "",
    y = "Gasto",
    fill = "Región"
  ) +
  coord_flip()+
  theme(legend.position ="right",plot.title = element_text(hjust = 0.5))



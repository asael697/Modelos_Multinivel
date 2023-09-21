## Limpieza de la base datos ECV 2021
library(haven)
library(foreign)
ECV2021<- read.spss("Datos/Base de la ECV 2021.sav")
ECV2021 <-data.frame(ECV2021)
## Filtrado de las variables que se necesitan para la estimación 

ECV2021N <- subset(ECV2021, select = c(Validas,Mes,Trimestre,P04,CodCiuRes,
                                       P04_RegionVA,P04_RegionVF,P11_Zona1,PGastoTotal))

## Filtrado solo para los datos de Gasto y Perfil
ECV2021N <- ECV2021N[!is.na(ECV2021N$PGastoTotal), ]
## Filtrado solo para todas las zona menos la desconocida
ECV2021NF <- subset(ECV2021N, P11_Zona1 %in% c("Zona Centro","Zona Insular",
                                                   "Zona Norte","Zona Occidental",
                                                   "Zona Oriental","Zona Sur"))
## Gráficos de las densidades y cajas de las zonas del gasto turistico
library(tidyverse)
library(tidyquant)
library(ggdist)
library(ggthemes)
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

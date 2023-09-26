## Limpieza de la base datos EGYPV2016Turistas
library(haven)
library(foreign)
EGYPV2016Turistas <- read.spss("Datos/EGYPV 2016 F01 - Turistas.sav")
EGYPV2016Turistas <-data.frame(EGYPV2016Turistas)

## Filtrado de las variables que se necesitan para la estimación 
EGYPV2016TN <- subset(EGYPV2016Turistas, select = c(Validas, TipViajero, TipVisitante, Mes, CodRes, RegRes, CodPrimVisita,P29_Mu4,
                                                    GruViaje, P10_3NumNoch, Pernocto, GruNoch, P11_THoteles, P11_TAmigos,
                                                    P11_TAlojP, CodTipMot, CodModViaje, P28_Total, CodEstCivil, P9_GruViaje, 
                                                    P10_Pernocto, GastoTotalXPers, GastoFin, GastoTotalXPersXDia, Ciudad1, Zona1))

## Filtrado solo para los datos de Gasto y Perfil
EGYPV2016TNF <- subset(EGYPV2016TN,Validas == "Gasto y Perfil")
## Filtrado solo para todas las zona menos la desconocida
EGYPV2016TNF2 <- subset(EGYPV2016TNF, Zona1 %in% c("Zona Centro","Zona Insular",
                                                   "Zona Norte","Zona Occidental",
                                                   "Zona Oriental","Zona Sur"))

# Variables adicionales
GastoFinN = EGYPV2016TNF$GastoFin
glevels = factor(EGYPV2016TNF$Zona1)

gl = as.numeric(glevels[!is.na(log(GastoFinN))])
gl
LogGFN = na.exclude(log(GastoFinN))

save.image("Datos2016.RData")
## Gráficos de las densidades y cajas de las zonas del gasto turistico
library(tidyverse)
library(tidyquant)
library(ggdist)
library(ggthemes)
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



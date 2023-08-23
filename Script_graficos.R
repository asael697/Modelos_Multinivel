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

## Conversión del Gasto Fin a escala Logarítmica
GastoFinN = EGYPV2016TNF$GastoFin
glevels = factor(EGYPV2016TNF$Zona1)

gl = as.numeric(glevels[!is.na(log(GastoFinN))])
gl
LogGFN = na.exclude(log(GastoFinN))

## Gráficos de las densidades para GastoFinN y LogGFN

library(ggplot2)
ggplot(data = EGYPV2016TNF) +
  geom_density(aes(x =LogGFN, fill = Zona1))+
  xlab("LogGFN") +
  ylab("") +
  ggtitle("Densidades del LogGFN por Zona")+facet_wrap(~ Zona1, nrow = 2)

## Gráficos de otros casos
library(tidyverse)
library(tidyquant)
library(ggdist)
library(ggthemes)

EGYPV2016TNF %>% 
  filter(Zona1 %in% c("Zona Centro", "Zona Insular", "Zona Norte","Zona Occidental",
                      "Zona Oriental","Zona Sur","Desconocido")) %>% 
  ggplot(aes(x = factor(Zona1), y =log(EGYPV2016TNF$GastoFin), fill = factor(Zona1)))+
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
  coord_flip()

### Grafico Por Regiones
EGYPV2016TNF %>% 
  filter(RegRes %in% c("Centroamérica", "Europa", "Norteamérica","Resto del Mundo")) %>% 
  ggplot(aes(x = factor(RegRes), y = log(EGYPV2016TNF$GastoFin), fill = factor(RegRes)))+
  
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
    title = "Densidades y Box-plots del LogGFN por Region de Residencia",
    x = "",
    y = "Gasto Logaritmico",
    fill = "Zonas"
  ) +
  coord_flip()


# Graficos del gasto  final por zonas
library(ggridges)
library(ggplot2)
# basic example
ggplot(EGYPV2016TNF, aes(x = GastoFinN, y = Zona1, fill = Zona1)) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none")+
  labs(
    title = "Densidades del gasto final por Zonas",
    x = "",
    y = "",
    fill = "Zonas"
  ) 


ggplot(EGYPV2016TNF, aes(x = log(EGYPV2016TNF$GastoFin), y = Zona1, fill = Zona1)) +
  geom_boxplot()+
  theme_ridges() + 
  theme(legend.position = "none")+
  labs(
    title = "Densidades del gasto final por Zonas",
    x = "",
    y = "",
    fill = "Zonas"
  ) 

### Calculo de las estadisticas descriptivas de la variables GastoFin
### y por Zonas respectivamente

summary(EGYPV2016TNF$GastoFin)
##Desviacion
sd(GastoFinN,na.rm = TRUE)
## Asimetria
skewness(GastoFinN)
## Curtosis
kurtosis(GastoFinN)


## Resumen de los turisticas de cuantas noches pernotaron en el pais
summary(EGYPV2016TNF$P10_3NumNoch)
## Base para identificar los turistas que gastaron arriba y menos de 700 dolares

EGYP1<-subset(EGYPV2016TNF,GastoFin > 700)
EGYP2<-subset(EGYPV2016TNF,GastoFin < 700)

## Estadística Descriptivas por Zonas
resumen_zonas <- EGYPV2016TNF %>%
  group_by(Zona1) %>%
  summarise(
    q1 = quantile(GastoFin, 0.25,na.rm = TRUE),
    Media = mean(GastoFin, na.rm = TRUE),
    Mediana = median(GastoFin, na.rm = TRUE),
    DesvEstandar = sd(GastoFin, na.rm = TRUE),
    q3 = quantile(GastoFin, 0.75,na.rm = TRUE),
    Minimo = min(GastoFin, na.rm = TRUE),
    Maximo = max(GastoFin, na.rm = TRUE),
    asimetria=skewness(GastoFin,na.rm = TRUE),
    Curtosis=kurtosis(GastoFin,na.rm=TRUE)
  )
print(resumen_zonas)

resumen_zonas1 <- EGYPV2016TNF %>%
  group_by(Zona1) %>%
  count()
print(resumen_zonas1)
boxplot(GastoFinN)

##Correccion de los graficos

library(tidyverse)
library(tidyquant)
library(ggdist)
library(ggthemes)
EGYPV2016TNF %>% 
  filter(Zona1 %in% c("Zona Centro", "Zona Insular", "Zona Norte","Zona Occidental",
                      "Zona Oriental","Zona Sur","Desconocido"),
         GastoFin>=200, GastoFin<= 2000) %>% 
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
  theme_tq() +
  labs(
    title = "Densidades",
    x = "",
    y = "Gasto ",
    fill = "Zonas"
  )+ 
  coord_flip()+
  theme(legend.position ="right",plot.title = element_text(hjust = 0.5))

## Porcentaje de valores atipicos
Q1 <- quantile(GastoFinN, 0.25,na.rm = TRUE)
Q3 <- quantile(GastoFinN, 0.75,na.rm = TRUE)
IQR <- Q3 - Q1

# Definir límites para detectar valores atípicos
lim_inf <- Q1 - 1.5 * IQR
lim_sup <- Q3 + 1.5 * IQR

# Calcular porcentaje de valores atípicos
valores_atipicos <- GastoFinN[GastoFinN < lim_inf | GastoFinN > lim_sup]
porcentaje_atipicos <- (length(valores_atipicos) / length(GastoFinN)) * 100

cat("Porcentaje de valores atípicos:", porcentaje_atipicos, "%\n")
### Grafico de caja 
library(ggplot2)
EGYPV2016TNF %>% 
  filter(GastoFin >= 200, GastoFin <= 2000) %>% 
  ggplot(aes(x = "", y = GastoFin)) +
  geom_boxplot(width = 0.15,fill = "green") +
  labs(
    title = "Gráfico de Caja",
    x = "Gasto",
    y = ""
  ) + 
  theme_tq() +
  theme(plot.title = element_text(hjust = 0.5)) 

###Logaritmico
library(tidyverse)
library(tidyquant)
library(ggdist)
library(ggthemes)
EGYPV2016TNF %>% 
  filter(Zona1 %in% c("Zona Centro", "Zona Insular", "Zona Norte","Zona Occidental",
                      "Zona Oriental","Zona Sur","Desconocido")) %>% 
  ggplot(aes(x = factor(Zona1), y=GastoFinN, fill = factor(Zona1)))+
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

## Boxplot por zonas del gasto turistico

EGYPV2016TNF %>% 
  filter(Zona1 %in% c("Zona Centro", "Zona Insular", "Zona Norte","Zona Occidental",
                      "Zona Oriental","Zona Sur","Desconocido"),
         GastoFin>=200, GastoFin<= 2000) %>% 
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



##Graficos 
library(tidyverse)
library(tidyquant)
library(ggdist)
library(ggthemes)

EGYPV2016TNF %>% 
  filter(GastoFin>= 200, GastoFin <= 3000) %>%
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
    fill="#61D04F"
  )+
  geom_boxplot(
    width = 0.12,
    alpha = 0.7,
    fill="#61D04F",
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

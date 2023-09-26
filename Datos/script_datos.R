library(haven)
library(foreign)

#####################################################################################
#               Datos para el 2021
#####################################################################################
ECV2021 <- read.spss("~/Documents/Modelos_Multinivel/Datos/Base de la ECV 2021.sav")
ECV2021 <- data.frame(ECV2021)
## Filtrado de las variables que se necesitan para la estimación 

ECV2021N <- subset(ECV2021, select = c(Validas,Mes,Trimestre,P04,CodCiuRes,
                                       P04_RegionVA,P04_RegionVF,P11_Zona1,PGastoTotal))

## Filtrado solo para los datos de Gasto y Perfil
ECV2021N <- ECV2021N[!is.na(ECV2021N$PGastoTotal), ]
## Filtrado solo para todas las zona menos la desconocida
ECV2021NF <- subset(ECV2021N, P11_Zona1 %in% c("Zona Centro","Zona Insular",
                                               "Zona Norte","Zona Occidental",
                                               "Zona Oriental","Zona Sur"))

## Conversión del Gasto Fin a escala Logarítmica
GastoTotal= ECV2021NF$PGastoTotal
glevels = factor(ECV2021NF$P11_Zona1)

gl = as.numeric(glevels[!is.na(log(GastoTotal))])
gl
LogGTN = na.exclude(log(GastoTotal))

# setwd("Modelos_Multinivel/Datos")
save.image("~/Documents/Modelos_Multinivel/Datos/Datos2021.RData")
rm(list = ls())

#####################################################################################
#               Datos para el 2016
#####################################################################################

EGYPV2016Turistas <- read.spss("~/Documents/Modelos_Multinivel/Datos/EGYPV 2016 F01 - Turistas.sav")
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
GastoFinN = EGYPV2016TNF2$GastoFin
glevels = factor(EGYPV2016TNF2$Zona1)

gl = as.numeric(glevels[!is.na(log(GastoFinN))])
gl
LogGFN = na.exclude(log(GastoFinN))

save.image("~/Documents/Modelos_Multinivel/Datos/Datos2016.RData")

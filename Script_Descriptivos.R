library(xtable)
## Conversión del Gasto Fin a escala Logarítmica
GastoFinN = EGYPV2016TNF2$GastoFin
glevels = factor(EGYPV2016TNF2$Zona1)

gl = as.numeric(glevels[!is.na(log(GastoFinN))])
LogGFN = na.exclude(log(GastoFinN))
### Calculo de las estadísticas descriptivas de la variables Gasto Fin
### y por Zonas respectivamente

summary(EGYPV2016TNF2$GastoFin)
##Desviacion
sd(EGYPV2016TNF2$GastoFin,na.rm = TRUE)
## Asimetria
skewness(EGYPV2016TNF2$GastoFin,na.rm = TRUE)
## Curtosis
kurtosis(EGYPV2016TNF2$GastoFin,na.rm = TRUE)


## Resumen de los turisticas de cuantas noches pernotaron en el pais
summary(EGYPV2016TNF2$P10_3NumNoch)
## Base para identificar los turistas que gastaron arriba y menos de 700 dolares

EGYP1<-subset(EGYPV2016TNF2,GastoFin > 700)
EGYP2<-subset(EGYPV2016TNF2,GastoFin < 700)

## Estadística Descriptivas por Zonas
resumen_zonas <- EGYPV2016TNF2 %>%
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
##Codigo latex de la tabla
xtable(print(resumen_zonas,simplify = FALSE, digits = 2))
resumen_zonas1 <- EGYPV2016TNF2 %>%
  group_by(Zona1) %>%
  count()
print(resumen_zonas1)
##Codigo latex de la tabla
xtable(print(resumen_zonas1,simplify = FALSE, digits = 2))
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

cat("Porcentaje de valores atípicos del gasto:", porcentaje_atipicos, "%\n")

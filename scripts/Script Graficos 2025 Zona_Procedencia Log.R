library(dplyr)
library(ggplot2)
library(ggridges)
library(tidyquant)
library(forcats)
library(scales)

base_family <- "Times New Roman"

# Mapas y niveles
niveles_zona_raw <- c("Zona Centro","Zona Insular","Zona Norte","Zona Occidental","Zona Oriental","Zona Sur")
niveles_zona_lab <- c("Centro","Insular","Norte","Occidental","Oriental","Sur")
map_zona <- setNames(niveles_zona_lab, niveles_zona_raw)

niveles_proc <- c("Caribe","Centro-América","Europa","Norte-América","Resto del Mundo","Sur-América")

# --- Filtro en escala original y luego LOG del gasto ---
df <- ECV2021NF %>%
  filter(P11_Zona1 %in% niveles_zona_raw,
         Procedencia %in% niveles_proc,
         !is.na(PGastoTotal),
         PGastoTotal >= 200, PGastoTotal <= 6000) %>%                 # recorte original
  transmute(
    Zona        = factor(recode(P11_Zona1, !!!map_zona), levels = niveles_zona_lab),
    Procedencia = factor(Procedencia, levels = niveles_proc),
    Valor       = log(as.numeric(PGastoTotal))                        # <-- LOG del gasto
  ) %>%
  add_count(Zona, Procedencia, name = "n") %>%
  filter(n >= 3)

# Ridgeline: Y = Zona, colores = Procedencia (UNA sola leyenda)
g2 <- ggplot(df, aes(x = Valor, y = Zona, fill = Procedencia)) +
  geom_density_ridges(
    alpha = 0.35,
    scale = 0.95,             # un poco menor para menos solapamiento
    rel_min_height = 0.01,
    color = "white",          # contorno sutil
    linewidth = 0.3,          # <- reemplaza 'size'
    position = "identity",
    bw = "SJ",                # ancho de banda "Sheather-Jones" (robusto); opcional
    na.rm = TRUE              # silencia warnings por NA
  ) +
  scale_fill_brewer(palette = "Set2") +   # (o tu paleta manual)
  labs(
    title = "Densidades del gasto turístico por zona según procedencia (escala logaritmica)",
    x = "Logaritmo del gasto turístico", y = "Zonas", fill = "Procedencia"
  ) +
  theme_tq(base_family = base_family) +
  theme(
    legend.position = "right",
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5)
  ) +
  guides(fill = guide_legend(nrow = 5, byrow = TRUE)) +
  scale_x_continuous(labels = comma)  # (etiquetas numéricas; puedes quitar 'comma' si prefieres)

g2


# ==============================================================================
# ---- Analizar mortalidad por cáncer de mama ----
# ==============================================================================
# ---- Carga de librerías ----
library(data.table)
library(ggplot2)
library(viridis)

# ---- Importar y filtrar datos ----
def_full <- readRDS("datos_consolidados.rds")
poblaciones_long <- readRDS("poblaciones_consolidadas.rds")
setkey(def_full, causa)
ca <- def_full["C50"][nomsexo == "Mujer"]

# ---- Ver tasa bruta ----
muertes_ca <- ca[, .(cuenta = sum(cuenta, na.rm = TRUE)), keyby = .(ano, provres, nomprov)]

pob_total_fem <- poblaciones_long[nomsexo == "Mujer",
                                  .(pob_total = sum(Total, na.rm = TRUE)),
                                  keyby = .(provres, ano = Año)]

tasas_ca <- muertes_ca[pob_total_fem, on = .(provres, ano), nomatch = NULL]
tasas_ca[, tasa_bruta := cuenta / pob_total * 100000]

g0 <- ggplot(tasas_ca) +
  aes(x = ano, y = tasa_bruta) +
  geom_line() +
  facet_wrap(~nomprov)

# ---- Ver por grupo etario ----
plot_data <- ca[!grupo_edad %ilike% "^[0-2]" &
                  ! is.na(grupo_edad) & 
                  provres == 1]

total <- plot_data[,
                   sum(cuenta)]


g1 <- ggplot(plot_data) +
  aes(x = ano, y = tasa) +
  geom_line(linewidth = .5, colour = "steelblue", alpha = 0.4) + 
  geom_smooth(
    method = "loess", 
    se = FALSE,          
    span = 0.5,          # Grado de suavizado (ajustar entre 0.3 y 0.7)
    colour = "firebrick", 
    linewidth = 1
  ) +
  facet_wrap(~grupo_edad, scales = "free_y") + # Usamos free_y para ver tendencia interna
  theme_minimal() +
  labs(
    title = "Tasas específicas de mortalidad por cáncer de mama según grupo de edad",
    subtitle = sprintf("República Argentina, período %d-%d, n = %s \nTendencia suavizada mediante LOESS",
                       min(ca$ano, na.rm = T),
                       max(ca$ano, na.rm = T),
                       format(total, big.mark = ".", decimal.mark = ",")),
    caption = "Fuente: Elaboración propia a partir de datos de DEIS e INDEC",
    y = "Defunciones cada 100.000 mujeres",
    x = "Año"
  )
g1


g2 <- ca[!grupo_edad %ilike% "^[0-2]" &
           ! is.na(grupo_edad)] |> 
  ggplot() +
  aes(x = ano, y = reorder(nomprov, tasa), fill = tasa) +
  geom_tile() +
  facet_wrap(~grupo_edad) +
  labs(
    title = "Tasas específicas de mortalidad por cáncer de mama según grupo de edad por provincia",
    subtitle = sprintf("República Argentina, período %d-%d, n = %s",
                       min(ca$ano, na.rm = T),
                       max(ca$ano, na.rm = T),
                       format(total, big.mark = ".", decimal.mark = ",")),
    caption = "Fuente: Elaboración propia a partir de datos de DEIS e INDEC",
    y = "Defunciones cada 100.000 mujeres",
    x = "Año"
  ) +
  theme_minimal() +
  viridis::scale_fill_viridis()
g2
# plotly::ggplotly(g1)  

plot_data <- ca[!grupo_edad %ilike% "^[0-2]" & 
                  !is.na(grupo_edad) & 
                  provres %in% c(1, 58, 6, 2)]

g3 <- ggplot(plot_data) +
  aes(x = ano, y = tasa, colour = nomprov, group = nomprov) +
  # Líneas originales con mucha transparencia para dejar protagonismo a la tendencia
  geom_line(linewidth = .4, alpha = 0.1) + 
  geom_smooth(
    method = "loess", 
    se = FALSE,          
    span = 0.5, 
    linewidth = 1.2
  ) +
  facet_wrap(~grupo_edad, scales = "free_y") +
  theme_minimal() +
  scale_color_manual(values = c("República Argentina (total país)" = "grey60", 
                                "Neuquén" = "firebrick",
                                "Buenos Aires" = "steelblue",
                                "Ciudad Aut. de Buenos Aires" = "coral")) +
  labs(
    title = "Comparativa de Mortalidad Específica: Provincias seleccionadas vs. Total País",
    subtitle = "Tasas suavizadas (LOESS) por grupo de edad",
    y = "Defunciones cada 100.000 mujeres",
    x = "Año",
    colour = "Jurisdicción"
  ) +
  theme(legend.position = "bottom")

print(g3)

library(data.table)
library(ggplot2)
library(echarts4r)
# ---- Tasa bruta ----
plot_data <- ca[!grupo_edad %ilike% "^[0-2]" &
                  ! is.na(grupo_edad) & 
                  provres == 1]

total <- plot_data[,
                   sum(cuenta)]

colores_eje <- ifelse(levels(reorder(tasas_consolidadas$nomprov, tasas_consolidadas$tasa_bruta)) %ilike% "Argentina", 
                      "red", "black")
g0 <- ggplot(tasas_consolidadas) +
  aes(x = ano, y = reorder(nomprov, tasa_bruta), fill = tasa_bruta) +
  geom_tile() +
  viridis::scale_fill_viridis(option = "inferno", direction = -1) +
  labs(title = "Tasa bruta de mortalidad por cáncer de mama según provincia por año",
       subtitle = sprintf("República Argentina, período %d-%d, n = %s",
                          min(ca$ano, na.rm = T),
                          max(ca$ano, na.rm = T),
                          format(total, big.mark = ".", decimal.mark = ",")),
       x = "Año", 
       y = "Tasa bruta de mortalidad por cáncer de mama cada 100.000 mujeres",
       fill = "Tasa bruta") +
  theme_minimal() +
  theme(axis.text.y = element_text(colour = colores_eje)) 


g0

# ---- Ver por grupo etario ----

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
# plotly::ggplotly(g1)  

plot_data2 <- ca[!grupo_edad %ilike% "^[0-2]" & 
                   !is.na(grupo_edad) & 
                   provres %in% c(1, 58, 6, 2)]

g3 <- ggplot(plot_data2) +
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



colores_eje <- ifelse(levels(reorder(tasas_finales$nomprov, tasas_finales$tasa_ajustada)) %ilike% "Argentina", 
                      "red", "black")

g4 <- ggplot(tasas_finales[ano %in% 2005:2024]) +
  aes(x = ano, y = reorder(nomprov, tasa_ajustada), fill = tasa_ajustada) +
  geom_tile() +
  viridis::scale_fill_viridis(option = "inferno", direction = -1) +
  labs(title = "Tasa ajustada de mortalidad por cáncer de mama según provincia por año",
       subtitle = sprintf("República Argentina, período %d-%d, n = %s",
                          min(ca$ano, na.rm = T),
                          max(ca$ano, na.rm = T),
                          format(total, big.mark = ".", decimal.mark = ",")),
       caption = "Ajuste según población mundial estándar de la OMS",
       x = "Año", 
       y = "Tasa ajustada de mortalidad por cáncer de mama cada 100.000 mujeres",
       fill = "Tasa ajustada") +
  theme_minimal() +
  theme(axis.text.y = element_text(colour = colores_eje))


g5 <- ggplot(tasas_finales[provres %in% c(1, 58, 2, 6)]) +
  aes(x = ano, y = tasa_ajustada, colour = nomprov, group = nomprov) +
  geom_line(linewidth = .4, alpha = 0.2) + 
  geom_smooth(
    method = "loess", 
    se = FALSE,          
    span = 0.5, 
    linewidth = 1.2
  ) +
  theme_minimal() +
  scale_color_viridis_d(option = "D") +
  labs(title = "Comparativa de mortalidad estandarizada según jurisdicción por año",
       subtitle = sprintf("República Argentina, período %d-%d, n = %s
                           \nTendencia suavizada mediante LOESS",
                          min(ca$ano, na.rm = T),
                          max(ca$ano, na.rm = T),
                          format(total, big.mark = ".", decimal.mark = ",")),
       caption = "Ajuste según población mundial estándar de la OMS
       \nFuente: Elaboración propia a partir de datos de DEIS e INDEC",
       x = "Año", 
       y = "Tasa ajustada de mortalidad por cáncer de mama cada 100.000 mujeres",
       colour = "Jurisdicción") +
  theme(legend.position = "bottom") 



g6 <- ggplot(tasas_finales[provres %in% c(1, 58, 62, 94, 26, 78, 42)]) +
  aes(x = ano, y = tasa_ajustada, colour = nomprov, group = nomprov) +
  geom_line(linewidth = .4, alpha = 0.2) + 
  geom_smooth(
    method = "loess", 
    se = FALSE,          
    span = 0.5, 
    linewidth = 1.2
  ) +
  scale_color_viridis_d(option = "D") +
  theme_minimal() +
  labs(title = "Comparativa de mortalidad estandarizada por cáncer de mama según jurisdicción por año",
       subtitle = sprintf("Región Patagonia, período %d-%d, n = %s
                           \nTendencia suavizada mediante LOESS",
                          min(ca$ano, na.rm = T),
                          max(ca$ano, na.rm = T),
                          format(total, big.mark = ".", decimal.mark = ",")),
       caption = "Ajuste según población mundial estándar de la OMS
       \nFuente: Elaboración propia a partir de datos de DEIS e INDEC",
       x = "Año", 
       y = "Tasa ajustada de mortalidad por cáncer de mama cada 100.000 mujeres",
       colour = "Jurisdicción") +
  theme(legend.position = "bottom")

plot_data3 <- prop_mort[provres %in% c(1, 58)]

g7 <- ggplot(plot_data3) +
  aes(x = ano, y = prop_sobre_total, colour = nomprov, group = nomprov) +
  geom_line(linewidth = .4, alpha = 0.2) + 
  geom_smooth(
    method = "loess", 
    se = FALSE,          
    span = 0.5, 
    linewidth = 1.2
  ) +
  scale_color_viridis_d(option = "D") +
  theme_minimal() +
  labs(title = "Comparativa de mortalidad proporcional por cáncer de mama según jurisdicción por año",
       subtitle = sprintf("Jurisdicciones seleccionadas, período %d-%d, mujeres entre 40 y 49 años
                           \nTendencia suavizada mediante LOESS",
                          min(ca$ano, na.rm = T),
                          max(ca$ano, na.rm = T)
                          # format(plot_data3[, sum(total_muertes), nomprov][1,V1], big.mark = ".", decimal.mark = ",")
                          ),
       caption = "Mortalidad proporcional
       \nFuente: Elaboración propia a partir de datos de DEIS e INDEC",
       x = "Año", 
       y = "Porcentaje de defunciones debidas a cáncer de mama sobre las totales",
       colour = "Jurisdicción") +
  theme(legend.position = "bottom") 


library(data.table)
library(htmltools)
# ---- Objetos----  
ano_min <- resumen_epidemiologico[, min(ano)]

ano_max <- resumen_epidemiologico[, max(ano)]

total <- resumen_epidemiologico[provres == 1, sum(muertes_mama_total)] |> 
  format(big.mark = ".", decimal.mark = ",")

# ---- Mortalidad cruda ----
hm_cruda <- crear_heatmap_echarts_mama(datos = resumen_epidemiologico,
                                              variable_tasa = "tasa_bruta",
                                              titulo = "Tasa bruta de mortalidad por cáncer de mama en mujeres",
                                              subtitulo = sprintf("República Argentina, período %d-%d, n = %s
        \nFuente: Elaboración propia a partir de datos de DEIS e INDEC",
                                                                          ano_min,
                                                                          ano_max,
                                                                          total), 
                                       nombre_tasa = "Tasa bruta"
                                       )



lineas_cruda <- crear_grafico_lineas_mama(datos_lineas = resumen_epidemiologico,
                                          variable_tasa = "tasa_bruta",
                                          valor_label = "Tasa bruta",
                                          titulo = "Tasa bruta de mortalidad por cáncer de mama en mujeres", 
                                          subtitulo = sprintf("República Argentina, período %d-%d, n = %s
        \nFuente: Elaboración propia a partir de datos de DEIS e INDEC",
                                                                          ano_min,
                                                                          ano_max,
                                                                          total
                                                              )
                                          )

mapa_cruda <- crear_mapa_interactivo_mama(datos = resumen_epidemiologico, 
                            variable_tasa = "tasa_bruta",
                            titulo = "Distribución geográfica", 
                            subtitulo = "Tasa bruta de mortalidad por cáncer de mama cada 100-k mujeres",
                            etiqueta_tasa = "Tasa bruta"
                            )

# ---- Mortalidad ajustada ----
hm_ajustada <- crear_heatmap_echarts_mama(datos = resumen_epidemiologico,
                                       variable_tasa = "tasa_ajustada",
                                       titulo = "Tasa ajustada de mortalidad por cáncer de mama en mujeres",
                                       subtitulo = sprintf("República Argentina, período %d-%d, n = %s
        \nFuente: Elaboración propia a partir de datos de DEIS e INDEC",
                                                           ano_min,
                                                           ano_max,
                                                           total))



lineas_ajustada <- crear_grafico_lineas_mama(datos_lineas = resumen_epidemiologico,
                                          variable_tasa = "tasa_ajustada", 
                                          titulo = "Tasa ajustada de mortalidad por cáncer de mama en mujeres", 
                                          subtitulo = sprintf("República Argentina, período %d-%d, n = %s
        \nFuente: Elaboración propia a partir de datos de DEIS e INDEC",
                                                              ano_min,
                                                              ano_max,
                                                              total
                                          )
)

mapa_ajustada <- crear_mapa_interactivo_mama(datos = resumen_epidemiologico, 
                                          variable_tasa = "tasa_ajustada",
                                          titulo = "Distribución geográfica", 
                                          subtitulo = "Tasa ajustada de mortalidad por cáncer de mama cada 100-k mujeres",
                                          etiqueta_tasa = "Tasa ajustada"
)


# ---- Mortalidad proporcional ----
total_prop <- def_full[nomsexo == "Mujer" & grupo_edad %in% c("40-44", "45-49") &causa=="C50", sum(cuenta)] |> 
  format(big.mark = ".", decimal.mark = ",")

hm_proporcional <- crear_heatmap_echarts_mama(datos = resumen_epidemiologico,
                                       variable_tasa = "prop_mama_sobre_total_40_49",
                                       denominador = "%",
                                       titulo = "Mortalidad proporcional por cáncer de mama en mujeres de 40 a 49 años",
                                       subtitulo = sprintf("República Argentina, período %d-%d, n = %s
        \nFuente: Elaboración propia a partir de datos de DEIS e INDEC",
                                                           ano_min,
                                                           ano_max,
                                                           total_prop), 
                                       nombre_tasa = "Mortalidad proporcional 40-49"
)



lineas_proporcional <- crear_grafico_lineas_mama(datos_lineas = resumen_epidemiologico,
                                          variable_tasa = "prop_mama_sobre_total_40_49",
                                          valor_label = "Mortalidad proporcional 40-49",
                                          titulo = "Mortalidad proporcional por cáncer de mama en mujeres de 40 a 49 años",
                                          subtitulo = sprintf("República Argentina, período %d-%d, n = %s
        \nFuente: Elaboración propia a partir de datos de DEIS e INDEC",
                                                              ano_min,
                                                              ano_max,
                                                              total_prop)
                                          )

mapa_proporcional <- crear_mapa_interactivo_mama(datos = resumen_epidemiologico, 
                                          variable_tasa = "prop_mama_sobre_total_40_49",
                                          titulo = "Distribución geográfica", 
                                          subtitulo = "Mortalidad proporcional por cáncer de mama en mujeres de 40 a 49 años",
                                          etiqueta_tasa = "Mortalidad proporcional 40-49"
)
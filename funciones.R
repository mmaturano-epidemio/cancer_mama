# =============================================================================
# Funciones para producir gráficos
# =============================================================================

# 1. Cargar librerías ---------------------------------------------------------
if (!require(pacman)) install.packages("pacman")
pacman::p_load(data.table, echarts4r, htmlwidgets, plotly, crosstalk, 
               htmltools, sf, rnaturalearth, geojsonsf, dplyr)

# 2. Función para crear heatmap interactivo (Echarts) -------------------------
crear_heatmap_echarts_mama <- function(datos, 
                                       titulo = "Mortalidad por Cáncer de Mama", 
                                       subtitulo = "Evolución histórica de tasas",
                                       nombre_pais = "República Argentina (total país)",
                                       variable_tasa = "tasa_ajustada",
                                       nombre_tasa = "Tasa ajustada",
                                       denominador = " x 100k mujeres",
                                       height = "600px", width = "100%",
                                       etiqueta = "Muertes") {
  
  # Copiar y renombrar variables para homogeneizar el uso interno
  datos_plot <- copy(as.data.table(datos)) 
  setnames(datos_plot, "nomprov", "pvcia")
  setnames(datos_plot, variable_tasa, "tasa_plot")
  
  datos_plot[, pvcia := as.character(pvcia)]
  
  # Ordenar el eje Y según el promedio histórico de la tasa seleccionada
  orden_provincias <- datos_plot[, .(tasa_promedio = mean(tasa_plot, na.rm = TRUE)), 
                                 by = pvcia][order(-tasa_promedio), pvcia]
  
  # Construir el tooltip con los datos específicos de mama
  datos_plot[, tooltip := sprintf(
    "<b>%s</b><br>Año: %s<br>%s: %.2f%s <br>%s: %d<br>Pob. Femenina: %d",
    pvcia, ano, nombre_tasa, tasa_plot, denominador, etiqueta, muertes_mama_total, pob_total_fem
  )]
  
  datos_plot[, ano := as.character(ano)]
  
  p <- datos_plot |>
    e_charts(ano, height = height, width = width) |>
    e_heatmap(pvcia, tasa_plot, bind = tooltip) |>
    e_visual_map(
      tasa_plot,
      inRange = list(color = c("#FDE725", "#21918c", "#3b528b", "#440154")),
      calculable = TRUE,
      orient = "horizontal",
      left = "center",
      bottom = 20
    ) |>
    e_title(text = titulo, subtext = subtitulo) |>
    e_x_axis(
      type = "category",
      data = unique(datos_plot$ano),
      axisLabel = list(rotate = 45, interval = 0, fontSize = 11),
      name = "Año",
      nameLocation = "middle",
      nameGap = 35
    ) |>
    e_y_axis(
      type = "category",
      data = orden_provincias,
      axisLabel = list(
        interval = 0,
        fontWeight = htmlwidgets::JS(sprintf("
          function(value) {
            return value === '%s' ? 'bold' : 'normal';
          }
        ", nombre_pais)),
        color = htmlwidgets::JS(sprintf("
          function(value) {
            return value === '%s' ? '#440154' : '#333333';
          }
        ", nombre_pais)),
        fontSize = 11
      ),
      name = "Jurisdicción",
      nameLocation = "middle",
      nameGap = 50
    ) |>
    e_tooltip(formatter = htmlwidgets::JS("function(params) { return params.name; }")) |>
    e_grid(left = "25%", right = "8%", bottom = "25%", top = "15%") |>
    e_datazoom(type = "slider", orient = "bottom") |>
    e_theme("walden")
  
  return(p)
}


# 3. Función para gráfico de líneas con Crosstalk (Plotly) --------------------
crear_grafico_lineas_mama <- function(datos_lineas, 
                                      titulo      = "Tendencia de Mortalidad",
                                      subtitulo   = "Seleccione para comparar",
                                      variable_tasa = "tasa_ajustada",
                                      y_label     = "Tasa x 100.000 Mujeres",
                                      valor_label = "Tasa Ajustada") {
  
  datos_plot <- copy(as.data.frame(datos_lineas))
  datos_plot$ano <- as.numeric(as.character(datos_plot$ano))
  
  # Homogeneizar nombres
  names(datos_plot)[names(datos_plot) == "nomprov"] <- "pvcia"
  names(datos_plot)[names(datos_plot) == variable_tasa] <- "tasa_plot"
  
  # Generar ID único
  wrapper_id <- paste0("crosstalk-wrap-", sample(10000:99999, 1))
  
  shared <- SharedData$new(datos_plot, group = wrapper_id)
  
  p <- plot_ly(shared, 
               x = ~ano, 
               y = ~tasa_plot, 
               color = ~pvcia, 
               colors = "viridis", 
               type = 'scatter', 
               mode = 'lines+markers',
               hoverinfo = 'text',
               text = ~paste0("<b>", pvcia, "</b><br>",
                              "Año: ", ano, "<br>",
                              valor_label, ": ", round(tasa_plot, 2))) %>%
    layout(
      showlegend = FALSE, 
      xaxis = list(title = "Año", gridcolor = "#f0f0f0"),
      yaxis = list(title = y_label, gridcolor = "#f0f0f0"),
      margin = list(t = 50, b = 50, l = 50, r = 20)
    ) %>%
    config(displayModeBar = FALSE)
  
  # Interfaz (Se quitó el filtro de Sexo por ser irrelevante en este dataset)
  div(
    id = wrapper_id,
    class = "crosstalk-wrapper empty-state",
    
    tags$style(HTML(sprintf("
      #%s.empty-state .scatterlayer { display: none !important; }
      #%s.empty-state .js-plotly-plot::after {
        content: 'Seleccione una o más jurisdicciones para visualizar la tendencia';
        position: absolute; top: 50%%; left: 50%%;
        transform: translate(-50%%, -50%%);
        font-size: 16px; color: #7f8c8d; font-family: sans-serif;
        pointer-events: none; background: rgba(255,255,255,0.8);
        padding: 10px 20px; border-radius: 5px;
      }
    ", wrapper_id, wrapper_id))),
    
    div(
      class = "filter-panel",
      style = "display: flex; align-items: flex-end; gap: 15px; flex-wrap: wrap;",
      
      div(style = "flex: 2; min-width: 250px;", 
          tags$label("1. Seleccione Jurisdicciones:"),
          filter_select("prov_sel", NULL, shared, ~pvcia)),
      
      div(style = "padding-bottom: 5px;",
          tags$button("Borrar Selección", 
                      class = "btn btn-outline-danger btn-sm clear-selection-btn",
                      type = "button",
                      style = "cursor: pointer; padding: 6px 12px; border-radius: 4px; border: 1px solid #dc3545; background: white; color: #dc3545; font-weight: bold;"))
    ),
    
    div(class = "chart-panel", style = "position: relative; min-height: 400px;", p),
    
    tags$script(HTML(sprintf("
      $(document).ready(function() {
        setTimeout(function() {
          var wrapper = $('#%s');
          var selectEl = wrapper.find('.selectized')[0];
          if (!selectEl) return;
          var selectize = selectEl.selectize;
          
          function checkSelection() {
            var items = selectize.items;
            if (items && items.length > 0) { wrapper.removeClass('empty-state'); } 
            else { wrapper.addClass('empty-state'); }
          }
          
          selectize.on('change', checkSelection);
          wrapper.find('.clear-selection-btn').on('click', function() {
            selectize.clear(); checkSelection();
          });
        }, 500); 
      });
    ", wrapper_id)))
  )
}

# 4. Función para Mapa Interactivo DEIS (Echarts) -----------------------------
crear_mapa_interactivo_mama <- function(datos, 
                                        titulo = "Distribución Geográfica", 
                                        subtitulo = "",
                                        etiqueta_tasa = "Tasa Ajustada",
                                        variable_tasa = "tasa_ajustada") {
  
  dt <- as.data.table(copy(datos))
  # Excluir el total país (código 1) para mapear
  dt <- dt[!provres %in% c(0, 1)] 
  dt[, provres := as.character(provres)]
  
  # Setear la variable elegida a un nombre estándar interno
  setnames(dt, variable_tasa, "tasa_mapa")
  
  arg_sf <- rnaturalearth::ne_states(country = "argentina", returnclass = "sf") %>%
    st_simplify(dTolerance = 0.01)
  
  codigos <- data.table(
    provres = c("2", "6", "10", "14", "18", "22", "26", "30", "34", "38", "42", "46", "50", "54", "58", "62", "66", "70", "74", "78", "82", "86", "90", "94"),
    name = c("Ciudad Autónoma de Buenos Aires", "Buenos Aires", "Catamarca", "Córdoba", "Corrientes", "Chaco", "Chubut", "Entre Ríos", "Formosa", "Jujuy", "La Pampa", "La Rioja", "Mendoza", "Misiones", "Neuquén", "Río Negro", "Salta", "San Juan", "San Luis", "Santa Cruz", "Santa Fe", "Santiago del Estero", "Tucumán", "Tierra del Fuego")
  )
  
  dt_agg <- merge(dt, codigos, by = "provres", all.x = TRUE)
  geojson <- geojsonsf::sf_geojson(arg_sf)
  
  dt_agg %>%
    group_by(ano) %>%
    e_charts(name, timeline = TRUE) %>%
    e_map_register("mapa_arg", geojson) %>%
    e_map(tasa_mapa, map = "mapa_arg", name = etiqueta_tasa) %>%
    e_visual_map(
      tasa_mapa,
      min = min(dt_agg$tasa_mapa, na.rm = TRUE),
      max = max(dt_agg$tasa_mapa, na.rm = TRUE),
      inRange = list(color = c('#313695', '#abd9e9', '#fee090', '#f46d43', '#a50026'))
    ) %>%
    e_tooltip(formatter = htmlwidgets::JS(sprintf("
      function(params) {
        if(params.value) {
          return '<b>' + params.name + '</b><br>%s: ' + parseFloat(params.value).toFixed(2);
        }
        return params.name;
      }
    ", etiqueta_tasa))) %>%
    e_title(text = titulo, subtext = subtitulo) %>%
    e_timeline_opts(autoPlay = FALSE)
}

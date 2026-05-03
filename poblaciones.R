# ==============================================================================
# ---- Obtener poblaciones ----
# ==============================================================================
# ---- Carga de librerías ----
library(readxl)
library(data.table)

# ---- Función para parsear poblaciones por año y jurisdicción ---- 
obtener_año <- function(jurisdicción = "total",
                        año = 2022){
  require(readxl)
  require(data.table)
  libro <- ifelse(año >= 2022, 
                  "./poblaciones/proyecciones_jurisdicciones_2022_2040_c2.xlsx",
                  "./poblaciones/c2_proyecciones_prov_2010_2040.xls")
  
  nombres_reales <- excel_sheets(libro)
  sheet_names <- gsub("SANTE FE", "SANTA FE", nombres_reales)
  
  col_start_num <- fcase(año >= 2022, 2 + (año - 2022) * 4,
                         año %in% 2010:2015, 2 + (año - 2010) * 4,
                         default = 2 + (año - 2016) * 4)
  
  col_start <- LETTERS[col_start_num]
  col_end <- LETTERS[col_start_num + 2]
  row_start <- ifelse(año %in% 2016:2021, 32, 4)
  row_end <- ifelse(año >= 2022, row_start + 23, row_start + 24)
  
  indice <- which(sheet_names %ilike% jurisdicción)
  nom_jurisidicción <- sub(pattern = ".*-", replacement = "", x = sheet_names[indice])
  sheet_original <- nombres_reales[indice]
  
  defunciones <- suppressWarnings(
    read_excel(path = libro,
               sheet = sheet_original, 
               range = paste0(col_start, row_start, ":", col_end, row_end)))
  
  if (año >= 2022) {
    grupo <- rbind("Total", " ",
                   suppressWarnings(
                     read_excel(path = libro,
                                sheet = sheet_original, # <-- CAMBIO AQUÍ
                                range = "A6:A27")))
  } else {
    grupo <- rbind(" ", "Total",
                   suppressWarnings(
                     read_excel(path = libro,
                                sheet = sheet_original, # <-- CAMBIO AQUÍ
                                range = "A6:A28")))
  }
  
  names(grupo) <- "Grupo"
  df <- cbind(grupo, defunciones)
  df$Año <- año
  df$Jurisdicción <- nom_jurisidicción
  
  if(año >= 2022) {dt <- setDT(df[-2, ])} else {
    dt <- setDT(df[-c(1,3),])
  }
  
  return(dt)
}

# ---- Importar datos y crear dataset unificado ----

jurisdicciones <- excel_sheets(
  "./poblaciones/proyecciones_jurisdicciones_2022_2040_c2.xlsx")[-c(1:2)]  # excluir índice y notas

años <- 2010:2025


lista_por_año <- lapply(años, function(ano) {
  cat("\nProcesando año:", ano, "\n")
  
  datos_año <- lapply(jurisdicciones, function(juris) {
    nombre <- sub(".*-", "", juris)
    
    cat("  -", nombre, "\n")
    
    tryCatch({
      obtener_año(jurisdicción = nombre,
                  año = ano)
    }, error = function(e) {
      cat("    Error:", e$message, "\n")
      return(NULL)
    })
  })
  
  rbindlist(datos_año, fill = TRUE)
})

poblacion_total <- rbindlist(lista_por_año, fill = TRUE)

saveRDS(poblacion_total, "./poblaciones/poblacion_total_10_25.rds")

# ---- Hacer retroproyecciones con método exponencial para período 2005-2009 ----

# Función de retroproyección (exponencial, por separado para cada sexo)

retroproyectar_exp <- function(dt_ajuste, anios_pred) {
  # dt_ajuste YA DEBE ser solo los años 2010-2015
  modelo <- lm(log(poblacion) ~ Año, data = dt_ajuste)
  pred_log <- predict(modelo, newdata = data.frame(Año = anios_pred))
  pred <- round(exp(pred_log))
  return(data.table(Año = anios_pred, poblacion = pred))
}

# Preparar datos en formato largo
pob_largo <- poblacion_total[Grupo != "Total", 
                             .(Jurisdicción, Año, Grupo, 
                               Varones, Mujeres, `Ambos sexos`)]

pob_largo <- melt(pob_largo,
                  id.vars = c("Jurisdicción", "Año", "Grupo"),
                  measure.vars = c("Varones", "Mujeres", "Ambos sexos"),
                  variable.name = "Sexo",
                  value.name = "poblacion")

# Años a predecir
anios_retro <- 2005:2009

# Extraer datos base (2010-2015) - esto es lo que va a la función
pob_base <- pob_largo[Año %in% 2010:2015]

# Verificar que tenemos datos
stopifnot(nrow(pob_base) > 0)

# Aplicar retroproyección a cada combinación
pob_retro <- pob_base[, {
  retroproyectar_exp(.SD, anios_retro)
}, by = .(Jurisdicción, Grupo, Sexo)]

# Unir datos observados (2010-2025) con retroproyectados (2005-2009)
pob_sexo_completa <- rbind(
  pob_largo[Año >= 2010],  # datos reales a partir de 2010
  pob_retro,               # datos estimados 2005-2009
  fill = TRUE
)

setorder(pob_sexo_completa, Jurisdicción, Grupo, Sexo, Año)

# Reconstruir la columna Ambos sexos
pob_ancho <- dcast(pob_sexo_completa,
                   Jurisdicción + Grupo + Año ~ Sexo,
                   value.var = "poblacion")

# Para años retroproyectados, crear Ambos sexos como suma
pob_ancho[Año < 2010, `Ambos sexos` := Varones + Mujeres]

# Reordenar columnas
setcolorder(pob_ancho, c("Jurisdicción", "Año", "Grupo", 
                         "Ambos sexos", "Varones", "Mujeres"))
setorder(pob_ancho, Jurisdicción, Grupo, Año)

# str(pob_ancho)

saveRDS(pob_ancho, "./poblaciones/poblacion_proyectada_05_25.rds")

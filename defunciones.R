# ==============================================================================
# ---- Obtener defunciones ----
# ==============================================================================
# ---- Carga de librerías ----
library(data.table)
library(readxl)
library(janitor)

# ---- Importar datos ----
list.files(path = "./defunciones")

archivos <- list.files(path = "./defunciones", 
                       pattern = "^defweb", 
                       full.names = TRUE)

lista <- lapply(archivos, function(file) {
  # Extraer año del nombre del archivo
  año <- as.integer(gsub("./defunciones/defweb|.csv|_0", "", file))
  # Leer el archivo
  dt <- fread(file, encoding = "Latin-1")
  # Agregar columna de año
  dt[, año := año]
  return(dt)
})

# Combinar todos
datos <- rbindlist(lista) |> clean_names()

# ---- Limpiar datos ----
tablas <- excel_sheets("./defunciones/descdef1.xlsx")
provres <- read_excel("./defunciones/descdef1.xlsx", sheet = 2) |> 
  as.data.table() |> 
  janitor::clean_names()
provres[, codigo := as.integer(codigo)]

sexo <- read_excel("./defunciones/descdef1.xlsx", sheet = 3) |> 
  as.data.table() |> 
  janitor::clean_names()
sexo[, codigo := as.integer(codigo)]

codmuer <- read_excel("./defunciones/descdef1.xlsx", sheet = 4) |> 
  as.data.table() |> 
  janitor::clean_names()

datos[, ano := ano + 2000]

datos[provres, nomprov := i.valor, on = .(provres = codigo)]
datos[sexo, nomsexo := i.valor, on = .(sexo = codigo)]
datos[codmuer, nomcausa := i.valor, on = .(causa = codigo)]


saveRDS(datos, "./defunciones/defunciones_totales.rds")


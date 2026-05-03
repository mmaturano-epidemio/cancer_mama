# ==============================================================================
# ---- Analizar mortalidad ----
# ==============================================================================
# ---- Carga de librerías ----
library(data.table)
library(ggplot2)

# ---- Importar datos ----
defunciones <- readRDS("./defunciones/defunciones_totales.rds")
poblaciones <- readRDS("./poblaciones/poblacion_proyectada_05_25.rds")
poblaciones <- poblaciones[Año %in% unique(defunciones$ano)]

# ---- Adecuar datos ----
# Coincidir grupos etarios
poblaciones[Grupo %ilike% "^0-|^5-", Grupo := "0-9"]
poblaciones[Grupo %ilike% "^8|^9|^100", Grupo := "80+"]
poblaciones <- poblaciones[, .(`Ambos sexos` = sum(`Ambos sexos`),
                               Varones = sum(Varones),
                               Mujeres = sum(Mujeres)),
                           by = .(Jurisdicción, Año, Grupo)]

defunciones[, grupo_edad := fcase(
  grupedad %ilike% "01\\.0 a 6 d|01_Menor de 1|02\\.7 a 27 d|03\\.28 d|031\\. Menor|04\\.1 a|05\\.2 a|06\\.3 a|07\\.4 a", "0-9",
  grupedad %ilike% "02_1 a 9|08\\.5 a 9", "0-9",
  grupedad %ilike% "03_10 a 14|09\\.10 a 14", "10-14",
  grupedad %ilike% "04_15 a 19|10\\.15 a 19", "15-19",
  grupedad %ilike% "05_20 a 24|11\\.20 a 24", "20-24",
  grupedad %ilike% "06_25 a 29|12\\.25 a 29", "25-29",
  grupedad %ilike% "07_30 a 34|13\\.30 a 34", "30-34",
  grupedad %ilike% "08_35 a 39|14\\.35 a 39", "35-39",
  grupedad %ilike% "09_40 a 44|15\\.40 a 44", "40-44",
  grupedad %ilike% "10_45 a 49|16\\.45 a 49", "45-49",
  grupedad %ilike% "11_50 a 54|17\\.50 a 54", "50-54",
  grupedad %ilike% "12_55 a 59|18\\.55 a 59", "55-59",
  grupedad %ilike% "13_60 a 64|19\\.60 a 64", "60-64",
  grupedad %ilike% "14_65 a 69|20\\.65 a 69", "65-69",
  grupedad %ilike% "15_70 a 74|21\\.70 a 74", "70-74",
  grupedad %ilike% "16_75 a 79|22\\.75 a 79", "75-79",
  grupedad %ilike% "23\\.80 a 84|17_80 y m|24\\.85 y m", "80+",
  grupedad %ilike% "25\\.|99_", NA_character_,
  default = NA_character_
)]

grupos <- poblaciones$Grupo |> unique() |> sort()
poblaciones[, Grupo := factor(Grupo, levels = grupos)]
defunciones[, grupo_edad := factor(grupo_edad, levels = grupos)]

# Coincidir sexo
poblaciones_long <- melt(poblaciones, 
                         measure.vars = c("Ambos sexos", "Varones", "Mujeres"), 
                         variable.name = "nomsexo", 
                         value.name = "Total")

poblaciones_long[, nomsexo := fcase(nomsexo %ilike% "Mujeres", "Mujer",
                                    nomsexo %ilike% "Varones", "Varón",
                                    nomsexo %ilike% "Ambos", "Ambos sexos",
                                    default = NA)]

# Coincidir jurisdicciones
poblaciones_long[, provres := fcase(
  Jurisdicción == "BUENOS AIRES", 6,
  Jurisdicción == "CABA", 2,
  Jurisdicción == "CATAMARCA", 10,
  Jurisdicción == "CHACO", 22,
  Jurisdicción == "CHUBUT", 26,
  Jurisdicción == "CÓRDOBA", 14,
  Jurisdicción == "CORRIENTES", 18,
  Jurisdicción == "ENTRE RÍOS", 30,
  Jurisdicción == "FORMOSA", 34,
  Jurisdicción == "JUJUY", 38,
  Jurisdicción == "LA PAMPA", 42,
  Jurisdicción == "LA RIOJA", 46,
  Jurisdicción == "MENDOZA", 50,
  Jurisdicción == "MISIONES", 54,
  Jurisdicción == "NEUQUÉN", 58,
  Jurisdicción == "RÍO NEGRO", 62,
  Jurisdicción == "SALTA", 66,
  Jurisdicción == "SAN JUAN", 70,
  Jurisdicción == "SAN LUIS", 74,
  Jurisdicción == "SANTA CRUZ", 78,
  Jurisdicción == "SANTA FE", 82,
  Jurisdicción == "SANTIAGO DEL ESTERO", 86,
  Jurisdicción == "TIERRA DEL FUEGO", 94,
  Jurisdicción == "TUCUMÁN", 90,
  Jurisdicción == "TOTAL DEL PAÍS", 1,
  default = NA_character_
)]

# Construir tasas
defunciones[poblaciones_long, poblacion := i.Total, 
            on = .(nomsexo, provres, grupo_edad = Grupo, ano = Año)]

setkey(defunciones, provres)
defunciones <- defunciones[!provres %in% 98:99]
defunciones[, tasa := cuenta / poblacion * 100000]
total_pais <- defunciones[, .(cuenta = sum(cuenta),
                              provres = 1,
                              nomprov = "República Argentina (total país)"), 
                          .(sexo, causa, mat, grupedad, ano, nomsexo,
                            nomcausa, grupo_edad)]
total_pais[, `:=`(provres = 1,
                  nomprov = "República Argentina (total país)")]


pob_nac <- poblaciones_long[provres != 1, .(Total = sum(Total, na.rm = TRUE)), 
                            keyby = .(nomsexo, ano = Año, grupo_edad = Grupo)]

total_pais[pob_nac, poblacion := i.Total, on = .(nomsexo, ano, grupo_edad)]
total_pais[, tasa := cuenta / poblacion * 100000]

def_full <- rbind(defunciones, total_pais)
nomjur <- c("República Argentina (total país)", 
            def_full[provres != 1, sort(unique(nomprov))])
def_full[, nomprov := factor(nomprov, levels = nomjur)]

saveRDS(def_full, "datos_consolidados.rds")
saveRDS(poblaciones_long, "poblaciones_consolidadas.rds")

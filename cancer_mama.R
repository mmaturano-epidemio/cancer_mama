# ==============================================================================
# ---- Analizar mortalidad por cáncer de mama ----
# ==============================================================================
# ---- Carga de librerías ----
library(data.table)

# ==============================================================================
# 1. IMPORTAR Y PREPARAR DATOS
# ==============================================================================
def_full <- readRDS("datos_consolidados.rds")
def_full[, region := fcase(
  nomprov %ilike% "jujuy|salta|tucum|catamar|santiago|rioja", "NOA",
  nomprov %ilike% "formosa|chaco|misiones|corriente|entre", "NEA",
  nomprov %ilike% "rdoba|santa f|buenos|caba|buenos", "Pampeana",
  nomprov %ilike% "mendoza|luis|juan|juán", "Cuyo",
  nomprov %ilike% "cruz|neuq|negro|chubut|fuego|pampa", "Patagonia",
  nomprov %ilike% "argen", "República Argentina (total país)",
  default = "Otro país o sin especificar"
)]

setkey(def_full, causa)
ca <- def_full["C50"][nomsexo == "Mujer" & !is.na(grupo_edad)]

# Cargar poblaciones y población estándar OMS
poblaciones_long <- readRDS("poblaciones_consolidadas.rds")
pob_estandar <- readRDS("poblacion_estandar.rds")
setnames(pob_estandar, "Grupo.de.edad", "grupo_edad")
pob_estandar[, peso := Total / sum(Total)]

# ==============================================================================
# 2. CÁLCULO DE POBLACIÓN TOTAL DE MUJERES (TODOS LOS GRUPOS ETARIOS)
# ==============================================================================
# Esto incluye TODOS los grupos de edad, no solo los que tienen casos de cáncer
pob_total_mujeres <- poblaciones_long[nomsexo == "Mujer", 
                                      .(pob_total_fem = sum(Total, na.rm = TRUE)), 
                                      by = .(ano = Año, provres, nomprov = Jurisdicción)]

# ==============================================================================
# 3. CÁLCULO DE TASAS BRUTAS Y AJUSTADAS
# ==============================================================================
# Unir pesos de población estándar OMS
ca_est <- merge(ca, 
                pob_estandar[, .(grupo_edad, peso)], 
                by = "grupo_edad", 
                all.x = TRUE)

# Calcular muertes y tasa ajustada por edad
muertes_ca <- ca_est[, .(
  muertes_mama_total = sum(cuenta, na.rm = TRUE),
  tasa_ajustada = sum(tasa * peso, na.rm = TRUE)
), keyby = .(ano, provres, nomprov)]

# Hacer merge con población total de mujeres
tasas_consolidadas <- merge(
  muertes_ca,
  pob_total_mujeres,
  by = c("ano", "provres"),
  all.x = TRUE
)

# Calcular tasa bruta con población total correcta
tasas_consolidadas[, tasa_bruta := (muertes_mama_total / pob_total_fem) * 100000]

# Mantener solo las columnas relevantes y el nombre de provincia correcto
tasas_consolidadas[, nomprov := nomprov.x]
tasas_consolidadas[, c("nomprov.x", "nomprov.y") := NULL]

# ==============================================================================
# 4. CÁLCULO DE MORTALIDAD PROPORCIONAL (Grupo específico 40-49 años)
# ==============================================================================
def_objetivo <- def_full[nomsexo == "Mujer" & grupo_edad %in% c("40-44", "45-49")]

prop_mort <- def_objetivo[, .(
  muertes_40_49_todas = sum(cuenta, na.rm = TRUE),
  muertes_40_49_cancer = sum(cuenta[causa %ilike% "^C"], na.rm = TRUE),
  muertes_40_49_mama = sum(cuenta[causa == "C50"], na.rm = TRUE)
), by = .(ano, provres, nomprov)]

# Calcular proporciones específicas
prop_mort[, prop_mama_sobre_total_40_49 := round((muertes_40_49_mama / muertes_40_49_todas) * 100, 2)]
prop_mort[, prop_mama_sobre_cancer_40_49 := round((muertes_40_49_mama / muertes_40_49_cancer) * 100, 2)]

# ==============================================================================
# 5. UNIFICACIÓN EN UN ÚNICO OBJETO
# ==============================================================================
resumen_epidemiologico <- merge(
  tasas_consolidadas, 
  prop_mort[, .(ano, provres, nomprov, prop_mama_sobre_total_40_49, prop_mama_sobre_cancer_40_49)], 
  by = c("ano", "provres", "nomprov"), 
  all.x = TRUE
)

# Ordenar por jurisdicción y año
setkey(resumen_epidemiologico, provres, ano)

# ==============================================================================
# 6. VERIFICACIÓN
# ==============================================================================
print("Resumen para total país en 2022:")
resumen_epidemiologico[provres == 1 & ano == 2022]

print("\nPrimeras filas del resumen:")
head(resumen_epidemiologico, 10)
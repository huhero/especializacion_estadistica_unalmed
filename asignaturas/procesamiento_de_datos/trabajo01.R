# tituló: trabajo 01
# Integrantes: XXXX
# CC : XXXXX

# lectura de datos
library(readr)
datos <- read_csv("data/Evaluaciones_Agropecuarias_Municipales_EVA_20240310.csv")

# validación si hay filas con problemas 
datos.problems01 <- problems(datos)
datos.problems01

# revisando nombre de las variables
names(datos)

# Estandarizando/ limpiendo nombre de las variables
require(janitor)
datos <- clean_names(datos)

# Revisando structura de los datos
require(tibble) 
glimpse(datos)

# Ajustando tipo de algunas variables
datos$cod_dep <- as.factor(datos$cod_dep)
datos$departamento <- as.factor(datos$departamento)
datos$cod_mun <- as.factor(datos$cod_mun)
datos$municipio <- as.factor(datos$municipio)
datos$grupo_de_cultivo <- as.factor(datos$grupo_de_cultivo)
datos$subgrupo_de_cultivo <- as.factor(datos$subgrupo_de_cultivo)
datos$cultivo <- as.factor(datos$cultivo)
datos$desagregacion_regional_y_o_sistema_productivo <- as.factor(datos$desagregacion_regional_y_o_sistema_productivo)
datos$ano <- as.factor(datos$ano)
datos$periodo <- as.factor(datos$periodo)
datos$estado_fisico_produccion <- as.factor(datos$estado_fisico_produccion)
datos$nombre_cientifico <- as.factor(datos$nombre_cientifico)
datos$ciclo_de_cultivo <- as.factor(datos$ciclo_de_cultivo)
datos$area_sembrada_ha <- as.numeric(datos$area_sembrada_ha)
datos$area_cosechada_ha <- as.numeric(datos$area_cosechada_ha)


# Generando algunos estadisticos
summary(datos)

# revisando head y tail 
head(datos)

tail(datos)


# Revisando cuales filas no tienen completos sus datos
vector.datos.nas <- !complete.cases(datos)
datos.nas <- datos[vector.datos.nas,]
datos.nas 

# descripcion de las base de datos:
## 
# La base de datos **RENDIMIENTO DE CULTIVOS EN COLOMBIA POR AÑO** 
# contiene información histórica relacionada con la producción agrícola nacional 
# en el período comprendido entre los años 2007 y 2018. 
# Esta base proporciona datos específicos para distintos actores, 
# con especial énfasis en los productores, para fortalecer los procesos productivos 
# y de comercialización a nivel regional y nacional.
# 
# ### Columnas:
# 
# 1. **cod_dep:**
#   - Tipo: Factor
# - Descripción: Código del departamento al que pertenece la información.


table(datos$cod_dep)

# 2. **departamento:**
#   - Tipo: Factor
# - Descripción: Nombre del departamento al que pertenece la información.

barplot(sort(table(datos$departamento),decreasing = T))

# 3. **cod_mun:**
#   - Tipo: Factor
# - Descripción: Código del municipio al que pertenece la información.

# 4. **municipio:**
#   - Tipo: Factor
# - Descripción: Nombre del municipio al que pertenece la información.

barplot(sort(table(datos$municipio),decreasing = T))

# 5. **grupo_de_cultivo:**
#   - Tipo: Factor
# - Descripción: Grupo al que pertenece el cultivo, como "HORTALIZAS" u otros.

barplot(sort(table(datos$grupo_de_cultivo),decreasing = T))

# 6. **subgrupo_de_cultivo:**
#   - Tipo: Factor
# - Descripción: Subgrupo al que pertenece el cultivo, como "ACELGA" u otros.

barplot(sort(table(datos$subgrupo_de_cultivo),decreasing = T))

# 7. **cultivo:**
#   - Tipo: Factor
# - Descripción: Nombre específico del cultivo, como "ACELGA" u otros.

barplot(sort(table(datos$cultivo),decreasing = T))

# 8. **desagregacion_regional_y_o_sistema_productivo:**
#   - Tipo: Factor
# - Descripción: Desagregación regional y/o sistema productivo al que está asociado el cultivo.

# top 5 sistemas productivos
df.table01 <- data.frame(sort(table(datos$desagregacion_regional_y_o_sistema_productivo),decreasing = T))
df.table01[1:5,]

# 9. **ano:**
#   - Tipo: Factor
# - Descripción: Año al que corresponde la información.
# años con más cultivos reportados
data.frame(sort(table(datos$ano), decreasing = T))

# 10. **periodo:**
#   - Tipo: Factor
# - Descripción: Periodo específico del año, identificado con un código, como "2006B" u otros.
# 
# 11. **area_sembrada_ha:**
#   - Tipo: Numérico (double)
# - Descripción: Área sembrada en hectáreas.

require(dplyr)

summarise(group_by(datos,departamento),
          sum_area_sembrada =  sum(area_sembrada_ha),
          sum_area_cosechada =  sum(area_cosechada_ha))


# 12. **area_cosechada_ha:**
#   - Tipo: Numérico (double)
# - Descripción: Área cosechada en hectáreas.
#
# 13. **produccion_t:**
#   - Tipo: Numérico (double)
# - Descripción: Producción total en toneladas.
# 
# 14. **rendimiento_t_ha:**
#   - Tipo: Numérico (double)
# - Descripción: Rendimiento del cultivo en toneladas por hectárea.
# 
# 15. **estado_fisico_produccion:**
#   - Tipo: Factor
# - Descripción: Estado físico de la producción, como "FRUTO FRESCO" u otros.
barplot(sort(table(datos$estado_fisico_produccion),decreasing = T))
# 16. **nombre_cientifico:**
#   - Tipo: Factor
# - Descripción: Nombre científico del cultivo, como "BETA VULGARIS" u otros.
barplot(sort(table(datos$nombre_cientifico),decreasing = T))
# 17. **ciclo_de_cultivo:**
#   - Tipo: Factor
# - Descripción: Ciclo de cultivo, como "TRANSITORIO" u otros.
barplot(sort(table(datos$ciclo_de_cultivo),decreasing = T))





nombres = c("Marcos","Juan","Isabel","Carmen")
edad = c(21,23,19,20)
genero = c("Masculino","Masculino","Femenino","Femenino")
dataframe01 = data.frame(nombres,edad,genero)

source("asignaturas/procesamiento_de_datos/utils/filtros.R")

filtro_por_genero(dataframe01,"Femenino")
filtro_por_edad(dataframe01,21)

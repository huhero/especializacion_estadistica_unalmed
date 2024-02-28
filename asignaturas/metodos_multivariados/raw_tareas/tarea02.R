# Cargo los datos desde excel original
library(readxl)
Bd.Pruebas.Saber.2015.02 <- read_excel("datos_icfes/saber11_20152.xls", skip = 3)

# identifico los departamentos
unique(saber11_20152$DEPARTAMENTO)

# filtro los datos del departamento del cesar
Bd.Pruebas.Saber.2015.02.cesar <- filter(Bd.Pruebas.Saber.2015.02,DEPARTAMENTO=="CESAR")

# revisso si hay problemas con la BD
problems(Bd.Pruebas.Saber.2015.02.cesar)

# reviso columnas existentes
colnames(Bd.Pruebas.Saber.2015.02.cesar)

# filtro columnas necesarios
Dataframe.Cesar <- Bd.Pruebas.Saber.2015.02.cesar[,c(
  "NOMBREMUNICIPIO","NATURALEZA","JORNADA", # Variables Categoricas
  "PROMLECTURACRITICA","PROMMATEMATICA","PROMSOCIALESYCIUDADANAS", # Varibales numericas
  "PROMCIENCIASNATURALES","PROMINGLES")]


# reviso estructura de lso datos
str(Dataframe.Cesar)

# cambio tipo de variables char a factor
Dataframe.Cesar$NOMBREMUNICIPIO <- as.factor(Dataframe.Cesar$NOMBREMUNICIPIO)
Dataframe.Cesar$NATURALEZA <- as.factor(Dataframe.Cesar$NATURALEZA)
Dataframe.Cesar$JORNADA <- as.factor(Dataframe.Cesar$JORNADA)


# reviso estructura de los datos
str(Dataframe.Cesar)

# reviso estadisticos
summary(Dataframe.Cesar)

# --------------------------------------------------------------------------------
# ---    PUNTO 1
# --------------------------------------------------------------------------------

# defino la matriz X con solo las variables de los valores promedio por tipo de prueba de cada colegio
Matriz.X <- as.matrix(Dataframe.Cesar[,c("PROMLECTURACRITICA","PROMMATEMATICA","PROMSOCIALESYCIUDADANAS",
                     "PROMCIENCIASNATURALES","PROMINGLES")])




# Validacion de tipo y dimension de la matriz X
class(Matriz.X)
dim(Matriz.X)

# Defino el vector  con los pesos asignados
Vector.a <- matrix(c(0.5,0.5,2,0,2))


#  Validacion de tipo y dimension del vector a
class(Vector.a)
dim(Vector.a)


# Vector Y1 de valores con el resultado de la multiplicacion de la matriz X*a
Vector.Y1<-Matriz.X%*%Vector.a

# encabezado y cola
head(Vector.Y1)
tail(Vector.Y1)


# tabla con muestra de los tados mostrando encabezado con los primeros 6 valores y cola con los últimos 6

table01.values <- c(
  head(Vector.Y1),
  tail(Vector.Y1)
)

Df.table01 <- data.frame(table01.values)

colnames(Df.table01) <- ("y1")
Df.table01

# media muestral y varianza muestral
Df.estadisticos.table01.names <- c("Media muestras", "Mediana muestral", "Varianza muestral", 
                        "Desviacion estandar", "Percentil 25", "Percentil 75",
                        "Minimo", "Maximo")
Df.estadisticos.table01.values <- c(
  mean(Df.table01$y1),
  median(Df.table01$y1),
  var(Df.table01$y1),
  sd(Df.table01$y1),
  summary(Df.table01$y1)[2],
  summary(Df.table01$y1)[5],
  min(Df.table01$y1),
  max(Df.table01$y1)
)

Df.estadisticos.table01 <- data.frame(Df.estadisticos.table01.names,Df.estadisticos.table01.values)
colnames(Df.estadisticos.table01) <- c("Medidas","Valores")
Df.estadisticos.table01

# tabla de fecuencia

boxplot(Matriz.Y1)

# Conclusiones Punto 1
# Se observa que la muestra presenta un conjunto de datos que reflejan una distribución centrada en torno a la media y la mediana.
# con un promedio de aproximadamente 258.89 y una mediana de 252.39. Esta similitud entre la media y la mediana sugiere 
# una distribución que podría ser simétrica o cercana a la simetría. Sin embargo, la dispersión de los datos es moderada,
# Como se evidencia en la varianza muestral de 744.55 y la desviación estándar de 27.29. Estos valores indican que los datos tienden 
# a estar relativamente dispersos alrededor de la media


# --------------------------------------------------------------------------------
# ---    PUNTO 2
# --------------------------------------------------------------------------------

# Defino el vector b con los pesos asignados
Vector.b <- as.matrix(c(0,1,2,-1,2))


# Defino el vector c con los pesos asignados
Vector.c <- as.matrix(c(-1,0,2,1,0))

# uno vectores a,b,c para crear matriz D
Matriz.D <- cbind(Vector.a, Vector.b, Vector.c)

# Valido dimesiones de matrices
dim(Matriz.X)
dim(Matriz.D )

# matriz X*d
Matriz.Y2<-Matriz.X%*%Matriz.D 


# cabeza y cola de Y
head(Matriz.Y2)
tail(Matriz.Y2)

# tabla con muestra de los tados mostrando encabezado con los primeros 6 valores y cola con los últimos 6
table02.values <- rbind(
  head(Matriz.Y2),
  tail(Matriz.Y2)
)

table02.values

Df.table02 <- data.frame(table02.values)

colnames(Df.table02) <- c("Y1","Y2","Y3")
Df.table02

# Vector de medias 
Df.table02.medias.names <- c("Promedio Y1", "Promedio Y2", "Promedio Y3")
Df.table02.medias.values <- c(
  mean(Df.table02$Y1),
  mean(Df.table02$Y2),
  mean(Df.table02$Y3)  
)

Df.table03.medias <- data.frame(cbind(Df.table02.medias.names,Df.table02.medias.values))
colnames(Df.table03.medias )<-c("Variables", "Valores")
Df.table03.medias

# Matriz de Var-Cov
var(table02.values)

# Matriz de correlación
cor(table02.values)

# boxplot
boxplot(table02.values)

# se observa que las tres variables tienen diferentes rangos y distribuciones, con la Variable V1 
# mostrando la mayor dispersión y la V3 la menor. Sin embargo, todas muestran una distribución 
# que tiende a ser simétrica o aproximadamente simétrica, con la mediana y la media proporcionando 
# indicadores centrales consistentes. 


# --------------------------------------------------------------------------------
# ---    PUNTO 3
# --------------------------------------------------------------------------------

head(Matriz.X)

# Matriz de correlacion
cor(Matriz.X)

# las variables más con más correlación son:
# 1) PROMSOCIALESYCIUDADANAS + PROMLECTURACRITICA : 0.92
# 2) PROMCIENCIASNATURALES + PROMMATEMATICA : 0.93

head(Matriz.X[,c(3,1,2,4,5)])

# --------------------------------------------------------------------------------
# ---    PUNTO 4
# --------------------------------------------------------------------------------

head(Matriz.Y2)

# Matriz de correlacion
cor(Matriz.Y2)


# las variables más con más correlación son:
# 1) Y2 + Y1 : 0.99
# 2) Y3 + Y1 : 0.91


library(readxl)
saber11_20152 <- read_excel("datos_icfes/saber11_20152.xls", skip = 3)

# identifico los departamentos
unique(saber11_20152$DEPARTAMENTO)

# filtro los datos del departamento del cesar
saber11_20152.cesar <- filter(saber11_20152,DEPARTAMENTO=="CESAR")

# revisso si hay problemas con la BD
problems(saber11_20152.cesar)

# reviso columnas existentes
colnames(saber11_20152.cesar)

# filtro columnas necesarios
datos_filtrados <- saber11_20152.cesar[,c(
  "NOMBREMUNICIPIO","NATURALEZA","JORNADA", # Variables Categoricas
  "PROMLECTURACRITICA","PROMMATEMATICA","PROMSOCIALESYCIUDADANAS", # Varibales numericas
  "PROMCIENCIASNATURALES","PROMINGLES")]

# reviso estructura de lso datos
str(datos_filtrados)

# cambio tipo de variables char a factor
datos_filtrados$NOMBREMUNICIPIO <- as.factor(datos_filtrados$NOMBREMUNICIPIO)
datos_filtrados$NATURALEZA <- as.factor(datos_filtrados$NATURALEZA)
datos_filtrados$JORNADA <- as.factor(datos_filtrados$JORNADA)


# reviso estructura de lso datos
str(datos_filtrados)

# reviso estadisticos
summary(datos_filtrados)

# defino X
X<-datos_filtrados[,c("PROMLECTURACRITICA","PROMMATEMATICA","PROMSOCIALESYCIUDADANAS",
                     "PROMCIENCIASNATURALES","PROMINGLES")]

#  imprimo tipo de variable X
class(X)
dim(X)

# creao matrix a
a <- matrix(c(0.5,0.5,2.0,0.0,2.0))


#  imprimo tipo de variable X
class(a)
dim(a)

# vector de valores observados de Y1,
y<-as.matrix(X)%*%a

# encabezado y cola
head(y)
tail(y)

# media muestral y varianza muestral
mean(y)
var(y)
sd(y)
summary(y)

# tabla de fecuencia
brea
hist(y,breaks="Sturges")

boxplot(y)



# creao matrix b
b <- matrix(c(0.0,1.0,2.0,-1.0,2.0))


# creao matrix c
c <- matrix(c(-1.0,0.0,2.0,1.0,0.0))


# uno matrices
dim(as.matrix(X))
dim(cbind(a, b, c))

# matriz X*d
Y<-as.matrix(X)%*%cbind(a, b, c)


# cabeza y cola de Y
head(Y)
tail(Y)

# medias, medianas 
summary(Y)

# var y cor
var(Y)
cor(Y)

# boxplot
boxplot(Y)


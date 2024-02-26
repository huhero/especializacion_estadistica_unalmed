# TIPOS DE VARIABLES EN R:

# Para crear una variable numérica podemos asignar:
a <- 2.1
class(a)

# Para crear una variable tipo caracter:
b <- "hola"
class(b)

# Podemos crear un vector con varios números usando la
# letra c que significa "concatenate":
d <- c(2.1, 3.4, 5, 7)
class(d)

# Podemos crear un vector con varios caracteres usando la
# letra c que significa "concatenate":
e <- c("hola", "como", "estas", "?")
class(e)

# Cuando mezclamos números con caracteres todo 
# se tranforma en caracteres:
f <- c(2.1, 4, "hola", "como", 2.3)
class(f)

# Podemos crear data frames o bases de datos como:
edad <- c(31, 23, 24, 54, 19) 
nombre <- c("Pedro", "Luis", "Carla", "Ana", "María") 
genero <- c("M", "M", "F", "F", "F") 
puntaje <- c(2.3, 4.2, 2.1, 4.9, 3.1) 
df1 <- data.frame(nombre, edad, genero, puntaje)
class(df1)

# La ventaja de trabajar con data frames es que trata cada
# variable según su tipo. Podemos ver esto con la función
# str (la abreviatura de structure):
str(df1)

# Leamos la siguiente base de datos para practicar:
require(readr)
direccion <- "Delito_Hurto_Motocicletas.csv"
datos_2 <- read_csv(direccion,col_names=TRUE,locale=locale(asciify = TRUE))
# direccion: lugar donde ubicó la base de datos con extensión
# col_names=TRUE: pone como variables los nombres de la primera fila de la base de datos
# locale=locale(asciify = TRUE): Para considerar los acentos latinos

# Veamos dónde hay problemas al leer los datos:
problems(datos_2)

# Veamos los nombres de las variables en la base de datos:
names(datos_2)

# Simplificamos los nombres de las variables:
require(janitor) # Paquete que contiene la función clean_names
datos_2 <- clean_names(datos_2)
names(datos_2)
# Veamos las primeras filas de la base de datos:
head(datos_2)

# Veamos las últimas filas de la base de datos:
tail(datos_2)

# Veamos la estructura de las variables en la base de datos:
str(datos_2)

# Analicemos los registros según los departamentos:
table(datos_2$departamento)

# Grafiquemos la tabla anterior:
barplot(table(datos_2$departamento), las=2)

# Ordenemos la tabla anterior:
barplot(sort(table(datos_2$departamento), decreasing = TRUE), las=2)

# estado civil
barplot(sort(table(datos_2$estado_civil), decreasing = TRUE), las=2)

# color
barplot(sort(table(datos_2$color), decreasing = TRUE), las=2)
unique(datos_2$color)

datos_22 <- subset(datos_2, subset=(estado_civil %in% c("SOLTERO","UNION LIBRE","CASADO")))

xtabs(~estado_civil+departamento, data=datos_22)

barplot(xtabs(~estado_civil+departamento, data=datos_22), las=2, legend=unique(datos_22$estado_civil),
        args.legend = list(title="Estado civil") )

# color de motocicleta
barplot(xtabs(~color+departamento, data=datos_22), las=2, legend=unique(datos_22$color),
        args.legend = list(title="Estado civil") )




require(tidyverse)
table(str_subset(datos_2$color,"AZUL"))
table(str_match(datos_2$color, "AZUL.*"))


table(str_match(datos_2$fecha,'"(.*)"'))


# estado civil
sort(table(datos_2$municipio), decreasing = TRUE)
barplot(sort(table(datos_2$municipio), decreasing = TRUE), las=2)

resum_21 <- table(datos_2$genero)
resum_21
sort(resum_21[resum_21], decreasing = T)
par(mar=c(11,4.1,0.5,2))
barplot(sort(resum_21, decreasing = T), las=2)



str(datos_2)

datos_2a <-separate(datos_2,hora,sep=":",into = c("hora","min","seg"))

datos_2a$hora <- as.numeric(datos_2a$hora)
datos_2a$min <- as.numeric(datos_2a$min)
datos_2a$seg <- as.numeric(datos_2a$seg)

str(datos_2a)
datos_2a$horas_24 <- datos_2a$hora + round((datos_2a$min/60),2)

hist(datos_2a$horas_24, freq = F)

ggplot(datos_2a,
       subset=(genero % in % c("FEMENINO","MASCULINO")),
       aes(x=horas_24), fill=zona)+
  geom_density(alpha=0.3)


################################
require(ggplot2)
ggplot(datos_2a, aes(x=horas_24)) +
  geom_density(alpha=0.4, fill="blue")


datos_23<-subset(datos_2a,
                 subset=(movil_agresor%in% c("A PIE","PASAJERO MOTOCICLETA","CONDUCTOR MOTOCICLETA")))

ggplot(datos_23, aes(x=horas_24, fill=movil_agresor)) +
  geom_density(alpha=0.4)


require(ggridges)

ggplot(datos_23, aes(x=horas_24, y=genero)) +
  geom_density_ridges(col="blue")

# evitar notacion cintifica
options(scipen = 100)

100*sum(table(str_subset(datos_2$color,"NEGRO")))/nrow(datos_2)


100*table(str_match(datos_2$color, "NEGRO.*"))/nrow(datos_2)


xtabs(~movil_agresor+genero, data=datos_2)/nrow(datos_2)

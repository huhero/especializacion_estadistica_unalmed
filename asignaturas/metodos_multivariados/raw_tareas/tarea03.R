# Teoría Sobre Datos Normales Multivariados

# 1. Generación de datos normales multivariados.

# Para generar una muestra de datos con distribución normal multivariada, se puede utilizar la función
# mvrnorm de la librería MASS.

require(MASS)

# principales argumentos: 
# n (el tamaño de la muestra que se quiere generar), 
# μ (el vector de medias) y 
# Σ la matriz de var-cov.

n.filas<-5000 # n
vector.medias<-c(0,0) # μ
matriz.covarianza<-matrix(c(1, 0.8, 0.8, 1),ncol=2) # sigma Σ la matriz de var-cov
datos<-mvrnorm(n.filas,vector.medias,matriz.covarianza)
head(datos)


# Plot
plot(datos,pch='.')



# Ejercicio-1: General datos Normales 2-variados y realizar un análisis descriptivo 
# de dichos datos que incluya cómo mínimo:

# • Resúmenes descriptivos numéricos tales como: 
# vector de medias, 
apply(datos, 2, mean)
# matriz de var-cov, 
var(datos)
# matriz de cor-relaciónes, 
cor(datos)
# algunos gráficos univariados
distribucion.x1 <- hist(datos[,1], plot = F)
distribucion.intervalos.x1 <- cut(datos[,1], distribucion.x1$breaks)
table.frecuencia.abosoluta.x1 <- table(distribucion.intervalos.x1)

data.frame(table.frecuencia.abosoluta.x1)
summary(datos)
hist(datos[,1],xlim = c(-4,4), ylim=c(0,1000), xlab="X1", main="Histograma")

# algunos gráficos bi-variados. 


boxplot(datos, main = 'Datos aleatorios MV', horizontal = F, xlab = 'X aleatoria', col = "skyblue")

plot(datos[,1],datos[,2], xlab = "x aletoriaa", ylab = "y aleatoria", main="Datos aleatorios MV")
# Comente algo al respecto.
# De acuerdo con el grafico de dispersion se observa una relacion lineal positiva
# entre las variables aleatorias x1, x2, de hecho en la tabla de correlacion es
# cerca al 80% aproximadanmente. De manera que hay un alto nivel de dependencia 
# entre las variables


# • Verificar la normalidad univariada y bi-variada de los datos mediante juicios gráficos o pruebas formales
# de normalidad.

datos.media<-apply(datos,2,mean)
datos.var<-var(datos)

distancias2<-mahalanobis(datos,datos.media,datos.var)

d2<-sort(distancias2)

y<-c(rep(0,nrow(datos)))

for(j in 1:nrow(datos)){
  y[j]<-(j-0.5)/nrow(datos)
}

q<-qchisq(y,4)

plot(d2,q)


library(MVN)
resul <- mvn(datos, mvnTest=c("mardia"), univariateTest=c("SW"))
resul

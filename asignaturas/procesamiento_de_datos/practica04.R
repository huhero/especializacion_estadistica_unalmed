library(readxl)
licores <- read_excel("licores.xlsx")

library(Amelia)
missmap(licores)

summary(licores)

par(mfrow=c(1,1))
boxplot(licores$PI, main="PI")
boxplot(licores$GAE, main="GAE")
boxplot(licores$GAQ, main="GAQ")
boxplot(licores$CE, main="CE")


require(dplyr)
filter(licores, PI>100000)

filter(licores,CE<200)

licores$PI[licores$PI>100000]<-NA
licores$CE[licores$CE<200]<-NA

par(mfrow=c(1,4))
boxplot(licores$PI, main="PI")
boxplot(licores$GAE, main="GAE")
boxplot(licores$GAQ, main="GAQ")
boxplot(licores$CE, main="CE")


medias<-colMeans( licores[,-1], na.rm=T)
medias

medias2 <- apply(licores[,-1], 2, mean, na.rm=TRUE)



require(tidyr) # Cargamos el paquete

# Creamos una lista con los reemplazos:
reemplazos<- list(PI=medias[1], GAE=medias[2],
                  GAQ=medias[3], CE=medias[4])

# Reemplazamos y guardamos en DATOS:
licores<-replace_na(licores,reemplazos)
summary(licores)

missmap(licores)


require(stringr)
licores$TL<-str_replace(licores$TL,"Aguardientre","Aguardiente")
reemplazos<-c("Run"="Ron","Whiski"="Whisky")

licores$TL<-str_replace_all( licores$TL, reemplazos )

unique(licores$TL)

FACTOR_TL<-as.factor(licores$TL) # Convertimos en factor TL
FRECUENCIAS_TL<-summary(FACTOR_TL) # Tabla de frecuencias
barplot(FRECUENCIAS_TL, xlab = "tipo de licore") # Graficamos las frecuencias


####################################################################
# actividad

# import data
library(readr)
bd.master <- read_csv("bd/master.csv")

require(janitor)
bd.master <- clean_names(bd.master)

# analizando caracteristicas
str(bd.master)

bd.master$country <- as.factor(bd.master$country)
bd.master$year <- as.factor(bd.master$year)
bd.master$sex <- as.factor(bd.master$sex)
bd.master$age <- as.factor(bd.master$age)
bd.master$generation <- factor(bd.master$generation, 
                                  levels = c("G.I. Generation","Silent", 
                                    "Boomers","Generation X",
                                    "Millenials","Generation Z"))

summary(bd.master)

problems(bd.master)

unique(bd.master$country)

# cual es generacion que presenta la mayor tasa de suicidios
barplot(
  frec.relativa <- table(bd.master$generation)
)


bd.master.col <- filter(bd.master, country=="Colombia")

bd.master.agg.col <- summarise( group_by(bd.master.col,year),
           sum_suic = sum(suicides_no),
           sum_pobla = sum(population))

bd.master.agg.col

bd.master.agg.col$tasa_suic_x100k <- 
  (bd.master.agg.col$sum_suic/
  bd.master.agg.col$sum_pobla)*10

bd.master.agg.col

plot(bd.master.agg.col$year,
     bd.master.agg.col$tasa_suic_x100k, type = "l")

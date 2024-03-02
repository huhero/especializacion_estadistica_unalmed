library(readr)
df02 <- read_delim(
  "data01.csv", 
  delim = ";",
  escape_double = FALSE, 
  trim_ws = TRUE)

df02$GENERO <- as.factor(df02$GENERO)
df02$ESTRATO <- as.factor(df02$ESTRATO)
df02$FUMA <- as.factor(df02$FUMA)
summary(df02)


# PUNTO 1
SUBC1 <- df02[1:4,]

require(tidyverse)

# PUNTO 2
SUBC2 <- select_if(df02, is.factor)

# PUNTO 3
SUBC2 <- select_if(df02, is.numeric)


filtro01 <- function(df,v,c){
  return(df[df[v]==c,])
}

filtro01(df02,"GENERO","MUJER")
filtro01(df02,3,"MUJER")


filtro_mul <- function(df, vars, cats){
  n <- length(vars)
  
  for(i in 1:n){
    dfa <- filtro01(df,vars[i], cats[i])
  }
  
  return(dfa)
}


filtro_mul(df02,
           c("GENERO","FUMA"),
           c("MUJER","SI"))
c("string",2)





library(readxl)
licores <- read_excel("licores.xlsx")
licores

licores$TL <- as.factor(licores$TL)
summary(licores)
unique(licores$TL)

head(licores)


# TL: Tipo de licor
# PI: Precio de incautación. Se refiere al precio de venta en
# el establecimiento por unidad.
# GAE: Grados de alcohol en etiqueta.
# GAQ: Grados de alcohol en prueba química.
# CE: Cantidad estandarizada. Número de unidades estandarizadas a 750 ml.

colnames(licores)
str(licores)
glimpse(licores)


table(licores$TL,useNA = "ifany")
unique(licores$GAE)

# remplazo 1 a 1
licores$TL <- str_replace(licores$TL,"Aguardientre","Aguardiente")
unique(licores$TL)

# varios reemplazos
reemplazos <- c("Run"="Ron","Whiski"="Whisky")

licores$TL <- str_remove_all(licores$TL, reemplazos)
unique(licores$TL)


# filtrar 1 a 1
licores[is.na(licores$TL) | is.na(licores$PI),]

table(licores[licores$GAE==29,]$TL)


licores[!complete.cases(licores),]
which(!complete.cases(licores))

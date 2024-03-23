# Cargamos algunos paquetes:
require(tidyverse)
require(magrittr)
require(janitor)
require(lubridate)

# Leemos la base de datos:
nacidos <- read_csv("datos/Nacidos_Vivos_en_Hospital_Manuel_Uribe_Angel_20240314.csv")
nacidos %>% problems()

# Vemos encabezado y cola:
nacidos %>% head()
nacidos %>% tail()

# Vemos las dimensiones de la BD:
nacidos %>% dim()

# Nombres de las variables:
nacidos %>% names()

# Simplificamos los nombres
nacidos %<>% clean_names() 

# Vemos la estructura de la BD:
nacidos %>% str()

# Veamos problemas en la BD:
nacidos %$% unique(departamento)
nacidos %$% unique(municipio)
nacidos %$% unique(area_nacimiento)
nacidos %$% unique(sexo)
nacidos %$% summary(peso_gramos) 
nacidos %$% summary(talla_centimetros)
nacidos$fecha_nacimiento %<>% strptime(format = "%m/%d/%Y") %>% as.Date()
nacidos %$% summary(tiempo_de_gestacion) # Problema tiene ceros como mínimo
nacidos %$% table(numero_consultas_prenatales)
nacidos %$% unique(tipo_parto)
nacidos %$% unique(multiplicidad_embarazo) 
nacidos %$% unique(pertenencia_etnica) 

nacidos %$% table(pertenencia_etnica) 

nacidos %$% unique(grupo_indigena)
nacidos %$% summary(edad_madre) 

plot(nacidos %$% table(edad_madre) )

nacidos %$% unique(regimen_seguridad) 
nacidos %$% unique(nombre_administradora)
nacidos %$% summary(edad_padre) 
nacidos %$% unique(nivel_educativo_padre) 
nacidos %$% unique(departamento_expedicion)
nacidos %$% unique(municipio_expedicion)
nacidos %$% unique(estado_conyugal_de_la_madre)
nacidos %$% unique(nivel_educativo_de_la_madre)
nacidos %$% summary(numero_de_hijos_nacidos_vivos) 
nacidos %$% summary(numero_de_embarazos) 
nacidos %$% unique(area_de_residencia) 
nacidos %$% unique(pais_de_residencia) 
nacidos %$% unique(departamento_residencia) 
nacidos %$% unique(municipio_residencia) 


# ACTIVIDAD:

# Considere lo siguiente:

# 1. ¿Cómo resolver los problemas que se encontraron?
# correcion municipio
nacidos$municipio %<>%  recode('RIONEGRO'='ENVIGADO')

# correcion timepo de gestacion
norm_atipicos <- function (x,min,max){
  x[x<min | x>max] <- NA
  return(x)
}

vec01 <- c(3,8,7,27,40)
norm_atipicos(vec01,5,30)

table(nacidos$tiempo_de_gestacion)
nacidos$tiempo_de_gestacion %<>% norm_atipicos(29,43)
table(nacidos$tiempo_de_gestacion)

# correcion edad de padre
nacidos$edad_padre %>% summary()
nacidos$edad_padre %<>%  norm_atipicos(10,Inf)
nacidos$edad_padre %>% summary()

# correcion grupo indigena

# 2. ¿Existen diferencias entre niños y niñas en el peso al nacer? ¿Cómo 
#     cuantificar dicha diferencia?
 nacidos %>%  ggplot(aes(x=peso_gramos, fill=sexo))+
   geom_density(alpha=0.4)

nacidos %>% ggplot(aes(x=sexo, y=peso_gramos))+
  geom_boxplot()

nacidos %>% group_by(sexo) %>% 
  summarise(
    promedios= mean(peso_gramos,na.rm = TRUE), 
    n =n(),
    medianas=median(peso_gramos,na.rm=TRUE),
    desviaciones = sd(peso_gramos, na.rm=TRUE))  
# 3. ¿Existen diferencias entre niños y niñas en la talla al nacer? ¿Cómo 
#     cuantificar dicha diferencia?

nacidos %>%  ggplot(aes(x=talla_centimetros, fill=sexo))+
  geom_density(alpha=0.4, bw=1)

nacidos %>%  ggplot(aes(x=talla_centimetros, fill=sexo))+
  geom_histogram(alpha=0.4)


nacidos %>% ggplot(aes(x=sexo, y=talla_centimetros))+
  geom_boxplot()

nacidos %>% group_by(sexo) %>% 
  summarise(
    promedios= mean(talla_centimetros,na.rm = TRUE), 
    n =n(),
    medianas=median(talla_centimetros,na.rm=TRUE),
    desviaciones = sd(talla_centimetros, na.rm=TRUE))  

# 4. ¿Existe un efecto del tiempo de gestación en el peso al nacer? ¿ Cómo
#     cuantificar dicho efecto?
nacidos %>% ggplot(aes(x=tiempo_de_gestacion, y=peso_gramos, fill=sexo))+
  geom_point()+
  geom_smooth()

modelo01 <- lm(peso_gramos~tiempo_de_gestacion*sexo,nacidos)
modelo01
# 5. ¿Existe un efecto del tiempo de gestación en la talla al nacer? ¿ Cómo
#     cuantificar dicho efecto?

nacidos %>% ggplot(aes(x=talla_centimetros, y=peso_gramos))+
  geom_point()+
  geom_smooth(method = "lm")

modelo01 <- lm(peso_gramos~talla_centimetros*sexo,nacidos)
modelo01 %>% summary()

# 6. Plantear 5 preguntas de interés sobre la BD y
#     posibles respuestas con tablas o  gráficos.

nacidos %>% ggplot(aes(x=numero_de_embarazos, y=peso_gramos))+
  geom_point()+
  geom_smooth()

modelo01 <- lm(peso_gramos~pertenencia_etnica,nacidos)
modelo01
modelo01 %>% summary()




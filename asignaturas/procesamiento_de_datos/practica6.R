# Pipes en R
# 
# sacar abs a 5.2 luego extraer sqrt y al resultado calcular logn

x<-5.2

res_abs <- abs(x)
res_sqrt <- sqrt(res_abs)
res_ln <- log(res_sqrt)


require(magrittr)
require(carData)
data("esoph")

# ejemplo %>%
res_pipe <- x %>% abs() %>% sqrt() %>% log()
esoph %>% names()
esoph %>% summary()
esoph$alcgp %>% table() %>% barplot()

library(readr)
require(janitor)

# ejemplo pipe compuesto
Nacidos <- read_csv("Nacidos.csv")
Nacidos %<>% clean_names()

Nacidos$sexo <- as.factor(Nacidos$sexo)


Nacidos$sexo %>% table() %>% barplot()
Nacidos$sexo %>% table() %>% prop.table() 

# ejemplo pipe exposicion
plot(Nacidos$peso_gramos, Nacidos$talla_centimetros)

Nacidos %$% plot(peso_gramos, talla_centimetros)
Nacidos %$% table(sexo, tipo_parto) %>% prop.table() %>% barplot()


# ejemplo pipe té
Nacidos %>% select(peso_gramos, talla_centimetros) %T>%  plot() %>% summary() 

# ejemplo eager pipe
f1 <- function( x) {print("hola")}
f2 <- function( x) {print("como")}
f3 <- function( x) {print("estas")}


# ejemplo eager pipe
3 %!>% f1() %!>% f2() %!>% f3


Nacidos$pertenencia_etnica %<>% as.factor()

Nacidos %>% str()

Nacidos %<>% mutate_if(is.character, as.factor)
Nacidos %>% str


Nacidos$tiempo_de_gestacion[Nacidos$tiempo_de_gestacion <20 | Nacidos$tiempo_de_gestacion > 40] <- NA

Nacidos %>% ggplot(aes(x=talla_centimetros, y=peso_gramos,col=tiempo_de_gestacion))+
  geom_point()+
  geom_smooth()


Nacidos$group_type_gestacion <- cut(Nacidos$tiempo_de_gestacion, breaks = 4)
  
Nacidos %$% table(group_type_gestacion)

Nacidos %>% ggplot(aes(x=talla_centimetros, y=peso_gramos,col=group_type_gestacion))+
  geom_point()+
  geom_smooth()

# %>% pipe dstr()# %>% pipe de continuidad
# %<>% pipe compuesto
# %$% pipe de exposicon
# %T% pipe té
# %!>% eager  pipe


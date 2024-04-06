require(magrittr)
require(tidyverse)

library(readr)
master <- read_csv("master.csv")

viejos <- master %>% names


master %<>% rename(
  pais=viejos[1], 
  anio=viejos[2],
  sexo=viejos[3],
  edad=viejos[4],
  num_suic=viejos[5],
  poblacion=viejos[6],
  suic_x100k=viejos[7],
  pais_anio=viejos[8],
  idh_anio=viejos[9],
  pib_anio=viejos[10],
  pib_pcap=viejos[11],
  generacion=viejos[12])

master %<>% select(-pais_anio)


master %<>% mutate(pib_anio_mill=pib_anio/1000000)


master %<>% mutate_at(vars(pais, sexo, edad,generacion),
                       as.factor)



master %>% glimpse

master$edad %>% levels


master$edad %<>% factor(
  levels=c("5-14 years","15-24 years",
           "25-34 years","35-54 years","55-74 years","75+ years"))

master$generacion %<>% factor(levels=c('G.I. Generation','Silent',
                                       'Boomers', 'Generation X',
                                       'Millenials', 'Generation Z'))

master_col <- subset(master,subset=(pais=="Colombia"))

master_col %<>% droplevels

master_col %>% glimpse

master_col %>% summary



resum_col_1 <- master_col %>% group_by(anio) %>%
  summarise(pib_anio_mill=mean(pib_anio_mill),
            suic_x100k=mean(suic_x100k))

resum_col_1


resum_col_1 %<>% select(anio,pib_anio_mill,suic_x100k)

resum_col_1


ggplot(resum_col_1, aes(pib_anio_mill,suic_x100k))+
  geom_point(col="blue", cex=2)


ggplot(resum_col_1, aes(anio,suic_x100k))+
  geom_line(col="red")


ggplot(resum_col_1, aes(anio,pib_anio_mill))+
  geom_line(col="black")

require(ggpubr)


fig_1 <- ggplot(resum_col_1, aes(pib_anio_mill,suic_x100k))+
  geom_point(col="blue", cex=2)
fig_2 <- ggplot(resum_col_1, aes(anio,pib_anio_mill))+
  geom_line(col="black")
fig_3 <- ggplot(resum_col_1, aes(anio,suic_x100k))+
  geom_line(col="red")


ggarrange(fig_1, fig_2, fig_3,labels = c("A","B","C"),
          ncol = 2, nrow = 2)


resum_col_2 <- master_col %>% group_by(anio,sexo) %>%
  summarise(pib_anio_mill=mean(pib_anio_mill),
            suic_x100k=mean(suic_x100k))
resum_col_2 %<>% select(sexo, anio, pib_anio_mill,
                         suic_x100k)
resum_col_2


ggplot(resum_col_2, aes(x=anio,y=suic_x100k, color=sexo))+
  geom_point(cex=4)


ggplot(resum_col_2, aes(x=anio,y=suic_x100k, color=sexo))+
  geom_line(cex=1)


ggplot(resum_col_2, aes(x=suic_x100k, fill=sexo))+
  geom_density(alpha=0.4)



resum_col_3 <- master_col %>% group_by(generacion,sexo) %>% summarise(num_suic=sum(num_suic))
resum_col_3 %<>% select(generacion, sexo, num_suic)
resum_col_3

# graficas barras stack
ggplot(resum_col_3, aes(x=generacion,y=num_suic, fill=sexo))+
  geom_bar(stat="identity")


# grafica de barras separadas
ggplot(resum_col_3, aes(x=generacion,y=num_suic, fill=sexo))+
  geom_bar(stat="identity", position=position_dodge())


# grafica de barras con etiquetas
ggplot(resum_col_3, aes(x=generacion,y=num_suic, fill=sexo))+
  geom_bar(stat="identity", position="dodge")+
  geom_text(aes(label=num_suic),
            position=position_dodge(width=0.9), vjust=-0.2)


# grafica por paneles separadas

fig_1 <- ggplot(resum_col_3, aes(x=generacion,y=num_suic,
                                 fill=sexo))+ geom_bar(stat="identity")
fig_1 + facet_grid(rows = vars(sexo))


##########################################################
# Resumen 4 -> 4 variables
##########################################################
resum_col_4 <- master_col %>% group_by(sexo, anio, edad) %>% summarise(suic_x100k=mean(suic_x100k))
resum_col_4 %<>% select(sexo, anio, edad, suic_x100k)
resum_col_4

# ejemplo 01
fig_1 <- ggplot(resum_col_4,aes(x=anio,y=suic_x100k,
                                col=edad))+geom_point()
fig_1 + facet_grid(rows = vars(sexo))

# ejemplo 02
fig_1 <- ggplot(resum_col_4,aes(x=anio,y=suic_x100k,
                                col=sexo))+geom_point()
fig_1 + facet_grid(cols = vars(edad))

# ejemplo 03 cambiando tamaño
ggplot(resum_col_4,aes(x=anio,y=suic_x100k,
                       col=sexo, size=edad))+geom_point(alpha=0.4)

#################
# Contrastanto variables categoricas
############################

# ejemplo 01
require(CGPfunctions)
PlotXTabs(master_col, generacion,edad)


##################################
# resumen 5
#################################

resum_col_5 <- master_col %>% 
  group_by(anio,sexo) %>% 
  summarise(suic_x100k=mean(suic_x100k),
              pib_anio=mean(pib_anio),
              pib_anio_mill=mean(pib_anio_mill),
              num_suic=sum(num_suic),
              poblacion=sum(poblacion))
resum_col_5 %<>% select(anio, sexo, suic_x100k,pib_anio,
                         pib_anio_mill,num_suic,poblacion)
resum_col_5

# ejemplo 01
require(GGally)
resum_col_5 %>% ggpairs(mapping=aes(col=sexo, alpha=0.4))


# ejemplo 02
fig_1 <- ggplot(resum_col_5, aes(y=num_suic,x=poblacion,
                                 col=sexo,fill=sexo)) +
  geom_point() +
  geom_smooth(method = "lm")
fig_1


fig_2 <- ggplot(resum_col_5, aes(x=anio,y=pib_anio_mill,
                                 col=sexo,fill=sexo)) +
  geom_point() +
  geom_smooth(method = "lm")

require(ggpubr)
ggarrange(fig_1,fig_2,labels=c("A","B"),ncol=1,nrow=2)




################################################################################
# Desempleo antioquia
################################################################################


Datos_1<-read.table("DESEMPLEO_ECV_2017_MUNICIPIOS.txt",
                   sep=";",header=TRUE, encoding = "latin1")
names(Datos_1)
head(Datos_1)


Datos_2 <- gather(Datos_1,
  "Hombre","Mujer",key="Sexo",value="Valores"
)


# grafico de densidad

Datos_2 %>% ggplot( aes(x=Valores, fill=Sexo))+geom_density(alpha=0.4)

Datos_2 %>% ggplot( aes(x=Valores, y=Sexo))+
  geom_boxplot()+
  geom_text()

Datos_3 <- gather(Datos_2,
                  "Abierto","Oculto",key="Tipo",value="Valores_tipo"
)



Datos_4 <- gather(Datos_3,
                  "Urbano","Rural",key="Zona",value="Valores_zona"
)

# density graph
Datos_4 %>% ggplot( aes(x=Valores_zona, fill=Zona))+geom_density(alpha=0.4)

# boxplot graph
Datos_4 %>% ggplot( aes(x=Valores_zona, y=Zona))+
  geom_boxplot()


Datos_4 %>% group_by(Zona,Municipio,Sexo) %>% summarise(
  medias=mean(Valores_zona, na.rm = TRUE),
  n=n()
)

Datos_1 %>% mean(Rural, na.rm = TRUE)
Datos_1 %>% mean(Urbano, na.rm = TRUE)

Datos_1 %$% which.min(Mujer)

Datos_1[119,]

# identificar faltantes forma grafica
require(Amelia)
Datos_1 %>% missmap()


Datos_1[!complete.cases(Datos_1),]







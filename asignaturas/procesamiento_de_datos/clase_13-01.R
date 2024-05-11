require(sf)
require(leaflet)
require(tidyverse)
require(magrittr)
require(janitor)


municipios <- st_read("./MGN2023_MPIO_POLITICO/MGN_ADM_MPIO_GRAFICO.shp")


municipios %>% ggplot()+
  geom_sf()+
  theme(rect=element_blank(), # quitar fondo
        # ajustes
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank()
        ) 


desempleo <- read.table("./desempleo.txt", sep = ";", fileEncoding = "latin1", header=TRUE)

# filtrando solo por antioquia
str(municipios)
unique(municipios$dpto_ccdgo)
ant <- municipios %>% filter(dpto_ccdgo == "05")


ant %>% ggplot()+
  geom_sf()+
  theme(rect=element_blank(), # quitar fondo
        # ajustes
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank()
  ) 

municipios$mpio_cnmbr
desempleo$Municipio

# merge las bases de datos municipios y desempleo

# columna para merge las dejo con el mismo nombre
desempleo %<>% rename("mpio_cnmbr"="Municipio")

desempleo$mpio_cnmbr %<>% toupper()

desempleo$mpio_cnmbr[which(!(desempleo$mpio_cnmbr %in% ant$mpio_cnmbr ))]
ant$mpio_cnmbr[which(!(ant$mpio_cnmbr %in% desempleo$mpio_cnmbr ))]


# ajusto valores diferentes
desempleo$mpio_cnmbr %<>% recode("SANTA FE DE ANTIOQUIA"="SANTA FÉ DE ANTIOQUIA") 
ant$mpio_cnmbr %<>% recode(
  "SAN ANDRÉS DE CUERQUÍA"="SAN ANDRÉS DE CUERQUIA",
  "LA CEJA"="LA CEJA DEL TAMBO",
  "PEÑOL"="EL PEÑOL",
  "RETIRO"="EL RETIRO",
  "CAROLINA"="CAROLINA DEL PRÍNCIPE"
  )

antioquia <- merge(ant,desempleo,by="mpio_cnmbr")




antioquia %>% ggplot()+
  geom_sf(aes(fill=Total))+
  geom_sf_label(aes(label=mpio_cnmbr),size=1)+
  theme(rect=element_blank(), # quitar fondo
        # ajustes
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank()
  ) 


antioquia$tot_cat <- cut(antioquia$Total, breaks = c(-1,5,10,100), labels = c("<5%","5-10",">100"))

antioquia %>% ggplot()+
  geom_sf(aes(fill=tot_cat))


antioquia %>% 
  leaflet() %>% 
  addTiles() %>% 
  addPolygons(weight = 1,
              popup = paste0(
                "<b>Municipio: </b>"
                , antioquia$mpio_cnmbr
                , "<br>"
              ),
              , labelOptions = labelOptions(
                style = list("font-weight" = "normal"
                             , padding = "3px 8px"
                             , textsize = "15px"
                             , direction = "auto" ) )
              , highlightOptions = highlightOptions( 
                color = "#10539A"
                , weight = 3
                , fillColor = NA
              )
              )


#### cOLOMBIA
departamentos <- st_read("./MGN2023_DPTO_POLITICO/MGN_ADM_DPTO_POLITICO.shp")


departamentos %>% 
  leaflet() %>% 
  addTiles() %>% 
  addPolygons(weight = 1)

cultivos <- read.table("./data/cultivos.csv", sep = ",", header = TRUE)

str(cultivos)
cultivos %>% filter(cod_mun==15114)

result <- cultivos %>% filter(cod_mun==15114) %>%  
    group_by(departamento,grupo_de_cultivo) %>% 
    summarise(
      tot=sum(area_sembrada_ha)
    )

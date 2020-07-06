##Proyecto: COVID_MX_mun
#1_Agregar_Datos
#Fecha: 06-25-2020
#Autor: Mariano Moran Ventura
#thanks Maxwell!

#Locación del proyecto: C:\Users\Mariano\Documents\ArcGIS\MEX\COVID_19_SSA_Mex

#Datos:

#Exportados:
#C:\Users\Mariano\Documents\ArcGIS\MEX\COVID_19_SSA_Mex\EXPORTED
install.packages("tidyverse")
library(tidyverse)
library(lubridate)
library(gganimate)
library(gifski)
library(reshape2)
require("maptools")
require("plyr")
library(rgdal)
library(geojsonio)
library(leaflet)
library(sf)
library(ggplot2)
library(dplyr)

setwd("C:/Users/Mariano/Documents/GitHub/COVID_MX_mun/")
####################################################################################
####################################################################################
##########################    1.IMPORT      ########################################
####################################################################################
####################################################################################
####################################################################################
str(cases)
casos_covid_municipio <- "DATA/Casos_Diarios_Municipio_Confirmados_20200628.csv" %>% 
  read_csv() %>% 
  rename( #cambiar nombres a columnas para claridad y comodidad
    id = cve_ent,
    pop = poblacion,
    nombre = nombre
  ) %>% 
  pivot_longer(
    cols = matches("\\d{2}-\\d{2}-\\d{4}"), #los pivotes seran solo columnas que sean fechas
    names_to = "fecha", 
    #names_transform = list(fecha = dmy),
    values_to = "casos_nuevos"#, #alargar (mas filas) el dataset agregando un valor/una fila por cada municipio + fecha
    #values_transform = list(fecha = as.integer)
  ) %>% 
  arrange(id, fecha) %>% 
  group_by(id) %>% #agrupar por municipio
  mutate(casos_acum = cumsum(casos_nuevos)) %>% #cumsum, suma acumulada
  #mutate(casos_acum_100 = (casos_acum/pop)*100000)) %>%   #ajustada por poblacion
  mutate(casos_acum_100 = 100000 * (cumsum(casos_nuevos)/pop)) %>%
  ungroup()

casos_covid_municipio[4:64] <-NULL  #No necesito las columnas por cada fecha ya, las tengo como filas

#Asi, solo necesitas seleccionar que fecha quieres para graficar todos los casos de todos los municipios de cierta fecha

#Ahora a intentarlo por melt y cast #CREO QUE NO SE PUEDE
#Ya tengo los datos, ahora necesito mapearlo

####################################################################################
####################################################################################
##########################    2.GRAPH?/ MAP?    ####################################
####################################################################################
####################################################################################
####################################################################################

#First I'll try to map one cumulative number of cases in one date
#Option 2: Import modified shapefile from INEGI, then leaflet

#via sf
#using INEGI shapefile
municipios_sf <- st_read(dsn = "C:/Users/Mariano/Documents/ArcGIS/MEX/Marco geoestadistico/municipios.shp") %>%
  st_transform(4326)


ggplot(data = municipios_sf) +
  geom_sf()

#Join data to map

#It works! Let's build the map now!
ggplot() +
  geom_sf(data = mexico, color = "red", size = .3) +
  scale_fill_manual(values = c("#cbc9e2", "#9e9ac8", "#756bb1", "#54278f"),
                    labels = c("$2,000 or less", "$2,001-$2,500", "$2,501-$3,000", 
                               "More than $3,000")) +
  coord_sf(datum = NA) + #removes the grid
  theme_void() + #removes the default theme
  theme(
    #legend.title = element_text(size=9),
    plot.title = element_text(size= 12, hjust=0.01, color = "black", face = "bold", 
                              margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    #plot.subtitle = element_text(size= 10, hjust=0.01, color = "black", 
    #margin = margin(b = -0.1, t = 0.43, l = 2, unit = "cm")),
    plot.caption = element_text(size=9, color = "#4e4d47", face = "italic", 
                                margin = margin(b = 0.3, r=-99, unit = "cm") ),
    legend.position = c(0.15, .70)) +
  labs(
    fill = "", #no legend title
    title = "Figure 5: Median Asking Rent by Community District, 2018")#,
    #caption = sources)
library(sf)
nc <- st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
library(ggplot2)
ggplot(nc) + geom_sf(aes(fill = AREA))
mex <- ggplot(mexico) + aes(fill = cve_num)
sessionInfo()

#Join to data

#Then iteratively

####################################################################################
####################################################################################
##########################    3.ANIMATE       ######################################
####################################################################################
####################################################################################
####################################################################################

animate(p, duration = 5, fps = 20, width = 600, height = 600, renderer = gifski_renderer())

anim_save("covid-cases-plot.gif")

####################################################################################
####################################################################################
##########################    4.EXPORT     ####################################
####################################################################################
####################################################################################
####################################################################################

write.csv(casos_covid_municipio, "C:/Users/Mariano/Documents/ArcGIS/MEX/COVID_19_SSA_Mex/DATA/casos_covid_municipio.csv")

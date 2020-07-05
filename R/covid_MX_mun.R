##Proyecto: COVID_MX_mun
#1_Agregar_Datos
#Fecha: 06-25-2020
#Autor: Mariano Moran Ventura
#thanks Maxwell!

#Locación del proyecto: C:\Users\Mariano\Documents\ArcGIS\MEX\COVID_19_SSA_Mex

#Datos:

#Exportados:
#C:\Users\Mariano\Documents\ArcGIS\MEX\COVID_19_SSA_Mex\EXPORTED

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
#Option 1: Import geojson municipal boundary map (copied the console response from https://datos.covid-19.conacyt.mx/#COMNac)

mexico_municipios = readOGR(dsn="C:/Users/Mariano/Documents/ArcGIS/MEX/COVID_19_SSA_Mex/ssa_covid_template.geojson")
mexico_municipios <- geojson_read("C:/Users/Mariano/Documents/ArcGIS/MEX/COVID_19_SSA_Mex/ssa_covid_template.geojson",what="sp")

# Plot mexico, try
mexico <- leaflet(mexico_municipios_INEGI) #%>% 
addTiles() %>% 
  addPolygons()

mexico #does not show anything

#Option 2: Import modified shapefile from INEGI, then leaflet
mexico_municipios_INEGI <- readOGR( 
  dsn= "C:/Users/Mariano/Documents/ArcGIS/MEX/Marco geoestadistico/municipios.shp", 
  layer="municipios",
  verbose=FALSE
)

mexico_2 <- leaflet(mexico_municipios) #%>% 
addTiles() %>% 
  addPolygons()

mexico_2 #does not show anything




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

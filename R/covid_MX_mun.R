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
    mun = nombre
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
  mutate(casos_acum_ajustados = 100000 * (cumsum(casos_nuevos)/pop)) %>% #ajustada por poblacion
  ungroup()

casos_covid_municipio$casos_acum_ajustados <- format(casos_covid_municipio$casos_acum_ajustados, digits=2, nsmall=2) #acortar el numero de casos dividido por poblacion
casos_covid_municipio[4:64] <-NULL  #No necesito las columnas por cada fecha ya, las tengo como filas
casos_covid_municipio$casos_acum_ajustados <- as.numeric(casos_covid_municipio$casos_acum_ajustados)
casos_covid_municipio

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


#######Join data to map
mexico <- municipios_sf %>%
  mutate(id = cve_num) %>%
  full_join(casos_covid_municipio, by = "id")
#Wait! How do I join it! I will have many rows (dates) per every municipality. What I need for mapping is only one row per municipality
#maybe i have to join it by date?
#or i could filter it by day and then map?

#14052020
mexico_14052020 <- mexico %>%
  filter(fecha == "14-05-2020")

#Map
ggplot(data = municipios_sf) +
  geom_sf()

#It works! Let's build the map now!
map_14052020 <- ggplot() +
  geom_sf(data = mexico_14052020, color = "white", size = .2) +
  aes(fill = casos_acum_ajustados) +
  scale_fill_gradient(low = "white", high = "red", na.value = "gray", guide = "legend")


map_14052020

#14062020
mexico_14062020 <- mexico %>%
  filter(fecha == "14-06-2020")

#It works! Let's build the map now!
map_14062020 <- ggplot() +
  geom_sf(data = mexico_14062020, color = "white", size = .2) +
  aes(fill = casos_acum_ajustados) +
  scale_fill_gradient(low = "white", high = "red", na.value = "gray", guide = "legend")

map_14062020


?scale_fill_gradient()

?palette
palette
?colorRamp
?scale_fill_continuous



(values = c("#cbc9e2", "#9e9ac8", "#756bb1", "#54278f"),
                    labels = c(10,20,30,100)) #+
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

#Now iteratively

  #trying
  p <- mexico %>% 
  # 10 random dates
  filter(fecha %in% sample(unique(fecha), 10)) %>%
  arrange(fecha) +
  ggplot() +
  geom_sf(data = mexico, color = "white", size = .2) +
  aes(fill = casos_acum_ajustados) +
  scale_fill_gradient(low = "white", high = "red", na.value = "gray", guide = "legend") +
  transition_time(fecha)

####################################################################################
####################################################################################
##########################    3.ANIMATE       ######################################
####################################################################################
####################################################################################
####################################################################################
?animate

m <- map_14042020  
    
animate(, duration = 5, fps = 20, width = 600, height = 600, renderer = gifski_renderer())

anim_save("covid-cases-plot.gif")

####################################################################################
####################################################################################
##########################    4.EXPORT     ####################################
####################################################################################
####################################################################################
####################################################################################

write.csv(casos_covid_municipio, "C:/Users/Mariano/Documents/ArcGIS/MEX/COVID_19_SSA_Mex/DATA/casos_covid_municipio.csv")

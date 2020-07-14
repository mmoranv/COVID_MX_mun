##Proyecto: COVID_MX_mun
#1_Agregar_Datos
#Fecha: 06-25-2020
#Autor: Mariano Moran Ventura
#thanks Maxwell!

#Locaci√≥n del proyecto: C:\Users\Mariano\Documents\ArcGIS\MEX\COVID_19_SSA_Mex

#Datos:

#Exportados:
#C:\Users\Mariano\Documents\ArcGIS\MEX\COVID_19_SSA_Mex\EXPORTED
library(tidyverse)
library(lubridate)
library(gganimate)
library(gifski)
library(reshape2)
library(maptools)
library(rgdal)
library(leaflet)
library(sf)
library(ggplot2)
#library(plyr)
library(dplyr)


setwd("C:/Users/Mariano/Documents/GitHub/COVID_MX_mun/")
####################################################################################
####################################################################################
##########################    1.IMPORT      ########################################
####################################################################################
####################################################################################
####################################################################################
#Se deben limpiar las fechas del excel que bajas de los datos oficiales, para que esten en el formato YY-MM-DD
casos_covid_municipio <- "DATA/Casos_Diarios_Municipio_Confirmados_20200628.csv" %>% 
  read_csv() %>% 
  rename(
    id = cve_ent,
    pop = poblacion,
    mun = nombre
  ) %>% 
  pivot_longer(
    cols = matches("\\d{2}/\\d{2}/\\d{2}"), #los pivotes seran solo columnas que parezcan fechas, tuve que modificar manualmente para que las fechas sean leidas como MES-DIA-A—O
    names_to = "fecha", 
    names_transform = list(fecha = dmy),
    values_to = "casos_nuevos", #alargar (mas filas) el dataset agregando un valor/una fila por cada municipio + fecha
    values_transform = list(fecha = as.integer)
  ) %>% 
  arrange(id, fecha) %>% 
  group_by(id) %>% #agrupar por municipio
  mutate(casos_acum = cumsum(casos_nuevos)) %>% #cumsum, suma acumulada
  mutate(casos_acum_ajustados = 100000 * (cumsum(casos_nuevos)/pop)) %>% #ajustada por poblacion
  ungroup()

casos_covid_municipio$casos_acum_ajustados <- format(casos_covid_municipio$casos_acum_ajustados, digits=2, nsmall=2) #acortar el numero de casos dividido por poblacion
casos_covid_municipio$casos_acum_ajustados <- as.numeric(casos_covid_municipio$casos_acum_ajustados)
str(casos_covid_municipio)
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
municipios_con_casos <- municipios_sf %>%
  mutate(id = cve_num) %>%
  full_join(casos_covid_municipio, by = "id")
#i could filter it by day and then map?

#sample de fechas
fechas <- unique(municipios_con_casos$fecha)

sample_fechas <- fechas[100:169]

mex_sample <- municipios_con_casos %>% 
  filter(fecha %in% c(sample_fechas))
  
#Mapa base

mex_test <- municipios_con_casos %>% 
  filter(fecha == "2020-06-27")

fuente <- "Fuente: SecretarÌa de Salud, 2020"

#construyendo el mapa para una fecha
mapa_test <- ggplot() +
  #geom_sf(data = municipios_sf, colour = "black", fill = "white") +
  #geom_sf(data = mex_sample, aes(fill(casos_acum_ajustados, frame = fecha)))+
  #geom_sf(data = mex_sample, aes(fill(casos_acum_ajustados)))+
  geom_sf(data = mex_test, size = .1)+
  aes(fill = casos_acum_ajustados) +
    theme_void() + #removes the default theme
  theme_light(base_line_size = .1) +
  scale_fill_gradient(low = "white", high = "red", na.value = "gray", guide = "legend") +
  labs(title = "COVID-19: Casos confirmados en MÈxico por municipio", 
       subtitle = mex_test$fecha,
    caption = "Fuente: SecretarÌa de Salud, 2020")
  #geom_smooth(aes(group = fecha),
  #         method = "lm", 
  #        show.legend = FALSE) +
  #facet_wrap(~continent, scales = "free") +
  #scale_x_log10() +
  #transition_manual(fecha)
mapa_test


####################################################################################
####################################################################################
##########################    3.ANIMATE       ######################################
####################################################################################
####################################################################################
####################################################################################
?animate


    
animate(p, duration = 5, fps = 20, width = 600, height = 600, renderer = gifski_renderer())

anim_save("covid-cases-plot.gif")

library(tidyverse) # dev ggplot version required: devtools::install_github("hadley/ggplot2")
library(sf)
library(readxl)
library(httr)
library(ggmap)
library(gganimate) # devtools::install_github("dgrtwo/gganimate")
library(hrbrthemes) # devtools::install_github("hrbrmstr/hrbrthemes")


# plot base map + filtered map with fill on winner/runner-up variable and frame as year for animation
# then a point at each final location along with the place name text
# set the projection and all the theme commands to give it a dark and mysterious aesthetic
wc_map <- ggplot() +
  geom_sf(data = world, colour = "#ffffff20", fill = "#2d2d2d60", size = .5) +
  geom_sf(data = wc_geo, aes(fill = w_l, frame = Year)) +
  geom_sf(data = locations_sf, aes(frame = Year), size = .2, colour = "#ffffff90") +
  geom_text(data = locations_sf, aes(x, y, label = Location, frame = Year), 
            colour = "white", fill = "#00000040", nudge_y = -5) +
  coord_sf(crs = st_crs(world), datum = NA) +
  labs(title = "FIFA World Cup Winners, Runners Up & Final Locations", x=NULL, y=NULL,
       caption = "Culture of Insight / @paulcampbell91 / Source: Wikipedia") +
  theme_modern_rc(axis = FALSE, base_size = 16, caption_size = 18) +
  scale_fill_manual(values = c("#D9A441", "#A8A8A8"), name = NULL, labels = c("Winner", "Runner-Up")) +
  theme(legend.position = c(0.9, 1.01), legend.direction = "horizontal", axis.text = element_blank(), 
        panel.grid.minor = element_blank(), panel.grid.major = element_blank())

municipios_mapa <- ggplot() +
  geom_sf(data = municipios_sf, colour = "black", fill = "white")

municipios_casos <- ggplot() +
  geom_sf(data = mex_sample, size = .1)+
  aes(fill = casos_acum_ajustados) +
  theme_void() + #removes the default theme
  theme_light(base_line_size = .1) +
  scale_fill_gradient(low = "white", high = "red", na.value = "gray", guide = "legend") +
  labs(title = "COVID-19: Casos confirmados en MÈxico por municipio", 
       subtitle = mex_test$fecha,
       caption = "Fuente: SecretarÌa de Salud, 2020") + 
  transition_manual(fecha)

animate(municipios_casos, duration = 7, fps = 10, width = 1000, height = 800, renderer = gifski_renderer())
anim_save("covid-cases-plot.gif")
####################################################################################
####################################################################################
##########################    4.EXPORT     ####################################
####################################################################################
####################################################################################
####################################################################################

write.csv(casos_covid_municipio, "C:/Users/Mariano/Documents/ArcGIS/MEX/COVID_19_SSA_Mex/DATA/casos_covid_municipio.csv")

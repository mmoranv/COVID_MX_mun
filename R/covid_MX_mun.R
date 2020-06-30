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

casos_covid_municipio_1 <- "DATA/Casos_Diarios_Municipio_Confirmados_20200628.csv" %>% 
  read_csv() %>% 
  rename( #cambiar nombres a columnas para claridad y comodidad
    id = cve_ent,
    pop = poblacion,
    nombre = nombre
  )

fechas <- colnames(casos_covid_municipio_1)
fechas_2 <- fechas[4:172]

casos_covid_municipio_2 <-  melt(casos_covid_municipio_1, id=c("id"))
casos_covid_municipio_2 <-  melt(casos_covid_municipio_1, id=c(fechas_2,"id"))

#(mydata, id=c("id_variable_1","id_variable_2")) #you're creating a vector of identifiers, whatever should categorize but NOT be added
casos_cast <- dcast(casos_covid_municipio_2, id~c(fechas_2), cumsum)

####################################################################################
####################################################################################
##########################    2.GRAPH?/ MAP?   ####################################
####################################################################################
####################################################################################
####################################################################################

p <- casos_covid_municipio %>% 
  # 10 random cities
  filter(id %in% sample(unique(id), 10)) %>% 
  filter(fecha == "28-07-2020") %>% 
  ggplot() +
  aes(x = nombre, y = casos_acum) +
  coord_flip() +
  geom_col() +
  transition_time(fecha) +
  ease_aes('cubic-in-out') +
  exit_fade() +
  labs(
    title = 'Casos Confirmados Acumulados',
    subtitle = '{frame_time}'
  )

p

animate(p, duration = 5, fps = 20, width = 600, height = 600, renderer = gifski_renderer())

anim_save("covid-cases-plot.gif")

####################################################################################
####################################################################################
##########################    3.EXPORT     ####################################
####################################################################################
####################################################################################
####################################################################################

casos_covid_municipio %>% 
  write_csv("C:/Users/Mariano/Documents/ArcGIS/MEX/COVID_19_SSA_Mex/DATA/Acumulados_al_20200628.csv")
  

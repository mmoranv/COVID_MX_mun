##Project: COVID_MX_mun
#Date: 25/06/2020
#Updated: 04/03/2020
#Autor: Mariano Moran Ventura

#Abstract: I want to create a gif showing all the confirmed COVID cases in Mexico by municipality
#Project Location: C:\Users\Mariano\Documents\ArcGIS\MEX\COVID_19_SSA_Mex
#DATA: C:\Users\Mariano\Documents\GitHub\COVID_MX_mun\DATA
#SOURCES:
  #Cases 
  #https://datos.covid-19.conacyt.mx/#DownZCSV
  #Municipality boundaries 
  #https://www.inegi.org.mx/temas/mg/#Descargas
#EXPORTED:
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
library(dplyr)


setwd("C:/Users/Mariano/Documents/GitHub/COVID_MX_mun/")

####################################################################################
####################################################################################
##########################    1.IMPORT      ########################################
####################################################################################
####################################################################################
####################################################################################
#Source: https://datos.covid-19.conacyt.mx/#DownZCSV

#Manually replace "-" by "/" before anything, it's just easier
#Clean up dates from official data excel, to be in MM-DD-YY format

covid_cases_mun <- "DATA/Casos_Diarios_Municipio_Confirmados_20220402.csv" %>% 
  read_csv() %>% 
  rename(
    id = cve_ent, # Municipality ID
    pop = poblacion, # Municipality population
    mun = nombre # Municipality name
  ) %>% # using pivot_longer allows to make a "longer dataset", adding a row per each municipality per each day, this will allow to iteratively map only certain rows/dates for all municipalities 
  pivot_longer(
    cols = matches("\\d{2}/\\d{2}/\\d{2}"), #I'll pivot the data establishing columns as ONLY those fields resembling a regex date
    names_to = "date", 
    names_transform = list(date = dmy),
    values_to = "new_cases", #each value added to a row is the registered value on that date
    values_transform = list(date = as.integer)
  ) %>% 
  arrange(id, date) %>% #order by id and date
  group_by(id) %>% #group by municipality
  mutate(accumulated_cases = cumsum(new_cases)) %>% #cumsum, accumulated sum
  mutate(accumulated_cases_adj = 100000 * (cumsum(new_cases)/pop)) %>% #adjust for population
  ungroup()

str(covid_cases_mun)

covid_cases_mun$accumulated_cases_adj <- as.numeric(format(covid_cases_mun$accumulated_cases_adj, digits=2, nsmall=2)) #round

str(covid_cases_mun$accumulated_cases_adj)

####################################################################################
####################################################################################
###################################   2.MAPS    ####################################
####################################################################################
####################################################################################
####################################################################################
#using INEGI shapefile
#Source: https://www.inegi.org.mx/temas/mg/#Descargas
# Import modified shapefile from INEGI, then leaflet

#via sf
shapefile_mun <- st_read(dsn = "C:/Users/Mariano/Documents/ArcGIS/MEX/Marco geoestadistico/municipios.shp") %>%
  st_transform(4326) #Coordinate system

#######Join data to map
cases_by_mun <- shapefile_mun %>%
  mutate(id = cve_num) %>%
  full_join(covid_cases_mun, by = "id")

#i could filter it by day and then map?

#dates sample
dates <- unique(cases_by_mun$date) 
dates[1]
dates[483]

#483 days from 2020-02-26 until 2022-03-31

sample_dates <- dates[100:300]

#select all municiplaities for only the sample dates
mex_sample <- cases_by_mun %>% 
  filter(date %in% c(sample_dates))

#Basic map
mex_test <- cases_by_mun %>% 
  filter(date == "2020-06-27")

#Testing the map only for one date
map_test <- ggplot() +
  geom_sf(data = mex_test, size = .1)+ #border size
  aes(fill = accumulated_cases_adj) + #variable to be mapped
  theme_void() + #removes the default theme
  theme_light(base_line_size = .1) + 
  scale_fill_gradient(low = "white", high = "red", na.value = "gray", guide = "legend") + #color scale
  labs(title = "COVID-19: Confirmed Cases per municipality", 
       subtitle = mex_test$date,
       caption = "Source: Secretaría de Salud, 2020-2022")


map_test #It works!


####################################################################################
####################################################################################
#####################    3.ITERATIVE MAPS       ####################################
####################################################################################
####################################################################################
####################################################################################
library(tidyverse) # dev ggplot version required: devtools::install_github("hadley/ggplot2")
library(sf)
library(readxl)
library(httr)
library(ggmap)
library(gganimate) # devtools::install_github("dgrtwo/gganimate")
library(hrbrthemes) # devtools::install_github("hrbrmstr/hrbrthemes")

#Base municipality map
map_mun <- ggplot() +
  geom_sf(data = shapefile_mun, colour = "black", fill = "white", size = .1)
map_mun

#Populated municipality map
cases_by_mun_map <- ggplot() +
  geom_sf(data = cases_by_mun, size = .1)+
  aes(fill = accumulated_cases_adj) +
  theme_void() + #removes the default theme
  theme_light(base_line_size = .1) +
  scale_fill_gradient(low = "white", high = "red", na.value = "gray", guide = "legend") +
  labs(title = "COVID-19: Confirmed Cases per municipality", 
       subtitle = mex_test$date,
       caption = "Source: Secretaría de Salud, 2020") + 
  transition_manual(date) #iterative along date

?animate
#each frame is a map, each map is a day, so it'll take 43 seconds to go through 438 days
animate(cases_by_mun_map, duration = 43, fps = 15, width = 1000, height = 960, renderer = gifski_renderer())
anim_save("MX_covid_cases_2022_03_31.gif")

#it takes a while but it works!

####################################################################################
####################################################################################
##########################    4.EXPORT     #########################################
####################################################################################
####################################################################################
####################################################################################
write.csv(covid_cases_mun, "DATA/covid_cases_mun_04_03_22.csv")
#write.csv(covid_cases_mun, "C:/Users/Mariano/Documents/ArcGIS/MEX/COVID_19_SSA_Mex/DATA/covid_cases_mun.csv")
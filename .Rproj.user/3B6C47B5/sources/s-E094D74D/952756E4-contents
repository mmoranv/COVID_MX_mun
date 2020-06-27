##Proyecto: COVID_MX_mun
#1_Agregar_Datos
#Fecha: 06-25-2020
#Autor: Mariano Moran Ventura

#Locación del proyecto: C:\Users\Mariano\Documents\ArcGIS\MEX\COVID_19_SSA_Mex

#Datos:

#Exportados:
#C:\Users\Mariano\Documents\ArcGIS\MEX\COVID_19_SSA_Mex\EXPORTED

library(tidyverse)

setwd("C:/Users/Mariano/Documents/GitHub/COVID_MX_mun/")


####################################################################################
####################################################################################
##########################    1.IMPORT      ########################################
####################################################################################
####################################################################################
####################################################################################
confirmados_mun_raw <- read.csv("DATA/Municipio_Confirmados_20200624.csv")
str(confirmados_mun_raw)
confirmados_mun <- read.csv("DATA/Municipio_Confirmados_20200624_procesado.csv")


####################################################################################
####################################################################################
##########################    2.CLEAN AND AGG     ##################################
####################################################################################
####################################################################################
####################################################################################
confirmados_mun_1 <- confirmados_mun_raw
  
confirmados_mun_1$agg_12.01.2020 <- confirmados_mun_1$X12.01.2020
confirmados_mun_1$agg_13.01.2020 <- confirmados_mun_1$X13.01.2020 + confirmados_mun_1$agg_12.01.2020
confirmados_mun_1$agg_13.01.2020 <- confirmados_mun_1$X13.01.2020 + confirmados_mun_1$agg_12.01.2020
confirmados_mun_1$agg_13.01.2020 <- confirmados_mun_1$X13.01.2020 + confirmados_mun_1$agg_12.01.2020


#Estableciendo vectores de fechas
vector_col_names <- colnames(confirmados_mun_1)
fechas_cuenta <- length(vector_col_names)
vector_fechas <- vector_col_names[4:fechas_cuenta]

for
?control

# Create a vector filled with random normal values
vector_fechas <- vector_col_names[4:fechas_cuenta]

#Hoy es?
hoy <- "10.02.2020"

#Inicializar fecha
fecha_hoy <- paste("x",hoy,sep="")
fecha_hoy

for(i in vector_fechas) {
  # i-th element of `u1` squared into `i`-th position of `usq`
  confirmados_mun_1$acumulados[i] <- confirmados_mun_1$acumulados[i] + confirmados_mun_1$vector_fechas[i]
  print(confirmados_mun_1$acumulados[i])
}

print(i)
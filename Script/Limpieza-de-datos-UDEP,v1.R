#==================================================
# DESCRIPCIoN: DATOS UDEP
# AUTOR(ES): Nohelia e Isabella
#==================================================

## Establecer el Working Directory

setwd("C:/R/Tesis")

##Cargar librerias


library(openxlsx)
library(dplyr)
library(ggplot2)
library(assertive)
library(naniar)
library(lubridate)
library(xlsx)


## Desarrollo -----------------------------------------------------------------------------

path_UDEP <- file.path("Data", "UDEP_1991-2019.xlsx")
path_UDEP

# Leer all data de UDEP
data_UDEP = read.xlsx(path_UDEP, sheet='AllUDEP',startRow=3,colNames=TRUE)

# Revisar el tipo de data
dtype = sapply(data_UDEP, class)

# Cambiar de caracter a numérico, columna por columna y luego convertirlo a data frame
#apply(Matriz/vector/lista, 1:filas o 2:columnas, Operador que se aplica)
data_UDEP = as.data.frame( apply(data_UDEP, 2, as.numeric))


#Seleccionando por variables
year_UDEP = data_UDEP[,1]
month_UDEP = data_UDEP[,2]
#day_UDEP = data_UDEP[,3]
rain_UDEP = data_UDEP[,3]
temp_max_UDEP = data_UDEP[,4]
temp_min_UDEP = data_UDEP[,5]

head(data_UDEP)

miss_scan_count()
data_UDEP %>% 
  miss_scan_count(search = list("-99.9"))

#Reemplazar "valores faltantes" por NA en las variables seleccionadas.
data_UDEP_na <- replace_with_na_at(data_UDEP,
                                     .vars = c("rain", "temp_max", "temp_min"), 
                                     ~.x %in% c("N/A", "missing", "na", " ", "-99.9","T", "S/D"))

#Visualizar la data modificada (valores faltantes)
miss_var_summary(data_UDEP_na)
vis_miss(data_UDEP_na, sort_miss = TRUE)

#Data Mensual a anual
UDEP_byyear <- data_UDEP_na %>%
  group_by(year) %>%
dplyr::  select(year:rain)%>%
  summarise(rain_month = sum(rain))

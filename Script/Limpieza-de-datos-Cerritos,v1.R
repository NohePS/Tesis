#==================================================
# DESCRIPCIoN: DATOS Cerritos
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

path_Cerritos <- file.path("Data", "DataMetCerritos.xlsx")
path_Cerritos

# Leer all data de Cerritos
data_Cerritos = read.xlsx(path_Cerritos, sheet='AllCerritosMes',startRow=3,colNames=TRUE)

# Revisar el tipo de data
dtype = sapply(data_Cerritos, class)

# Cambiar de caracter a numérico, columna por columna y luego convertirlo a data frame
#apply(Matriz/vector/lista, 1:filas o 2:columnas, Operador que se aplica)
data_Cerritos = as.data.frame( apply(data_Cerritos, 2, as.numeric))


#Seleccionando por variables
year_Cerritos = data_Cerritos[,1]
month_Cerritos = data_Cerritos[,2]
#day_Cerritos = data_Cerritos[,3]
rain_Cerritos = data_Cerritos[,3]
#temp_max_Cerritos = data_Cerritos[,5]
#temp_min_Cerritos = data_Cerritos[,6]

head(data_Cerritos)

miss_scan_count()
data_Cerritos %>% 
  miss_scan_count(search = list("-99.9"))

#Reemplazar "valores faltantes" por NA en las variables seleccionadas.
data_Cerritos_na <- replace_with_na_at(data_Cerritos,
                                   .vars = c("rain" #"temp_max", "temp_min" 
                                             ), 
                                   ~.x %in% c("N/A", "missing", "na", " ", "-99.9","T", "S/D"))

#Visualizar la data modificada (valores faltantes)
miss_var_summary(data_Cerritos_na)
vis_miss(data_Cerritos_na, sort_miss = TRUE)

#Data Mensual a anual
cerritos_byyear <- data_Cerritos_na %>%
  group_by(year) %>%
  select(year:rain)%>%
  summarise(rain_month = sum(rain))
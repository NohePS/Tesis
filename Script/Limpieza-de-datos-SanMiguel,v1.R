#==================================================
# DESCRIPCIoN: DATOS Miguel
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

## Desarrollo -----------------------------------------------------------------------------

path_Miguel <- file.path("Data", "Miguel_1973-2020.xlsx")
path_Miguel

# Leer all data de Miguel
data_Miguel = read.xlsx(path_Miguel, sheet='AllmiguelDia',startRow=3,colNames=TRUE)

# Revisar el tipo de data
dtype = sapply(data_Miguel, class)

# Cambiar de caracter a numérico, columna por columna y luego convertirlo a data frame
#apply(Matriz/vector/lista, 1:filas o 2:columnas, Operador que se aplica)
data_Miguel = as.data.frame( apply(data_Miguel, 2, as.numeric))


#Seleccionando por variables
year_Miguel = data_Miguel[,1]
month_Miguel = data_Miguel[,2]
day_Miguel = data_Miguel[,3]
rain_Miguel = data_Miguel[,4]
temp_max_Miguel = data_Miguel[,5]
temp_min_Miguel = data_Miguel[,6]
temp_med_Miguel = data_Miguel[,7]

head(data_Miguel)

# Verificar máximos y mínimos
#b <- c(min(dm$month), max(dm$month))

#Verificación de datos faltantes
#any_na(temp_min)
#n_miss(temp_min)
#prop_miss(temp_min)
#miss_var_table(dm)

#Resume el n_datos faltantes y proporporción de los mismos en c/variable.
miss_var_summary(data_Miguel)
#Visualización de proporporción de los datos faltantes en c/variable.
vis_miss(data_Miguel)
#Visualización de sumary de los datos faltantes en c/variable.
gg_miss_var(data_Miguel)

#Visualización de un patrón general de ausencias
#gg_miss_upset(data_Miguel)
#Explorar la falta de un dataset de series de tiempos
#gg_miss_span(data_Miguel)

#Explorar y buscar diferentes valores
miss_scan_count()
data_Miguel %>% 
  miss_scan_count(search = list("-99.9"))

#Reemplazar "valores faltantes" por NA en las variables seleccionadas.
data_Miguel_na <- replace_with_na_at(data_Miguel,
                                     .vars = c("rain", "temp_max", "temp_min"), 
                                     ~.x %in% c("N/A", "missing", "na", " ", "-99.9","T", "S/D"))

#Visualizar la data modificada (valores faltantes)
miss_var_summary(data_Miguel_na)
vis_miss(data_Miguel_na, sort_miss = TRUE)
gg_miss_var(data_Miguel_na)


#Vizualización en consola (de manera horizontal)
glimpse(data_Miguel_na)

#Data diaria a mensual
data_Miguel_na <- data_Miguel_na %>%
  mutate(date_Miguel = make_date(year = year_Miguel, month = month_Miguel, day = day_Miguel))

head(data_Miguel_na)

Miguel_bymonth <- data_Miguel_na %>%
  group_by(year, month) %>%
dplyr::  select(year:temp_med)%>%
  summarise(rain_month = sum(rain),
            max_temp_max = max(temp_max),
            min_temp_min = min(temp_min),
            med_temp_med = mean(temp_med))
#mean_temp_max = mean(temp_max_Miguel))
head(data_Miguel_na$rain)

#Data Mensual a anual

Miguel_byyear <- Miguel_bymonth %>%
  group_by(year) %>%
  dplyr::  select(year: med_temp_med)%>%
  summarise(rain_month = sum(rain_month),
            max_temp_max = max(max_temp_max),
            min_temp_min = min(min_temp_min),
            med_temp_med = mean(med_temp_med))

#Exporta a excel
write.xlsx(Miguel_bymonth, file = "Miguel_mes_v1.xlsx", colNames = TRUE)


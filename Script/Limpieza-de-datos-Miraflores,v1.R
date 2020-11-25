#==================================================
# DESCRIPCIÓN: DATOS MIRAFLORES
# AUTOR(ES): Nohelia e Isabella
#==================================================

## Establecer el Working Directory

setwd("C:/R/Tesis")

##Cargar librerías

library(openxlsx)
library(dplyr)
library(ggplot2)
library(assertive)
library(naniar)
library(lubridate)

## Desarrollo -----------------------------------------------------------------------------

path_miraflores <- file.path("Data", "DataMiraflores1971-2020.xlsx")
path_miraflores

# Leer all data de miraflores
data_miraflores = read.xlsx(path_miraflores, sheet='AllmirafloresDia',startRow=3,colNames=TRUE)

# Revisar el tipo de data
dtype = sapply(data_miraflores, class)

# Cambiar de caracter a numérico, columna por columna y luego convertirlo a data frame
#apply(Matriz/vector/lista, 1:filas o 2:columnas, Operador que se aplica)
data_miraflores = as.data.frame( apply(data_miraflores, 2, as.numeric))


#Seleccionando por variables
year_miraflores = data_miraflores[,1]
month_miraflores = data_miraflores[,2]
day_miraflores = data_miraflores[,3]
rain_miraflores = data_miraflores[,4]
temp_max_miraflores = data_miraflores[,5]
temp_min_miraflores = data_miraflores[,6]
temp_med_miraflores = data_miraflores[,7]

data_miraflores

# Verificar máximos y mínimos
#b <- c(min(dm$month), max(dm$month))

#Verificación de datos faltantes
#any_na(temp_min)
#n_miss(temp_min)
#prop_miss(temp_min)
#miss_var_table(dm)

#Resume el n_datos faltantes y proporporción de los mismos en c/variable.
miss_var_summary(data_miraflores)
#Visualización de proporporción de los datos faltantes en c/variable.
vis_miss(data_miraflores)
#Visualización de sumary de los datos faltantes en c/variable.
gg_miss_var(data_miraflores)

#Visualización de un patrón general de ausencias
#gg_miss_upset(data_miraflores)
#Explorar la falta de un dataset de series de tiempos
#gg_miss_span(data_miraflores)

#Explorar y buscar diferentes valores
miss_scan_count()
data_miraflores %>% 
  miss_scan_count(search = list("-99.9"))

#Reemplazar "valores faltantes" por NA en las variables seleccionadas.
data_miraflores_na <- replace_with_na_at(data_miraflores,
                                     .vars = c("rain", "temp_max", "temp_min"), 
                                     ~.x %in% c("N/A", "missing", "na", " ", "-99.9", "T", "S/D"))

#Visualizar la data modificada (valores faltantes)
miss_var_summary(data_miraflores_na)
vis_miss(data_miraflores_na, sort_miss = TRUE)
gg_miss_var(data_miraflores_na)

#Vizualización en consola (de manera horizontal)
glimpse(data_miraflores_na)

#Data diaria a mensual
data_miraflores_na <- data_miraflores_na %>%
  mutate(date_miraflores = make_date(year = year_miraflores, month = month_miraflores, day = day_miraflores))

head(data_miraflores_na)

miraflores_bymonth <- data_miraflores_na %>%
  group_by(year, month) %>%
  select(year : temp_med)%>%
  summarise(rain_month = sum(rain),
            max_temp_max = max(temp_max),
            min_temp_min = min(temp_min),
            med_temp_med = mean(temp_med))
#mean_temp_max = mean(temp_max_miraflores))
head(data_miraflores_na$rain)

#Data Mensual a anual

miraflores_byyear <- miraflores_bymonth %>%
  group_by(year) %>%
  select(year:med_temp_med)%>%
  summarise(rain_month = sum(rain_month),
            max_temp_max = max(max_temp_max),
            min_temp_min = min(min_temp_min),
            med_temp_med = mean(med_temp_med))

write.xlsx(miraflores_bymonth, file = "miraflores_mes_v1.xlsx", colNames = TRUE)

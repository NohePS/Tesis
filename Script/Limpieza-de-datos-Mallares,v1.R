#==================================================
# DESCRIPCIÓN: DATOS Mallares
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

path_Mallares <- file.path("Data", "Estimadores.xlsx")
path_Mallares

# Leer all data de Mallares
data_Mallares = read.xlsx(path_Mallares, sheet='AllMallares',startRow=3,colNames=TRUE)

# Revisar el tipo de data
dtype = sapply(data_Mallares, class)

# Cambiar de caracter a numérico, columna por columna y luego convertirlo a data frame
#apply(Matriz/vector/lista, 1:filas o 2:columnas, Operador que se aplica)
data_Mallares = as.data.frame( apply(data_Mallares, 2, as.numeric))


#Seleccionando por variables
year_Mallares = data_Mallares[,1]
month_Mallares = data_Mallares[,2]
day_Mallares = data_Mallares[,3]
rain_Mallares = data_Mallares[,4]
temp_max_Mallares = data_Mallares[,5]
temp_min_Mallares = data_Mallares[,6]
temp_med_Mallares = data_Mallares[,7]
data_Mallares

# Verificar máximos y mínimos
#b <- c(min(dm$month), max(dm$month))

#Verificación de datos faltantes
#any_na(temp_min)
#n_miss(temp_min)
#prop_miss(temp_min)
#miss_var_table(dm)

#Resume el n_datos faltantes y proporporción de los mismos en c/variable.
miss_var_summary(data_Mallares)
#Visualización de proporporción de los datos faltantes en c/variable.
vis_miss(data_Mallares)
#Visualización de sumary de los datos faltantes en c/variable.
gg_miss_var(data_Mallares)

#Visualización de un patrón general de ausencias
#gg_miss_upset(data_Mallares)
#Explorar la falta de un dataset de series de tiempos
#gg_miss_span(data_Mallares)

#Explorar y buscar diferentes valores
miss_scan_count()
data_Mallares %>% 
  miss_scan_count(search = list("-99.9"))

#Reemplazar "valores faltantes" por NA en las variables seleccionadas.
data_Mallares_na <- replace_with_na_at(data_Mallares,
                                     .vars = c("rain", "temp_max", "temp_min"), 
                                     ~.x %in% c("N/A", "missing", "na", " ", "-99.9", "T", "S/D"))

#Visualizar la data modificada (valores faltantes)
miss_var_summary(data_Mallares_na)
vis_miss(data_Mallares_na , sort_miss = TRUE)
gg_miss_var(data_Mallares_na)
#gg_miss_upset(data_Mallares_na)

#Vizualización en consola (de manera horizontal)
glimpse(data_Mallares_na)

#Data diaria a mensual
data_Mallares_na <- data_Mallares_na %>%
  mutate(date_Mallares = make_date(year = year_Mallares, month = month_Mallares, day = day_Mallares))

head(data_Mallares_na)

Mallares_bymonth <- data_Mallares_na %>%
  group_by(year, month) %>%
  select(year : temp_med)%>%
  summarise(rain_month = sum(rain),
            max_temp_max = max(temp_max),
            min_temp_min = min(temp_min),
            med_temp_med = mean(temp_med))
#mean_temp_max = mean(temp_max_Mallares))
head(data_Mallares_na$rain)

#Data Mensual a anual

Mallares_byyear <- Mallares_bymonth %>%
  group_by(year) %>%
  select(year:med_temp_med)%>%
  summarise(rain_month = sum(rain_month),
            max_temp_max = max(max_temp_max),
            min_temp_min = min(min_temp_min),
            med_temp_med = mean(med_temp_med))

write.xlsx(Mallares_bymonth, file = "Mallares_mes_v1.xlsx", colNames = TRUE)

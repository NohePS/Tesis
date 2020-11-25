#==================================================
# DESCRIPCIÓN: DATOS La esperanza
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

path_Esperanza <- file.path("Data", "Estimadores.xlsx")
path_Esperanza

# Leer all data de Esperanza
data_Esperanza = read.xlsx(path_Esperanza, sheet='AllEsperanza',startRow=3,colNames=TRUE)

# Revisar el tipo de data
dtype = sapply(data_Esperanza, class)

# Cambiar de caracter a numérico, columna por columna y luego convertirlo a data frame
#apply(Matriz/vector/lista, 1:filas o 2:columnas, Operador que se aplica)
data_Esperanza = as.data.frame( apply(data_Esperanza, 2, as.numeric))


#Seleccionando por variables
year_Esperanza = data_Esperanza[,1]
month_Esperanza = data_Esperanza[,2]
day_Esperanza = data_Esperanza[,3]
rain_Esperanza = data_Esperanza[,4]
temp_max_Esperanza = data_Esperanza[,5]
temp_min_Esperanza = data_Esperanza[,6]
temp_med_Esperanza = data_Esperanza[,7]
data_Esperanza

# Verificar máximos y mínimos
#b <- c(min(dm$month), max(dm$month))

#Verificación de datos faltantes
#any_na(temp_min)
#n_miss(temp_min)
#prop_miss(temp_min)
#miss_var_table(dm)

#Resume el n_datos faltantes y proporporción de los mismos en c/variable.
miss_var_summary(data_Esperanza)
#Visualización de proporporción de los datos faltantes en c/variable.
vis_miss(data_Esperanza)
#Visualización de sumary de los datos faltantes en c/variable.
gg_miss_var(data_Esperanza)

#Visualización de un patrón general de ausencias
#gg_miss_upset(data_Esperanza)
#Explorar la falta de un dataset de series de tiempos
#gg_miss_span(data_Esperanza)

#Explorar y buscar diferentes valores
miss_scan_count()
data_Esperanza %>% 
  miss_scan_count(search = list("-99.9"))

#Reemplazar "valores faltantes" por NA en las variables seleccionadas.
data_Esperanza_na <- replace_with_na_at(data_Esperanza,
                                     .vars = c("rain", "temp_max", "temp_min"), 
                                     ~.x %in% c("N/A", "missing", "na", " ", "-99.9", "T", "S/D"))

#Visualizar la data modificada (valores faltantes)
miss_var_summary(data_Esperanza_na)
vis_miss(data_Esperanza_na , sort_miss = TRUE)
gg_miss_var(data_Esperanza_na)
#gg_miss_upset(data_Esperanza_na)

#Vizualización en consola (de manera horizontal)
glimpse(data_Esperanza_na)

#Data diaria a mensual
data_Esperanza_na <- data_Esperanza_na %>%
  mutate(date_Esperanza = make_date(year = year_Esperanza, month = month_Esperanza, day = day_Esperanza))

head(data_Esperanza_na)

Esperanza_bymonth <- data_Esperanza_na %>%
  group_by(year, month) %>%
  select(year : temp_med)%>%
  summarise(rain_month = sum(rain),
            max_temp_max = max(temp_max),
            min_temp_min = min(temp_min),
            med_temp_med = mean(temp_med))
#mean_temp_max = mean(temp_max_Esperanza))
head(data_Esperanza_na$rain)

#Data Mensual a anual

Esperanza_byyear <- Esperanza_bymonth %>%
  group_by(year) %>%
  select(year:med_temp_med)%>%
  summarise(rain_month = sum(rain_month),
            max_temp_max = max(max_temp_max),
            min_temp_min = min(min_temp_min),
            med_temp_med = mean(med_temp_med))
write.xlsx(Esperanza_bymonth, file = "LaEsperanza_mes_v1.xlsx", colNames = TRUE)

#==================================================
# DESCRIPCIÓN: DATOS Bernal
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

path_Bernal <- file.path("Data", "DataBernal1963-2020.xlsx")
path_Bernal

# Leer all data de Bernal
data_Bernal = read.xlsx(path_Bernal, sheet='AllBernalDia',startRow=3,colNames=TRUE)

# Revisar el tipo de data
dtype = sapply(data_Bernal, class)

# Cambiar de caracter a numérico, columna por columna y luego convertirlo a data frame
#apply(Matriz/vector/lista, 1:filas o 2:columnas, Operador que se aplica)
data_Bernal = as.data.frame( apply(data_Bernal, 2, as.numeric))


#Seleccionando por variables
year_Bernal = data_Bernal[,1]
month_Bernal = data_Bernal[,2]
day_Bernal = data_Bernal[,3]
rain_Bernal = data_Bernal[,4]
temp_max_Bernal = data_Bernal[,5]
temp_min_Bernal = data_Bernal[,6]
temp_med_Bernal = data_Bernal[,7]

data_Bernal

# Verificar máximos y mínimos
#b <- c(min(dm$month), max(dm$month))

#Verificación de datos faltantes
#any_na(temp_min)
#n_miss(temp_min)
#prop_miss(temp_min)
#miss_var_table(dm)

#Resume el n_datos faltantes y proporporción de los mismos en c/variable.
miss_var_summary(data_Bernal)
#Visualización de proporporción de los datos faltantes en c/variable.
vis_miss(data_Bernal)
#Visualización de sumary de los datos faltantes en c/variable.
gg_miss_var(data_Bernal)

#Visualización de un patrón general de ausencias
#gg_miss_upset(data_Bernal)
#Explorar la falta de un dataset de series de tiempos
#gg_miss_span(data_Bernal)

#Explorar y buscar diferentes valores
miss_scan_count()
data_Bernal %>% 
  miss_scan_count(search = list("-99.9"))

#Reemplazar "valores faltantes" por NA en las variables seleccionadas.
data_Bernal_na <- replace_with_na_at(data_Bernal,
                                     .vars = c("rain", "temp_max", "temp_min"), 
                                     ~.x %in% c("N/A", "missing", "na", " ", "-99.9", "T", "S/D"))

#Visualizar la data modificada (valores faltantes)
miss_var_summary(data_Bernal_na)
vis_miss(data_Bernal_na , sort_miss = TRUE)
gg_miss_var(data_Bernal_na)
#gg_miss_upset(data_Bernal_na)

#Vizualización en consola (de manera horizontal)
glimpse(data_Bernal_na)

#Data diaria a mensual
data_Bernal_na <- data_Bernal_na %>%
  mutate(date_Bernal = make_date(year = year_Bernal, month = month_Bernal, day = day_Bernal))

head(data_Bernal_na)

Bernal_bymonth <- data_Bernal_na %>%
  group_by(year, month) %>%
  select(year : temp_med)%>%
  summarise(rain_month = sum(rain),
            max_temp_max = max(temp_max),
            min_temp_min = min(temp_min),
            med_temp_med = mean(temp_med))
#mean_temp_max = mean(temp_max_Bernal))
head(data_Bernal_na$rain)

#Data Mensual a anual

Bernal_byyear <- Bernal_bymonth %>%
  group_by(year) %>%
  select(year:med_temp_med)%>%
  summarise(rain_month = sum(rain_month),
            max_temp_max = max(max_temp_max),
            min_temp_min = min(min_temp_min),
            med_temp_med = mean(med_temp_med))


write.xlsx(Bernal_bymonth, file = "Bernal_mes_v1.xlsx", colNames = TRUE)
Bernal_bymonth

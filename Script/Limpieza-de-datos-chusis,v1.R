#==================================================
# DESCRIPCIoN: DATOS CHUSIS
# AUTOR(ES): Nohelia e Isabella
#==================================================

## Establecer el Working Directory

setwd("C:/R/Tesis")

##Cargar librerias

install.packages('xlsx')
install.packages('ggfortify')

library(openxlsx)
library(dplyr)
library(ggplot2)
library(assertive)
library(naniar)
library(lubridate)
library(xlsx)
library(graphics)
library(base)
library(ggfortify)

## Desarrollo -----------------------------------------------------------------------------

path_chusis <- file.path("Data", "Chusis1963-2020.xlsx")
path_chusis

# Leer all data de Chusis
data_chusis = read.xlsx(path_chusis, sheet='AllChusis',startRow=3,colNames=TRUE)

# Revisar el tipo de data
dtype = sapply(data_chusis, class)

# Cambiar de caracter a numérico, columna por columna y luego convertirlo a data frame
#apply(Matriz/vector/lista, 1:filas o 2:columnas, Operador que se aplica)
data_chusis = as.data.frame( apply(data_chusis, 2, as.numeric))


#Seleccionando por variables
year_chusis = data_chusis[,1]
month_chusis = data_chusis[,2]
day_chusis = data_chusis[,3]
rain_chusis = data_chusis[,4]
temp_max_chusis = data_chusis[,5]
temp_min_chusis = data_chusis[,6]
temp_med_chusis = data_chusis[,7]
head(data_chusis)

# Verificar máximos y mínimos
#b <- c(min(dm$month), max(dm$month))

#Verificación de datos faltantes
#any_na(temp_min)
#n_miss(temp_min)
#prop_miss(temp_min)
#miss_var_table(dm)

#Resume el n_datos faltantes y proporporción de los mismos en c/variable.
miss_var_summary(data_chusis)
#Visualización de proporporción de los datos faltantes en c/variable.
vis_miss(data_chusis)
#Visualización de sumary de los datos faltantes en c/variable.
gg_miss_var(data_chusis)

#Visualización de un patrón general de ausencias
#gg_miss_upset(data_chusis)
#Explorar la falta de un dataset de series de tiempos
#gg_miss_span(data_chusis)

#Explorar y buscar diferentes valores
miss_scan_count()
data_chusis %>% 
  miss_scan_count(search = list("-99.9"))

#Reemplazar "valores faltantes" por NA en las variables seleccionadas.
data_chusis_na <- replace_with_na_at(data_chusis,
                                     .vars = c("rain", "temp_max", "temp_min"), 
                                     ~.x %in% c("N/A", "missing", "na", " ", "-99.9","T", "S/D"))

#Visualizar la data modificada (valores faltantes)
miss_var_summary(data_chusis_na)
vis_miss(data_chusis_na, sort_miss = TRUE)
gg_miss_var(data_chusis_na)


#Vizualización en consola (de manera horizontal)
glimpse(data_chusis_na)

#Data diaria a mensual
data_chusis_na <- data_chusis_na %>%
  mutate(date_chusis = make_date(year = year_chusis, month = month_chusis, day = day_chusis))

head(data_chusis_na)

chusis_bymonth <- data_chusis_na %>%
  group_by(year, month) %>%
  select(year:temp_med)%>%
  summarise(rain_month = sum(rain),
            max_temp_max = max(temp_max),
            min_temp_min = min(temp_min),
            med_temp_med = mean(temp_med))
            #mean_temp_max = mean(temp_max_chusis))
head(chusis_bymonth)

#Data Mensual a anual

chusis_byyear <- chusis_bymonth %>%
  group_by(year) %>%
  select(year:med_temp_med)%>%
  summarise(rain_month = sum(rain_month),
            max_temp_max = max(max_temp_max),
            min_temp_min = min(min_temp_min),
            med_temp_med = mean(med_temp_med))

#Exportar

write.xlsx(chusis_bymonth, file = "Chusis_mes_v1.xlsx", colNames = TRUE)
miss_var_summary(data_rain)

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




#######Grafica de todas las estaciones data diaria con datos NA
Datos_NA <- cbind(data_chusis_na[2923:20454,4:7], 
                  fecha= as.Date(data_chusis_na[2923:20454,8]),
                  rain_Bernal = data_Bernal_na[2923:20454,]$rain, temp_max_Bernal=data_Bernal_na[2923:20454,]$temp_max,
                  temp_min_Bernal=data_Bernal_na[2923:20454,]$temp_min, temp_med_Bernal=data_Bernal_na[2923:20454,]$temp_med, 
                  rain_miraflores = data_miraflores_na[365:17896,]$rain,
                  temp_max_miraflores=data_miraflores_na[365:17896,]$temp_max, 
                  temp_min_miraflores=data_miraflores_na[365:17896,]$temp_min, 
                  temp_med_miraflores=data_miraflores_na[365:17896,]$temp_med,
                  rain_SanMiguel = data_Miguel_na[365:17896,]$rain, temp_max_SanMiguel=data_Miguel_na[365:17896,]$temp_max, 
                  temp_min_SanMiguel=data_Miguel_na[365:17896,]$temp_min, 
                  temp_med_SanMiguel=data_Miguel_na[365:17896,]$temp_med
                  )  
library(reshape2)

Datos_NA_1 <- melt(Datos_NA, id.vars = c("fecha")) 

Data_rain_na <-  Datos_NA_1[c(1:17532,70129:87660,140257:157788,210385:227916),]
Data_temp_max_na <-  Datos_NA_1[c(17533:35064,87661:105192,157789:175320,227917:245448),]

Data_temp_min_na <-  Datos_NA_1[c(35065:52596,105193:122724,
                                  175321:192852,
                                  245449:262980),]
Data_temp_med_na <-  Datos_NA_1[c(52597:70128,122725:140256,192853:210384,
                                  262981:280512),]

estaciones_names <- list( "rain"="Chusis", 
                    "rain_Bernal"="Bernal",
                    "rain_miraflores"="Miraflores",
                    "rain_SanMiguel"="San Miguel"
                     )


plot_labeller <- function(variable, value){ 
  value <- droplevels(value)
  names_li <- list( "rain"="Chusis", 
                        "rain_Bernal"="Bernal",
                        "rain_miraflores"="Miraflores",
                        "rain_SanMiguel"="San Miguel"
  )
  return(names_li[value]) }


#colors <- c( "Chusis" = "red", "Bernal" = "blue", "Miraflores" = "green","San Miguel"="black")

rain_na<- ggplot(Data_rain_na, aes(x = (fecha), y=value, fill= variable))+
  geom_line (colour="#393535")+
  scale_x_date(date_labels = "%Y", breaks = seq.Date(as.Date("1972-01-01"), as.Date("2019-12-31"),
                                                        by = "1 year")) +
  scale_y_continuous(
    breaks = seq(0,800 ,40),
    name = "Precipitación (mm)") +
  labs(x = "Años", title =  "Precipitación diaria con data faltante del SENAMHI 1972-2019", 
       color  = ""
  ) +
  scale_color_manual(values = colors) +
  theme_bw()
rain_na + theme(legend.position="bottom") + guides(fill = guide_legend(nrow = 1))+
   theme(axis.text.x = element_text(angle = 90, hjust = 1))+ 
  facet_grid(variable ~., labeller=plot_labeller)
                                                                        

####Teperaturas
temp_na<- ggplot(Data_temp_min_na, aes(x = (fecha), y=value, fill= variable))+
  geom_line (colour="#393535")+
  scale_x_date(date_labels = "%Y", breaks = seq.Date(as.Date("1972-01-01"), as.Date("2019-12-31"),
                                                     by = "1 year")) +
  scale_y_continuous(
    breaks = seq(0,40 ,3),
    name = "Temperaturas(°C)") +
  #   sec.axis = sec_axis(~.*(2), name = "Precipitación (mm)" ,
  #          breaks = seq(0,80, 10)
  # geom_text(aes(x = month, y = mean_temp), nudge_y = 1) +
  labs(x = "Años", title =  "Temperatura mínima diaria con data faltante del SENAMHI 1972-2019", 
       # caption = "Fuente: Yo\n*Datos desde 1991 a 2019",
       color  = ""
  ) +
  scale_color_manual(values = colors) +
  theme_bw()
temp_na + theme(legend.position="bottom") + guides(fill = guide_legend(nrow = 1))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+ 
  facet_grid(variable ~., labeller=plot_labeller)






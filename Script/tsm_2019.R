#==================================================
# DESCRIPCIoN: DATOS CHUSIS
# AUTOR(ES): Nohelia e Isabella
#==================================================

## Establecer el Working Directory

setwd("C:/R/Tesis")

##Cargar librerias)

library(openxlsx)
library(dplyr)
library(ggplot2)
library(assertive)
library(naniar)
library(lubridate)
#library(xlsx)
library(graphics)
library(base)
library(ggfortify)

## Desarrollo -----------------------------------------------------------------------------

path_tsm <- file.path("Data", "Paita.xlsx")
#path_tsm<- read.csv("PruebaP.csv")
path_tsm
# Leer all data de Chusis
data_tsm = read.xlsx(path_tsm, sheet='TSM_2019',startRow=2,colNames=TRUE)

# Revisar el tipo de data
dtype = sapply(data_tsm, class)
dtype
# Cambiar de caracter a numérico, columna por columna y luego convertirlo a data frame
#apply(Matriz/vector/lista, 1:filas o 2:columnas, Operador que se aplica)
data_tsm = as.data.frame( apply(data_tsm, 2, as.numeric))
data_1 = make_date(data_tsm[,1]) 
data_1 = mdy (data_tsm[,1]) 
data_1
#Separar fecha y hora


data_2 <- mutate(data_tsm,d1=substr(fecha,7,8),mes=substr(fecha,1,2),dia=substr(fecha,4,5), year= paste("20",d1,sep=""))
head(data_2)

df3<- dplyr::  select(data_2,year, mes, dia, tsm)
head(df3)


data_4 <- df3 %>%
  group_by(year, mes, dia) %>%
  dplyr::  select(year:tsm)%>%
  summarise(tsm_day = mean(tsm))


data_5 <- data_4 %>%
  group_by(year, mes) %>%
  dplyr::  select(year:tsm_day)%>%
  summarise(tsm = mean(tsm_day))

#Exportar

write.xlsx(data_5, file = "data_tsm_2019_mes.xlsx", colNames = TRUE)
miss_var_summary(data_rain)


##Data TSM completa##################################################
path_tsm_mes <- file.path("Data", "DataTsmCEPaita.xlsx")
rain_mes <- file.path("Data", "Estimación_de_datos_faltantes.xlsx")
#path_tsm<- read.csv("PruebaP.csv")
path_tsm
# Leer all data de Chusis
data_tsm_mes = read.xlsx(path_tsm_mes, sheet='CEPMesT', rows = 4 : 62 , cols = 1:13,
                         colNames = TRUE)
data_tsm_AN_mes = read.xlsx(path_tsm_mes, sheet='CEPMesT', rows = 4 : 62 , cols = c(1, 19:31),
                                   colNames = TRUE)
data_tsm_dia = read.xlsx(path_tsm_mes, sheet='Tsm_dia', startRow = 2 , cols = 1:4,
                         colNames = TRUE)
chusis_estimado = read.xlsx(rain_mes, sheet='chusis', startRow = 1 ,
                            colNames = TRUE)
##Gráficas Mensuales##############################################################

tsm = c()
year_tsm = c()
mes = c()
df_P = 0
c = 0
as.data.frame(df_P)
f=0
m=0
for (a in 1963:2019) {
#a=1963
#for (i in 1:12) {#posicion en el nuevo data frame a crear


 #   print(mes)
  f = f+1
  for(i in 1:57) { 
   
    if (data_tsm_mes[i,]$Year == a){
     
      for (j in 1:12) {#meses
        m=m+1
        c = j+1
      tsm[m] = data_tsm_mes[f, c]
      #print(tsm[m])
      year_tsm[m] = a
      mes[m] = j
      #print(mes)
      df_P <-  data.frame(year_tsm, mes, tsm)
      #print(df_P)
      
    }
    }
 # print(a)
}
}

df_P

TSM_mes <- df_P%>% 
  group_by(mes) %>%
 select(year_tsm:tsm)%>%
  summarise(tsm_mean = mean(tsm),
            tsm_max = max(tsm),
            tsm_min = min(tsm))

meses <- factor (c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago","Sep","Oct", "Nov", "Dic"))

TSM_mes_1 <- cbind(TSM_mes,meses)

TSM_mes_1$meses = factor(TSM_mes_1$meses, levels=c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago","Sep","Oct", "Nov", "Dic"))


colors <- c( "T. máxima" = "red", "T. mínima" = "blue", "T. media" = "green")

TSM_graf <-ggplot(TSM_mes_1,aes(label = round(tsm_max, 1),group = 1)) +
  geom_line(aes(x = meses, y = tsm_max, color = "T. máxima")) +
  geom_line(aes(x = meses, y = tsm_min, color = "T. mínima"))+
  geom_line(aes(x = meses, y = tsm_mean, color = "T. media"))+
  geom_point(aes(x = meses, y = tsm_max), size = 1.5)+
  geom_point(aes(x = meses, y = tsm_min), size = 1.5)+
  geom_point(aes(x = meses, y = tsm_mean), size = 1.5)+
  #scale_x_continuous(breaks = 1:12) +
  scale_y_continuous(
    breaks = seq(10,40 ,2),
    name = "Temperatura (°C)"  ) +
 #   sec.axis = sec_axis(~.*(2), name = "Precipitación (mm)" ,
              #          breaks = seq(0,80, 10)
  # geom_text(aes(x = month, y = mean_temp), nudge_y = 1) +
  labs(x = "Meses", title =  "Temperatura superficial del Mar - Paita-1963-2019 ", 
       # caption = "Fuente: Yo\n*Datos desde 1991 a 2019",
       color  = ""
  ) +
  scale_color_manual(values = colors) +
  theme_bw()

TSM_graf + theme(legend.position="bottom") + guides(fill = guide_legend(nrow = 1))



###Gráfico diario 1963-2014#######################################

colors <- c( "T. máxima" = "red", "T. mínima" = "blue", "T. media" = "green")

tsm_gg <- ggplot(data_tsm_dia,aes(label = round(tsm_max, 1))) +
  geom_line(aes(x = dia, y = tsm_max, color = "T. máxima")) +
  geom_line(aes(x = dia, y = tsm_min, color = "T. mínima"))+
  geom_line(aes(x = dia, y = tsm_mean, color = "T. media"))+
  #geom_point(aes(x = dia, y = tsm_max), size = 0.1)+
  #geom_point(aes(x = dia, y = tsm_min), size = 0.1)+
  #geom_point(aes(x = dia, y = tsm_mean), size = 0.1)+
  scale_x_continuous(breaks = seq(1,365, 30)) +
  scale_y_continuous(
    breaks = seq(10,40 ,2),
    name = "Temperatura (°C)"  ) +
  #   sec.axis = sec_axis(~.*(2), name = "Precipitación (mm)" ,
  #          breaks = seq(0,80, 10)
  # geom_text(aes(x = month, y = mean_temp), nudge_y = 1) +
  labs(x = "Días", title =  "Promedio diario de Temperatura superficial del mar en Paita 1963-2019", 
       # caption = "Fuente: Yo\n*Datos desde 1991 a 2019",
       color  = ""
  ) +
  scale_color_manual(values = colors) +
  theme_bw()
tsm_gg + theme(legend.position="bottom") + guides(fill = guide_legend(nrow = 1))

###Gráfico de anomalías##############################
###WALTER <-  Consultas del gráfico para el color y leyenda- Revisión de código 


anomalia = c()
year_AN = c()
mes = c()
df_A = 0
c = 0
as.data.frame(df_A)
f=0
m=0
for (a in 1963:2019) {
  #a=1963
  #for (i in 1:12) {#posicion en el nuevo data frame a crear
  
  
  #   print(mes)
  f = f+1
  for(i in 1:57) { 
    
    if (data_tsm_AN_mes[i,]$Year == a){
      
      for (j in 1:12) {#meses
        m=m+1
        c = j+1
        anomalia[m] = data_tsm_AN_mes[f, c]
        #print(tsm[m])
        year_AN[m] = a
        mes[m] = j
        #print(mes)
        df_A <-  data.frame(year_AN, mes, anomalia)
        #print(df_P)
        
      }
    }
    # print(a)
  }
}

df_A


AN_vs_Rain <- data.frame(year = df_A[109:684,]$year_AN, mes = df_A[109:684,]$mes,
                         anomalia =df_A[109:684,]$anomalia,rain = chusis_estimado$rain,
                         fecha = make_date(year =  df_A[109:684,]$year_AN, month =df_A[109:684,]$mes ))  

colors <- c( "Anomalías" = "red", "Precipitación" = "blue")

gg_an_rain <- ggplot(AN_vs_Rain,aes (label = round(anomalia, 1))) +
  geom_line(aes(x = fecha, y = anomalia, color = "Anomalías")) +
#ggplot(chusis_estimado,aes (label = round(rain, 1))) +
  geom_bar(aes(x = fecha, (y = rain/50 -4), color = "Precipitación"), stat = "identity", 
           fill = "#84c6ed", alpha = .85)+
  #geom_line(aes(x = dia, y = tsm_mean, color = "T. media"))+
  #geom_point(aes(x = dia, y = tsm_max), size = 0.1)+
  #geom_point(aes(x = dia, y = tsm_min)t, size = 0.1)+
  #geom_point(aes(x = dia, y = tsm_mean), size = 0.1)+
  scale_x_date(date_labels = "%Y", breaks = seq.Date(as.Date("1972-01-01"), as.Date("2019-12-1"),
 by = "1 year")) +
  scale_y_continuous(
    breaks = seq(-4,12,2),
    name = "Anomalías",
    sec.axis = sec_axis(~(.+4)*50, name = "Precipitación (mm)" ,
           breaks = seq(0,800, 100)))+
  # geom_text(aes(x = month, y = mean_temp), nudge_y = 1) +
  labs(x = "Años", title =  "Anomalías mensuales de la TSM en Paita", 
       # caption = "Fuente: Yo\n*Datos desde 1991 a 2019",
       color  = "Leyenda"
  ) +
  scale_color_manual(values = colors) +
  theme_bw()
gg_an_rain
gg_an_rain$layers[[2]] <- geom_segment(mapping = aes(x = fecha, y = rain / 50 -4, xend = fecha, yend = -4
                                                     ),size = 1.2) 
gg_an_rain+ scale_fill_gradient(name = "Leyenda", labels = c('Anomalías', 'Precipitación'), values = colors) 
gg_an_rain + theme(axis.text.x = element_text(angle = 90, hjust = 1))



########TSM VS LLUVIA############################
##WALTER <- ecuación 
TSM_vs_Rain <- data.frame(year = df_P[109:684,]$year_tsm, mes = df_P[109:684,]$mes,
                          tsm =df_P[109:684,]$tsm,rain = chusis_estimado$rain,
                          fecha = make_date(year =  df_P[109:684,]$year_tsm, month =df_P[109:684,]$mes )) 
model_tsm_rain <- lm.fit (tsm ~ rain, data = TSM_vs_Rain)

summary(model_tsm_rain)

ggplot(TSM_vs_Rain, aes(x = tsm, y =rain)) + geom_point() 

write.xlsx(TSM_vs_Rain, file = "TSM_vs_Rain.xlsx", colNames = TRUE)






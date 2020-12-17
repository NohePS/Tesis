###Gráficas de años niño

library(readxl)
library(ggplot2)
library(dplyr)
library(openxlsx)
library(lubridate)
library(reshape2)
library(GGally)
library(ggfortify)
library(tidyverse)
library(fpp2)
library(zoo)
library(trend)


install.packages("trend")


####1982-1983
Estimados <- file.path("Data", "Estimación_de_datos_faltantes.xlsx")
paita_2017 <- file.path("Data", "DataTsmCEPaita.xlsx")
chusis_estimado = read.xlsx(Estimados, sheet='chusis', startRow = 1 ,
                            colNames = TRUE)
SanMiguel_estimado = read.xlsx(Estimados, sheet='San Miguel', startRow = 1 ,
                               colNames = TRUE)
Miraflores_estimado = read.xlsx(Estimados, sheet='Miraflores', startRow = 1 ,
                                colNames = TRUE)
UDEP_estimado = read.xlsx(Estimados, sheet='UDEP', startRow = 1 ,
                          colNames = TRUE)
Bernal_estimado = read.xlsx(Estimados, sheet='Bernal', startRow = 1 ,
                            colNames = TRUE)


Fen_2017_tsm =read.xlsx(paita_2017 , sheet='FEN', startRow = 1 ,
                    colNames = TRUE)




chusis_83 <- data.frame (rain = chusis_estimado[129 : 140,]$rain,
                         temp_max = chusis_estimado[129 : 140,]$temp_max,
                         temp_min = chusis_estimado[129 : 140,]$temp_min,
                         fecha = make_date(year = chusis_estimado[129 : 140,]$year, 
                                           month =chusis_estimado[129 : 140,]$month))

Bernal_83 <- data.frame (rain = Bernal_estimado[129 : 140,]$rain,
                         temp_max = Bernal_estimado[129 : 140,]$temp_max,
                         temp_min = Bernal_estimado[129 : 140,]$temp_min,
                         fecha = make_date(year = Bernal_estimado[129 : 140,]$year, 
                                           month =Bernal_estimado[129 : 140,]$month))

SanMiguel_83 <- data.frame (rain = SanMiguel_estimado[129 : 140,]$rain,
                         temp_max = SanMiguel_estimado[129 : 140,]$temp_max,
                         temp_min = SanMiguel_estimado[129 : 140,]$temp_min,
                         fecha = make_date(year = SanMiguel_estimado[129 : 140,]$year, 
                                           month =SanMiguel_estimado[129 : 140,]$month))

Miraflores_83 <- data.frame (rain = Miraflores_estimado[129 : 140,]$rain,
                            temp_max = Miraflores_estimado[129 : 140,]$temp_max,
                            temp_min = Miraflores_estimado[129 : 140,]$temp_min,
                            fecha = make_date(year = Miraflores_estimado[129 : 140,]$year, 
                                              month =Miraflores_estimado[129 : 140,]$month))


####1997-1998
chusis_98 <- data.frame (rain = chusis_estimado[309 : 320,]$rain,
                         temp_max = chusis_estimado[309 : 320,]$temp_max,
                         temp_min = chusis_estimado[309 : 320,]$temp_min,
                         temp_med = chusis_estimado[309 : 320,]$temp_med,
                         fecha = make_date(year = chusis_estimado[309 : 320,]$year, 
                                           month =chusis_estimado[309 : 320,]$month))

Bernal_98 <- data.frame (rain = Bernal_estimado[309 : 320,]$rain,
                         temp_max = Bernal_estimado[309 : 320,]$temp_max,
                         temp_min = Bernal_estimado[309 : 320,]$temp_min,
                         temp_med = Bernal_estimado[309 : 320,]$temp_med,
                         fecha = make_date(year = Bernal_estimado[309 : 320,]$year, 
                                           month =Bernal_estimado[309 : 320,]$month))

SanMiguel_98 <- data.frame (rain = SanMiguel_estimado[309 : 320,]$rain,
                            temp_max = SanMiguel_estimado[309 : 320,]$temp_max,
                            temp_min = SanMiguel_estimado[309 : 320,]$temp_min,
                            temp_med = SanMiguel_estimado[309 : 320,]$temp_med,
                            fecha = make_date(year = SanMiguel_estimado[309 : 320,]$year, 
                                              month =SanMiguel_estimado[309 : 320,]$month))

Miraflores_98 <- data.frame (rain = Miraflores_estimado[309 : 320,]$rain,
                             temp_max = Miraflores_estimado[309 : 320,]$temp_max,
                             temp_min = Miraflores_estimado[309 : 320,]$temp_min,
                             temp_med = Miraflores_estimado[309 : 320,]$temp_med,
                             fecha = make_date(year = Miraflores_estimado[309 : 320,]$year, 
                                               month =Miraflores_estimado[309 : 320,]$month))

UDEP_98 <- data.frame (rain = UDEP_estimado[81 : 92,]$rain,
                         temp_max = UDEP_estimado[81 : 92,]$temp_max,
                         temp_min = UDEP_estimado[81 : 92,]$temp_min,
                         temp_med = UDEP_estimado[81 : 92,]$temp_med,
                         fecha = make_date(year = UDEP_estimado[81 : 92,]$year, 
                                           month =UDEP_estimado[81 : 92,]$month))


###2016-2017
chusis_17 <- data.frame (rain = chusis_estimado[537 : 548,]$rain,
                         temp_max = chusis_estimado[537 : 548,]$temp_max,
                         temp_min = chusis_estimado[537 : 548,]$temp_min,
                         temp_med = chusis_estimado[537 : 548,]$temp_med,
                         fecha = make_date(year = chusis_estimado[537 : 548,]$year, 
                                           month =chusis_estimado[537 : 548,]$month))

Bernal_17 <- data.frame (rain = Bernal_estimado[537 : 548,]$rain,
                         temp_max = Bernal_estimado[537 : 548,]$temp_max,
                         temp_min = Bernal_estimado[537 : 548,]$temp_min,
                         temp_med = Bernal_estimado[537 : 548,]$temp_med,
                         fecha = make_date(year = Bernal_estimado[537 : 548,]$year, 
                                           month =Bernal_estimado[537 : 548,]$month))

SanMiguel_17 <- data.frame (rain = SanMiguel_estimado[537 : 548,]$rain,
                            temp_max = SanMiguel_estimado[537 : 548,]$temp_max,
                            temp_min = SanMiguel_estimado[537 : 548,]$temp_min,
                            temp_med = SanMiguel_estimado[537 : 548,]$temp_med,
                            fecha = make_date(year = SanMiguel_estimado[537 : 548,]$year, 
                                              month =SanMiguel_estimado[537 : 548,]$month))

Miraflores_17 <- data.frame (rain = Miraflores_estimado[537 : 548,]$rain,
                             temp_max = Miraflores_estimado[537 : 548,]$temp_max,
                             temp_min = Miraflores_estimado[537 : 548,]$temp_min,
                             temp_med = Miraflores_estimado[537 : 548,]$temp_med,
                             fecha = make_date(year = Miraflores_estimado[537 : 548,]$year, 
                                               month =Miraflores_estimado[537: 548,]$month))

UDEP_17 <- data.frame (rain = UDEP_estimado[309 : 320,]$rain,
                       temp_max = UDEP_estimado[309 : 320,]$temp_max,
                       temp_min = UDEP_estimado[309 : 320,]$temp_min,
                       temp_med = UDEP_estimado[309 : 320,]$temp_med,
                       fecha = make_date(year = UDEP_estimado[309 : 320,]$year, 
                                         month =UDEP_estimado[309 : 320,]$month))




##Precipitación
gg_fen <- ggplot(Miraflores_83 ,aes (label = round(rain, 1))) +
  #geom_line(aes(x = fecha, y = anomalia, color = "Anomalías")) +
  #ggplot(chusis_estimado,aes (label = round(rain, 1))) +
  geom_bar(aes(x = fecha, (y = rain)), stat = "identity", 
           fill = "#5AEF61", alpha = .85)+
  scale_x_date(date_labels = "%b-%Y", breaks = seq.Date(as.Date("1982-09-01"), as.Date("1983-08-1"),
                                                     by = "1 month")) +
  scale_y_continuous(
    breaks = seq(0,800, 50),
    name = "Precipitación (mm)")+
    #sec.axis = sec_axis(~(.+4)*50, name = "Precipitación (mm)" ,
       #                 breaks = seq(0,800, 100)))+
  # geom_text(aes(x = month, y = mean_temp), nudge_y = 1) +
  labs( x = "Años" ,title =  "Precipitación acumulada mensual durante el evento El Niño de 1982-1983\nEstación San Miguel" 
       # caption = "Fuente: Yo\n*Datos desde 1991 a 2019",
       #color  = "Leyenda"
  ) +
  #scale_color_manual(values = colors) +
  theme_bw()
  gg_fen
gg_fen + theme(axis.text.x = element_text(angle = 90, hjust = 1))



####### temp y lluvia añas niño #######
colors <- c("Temperatura máxima" = "red", "Temperatura mínima" = "blue", "Temperatura media" = "green","Precipitación"= "#FE7E7E")
gg_fen_t <- ggplot(chusis_98,aes (label = round(temp_max, 1))) +
  geom_bar(aes(x = fecha, (y = rain/20),color = "Precipitación"), stat = "identity", 
           fill = "#FE7E7E", alpha = .85)+
  geom_line(aes(x = fecha, y = temp_max, color = "Temperatura máxima")) +
  geom_line(aes(x = fecha, y = temp_med, color = "Temperatura media")) +
  #ggplot(chusis_estimado,aes (label = round(rain, 1))) +
  geom_line(aes(x = fecha, y = temp_min, color = "Temperatura mínima"))+
  geom_point(aes(x = fecha, y = temp_max), size = 0.1)+
  geom_point(aes(x = fecha, y = temp_min), size = 0.1)+
  geom_point(aes(x = fecha, y = temp_med), size = 0.1)+
  #geom_point(aes(x = dia, y = tsm_mean), size = 0.1)+
  scale_x_date(date_labels = "%b-%Y", breaks = seq.Date(as.Date("1997-09-01"), as.Date("1998-08-1"),
                                                        by = "1 month")) +
  scale_y_continuous(
    breaks = seq(0,40, 5),
    name = "Temperatura (°C)",
  sec.axis = sec_axis(~.*(20), name = "Precipitación(mm)",
                   breaks = seq(0,800, 100)))+
 #  geom_text(aes(x = month, y = T), nudge_y = 1) +
  labs( title =  "Temperatura máxima y mínima mensual durante el evento El Niño de 1997-1998\nEstación Chusis",
       # caption = "Fuente: Yo\n*Datos desde 1991 a 2019",
       color  = "" ) +
  scale_color_manual(values = colors) +
  theme_bw()

gg_fen_t + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  theme(legend.position="bottom") + guides(fill = guide_legend(nrow = 1))


gc()



###Precipitación años Niño para cada estación



FEN_chusis <- data.frame(fecha = chusis_83$fecha, rain_83 = chusis_83$rain,
                         rain_98 = chusis_98$rain, rain_17 = chusis_17$rain, temp_max_98 = chusis_98$temp_max,
                          temp_min_98 = chusis_98$temp_min,
                         temp_max_17 = chusis_17$temp_max, temp_min_17 = chusis_17$temp_min,
                         mes = factor(c("Sep","Oct", "Nov", "Dic","Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago")))

FEN_SanMiguel <- data.frame(fecha = SanMiguel_83$fecha, rain_83 = SanMiguel_83$rain,
                         rain_98 = SanMiguel_98$rain, rain_17 = SanMiguel_17$rain, temp_max_98 = SanMiguel_98$temp_max,
                         temp_min_98 = SanMiguel_98$temp_min,
                         temp_max_17 = SanMiguel_17$temp_max, temp_min_17 = SanMiguel_17$temp_min,
                         mes = factor(c("Sep","Oct", "Nov", "Dic","Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago")))

FEN_Miraflores <- data.frame(fecha = Miraflores_83$fecha, rain_83 = Miraflores_83$rain,
                         rain_98 = Miraflores_98$rain, rain_17 = Miraflores_17$rain, temp_max_98 = Miraflores_98$temp_max,
                         temp_min_98 = Miraflores_98$temp_min,
                         temp_max_17 = Miraflores_17$temp_max, temp_min_17 = Miraflores_17$temp_min,
                          mes = factor(c("Sep","Oct", "Nov", "Dic","Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago")))

FEN_Bernal <- data.frame(fecha = Bernal_83$fecha, rain_83 = Bernal_83$rain,
                             rain_98 = Bernal_98$rain, rain_17 = Bernal_17$rain, temp_max_98 = Bernal_98$temp_max,
                             temp_min_98 = Bernal_98$temp_min,
                             temp_max_17 = Bernal_17$temp_max, temp_min_17 = Bernal_17$temp_min,
                             mes = factor(c("Sep","Oct", "Nov", "Dic","Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago")))

FEN_UDEP <- data.frame(fecha = Bernal_98$fecha, 
                         rain_98 = Bernal_98$rain, rain_17 = UDEP_17$rain, temp_max_98 = UDEP_98$temp_max,
                         temp_min_98 = UDEP_98$temp_min,
                         temp_max_17 = UDEP_17$temp_max, temp_min_17 = UDEP_17$temp_min,
                         mes = factor(c("Sep","Oct", "Nov", "Dic","Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago")))
######FEN LLUVIA

FEN_chusis$mes = factor(FEN_chusis$mes, 
                      levels=c("Sep","Oct", "Nov", "Dic","Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago"))


FEN_chusis_1 <- melt(FEN_chusis[,2:9], id.vars = c("mes")) 
#rain_83 = "#5AEF61"
grays = c( rain_83 = "#5AEF61", rain_98 = "#FE7E7E", rain_17 = "#5798EC")


ggplot (FEN_chusis_1[1:36,], aes(x = (mes), y=value, fill= variable)) + 
  geom_bar (stat="identity",  position ="dodge",colour="#393535")+
  scale_x_discrete(limits = factor(c("Sep","Oct", "Nov", "Dic","Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago")))+
  scale_y_continuous( 
    breaks = seq(0,650, 50),
    name = "Precipitación(mm)")+
  labs( x= "Meses del año hidrológico" ,title =  "Precipitaciones acumuladas mensuales-Estación Chusis"
        # caption = "Fuente: Yo\n*Datos desde 1991 a 2019",
        ) + 
  scale_fill_manual("", values = grays, labels=c("FEN 1982-1983", "FEN 1997-1998", "FEN 2016-2017"))+
  theme_bw()+
  theme(legend.position="bottom") + guides(fill = guide_legend(nrow = 1))







##Temperaturas años Niños T maxima####

colors <- c("T. máxima 1982-1983" = "red", "T. máxima 1997-1998"= "blue", "T. máxima 2016-2017"= "green")
gg_fen_l <- ggplot(FEN_chusis,aes (label = round(temp_max_17, 1))) +
 # geom_line(aes(x = fecha, y = temp_max_83, color = "T. máxima 1982-1983")) +
  #ggplot(chusis_estimado,aes (label = round(rain, 1))) +
  geom_line(aes(x = fecha, y = temp_max_98, color = "T. máxima 1997-1998"))+
  geom_line(aes(x =fecha, y = temp_max_17, color = "T. máxima 2016-2017"))+
  #geom_point(aes(x = fecha, y = temp_max_83), size = 0.1)+
  geom_point(aes(x = fecha, y = temp_max_98), size = 0.1)+
  geom_point(aes(x = fecha, y = temp_max_17), size = 0.1)+
  scale_x_date(date_labels = "%m-%Y", breaks = seq.Date(as.Date("1982-09-01"), as.Date("1983-08-1"),
                                                        by = "1 month")) +
  #scale_x_discrete(limits = factor(c(9,10,11,12,1,2,3,4,5,6,7,8)))+
  scale_y_continuous(
    breaks = seq(0,40, 1),
    name = "Temperatura (°C)")+
    #sec.axis = sec_axis(~.*(20), name = "Precipitación(mm)",
     #                   breaks = seq(0,800, 100)))+
  #  geom_text(aes(x = month, y = T), nudge_y = 1) +
  labs( title =  "Temperatura máxima mensual durante el evento El Niño de 2016-2017\n(Estación Miraflores)",
        # caption = "Fuente: Yo\n*Datos desde 1991 a 2019",
        color  = "Leyenda" ) +
  scale_color_manual(values = colors) +
  theme_bw()

gg_fen_l+ theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Gráficos no se ven bien por eso no se toman



####FEN TSM###########


FEN_tsm = data.frame(fecha = make_date(year = df_P[237:248,]$year_tsm, 
                                       month = df_P[237:248,]$mes),
                     tsm_83 = df_P[237:248,3],tsm_98 = df_P[417:428,3], 
                     tsm_17 = df_P[645:656,3])
                                                                          

#colors <- c("T. máxima 1982-1983" = "red", "T. máxima 1997-1998"= "blue", "T. máxima 2016-2017"= "green")
ggplot(Fen_2017_tsm,aes (label = round(tsm, 1))) +
  geom_line(aes(x = dia, y = tsm), colour = "#38A8FF", size = 0.75) +
  #scale_size_manual(values = c(1, 4)) +
  scale_x_continuous(breaks = seq(1,365, 30))+
  scale_y_continuous(
    breaks = seq(0,40, 1),
    name = "Temperatura (°C)")+
  labs(x="Días", title =  "Temperatura superficial del mar diaria durante el año 2017")+
  theme_bw()


 










#######Grafica de series climáticas mensuales

chusis_anual_temp_max <- chusis_estimado%>%
  group_by(year) %>%
  select(year:temp_max)%>%
  summarise( rain = mean(temp_max))
chusis_anual_temp_max


#Chusis 
chusis_estimado_1 <- cbind(Fecha  = make_date(year = chusis_estimado$year, 
                                            month = chusis_estimado$month), chusis_estimado )
Bernal_estimado_1 <- cbind(Fecha  = make_date(year = Bernal_estimado$year, 
                                              month = Bernal_estimado$month), Bernal_estimado )
Miraflores_estimado_1 <- cbind(Fecha  = make_date(year = Miraflores_estimado$year, 
                                              month = Miraflores_estimado$month), Miraflores_estimado )
SanMiguel_estimado_1 <- cbind(Fecha  = make_date(year = SanMiguel_estimado$year, 
                                              month = SanMiguel_estimado$month), SanMiguel_estimado )
UDEP_estimado_1 <- cbind(Fecha  = make_date(year = UDEP_estimado$year, 
                                              month = UDEP_estimado$month), UDEP_estimado )


prep_estimada = read.xlsx(Estimados, sheet='Prep_estimada', startRow = 1 ,
                            colNames = TRUE)
Temp_max_estimada = read.xlsx(Estimados, sheet='Temp_max_estimada', startRow = 1 ,
                          colNames = TRUE)
Temp_min_estimada = read.xlsx(Estimados, sheet='Temp_min_estimada', startRow = 1 ,
                              colNames = TRUE)
Temp_med_estimada = read.xlsx(Estimados, sheet='Temp_med_estimada', startRow = 1 ,
                              colNames = TRUE)

prep_estimada <-  cbind(fecha  = make_date(year = prep_estimada$year, 
                                           month = prep_estimada$month), prep_estimada)
Temp_max_estimada <- cbind(fecha  = make_date(year = Temp_max_estimada$year, 
                                              month = Temp_max_estimada$month), Temp_max_estimada)
Temp_min_estimada <- cbind(fecha  = make_date(year = Temp_min_estimada$year, 
                                              month = Temp_min_estimada$month), Temp_min_estimada)
Temp_med_estimada <- cbind(fecha  = make_date(year = Temp_med_estimada$year, 
                                              month = Temp_med_estimada$month), Temp_med_estimada)


library(reshape2)

prep_estimada_1 <- melt(prep_estimada[,c(1,4:8)], id.vars = c("fecha"))
Temp_max_estimada_1 <- melt(Temp_max_estimada[,c(1,4:7,10)], id.vars = c("fecha"))
Temp_min_estimada_1 <- melt(Temp_min_estimada[,c(1,4:7,10)], id.vars = c("fecha"))
Temp_med_estimada_1 <- melt(Temp_med_estimada[,c(1,4:8)], id.vars = c("fecha"))

plot_labeller_1 <- function(variable, value){ 
  value <- droplevels(value)
  names_li <- list( "Bernal_temp_med"="Bernal", 
                    "chusis_temp_med"="Chusis",
                    "miraflores_temp_med"="Miraflores",
                    "miguel_temp_med"="San Miguel",
                    "UDEP_temp_med"="UDEP"
  )
  return(names_li[value]) }
gc()
#colors <- c( "Chusis" = "red", "Bernal" = "blue", "Miraflores" = "green","San Miguel"="black")
#-c(12,27,46) estaciones menos UDEP
rain_ts<- ggplot(prep_estimada_1, aes(x = fecha, y=value, fill = variable))+
  geom_line (colour="#393535")+
  scale_x_date(date_labels = "%Y", breaks = seq.Date(as.Date("1972-01-01"), as.Date("2019-12-1"),
                                                     by = "1 year")) +
  scale_y_continuous(
    breaks = seq(0,600 ,150),
    name = "Precipitación (mm)") +
  labs(x = "Años", title =  "Precipitaciones mensuales 1972-2019", 
       color  = ""
  ) +
  scale_color_manual(values = colors) +
  theme_bw()
rain_ts+ theme(legend.position="bottom") + guides(fill = guide_legend(nrow = 1))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+ 
  facet_grid(variable ~., labeller=plot_labeller_1)




####Teperaturas
temp_ts<- ggplot(Temp_min_estimada_1, aes(x = (fecha), y=value, fill= variable))+
  geom_line (colour="#393535")+
  scale_x_date(date_labels = "%Y", breaks = seq.Date(as.Date("1972-01-01"), as.Date("2019-12-31"),
                                                     by = "1 year")) +
  scale_y_continuous(
    breaks = seq(0,40 ,3),
    name = "Temperaturas(°C)") +
  #   sec.axis = sec_axis(~.*(2), name = "Precipitación (mm)" ,
  #          breaks = seq(0,80, 10)
  # geom_text(aes(x = month, y = mean_temp), nudge_y = 1) +
  labs(x = "Años", title =  "Temperatura mensual", 
       # caption = "Fuente: Yo\n*Datos desde 1991 a 2019",
       color  = ""
  ) +
  scale_color_manual(values = colors) +
  theme_bw()
temp_ts + theme(legend.position="bottom") + guides(fill = guide_legend(nrow = 1))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+ 
  facet_grid(variable ~., labeller=plot_labeller_1)
temp_ts
gc()

#######Grafica de series climáticas anuales

Chusis_rain_anual_ts <- ts(chusis_estimado[,3], start=1972, frequency=12)
Bernal_rain_anual_ts <- ts(Bernal_estimado[,3], start=1972, frequency=12)
Miraflores_rain_anual_ts <- ts(Miraflores_estimado[,3], start=1972, frequency=12)
SanMiguel_rain_anual_ts <- ts(SanMiguel_estimado[,3], start=1972, frequency=12)
UDEP_rain_anual_ts <- ts(UDEP_estimado[,3], start=1991, frequency=12)

Chusis_Tmax_anual_ts <- ts(chusis_estimado[229:576,4], start=1991, frequency=12)
Bernal_Tmax_anual_ts <- ts(Bernal_estimado[229:576,4], start=1991, frequency=12)
Miraflores_Tmax_anual_ts <- ts(Miraflores_estimado[229:576,4], start=1991, frequency=12)
SanMiguel_Tmax_anual_ts <- ts(SanMiguel_estimado[229:576,4], start=1991, frequency=12)
UDEP_Tmax_ts <- ts(UDEP_estimado[,4], start=1991, frequency=12)

Chusis_Tmin_anual_ts <- ts(chusis_estimado[229:576,5], start=1991, frequency=12)
Bernal_Tmin_anual_ts <- ts(Bernal_estimado[229:576,5], start=1991, frequency=12)
Miraflores_Tmin_anual_ts <- ts(Miraflores_estimado[229:576,5], start=1991, frequency=12)
SanMiguel_Tmin_anual_ts <- ts(SanMiguel_estimado[229:576,5], start=1991, frequency=12)
UDEP_Tmin_ts <- ts(UDEP_estimado[,5], start=1991, frequency=12)

Chusis_Tmed_anual_ts <- ts(chusis_estimado[229:576,6], start=1991, frequency=12)
Bernal_Tmed_anual_ts <- ts(Bernal_estimado[229:576,6], start=1991, frequency=12)
Miraflores_Tmed_anual_ts <- ts(Miraflores_estimado[229:576,6], start=1991, frequency=12)
SanMiguel_Tmed_anual_ts <- ts(SanMiguel_estimado[229:576,6], start=1991, frequency=12)
UDEP_Tmed_ts <- ts(UDEP_estimado[,6], start=1991, frequency=12)

plot(Chusis_Tmax_anual_ts[,2])
ggplot2::autoplot(Chusis_Tmax_anual_ts[,2]) + xlab("Year") + ylab("Precipitación(mm)") +
  ggtitle("Precipitación anual Estación Chusis")

ma(Chusis_Tmax_anual_ts[,2], 5)
autoplot(ma(Chusis_rain_anual_ts, 15))

autoplot(Chusis_rain_anual_ts, series="Data") +
  autolayer(ma(Chusis_rain_anual_ts,15), series="5-MA") +
  xlab("Year") + ylab("GWh") +
  ggtitle("Precipitación anual Estación Chusis") +
  scale_colour_manual(values=c("Data"="grey50","5-MA"="red"),
                      breaks=c("Data","5-MA"))


####Descomponer en series de tiempo
library(graphics)
library(forecast)


TSM_dfP <- cbind(Fecha  = make_date(year = df_P$year, 
                                    month = df_P$mes), df_P )
TSM_ts <- ts(TSM_dfP$tsm , start=1963, frequency=12)
  
autoplot(decompose(TSM_ts, type ="additive")) + 
  xlab("Año") +
 ggtitle("Serie de tiempo - Temperatura superficial del mar") +
  scale_x_continuous(breaks = seq(1963, 2020, 2 ))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
####Indentificando tendencia con ManKendall
attach(chusis_tmin_anual)
mk.test(temp_min)

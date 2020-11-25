###Gráficas de años niño

library(readxl)
library(ggplot2)
library(dplyr)
library(openxlsx)
library(lubridate)

####1982-1983
Estimados <- file.path("Data", "Estimación_de_datos_faltantes.xlsx")
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
                         fecha = make_date(year = chusis_estimado[309 : 320,]$year, 
                                           month =chusis_estimado[309 : 320,]$month))

Bernal_98 <- data.frame (rain = Bernal_estimado[309 : 320,]$rain,
                         temp_max = Bernal_estimado[309 : 320,]$temp_max,
                         temp_min = Bernal_estimado[309 : 320,]$temp_min,
                         fecha = make_date(year = Bernal_estimado[309 : 320,]$year, 
                                           month =Bernal_estimado[309 : 320,]$month))

SanMiguel_98 <- data.frame (rain = SanMiguel_estimado[309 : 320,]$rain,
                            temp_max = SanMiguel_estimado[309 : 320,]$temp_max,
                            temp_min = SanMiguel_estimado[309 : 320,]$temp_min,
                            fecha = make_date(year = SanMiguel_estimado[309 : 320,]$year, 
                                              month =SanMiguel_estimado[309 : 320,]$month))

Miraflores_98 <- data.frame (rain = Miraflores_estimado[309 : 320,]$rain,
                             temp_max = Miraflores_estimado[309 : 320,]$temp_max,
                             temp_min = Miraflores_estimado[309 : 320,]$temp_min,
                             fecha = make_date(year = Miraflores_estimado[309 : 320,]$year, 
                                               month =Miraflores_estimado[309 : 320,]$month))

UDEP_98 <- data.frame (rain = UDEP_estimado[81 : 92,]$rain,
                         temp_max = UDEP_estimado[81 : 92,]$temp_max,
                         temp_min = UDEP_estimado[81 : 92,]$temp_min,
                         fecha = make_date(year = UDEP_estimado[81 : 92,]$year, 
                                           month =UDEP_estimado[81 : 92,]$month))


###2016-2017
chusis_17 <- data.frame (rain = chusis_estimado[537 : 560,]$rain,
                         temp_max = chusis_estimado[537 : 560,]$temp_max,
                         temp_min = chusis_estimado[537 : 560,]$temp_min,
                         fecha = make_date(year = chusis_estimado[537 : 560,]$year, 
                                           month =chusis_estimado[537 : 560,]$month))

Bernal_17 <- data.frame (rain = Bernal_estimado[537 : 560,]$rain,
                         temp_max = Bernal_estimado[537 : 560,]$temp_max,
                         temp_min = Bernal_estimado[309 : 320,]$temp_min,
                         fecha = make_date(year = Bernal_estimado[537 : 560,]$year, 
                                           month =Bernal_estimado[537 : 560,]$month))

SanMiguel_17 <- data.frame (rain = SanMiguel_estimado[537 : 560,]$rain,
                            temp_max = SanMiguel_estimado[537 : 560,]$temp_max,
                            temp_min = SanMiguel_estimado[537 : 560,]$temp_min,
                            fecha = make_date(year = SanMiguel_estimado[537 : 560,]$year, 
                                              month =SanMiguel_estimado[537 : 560,]$month))

Miraflores_17 <- data.frame (rain = Miraflores_estimado[537 : 560,]$rain,
                             temp_max = Miraflores_estimado[537 : 560,]$temp_max,
                             temp_min = Miraflores_estimado[537 : 560,]$temp_min,
                             fecha = make_date(year = Miraflores_estimado[537 : 560,]$year, 
                                               month =Miraflores_estimado[309 : 320,]$month))

UDEP_17 <- data.frame (rain = UDEP_estimado[309 : 320,]$rain,
                       temp_max = UDEP_estimado[309 : 320,]$temp_max,
                       temp_min = UDEP_estimado[309 : 320,]$temp_min,
                       fecha = make_date(year = UDEP_estimado[309 : 320,]$year, 
                                         month =UDEP_estimado[309 : 320,]$month))



#colors <- c(  "Precipitación" = "blue")
##Precipitación
gg_fen <- ggplot(chusis_98 ,aes (label = round(rain, 1))) +
  #geom_line(aes(x = fecha, y = anomalia, color = "Anomalías")) +
  #ggplot(chusis_estimado,aes (label = round(rain, 1))) +
  geom_bar(aes(x = fecha, (y = rain)), stat = "identity", 
           fill = "#84c6ed", alpha = .85)+
  #geom_line(aes(x = dia, y = tsm_mean, color = "T. media"))+
  #geom_point(aes(x = dia, y = tsm_max), size = 0.1)+
  #geom_point(aes(x = dia, y = tsm_min)t, size = 0.1)+
  #geom_point(aes(x = dia, y = tsm_mean), size = 0.1)+
  scale_x_date(date_labels = "%m-%Y", breaks = seq.Date(as.Date("1997-09-01"), as.Date("1998-08-1"),
                                                     by = "1 month")) +
  scale_y_continuous(
    breaks = seq(0,800, 50),
    name = "Precipitación (mm)")+
    #sec.axis = sec_axis(~(.+4)*50, name = "Precipitación (mm)" ,
       #                 breaks = seq(0,800, 100)))+
  # geom_text(aes(x = month, y = mean_temp), nudge_y = 1) +
  labs(x = "Años", title =  "Precipitación acumulada mensual año 1997-1998 (Estación chusis)" 
       # caption = "Fuente: Yo\n*Datos desde 1991 a 2019",
       #color  = "Leyenda"
  ) +
  #scale_color_manual(values = colors) +
  theme_bw()
  gg_fen
gg_fen + theme(axis.text.x = element_text(angle = 90, hjust = 1))


colors <- c("Temperatura máxima" = "red", "Temperatura mínima" = "blue", "Precipitación"= "#84c6ed")
gg_fen_t <- ggplot(chusis_98 ,aes (label = round(temp_max, 1))) +
  geom_bar(aes(x = fecha, (y = rain/5),color = "Precipitación"), stat = "identity", 
           fill = "#84c6ed", alpha = .85)+
  geom_line(aes(x = fecha, y = temp_max, color = "Temperatura máxima")) +
  #ggplot(chusis_estimado,aes (label = round(rain, 1))) +
  geom_line(aes(x = fecha, y = temp_min, color = "Temperatura mínima"))+
  #geom_point(aes(x = dia, y = tsm_max), size = 0.1)+
  #geom_point(aes(x = dia, y = tsm_min)t, size = 0.1)+
  #geom_point(aes(x = dia, y = tsm_mean), size = 0.1)+
  scale_x_date(date_labels = "%m-%Y", breaks = seq.Date(as.Date("1997-09-01"), as.Date("1998-08-1"),
                                                        by = "1 month")) +
  scale_y_continuous(
    breaks = seq(0,40, 5),
    name = "Temperatura (°C)",
  sec.axis = sec_axis(~.*(10), name = "Precipitación(mm)",
                   breaks = seq(0,800, 50)))+
 #  geom_text(aes(x = month, y = T), nudge_y = 1) +
  labs(x = "Años", title =  "Temperatura máxima y mínima mensual año 1997-1998 (Estación chusis)",
       # caption = "Fuente: Yo\n*Datos desde 1991 a 2019",
       color  = "Leyenda" ) +
  scale_color_manual(values = colors) +
  theme_bw()

gg_fen_t + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
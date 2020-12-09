#####Climograma Bernal########
library(readxl)
library(ggplot2)
library(dplyr)
library(openxlsx)

datos_falt <- read_excel("Data/Estimación_de_datos_faltantes.xlsx",
                         sheet = "SanMiguel")
#datos_falt <- data.frame()
#[229:576,]

datos_falt_1 <- datos_falt[229:576,] %>% 
  group_by(month) %>% 
  summarise(mean_temp_max = mean(temp_max),
            mean_temp_min = mean(temp_min),
            mean_temp_med = mean(temp_med),
            mean_rain = mean(rain))
meses <- factor (c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago","Sep","Oct", "Nov", "Dic"))

datos_falt_2 <- cbind(datos_falt_1,meses)

colors <- c("Precipitación" = "#84c6ed", "T. máxima" = "red", "T. mínima" = "blue", "T. media" = "green")
datos_falt_2$meses = factor(datos_falt_2$meses, levels=c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago","Sep","Oct", "Nov", "Dic"))

climog <- datos_falt_2 %>% 
  ggplot(aes(label = round(mean_temp_max, 1), group = 1)) +
  geom_bar(aes(x = meses, y = mean_rain/2, color = "Precipitación"), stat = "identity", 
           fill = "#84c6ed", alpha = .85) +
  geom_line(aes(x = meses, y = mean_temp_max, color = "T. máxima")) +
  geom_line(aes(x = meses, y = mean_temp_min, color = "T. mínima"))+
  geom_line(aes(x = meses, y = mean_temp_med, color = "T. media"))+
  geom_point(aes(x = meses, y = mean_temp_max), size = 1.5)+
    geom_point(aes(x = meses, y = mean_temp_min), size = 1.5)+
    geom_point(aes(x = meses, y = mean_temp_med), size = 1.5)+
  #scale_x_continuous()  +
  #scale_x_date(date_labels = "%b", breaks = seq.Date(as.Date("1991-01"), as.Date("2019-12"),
   #                                                     by = "1 month"))
  scale_y_continuous(
    breaks = seq(0,40 ,5),
    name = "Temperatura (°C)",
    sec.axis = sec_axis(~.*(2), name = "Precipitación (mm)" ,
                        breaks = seq(0,80, 10)
                        )
  ) +
  # geom_text(aes(x = month, y = mean_temp), nudge_y = 1) +
  labs(x = "Meses", title = "Climograma de la Estación Metereológica San Miguel", 
      # caption = "Fuente: Yo\n*Datos desde 1991 a 2019",
       color  = ""
      ) +
  scale_color_manual(values = colors) +
  theme_bw()
#climog
climog + theme(legend.position="bottom") + guides(fill = guide_legend(nrow = 1))
gc()

########ANOMALÍAS##################################



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


###LLUVIA#############
####CHUSIS
normal_rain_ch <- chusis_estimado[109:468,]%>%
  group_by(year) %>%
  select(year:rain)%>%
  summarise( normal = sum(rain))
normal_prom <- mean(normal_rain_ch $normal)
normal_prom

chusis_rain_anual <- chusis_estimado%>%
  group_by(year) %>%
  select(year:rain)%>%
  summarise( rain = sum(rain))
chusis_anual

Year = 0
An_ch = c()
Anomalia_rain_chusis=0
as.data.frame(Anomalia_rain_chusis)
Y=0
for (i in 1 : 48) {
  Y=Y+1
  An_ch [Y] =  ((chusis_rain_anual[i,]$rain / normal_prom)-1)*100 
  Year[Y] = chusis_rain_anual[i,]$year
  Anomalia_rain_chusis <- data.frame(Year , An_ch)
  print(An_ch)
  
}


######SanMiguel
normal_rain_SM <- SanMiguel_estimado[109:468,]%>%
  group_by(year) %>%
  select(year:rain)%>%
  summarise( normal = sum(rain))
normal_prom <- mean(normal_rain_SM $normal)
normal_prom

SanMiguel_rain_anual <- SanMiguel_estimado%>%
  group_by(year) %>%
  select(year:rain)%>%
  summarise( rain = sum(rain))
SanMiguel_anual

Year = 0
An_SM = c()
Anomalia_rain_SM=0
as.data.frame(Anomalia_rain_SM)
Y=0
for (i in 1 : 48) {
  Y=Y+1
  An_SM [Y] =  ((SanMiguel_rain_anual[i,]$rain / normal_prom)-1)*100 
  Year[Y] = SanMiguel_rain_anual[i,]$year
  Anomalia_rain_SM <- data.frame(Year , An_SM)
  
}

###Miraflores

normal_rain_M <- Miraflores_estimado[109:468,]%>%
  group_by(year) %>%
  select(year:rain)%>%
  summarise( normal = sum(rain))
normal_prom <- mean(normal_rain_M $normal)
normal_prom

Miraflores_rain_anual <- Miraflores_estimado%>%
  group_by(year) %>%
  select(year:rain)%>%
  summarise( rain = sum(rain))


Year = 0
An_M = c()
Anomalia_rain_M=0
as.data.frame(Anomalia_rain_M)
Y=0
for (i in 1 : 48) {
  Y=Y+1
  An_M [Y] =  ((Miraflores_rain_anual [i,]$rain / normal_prom)-1)*100 
  Year[Y] = Miraflores_rain_anual[i,]$year
  Anomalia_rain_M <- data.frame(Year , An_M)
  
}

##UDEP

normal_rain_U <- UDEP_estimado %>%
  group_by(year) %>%
  select(year:rain)%>%
  summarise( normal = sum(rain))
normal_prom <- mean(normal_rain_U $normal)
normal_prom

UDEP_rain_anual <- UDEP_estimado%>%
  group_by(year) %>%
  select(year:rain)%>%
  summarise( rain = sum(rain))


Year = 0
An_U = c()
Anomalia_rain_U = 0
as.data.frame(Anomalia_rain_U)
Y=0
for (i in 1 : 29) {
  Y=Y+1
  An_U [Y] =  ((UDEP_rain_anual[i,]$rain / normal_prom)-1)*100 
  Year[Y] = UDEP_rain_anual[i,]$year
  Anomalia_rain_U <- data.frame(Year , An_U)
  
}


##Bernal

normal_rain_B <- Bernal_estimado[109:468,]%>%
  group_by(year) %>%
  select(year:rain)%>%
  summarise( normal = sum(rain))
normal_prom <- mean(normal_rain_B $normal)
normal_prom

Bernal_rain_anual <- Bernal_estimado%>%
  group_by(year) %>%
  select(year:rain)%>%
  summarise( rain = sum(rain))


Year = 0
An_B = c()
Anomalia_rain_B=0
as.data.frame(Anomalia_rain_B)
Y=0
for (i in 1 : 48) {
  Y=Y+1
  An_B [Y] =  ((Bernal_rain_anual[i,]$rain / normal_prom)-1)*100 
  Year[Y] = Bernal_rain_anual[i,]$year
  Anomalia_rain_B <- data.frame(Year , An_B)
  
}

###Gráfica anomalia Lluvia#####################################
#WALTER <- (Cambiar colores de negativos y positivos)
#Trazar linea 0
Anomalia_rain_B%>% 
ggplot(aes(label = round(An_B, 1))) +
  geom_bar(aes(x = Year, y = An_M), stat = "identity", 
           fill = "#84c6ed", alpha = .85) +
  scale_x_continuous(breaks = 1972:2019) +
  scale_y_continuous(
    breaks = seq(-100,1000 ,50),
    name = "Anomalía (mm)") +
  # geom_text(aes(x = month, y = mean_temp), nudge_y = 1) +
  labs(x = "Años", title = "Variabilidad interanual de los acumulados anuales de precipitación \n expresada por sus anomalías en la estación - Estación Bernal" 
       # caption = "Fuente: Yo\n*Datos desde 1991 a 2019",
  ) +
  theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1))








###Anomalías de Tmax y T min #################
#WALTER <- (Cambiar colores de negativos y positivos)
##chusis

normal_temp_max_ch <- chusis_estimado[229:576,]%>%
  group_by(year) %>%
  select(year:temp_max)%>%
  summarise( normal = mean(temp_max))
normal_prom <- mean(normal_temp_max_ch $normal)
normal_prom

chusis_tmax_anual <- chusis_estimado[229:576,]%>%
  group_by(year) %>%
  select(year:temp_max)%>%
  summarise( temp_max = mean(temp_max))
chusis_anual

Year = 0
An_ch = c()
Anomalia_temp_max_chusis=0
as.data.frame(Anomalia_temp_max_chusis)
Y=0
for (i in 1 : 29) {
  Y=Y+1
  An_ch [Y] =  (chusis_tmax_anual [i,]$temp_max - normal_prom) 
  Year[Y] = chusis_tmax_anual [i,]$year
  Anomalia_temp_max_chusis <- data.frame(Year , An_ch)
  
}

##SanMiguel
normal_temp_max_SM <- SanMiguel_estimado[229:576,]%>%
  group_by(year) %>%
  select(year:temp_max)%>%
  summarise( normal = mean(temp_max))
normal_prom <- mean(normal_temp_max_SM $normal)
normal_prom

SanMiguel_tmax_anual <- SanMiguel_estimado [229:576,]%>%
  group_by(year) %>%
  select(year:temp_max)%>%
  summarise( temp_max = mean(temp_max))
SanMiguel_anual

Year = 0
An_SM = c()
Anomalia_temp_max_SM=0
as.data.frame(Anomalia_temp_max_SM)
Y=0
for (i in 1 : 29) {
  Y=Y+1
  An_SM [Y] =  (SanMiguel_tmax_anual[i,]$temp_max-normal_prom)
  Year[Y] = SanMiguel_tmax_anual[i,]$year
  Anomalia_temp_max_SM <- data.frame(Year , An_SM)
  
}

###Miraflores

normal_temp_max_M <- Miraflores_estimado[229:576,]%>%
  group_by(year) %>%
  select(year:temp_max)%>%
  summarise( normal = mean(temp_max))
normal_prom <- mean(normal_temp_max_M $normal)
normal_prom

Miraflores_tmax_anual <- Miraflores_estimado[229:576,]%>%
  group_by(year) %>%
  select(year:temp_max)%>%
  summarise( temp_max = mean(temp_max))


Year = 0
An_M = c()
Anomalia_temp_max_M=0
as.data.frame(Anomalia_temp_max_M)
Y=0
for (i in 1 : 29) {
  Y=Y+1
  An_M [Y] =  (Miraflores_tmax_anual[i,]$temp_max - normal_prom)
  Year[Y] = Miraflores_tmax_anual[i,]$year
  Anomalia_temp_max_M <- data.frame(Year , An_M)
  
}

##UDEP

normal_temp_max_U <- UDEP_estimado %>%
  group_by(year) %>%
  select(year:temp_max)%>%
  summarise( normal = mean(temp_max))
normal_prom <- mean(normal_temp_max_U $normal)
normal_prom

UDEP_tmax_anual <- UDEP_estimado%>%
  group_by(year) %>%
  select(year:temp_max)%>%
  summarise( temp_max = mean(temp_max))


Year = 0
An_U = c()
Anomalia_temp_max_U = 0
as.data.frame(Anomalia_temp_max_U)
Y=0
for (i in 1 : 29) {
  Y=Y+1
  An_U [Y] =  (UDEP_tmax_anual[i,]$temp_max - normal_prom) 
  Year[Y] = UDEP_tmax_anual[i,]$year
  Anomalia_temp_max_U <- data.frame(Year , An_U)
  
}


##Bernal

normal_temp_max_B <- Bernal_estimado[229:576,]%>%
  group_by(year) %>%
  select(year:temp_max)%>%
  summarise( normal = mean(temp_max))
normal_prom <- mean(normal_temp_max_B $normal)
normal_prom

Bernal_tmax_anual <- Bernal_estimado[229:576,]%>%
  group_by(year) %>%
  select(year:temp_max)%>%
  summarise( temp_max = mean(temp_max))


Year = 0
An_B = c()
Anomalia_temp_max_B=0
as.data.frame(Anomalia_temp_max_B)
Y=0
for (i in 1 : 29) {
  Y=Y+1
  An_B [Y] =  (Bernal_tmax_anual[i,]$temp_max- normal_prom)
  Year[Y] = Bernal_tmax_anual[i,]$year
  Anomalia_temp_max_B <- data.frame(Year , An_B)
  
}

###Gráfica anomalia Tmax#####################################
Anomalia_temp_max_B%>% 
  ggplot(aes(label = round(An_B, 1))) +
  geom_bar(aes(x = Year, y = An_B), stat = "identity", 
           fill = "#84c6ed", alpha = .85) +
  scale_x_continuous(breaks = 1991:2019) +
  scale_y_continuous(
    breaks = seq(-5,5 ,1),
    name = "Anomalía (°C)") +
  # geom_text(aes(x = month, y = mean_temp), nudge_y = 1) +
  labs(x = "Años", title = "Variabilidad interanual de los promedios anuales de temperatura máxima expresada por sus anomalías en la estación
-Estación Bernal" 
       # caption = "Fuente: Yo\n*Datos desde 1991 a 2019",
  ) +
  theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1))






#####Anomalias Temp_Mínima
##Chusis
normal_temp_min_ch <- chusis_estimado[229:576,]%>%
  group_by(year) %>%
  select(year:temp_min)%>%
  summarise( normal = mean(temp_min))
normal_prom <- mean(normal_temp_min_ch $normal)
normal_prom

chusis_tmin_anual <- chusis_estimado[229:576,]%>%
  group_by(year) %>%
  select(year:temp_min)%>%
  summarise( temp_min = mean(temp_min))
chusis_anual

Year = 0
An_ch = c()
Anomalia_temp_min_chusis=0
as.data.frame(Anomalia_temp_min_chusis)
Y=0
for (i in 1 : 29) {
  Y=Y+1
  An_ch [Y] =  (chusis_tmin_anual[i,]$temp_min - normal_prom) 
  Year[Y] = chusis_tmin_anual[i,]$year
  Anomalia_temp_min_chusis <- data.frame(Year , An_ch)
  
}

##SanMiguel
normal_temp_min_SM <- SanMiguel_estimado[229:576,]%>%
  group_by(year) %>%
  select(year:temp_min)%>%
  summarise( normal = mean(temp_min))
normal_prom <- mean(normal_temp_min_SM $normal)
normal_prom

SanMiguel_tmin_anual <- SanMiguel_estimado [229:576,]%>%
  group_by(year) %>%
  select(year:temp_min)%>%
  summarise( temp_min = mean(temp_min))


Year = 0
An_SM = c()
Anomalia_temp_min_SM=0
as.data.frame(Anomalia_temp_min_SM)
Y=0
for (i in 1 : 29) {
  Y=Y+1
  An_SM [Y] =  (SanMiguel_tmin_anual[i,]$temp_min-normal_prom)
  Year[Y] = SanMiguel_tmin_anual[i,]$year
  Anomalia_temp_min_SM <- data.frame(Year , An_SM)
  
}

###Miraflores

normal_temp_min_M <- Miraflores_estimado[229:576,]%>%
  group_by(year) %>%
  select(year:temp_min)%>%
  summarise( normal = mean(temp_min))
normal_prom <- mean(normal_temp_min_M $normal)
normal_prom

Miraflores_tmin_anual <- Miraflores_estimado[229:576,]%>%
  group_by(year) %>%
  select(year:temp_min)%>%
  summarise( temp_min = mean(temp_min))


Year = 0
An_M = c()
Anomalia_temp_min_M=0
as.data.frame(Anomalia_temp_min_M)
Y=0
for (i in 1 : 29) {
  Y=Y+1
  An_M [Y] =  (Miraflores_tmin_anual[i,]$temp_min - normal_prom)
  Year[Y] = Miraflores_tmin_anual[i,]$year
  Anomalia_temp_min_M <- data.frame(Year , An_M)
  
}

##UDEP

normal_temp_min_U <- UDEP_estimado %>%
  group_by(year) %>%
  select(year:temp_min)%>%
  summarise( normal = mean(temp_min))
normal_prom <- mean(normal_temp_min_U $normal)
normal_prom

UDEP_tmin_anual <- UDEP_estimado%>%
  group_by(year) %>%
  select(year:temp_min)%>%
  summarise( temp_min = mean(temp_min))


Year = 0
An_U = c()
Anomalia_temp_min_U = 0
as.data.frame(Anomalia_temp_min_U)
Y=0
for (i in 1 : 29) {
  Y=Y+1
  An_U [Y] =  (UDEP_tmin_anual[i,]$temp_min - normal_prom) 
  Year[Y] = UDEP_tmin_anual[i,]$year
  Anomalia_temp_min_U <- data.frame(Year , An_U)
  
}


##Bernal

normal_temp_min_B <- Bernal_estimado[229:576,]%>%
  group_by(year) %>%
  select(year:temp_min)%>%
  summarise( normal = mean(temp_min))
normal_prom <- mean(normal_temp_min_B $normal)
normal_prom

Bernal_tmin_anual <- Bernal_estimado[229:576,]%>%
  group_by(year) %>%
  select(year:temp_min)%>%
  summarise( temp_min = mean(temp_min))


Year = 0
An_B = c()
Anomalia_temp_min_B=0
as.data.frame(Anomalia_temp_min_B)
Y=0
for (i in 1 : 29) {
  Y=Y+1
  An_B [Y] =  (Bernal_tmin_anual[i,]$temp_min- normal_prom)
  Year[Y] = Bernal_tmin_anual[i,]$year
  Anomalia_temp_min_B <- data.frame(Year , An_B)
  
}


###Gráfica anomalia Tmin#####################################
#WALTER <- (Cambiar colores de negativos y positivos)
Anomalia_temp_min_B%>% 
  ggplot(aes(label = round(An_B, 1))) +
  geom_bar(aes(x = Year, y = An_B), stat = "identity", 
           fill = "#84c6ed", alpha = .85) +
  scale_x_continuous(breaks = 1991:2019) +
  scale_y_continuous(
    breaks = seq(-5,5 ,1),
    name = "Anomalía (°C)") +
  # geom_text(aes(x = month, y = mean_temp), nudge_y = 1) +
  labs(x = "Años", title = "Variabilidad interanual de los promedios anuales de Temperatura mínima expresada por sus anomalías en la estación
-Estación Bernal" 
       # caption = "Fuente: Yo\n*Datos desde 1991 a 2019",
  ) +
  theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

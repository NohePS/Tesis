#==================================================
# DESCRIPCIoN: DATOS CHUSIS
# AUTOR(ES): Nohelia e Isabella
#==================================================

## Establecer el Working Directory

setwd("C:/R/Tesis")

##Cargar librerias)

library(gridExtra)
library(grid)
library(openxlsx)
library(dplyr)
library(ggplot2)
library(assertive)
library(naniar)
library(lubridate)
library(readxl)
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

# Modo de Walter
data_tsm <- read_excel("./Data/Paita.xlsx", sheet = 'TSM_2019', skip = 1)
data_tsm <- data_tsm %>% 
  mutate(fecha = dmy(fecha))

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




#####Gráfico de tsm FEN####
FEN_tsm_83 <- df_P[237:248,]
FEN_tsm_98 <- df_P[417:428,]
FEN_tsm_17 <- df_P[645:656,]
Fen <- cbind(FEN_tsm_83, tsm_98 = FEN_tsm_98$tsm,tsm_17 = FEN_tsm_17$tsm,
             fecha = make_date(year = FEN_tsm_98$year_tsm, 
                               month =FEN_tsm_98$mes))


colors <- c( "FEN 1982-1983" = "brown", "FEN 1997-1998" = "purple", "FEN 2016-2017" = "orange")





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
  labs(x = "Días", title =  "Promedio diario de Temperatura superficial del mar en Paita 1985-2014", 
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
  geom_bar(aes(x = fecha, (y = rain/50 -4), color = "Precipitación"), 
           stat = "identity",  alpha = .85)+
  scale_x_date(date_labels = "%Y", breaks = seq.Date(as.Date("1972-01-01"), 
                                                     as.Date("2019-12-1"),
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
  theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(legend.position="bottom") + guides(fill = guide_legend(nrow = 1))
 # scale_fill_gradient(name = "Leyenda", labels = c('Anomalías', 'Precipitación'), values = colors) 
gg_an_rain

gg_an_rain$layers[[2]] <- geom_segment(mapping = aes(x = fecha, y = rain / 50 -4, 
                                              xend = fecha, yend = -4),size = 1.2) 
gg_an_rain 
 



########TSM VS LLUVIA############################
<<<<<<< HEAD
##Essperar opinión del Ingeniero
#Para UDEP 337 Y demás estaciones empieza df_P en 109
TSM_vs_Rain <- data.frame(year = df_P[337:684,]$year_tsm, mes = df_P[337:684,]$mes,
                          tsm =df_P[337:684,]$tsm,rain = UDEP_estimado$rain,
                          fecha = make_date(year =  df_P[337:684,]$year_tsm, month =df_P[337:684,]$mes )) 
model_tsm_rain <- lm.fit (tsm ~ rain, data = TSM_vs_Rain)
TSM_vs_Rain
summary(model_tsm_rain)

ggplot(TSM_vs_Rain, aes(x = tsm, y =rain)) + geom_point() +geom_smooth(method = loess, formula = y ~ x ) + 
  labs(x = "Temperatura superficial del mar(°C)", 
       title =  "Gráfico de relación entre Tsm y Precipitación de la\nestación UDEP", 
       y = "Precipitación (mm)")+
  scale_x_continuous(breaks = seq(10,30, 2))+
  scale_y_continuous(breaks = seq(0,800, 100))+
  geom_vline(xintercept=24, color = "red")+
  geom_vline(xintercept=21, color = "green")+ theme_bw()
=======
##WALTER <- ecuación 
TSM_vs_Rain <- data.frame(year = df_P[109:684,]$year_tsm, 
                          mes = df_P[109:684,]$mes,
                          tsm =df_P[109:684,]$tsm,
                          rain = chusis_estimado$rain,
                          fecha = make_date(year =  df_P[109:684,]$year_tsm,
                                            month =df_P[109:684,]$mes )) 

model_tsm_rain <- lm.fit(tsm ~ rain, data = TSM_vs_Rain)

summary(model_tsm_rain)

ggplot(TSM_vs_Rain, aes(x = tsm, y =rain)) + 
  geom_point() 
>>>>>>> 7290bd547166e217ee92d28bc3f84075dc521c77


# Modo de Walter
tsm_rain <- TSM_vs_Rain %>% 
  as_tibble() %>% 
  mutate(rain = rain + 1)

# Coeficientes de un modelo potencial y = a*x^b ~ log(y) = log(a) + b.log(x)
lm(log(rain) ~ log(tsm), data = tsm_rain)

tsm_rain %>%
  mutate(y_est = exp(-13.467) * rain ^ 4.807) %>%
  ggplot(aes(x = tsm, y = rain)) +
  geom_point() +
  geom_line(aes(x = tsm, y = y_est), col = "blue") 

####Modo Walter

model_tsm_rain <- lm(log(rain +1 )~log(tsm), data=TSM_vs_Rain)
ggplot

write.xlsx(TSM_vs_Rain, file = "TSM_vs_Rain.xlsx", colNames = TRUE)


####Boxplot mensuales 
UDEP_box_plot <- data.frame(mes = factor(UDEP_estimado$month),
                              year = UDEP_estimado$year, 
                              rain = UDEP_estimado$rain,
                              temp_max = UDEP_estimado$temp_max, 
                              temp_min = UDEP_estimado$temp_min,
                              temp_med = UDEP_estimado$temp_med)

tsm_box_plot <- data.frame(mes = factor(df_P$mes),
                           year =df_P$year_tsm, tsm = df_P$tsm)
#[229:576,]
b1 <- ggplot(data = SanMiguel_box_plot, aes(mes,rain)) +
  scale_x_discrete(limits = factor(1:12), labels= c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago","Sep","Oct", "Nov", "Dic"))+
  labs(x = "Meses", 
       title =  "Precipitación Acumulada", 
       y = "Precipitación (mm)")+
  geom_boxplot()+ theme_bw()+
  stat_summary(fun=mean, geom="point", shape=18,
             size=2, color="red")+
  theme (text = element_text(size=9), 
         plot.title = element_text(hjust = 0.5))
      

b2 <-  ggplot(data = SanMiguel_box_plot[229:576,], aes(mes,temp_med)) +
  scale_x_discrete(limits = factor(1:12), labels= c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago","Sep","Oct", "Nov", "Dic"))+
  labs(x = "Meses", 
       title =  "Temperatura media", 
       y = "Temperatura (°C)")+
  geom_boxplot()+ theme_bw()+
  stat_summary(fun=mean, geom="point", shape=18,
               size=2, color="red")+
  theme (text = element_text(size=9), 
         plot.title = element_text(hjust = 0.5))

b3 <-  ggplot(data = SanMiguel_box_plot[229:576,], aes(mes,temp_max)) +
  scale_x_discrete(limits = factor(1:12), labels= c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago","Sep","Oct", "Nov", "Dic"))+
  labs(x = "Meses", 
       title =  "Temperatura máxima", 
       y = "Temperatura (°C)")+
  geom_boxplot()+ theme_bw()+
  stat_summary(fun=mean, geom="point", shape=18,
               size=2, color="red")+
  theme (text = element_text(size=9), 
         plot.title = element_text(hjust = 0.5))

b4 <-  ggplot(data = SanMiguel_box_plot[229:576,], aes(mes,temp_min)) +
  scale_x_discrete(limits = factor(1:12), labels= c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago","Sep","Oct", "Nov", "Dic"))+
  labs(x = "Meses", 
       title =  "Temperatura mínima", 
       y = "Temperatura (°C)")+
  geom_boxplot()+ theme_bw()+
  stat_summary(fun=mean, geom="point", shape=18,
               size=2, color="red")+
  theme (text = element_text(size=9), 
         plot.title = element_text(hjust = 0.5))

grid.arrange(b1, b2, b3, b4, ncol=2, nrow=2, 
             top =  textGrob("Box Plot de la estación San Miguel", gp = gpar(fontsize = 13, fontface = 'bold')))

gc()
#####Estaciones
verano_chusis <- chusis_estimado%>%
  group_by(year) %>%
  filter(month == 1 |month == 2 | month == 3 )%>%
  select(year:temp_med)%>%
  summarise(rain = sum(rain),
            temp_max = max(temp_max),
            temp_min = min(temp_min),
            temp_med = mean(temp_med))

otoño_chusis <- chusis_estimado%>%
  group_by(year) %>%
  filter(month == 4 |month == 5 | month == 6 )%>%
  select(year:temp_med)%>%
  summarise(rain = sum(rain),
            temp_max = max(temp_max),
            temp_min = min(temp_min),
            temp_med = mean(temp_med))

inverno_chusis <- chusis_estimado%>%
  group_by(year) %>%
  filter(month == 7 |month == 8 | month == 9 )%>%
  select(year:temp_med)%>%
  summarise(rain = sum(rain),
            temp_max = max(temp_max),
            temp_min = min(temp_min),
            temp_med = mean(temp_med))

primavera_chusis <- chusis_estimado%>%
  group_by(year) %>%
  filter(month == 10 |month == 11 | month == 12 )%>%
  select(year:temp_med)%>%
  summarise(rain = sum(rain),
            temp_max = max(temp_max),
            temp_min = min(temp_min),
            temp_med = mean(temp_med))
 

verano_Bernal <- Bernal_estimado%>%
  group_by(year) %>%
  filter(month == 1 |month == 2 | month == 3 )%>%
  select(year:temp_med)%>%
  summarise(rain = sum(rain),
            temp_max = max(temp_max),
            temp_min = min(temp_min),
            temp_med = mean(temp_med))

otoño_Bernal <- Bernal_estimado%>%
  group_by(year) %>%
  filter(month == 4 |month == 5 | month == 6 )%>%
  select(year:temp_med)%>%
  summarise(rain = sum(rain),
            temp_max = max(temp_max),
            temp_min = min(temp_min),
            temp_med = mean(temp_med))

inverno_Bernal <- Bernal_estimado%>%
  group_by(year) %>%
  filter(month == 7 |month == 8 | month == 9 )%>%
  select(year:temp_med)%>%
  summarise(rain = sum(rain),
            temp_max = max(temp_max),
            temp_min = min(temp_min),
            temp_med = mean(temp_med))

primavera_Bernal <- Bernal_estimado%>%
  group_by(year) %>%
  filter(month == 10 |month == 11 | month == 12 )%>%
  select(year:temp_med)%>%
  summarise(rain = sum(rain),
            temp_max = max(temp_max),
            temp_min = min(temp_min),
            temp_med = mean(temp_med))


verano_Miraflores <- Miraflores_estimado%>%
  group_by(year) %>%
  filter(month == 1 |month == 2 | month == 3 )%>%
  select(year:temp_med)%>%
  summarise(rain = sum(rain),
            temp_max = max(temp_max),
            temp_min = min(temp_min),
            temp_med = mean(temp_med))

otoño_Miraflores <- Miraflores_estimado%>%
  group_by(year) %>%
  filter(month == 4 |month == 5 | month == 6 )%>%
  select(year:temp_med)%>%
  summarise(rain = sum(rain),
            temp_max = max(temp_max),
            temp_min = min(temp_min),
            temp_med = mean(temp_med))

inverno_Miraflores <- Miraflores_estimado%>%
  group_by(year) %>%
  filter(month == 7 |month == 8 | month == 9 )%>%
  select(year:temp_med)%>%
  summarise(rain = sum(rain),
            temp_max = max(temp_max),
            temp_min = min(temp_min),
            temp_med = mean(temp_med))

primavera_Miraflores <- Miraflores_estimado%>%
  group_by(year) %>%
  filter(month == 10 |month == 11 | month == 12 )%>%
  select(year:temp_med)%>%
  summarise(rain = sum(rain),
            temp_max = max(temp_max),
            temp_min = min(temp_min),
            temp_med = mean(temp_med))



verano_SanMiguel <- SanMiguel_estimado%>%
  group_by(year) %>%
  filter(month == 1 |month == 2 | month == 3 )%>%
  select(year:temp_med)%>%
  summarise(rain = sum(rain),
            temp_max = max(temp_max),
            temp_min = min(temp_min),
            temp_med = mean(temp_med))

otoño_SanMiguel <- SanMiguel_estimado%>%
  group_by(year) %>%
  filter(month == 4 |month == 5 | month == 6 )%>%
  select(year:temp_med)%>%
  summarise(rain = sum(rain),
            temp_max = max(temp_max),
            temp_min = min(temp_min),
            temp_med = mean(temp_med))

inverno_SanMiguel <- SanMiguel_estimado%>%
  group_by(year) %>%
  filter(month == 7 |month == 8 | month == 9 )%>%
  select(year:temp_med)%>%
  summarise(rain = sum(rain),
            temp_max = max(temp_max),
            temp_min = min(temp_min),
            temp_med = mean(temp_med))

primavera_SanMiguel <- SanMiguel_estimado%>%
  group_by(year) %>%
  filter(month == 10 |month == 11 | month == 12 )%>%
  select(year:temp_med)%>%
  summarise(rain = sum(rain),
            temp_max = max(temp_max),
            temp_min = min(temp_min),
            temp_med = mean(temp_med))



verano_UDEP <- UDEP_estimado%>%
  group_by(year) %>%
  filter(month == 1 |month == 2 | month == 3 )%>%
  select(year:temp_med)%>%
  summarise(rain = sum(rain),
            temp_max = max(temp_max),
            temp_min = min(temp_min),
            temp_med = mean(temp_med))

otoño_UDEP <- UDEP_estimado%>%
  group_by(year) %>%
  filter(month == 4 |month == 5 | month == 6 )%>%
  select(year:temp_med)%>%
  summarise(rain = sum(rain),
            temp_max = max(temp_max),
            temp_min = min(temp_min),
            temp_med = mean(temp_med))

invierno_UDEP <- UDEP_estimado%>%
  group_by(year) %>%
  filter(month == 7 |month == 8 | month == 9 )%>%
  select(year:temp_med)%>%
  summarise(rain = sum(rain),
            temp_max = max(temp_max),
            temp_min = min(temp_min),
            temp_med = mean(temp_med))

primavera_UDEP <- UDEP_estimado%>%
  group_by(year) %>%
  filter(month == 10 |month == 11 | month == 12 )%>%
  select(year:temp_med)%>%
  summarise(rain = sum(rain),
            temp_max = max(temp_max),
            temp_min = min(temp_min),
            temp_med = mean(temp_med))


primavera_Bernal <- cbind(estacion = "Primavera", primavera_Bernal)


chusis_estaciones <- rbind(verano_chusis, otoño_chusis, inverno_chusis, primavera_chusis)
Bernal_estaciones <- rbind(verano_Bernal, otoño_Bernal, inverno_Bernal, primavera_Bernal)
Miraflores_estaciones <- rbind(verano_Miraflores, otoño_Miraflores, inverno_Miraflores, primavera_Miraflores)
SanMiguel_estaciones <- rbind(verano_SanMiguel, otoño_SanMiguel, inverno_SanMiguel, primavera_SanMiguel)
UDEP_estaciones <- rbind(verano_UDEP, otoño_UDEP, invierno_UDEP, primavera_UDEP)

UDEP_estaciones$estacion <- as.factor(UDEP_estaciones$estacion)



a1 <- ggplot(data = UDEP_estaciones, aes(estacion,rain)) +
  scale_x_discrete( limits = factor(c("Verano", "Otoño", "Invierno", "Primavera")), labels = c("V","O","I","P"))+
  labs(x = "Estaciones del año",  
       title ="Precipitación acum.",
       y = "Precipitación (mm)")+
  geom_boxplot()+ theme_bw()+
  scale_y_continuous(breaks = seq(0,1000,150))+
  stat_summary(fun=mean, geom="point", shape=18,
               size=2, color="red")+
  theme (text = element_text(size=9))


a2 <- ggplot(data = UDEP_estaciones, aes(estacion,temp_max)) +
  scale_x_discrete( limits = factor(c("Verano", "Otoño", "Invierno", "Primavera")), labels = c("V","O","I","P"))+
  labs(x = "Estaciones del año", 
       title = "Temperatura máxima",
       y = "Temperatura (°C)")+
  scale_y_continuous(breaks = seq(0,40,3))+
  geom_boxplot()+ theme_bw()+
  stat_summary(fun=mean, geom="point", shape=18,
               size=2, color="red")+
  theme (text = element_text(size=9))

#[c(20:48,68:96,116:144,164:192),]
a3 <- ggplot(data = UDEP_estaciones, aes(estacion,temp_min)) +
  scale_x_discrete( limits = factor(c("Verano", "Otoño", "Invierno", "Primavera")), labels = c("V","O","I","P"))+
  labs(x = "Estaciones del año", 
       title = "Temperatura mínima",
       y = "Temperatura (°C)")+
  scale_y_continuous(breaks = seq(0,40,3))+
  geom_boxplot()+ theme_bw()+
  stat_summary(fun=mean, geom="point", shape=18,
               size=2, color="red")+
  theme (text = element_text(size=9))

a4 <- ggplot(data = UDEP_estaciones, aes(estacion,temp_med)) +
  scale_x_discrete( limits = factor(c("Verano", "Otoño", "Invierno", "Primavera")), labels = c("V","O","I","P"))+
  labs(x = "Estaciones del año",
       title = "Temperatura media",
       y = "Temperatura (°C)")+
  scale_y_continuous(breaks = seq(0,40,3))+
  geom_boxplot()+ theme_bw()+
  stat_summary(fun=mean, geom="point", shape=18,
               size=2, color="red")+
  theme (text = element_text(size=9))




library(gridExtra)
library(grid)
install.packages("grid")
grid.arrange(a1, a2, a3,a4, ncol=4, nrow=1, 
             top =  textGrob("Box Plot de la estación UDEP", gp = gpar(fontsize = 13, fontface = 'bold')))
         


##Estadísticos Descriptivos
summary(df_P$tsm)
sd(df_P$tsm)
var(df_P$tsm)



#==================================================
# DESCRIPCIoN: Estimación de datos faltantes en la estacion Bernal
#Udep y Senamhi
# AUTOR(ES): Nohelia e Isabella
#==================================================

## Establecer el Working Directory



setwd("C:/R/Tesis")

##Cargar librerias
library(tibble)
library(openxlsx)
library(dplyr)
library(ggplot2)
library(assertive)
library(naniar)
library(lubridate)
library(MASS)
library(mosaicData)
library(simputation)
library(gtools)




#############Modelos de Bernal#########################################

model_B1 <- lm (Bernal_rain  ~   chusis_rain + miraflores_rain + miguel_rain + UDEP_rain,
          data = data_rain)
#model_B2 <- lm(Bernal_rain  ~   chusis_rain + miraflores_rain + miguel_rain,
#summary(model_B2)
model_B3 <- lm(Bernal_rain  ~   chusis_rain + miraflores_rain + UDEP_rain,
            data = data_rain)
model_B4 <- lm(Bernal_rain  ~   chusis_rain + miguel_rain + UDEP_rain ,
               data = data_rain)
#model_B5 <- lm(Bernal_rain  ~   miraflores_rain + miguel_rain + UDEP_rain ,
#              data = data_rain)

model_B6 <- lm(Bernal_rain  ~   chusis_rain + miraflores_rain,
               data = data_rain)
model_B7 <- lm(Bernal_rain  ~   chusis_rain + miguel_rain,
               data = data_rain)
model_B8 <- lm(Bernal_rain  ~   chusis_rain + UDEP_rain,
               data = data_rain)
model_B10 <- lm(Bernal_rain  ~   miraflores_rain + UDEP_rain,
                data = data_rain)
model_B11 <- lm(Bernal_rain  ~   miguel_rain + UDEP_rain,
                data = data_rain)
model_B12 <- lm (Bernal_rain ~   UDEP_rain + mallares_rain + Esperanza_rain,
                 data = data_rain)
model_B13 <- lm (Bernal_rain ~   UDEP_rain + Esperanza_rain,
                 data = data_rain)
model_B14 <- lm (Bernal_rain ~   chusis_rain + mallares_rain,
                 data = data_rain)
model_B15 <- lm (Bernal_rain ~   Esperanza_rain + mallares_rain,
                 data = data_rain)
model_B16 <- lm (Bernal_rain ~   miguel_rain + Esperanza_rain,
                 data = data_rain)
model_B17 <- lm (Bernal_rain ~   chusis_rain + Esperanza_rain + miguel_rain + mallares_rain
                 + miraflores_rain +0,
                 data = data_rain)
summary(model_B17)

############Código de estimación###################
n=0  
n <- c()
m <- 0
k=1


for (j in 1971:2019){
  cont=0
  for (i in 1:length(Bernal_bymonth$year)){
    
    
    if(Bernal_bymonth$year[i] == j){
      
      if(is.na(Bernal_bymonth$rain_month[i]) == TRUE){
        cont = cont +1
      }else {
        cont=cont
      }
      
      print(cont)
    }
  }
  n[k]=cont
  k=k+1
}
#n = numero de meses faltantes por año (lluvia)

n
m
data_faltante <- c(1:588) 
#data_faltante


library(dplyr)
n <-  data.frame(n)
#n

valor<-c()
n_datos <- data.frame(meses_faltantes = n)
#n_datos
for (i in 1:49) {
  
  if (n[i,]>=1) {
    valor[i]="yes" 
  }else{
    valor[i]="no" 
  }
}
valor <-data.frame(valor)
valor
n_datos <- data.frame(año=1971:2019, valor)
n_datos



#s es los meses

year<-0
h= c()
k=1
j=0
g = c()
jian = c()
yue = c()
est = c()
as.data.frame(faltantes)
data_predictora <- 0
for (i in 1:49) {
  for (r in 1:672) { #Filas en los meses de lluvia
    
    if (n_datos$valor[i]=="yes"){
      
      for (s in 1:12){
        if (data_rain$year[r]==n_datos$año[i] && data_rain$month[r]== s #&& is.na(data_rain$miguel_rain[r])==FALSE 
            #&& is.na(data_rain$UDEP_rain[r])==FALSE 
            #&&is.na(data_rain$miraflores_rain[r])==FALSE 
            #&& is.na(data_rain$chusis_rain[r])==FALSE
            #&& is.na(data_rain$Esperanza_rain[r])==FALSE
            #&& is.na(data_rain$mallares_rain[r])==FALSE
            #&& is.na(data_rain$cerritos_rain[r])==FALSE
            && is.na(data_rain$Bernal_rain[r])==TRUE
            ){
          j=j+1
          model_B17 <- lm (Bernal_rain ~   chusis_rain + Esperanza_rain + miguel_rain + mallares_rain
                           + miraflores_rain,
                           data = data_rain)
          data_predictora <- tibble(#UDEP_rain = data_rain[r,]$UDEP_rain,
                                    miguel_rain = data_rain[r,]$miguel_rain,
                                    miraflores_rain = data_rain[r,]$miraflores_rain,
                                    chusis_rain = data_rain[r,]$chusis_rain,
                                    mallares_rain = data_rain[r,]$mallares_rain,
                                    Esperanza_rain = data_rain[r,]$Esperanza_rain,
                                    #cerritos_rain = data_rain[r,]$cerritos_rain
          )
          
          g[j] = predict(model_B17, data_predictora)
          print (g)
          
          
          #}else{
          
          jian[j] = n_datos$año[i]
          yue[j] = s
          est[j] = g[j]
          
          faltantes <- data.frame(jian, yue, est)
          
          
        }
      }
      
    } 
    
  }
  #k=k+1
  #print(h)
}
print(data_predictora) 

g
faltantes













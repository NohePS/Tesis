#==================================================
# DESCRIPCIoN: Estimación de datos faltantes en la estacion UDEP
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




#############Modelos de UDEP#########################################


model_U1 <- lm(UDEP_rain  ~   chusis_rain + miraflores_rain + miguel_rain + Bernal_rain,
               data = data_rain)
model_U2 <- lm(UDEP_rain  ~   chusis_rain + miraflores_rain + miguel_rain,
               data = data_rain)
model_U3 <- lm(UDEP_rain  ~   chusis_rain + miraflores_rain + Bernal_rain,
               data = data_rain)
model_U4 <- lm(UDEP_rain  ~   chusis_rain + miguel_rain + Bernal_rain ,
               data = data_rain)
model_U5 <- lm(UDEP_rain  ~   miraflores_rain + miguel_rain + Bernal_rain ,
               data = data_rain)
model_U6 <- lm(UDEP_rain  ~   chusis_rain + miraflores_rain,
               data = data_rain)
model_U7 <- lm(UDEP_rain  ~   chusis_rain + miguel_rain,
               data = data_rain)
model_U8 <- lm(UDEP_rain  ~   chusis_rain + Bernal_rain,
               data = data_rain)
model_U9 <- lm(UDEP_rain  ~   miraflores_rain + miguel_rain,
               data = data_rain)
model_U10 <- lm(UDEP_rain  ~   miraflores_rain + Bernal_rain,
                data = data_rain)
model_U11 <- lm(UDEP_rain  ~   miguel_rain + Bernal_rain,
                data = data_rain)

summary(model_U11)

############Código de estimación###################
n=0  
n <- c()
m <- 0
k=1


for (j in 1991:2019){
  cont=0
  for (i in 1:length(data_UDEP_na$year)){
    
    
    if(data_UDEP_na$year[i] == j){
      
      if(is.na(data_UDEP_na$rain[i]) == TRUE){
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
data_faltante <- c(241:588) 
#data_faltante


library(dplyr)
n <-  data.frame(n)
#n

valor<-c()
n_datos <- data.frame(meses_faltantes = n)
#n_datos
for (i in 1:29) {
  
  if (n[i,]>=1) {
    valor[i]="yes" 
  }else{
    valor[i]="no" 
  }
}
valor <-data.frame(valor)
valor
n_datos <- data.frame(año=1991:2019, valor)
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
for (i in 1:29) {
  for (r in 241:588) { #Filas en los meses de lluvia
    
    if (n_datos$valor[i]=="yes"){
      
      for (s in 1:12){
        if (data_rain$year[r]==n_datos$año[i] && data_rain$month[r]== s && is.na(data_rain$miguel_rain[r])==FALSE 
            #&& is.na(data_rain$chusis_rain[r])==FALSE 
            #&&is.na(data_rain$miraflores_rain[r])==FALSE 
            && is.na(data_rain$Bernal_rain[r])==FALSE
            && is.na(data_rain$UDEP_rain[r])==TRUE){
          j=j+1
          
          model_U11 <- lm(UDEP_rain  ~   miguel_rain + Bernal_rain,
                          data = data_rain)
          data_predictora <- tibble(#chusis_rain = data_rain[r,]$chusis_rain,
                                    miguel_rain = data_rain[r,]$miguel_rain,
                                    #miraflores_rain = data_rain[r,]$miraflores_rain,
                                    Bernal_rain = data_rain[r,]$Bernal_rain
          )
          
          g[j] = predict(model_U11, data_predictora)
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



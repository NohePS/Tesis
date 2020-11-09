#==================================================
# DESCRIPCIoN: Estimación de datos faltantes en la estacion Chusis
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




#############Modelos de chusis#########################################

model_C1 <- lm(chusis_rain  ~   Bernal_rain + miraflores_rain + miguel_rain + UDEP_rain,
               data = data_rain)
model_C2 <- lm(chusis_rain  ~   Bernal_rain + miraflores_rain + miguel_rain,
               data = data_rain)
model_C3 <- lm(chusis_rain  ~   Bernal_rain + miraflores_rain + UDEP_rain,
               data = data_rain)
model_C4 <- lm(chusis_rain  ~   Bernal_rain + miguel_rain + UDEP_rain ,
               data = data_rain)
#model_C5 <- lm(chusis_rain  ~   miraflores_rain + miguel_rain + UDEP_rain ,
 #             data = data_rain)
model_C6 <- lm(chusis_rain  ~   Bernal_rain + miraflores_rain,
               data = data_rain)
model_C7 <- lm(chusis_rain  ~   Bernal_rain + miguel_rain,
               data = data_rain)
model_C8 <- lm(chusis_rain  ~   Bernal_rain + UDEP_rain,
               data = data_rain)
#model_C9 <- lm(chusis_rain  ~   miraflores_rain + miguel_rain,
  #           data = data_rain)
#model_C10 <- lm(chusis_rain  ~   miraflores_rain + UDEP_rain,
 #               data = data_rain)
#model_C11 <- lm(chusis_rain  ~   miguel_rain + UDEP_rain,
 #               data = data_rain)

model_C12 <- lm(chusis_rain  ~   Bernal_rain + mallares_rain + Esperanza_rain,
                 data = data_rain)
model_C13 <- lm(chusis_rain  ~   Bernal_rain + miraflores_rain + Esperanza_rain,
                 data = data_rain)
model_C14 <- lm(chusis_rain  ~   miguel_rain + miraflores_rain + Esperanza_rain,
                 data = data_rain)
model_C15 <- lm(chusis_rain  ~   UDEP_rain + mallares_rain + Esperanza_rain,
                 data = data_rain)
model_C16 <- lm(chusis_rain  ~   UDEP_rain + Esperanza_rain,
                 data = data_rain)
#model_C3.3 <- lm(chusis_rain  ~   Bernal_rain + Esperanza_rain,
               #W#  data = data_rain)
      
            
summary(model_C17)
#Estimaciones con Cerritos


model_C17 <- lm(chusis_rain  ~   UDEP_rain + cerritos_rain,
                data = data_rain)



summary(model_C18)

############Código de estimación###################
n=0  
n <- c()
m <- 0
k=1


for (j in 1971:2019){
  cont=0
  for (i in 1:length(chusis_bymonth$year)){
    
    
    if(chusis_bymonth$year[i] == j){
      
      if(is.na(chusis_bymonth$rain_month[i]) == TRUE){
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
#n_datos



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
            && is.na(data_rain$UDEP_rain[r])==FALSE 
            #&&is.na(data_rain$miraflores_rain[r])==FALSE 
            #&& is.na(data_rain$Bernal_rain[r])==FALSE
            && is.na(data_rain$Esperanza_rain[r])==FALSE
            #&& is.na(data_rain$mallares_rain[r])==FALSE
            #&& is.na(data_rain$cerritos_rain[r])==FALSE
            && is.na(data_rain$chusis_rain[r])==TRUE){
          j=j+1
          model_C17 <- lm(chusis_rain  ~   UDEP_rain + Esperanza_rain,
                          data = data_rain)
          data_predictora <- tibble(UDEP_rain = data_rain[r,]$UDEP_rain,
            #miguel_rain = data_rain[r,]$miguel_rain,
            #miraflores_rain = data_rain[r,]$miraflores_rain,
            #Bernal_rain = data_rain[r,]$Bernal_rain,
            #mallares_rain = data_rain[r,]$mallares_rain,
            Esperanza_rain = data_rain[r,]$Esperanza_rain,
            #cerritos_rain = data_rain[r,]$cerritos_rain
          )
          
          g[j] = predict(model_C17, data_predictora)
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



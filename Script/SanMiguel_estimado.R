#==================================================
# DESCRIPCIoN: Estimación de datos faltantes en la estacion San Miguel
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




#############Modelos de miguel#########################################

model_SM1 <- lm(miguel_rain  ~   chusis_rain + miraflores_rain + Bernal_rain + UDEP_rain,
               data = data_rain)
model_SM2 <- lm(miguel_rain  ~   chusis_rain + miraflores_rain + Bernal_rain,
               data = data_rain)
model_SM3 <- lm(miguel_rain  ~   chusis_rain + miraflores_rain + UDEP_rain,
               data = data_rain)
model_SM4 <- lm(miguel_rain  ~   chusis_rain + Bernal_rain + UDEP_rain ,
               data = data_rain)
model_SM5 <- lm(miguel_rain  ~   miraflores_rain + Bernal_rain + UDEP_rain ,
               data = data_rain)
model_SM6 <- lm(miguel_rain  ~   chusis_rain + miraflores_rain,
               data = data_rain)
model_SM7 <- lm(miguel_rain  ~   chusis_rain + Bernal_rain,
               data = data_rain)
model_SM8 <- lm(miguel_rain  ~   chusis_rain + UDEP_rain,
               data = data_rain)
model_SM9 <- lm(miguel_rain  ~   miraflores_rain + Bernal_rain,
               data = data_rain)
model_SM10 <- lm(miguel_rain  ~   miraflores_rain + UDEP_rain,
                data = data_rain)
model_SM11 <- lm(miguel_rain  ~   Bernal_rain + UDEP_rain,
                data = data_rain)

summary(model_SM18)
model_SM12 <- lm(miguel_rain  ~   mallares_rain + UDEP_rain + Esperanza_rain,
                 data = data_rain)
model_SM13 <- lm(miguel_rain  ~   mallares_rain + Esperanza_rain,
                  data = data_rain)
model_SM14 <- lm(miguel_rain  ~   mallares_rain + UDEP_rain + Esperanza_rain + miraflores_rain,
                  data = data_rain)
model_SM15 <- lm(miguel_rain  ~   mallares_rain + Esperanza_rain + miraflores_rain,
                  data = data_rain)
model_SM16 <- lm(miguel_rain  ~   Esperanza_rain  + miraflores_rain,
                  data = data_rain)
model_SM17 <- lm(miguel_rain  ~   mallares_rain  + miraflores_rain,
                  data = data_rain)
model_SM18 <- lm(miguel_rain  ~   UDEP_rain+ cerritos_rain,
                 data = data_rain)

############Código de estimación###################
n=0  
n <- c()
m <- 0
k=1


for (j in 1971:2019){
  cont=0
  for (i in 1:length(Miguel_bymonth$year)){
    
    
    if(Miguel_bymonth$year[i] == j){
      
      if(is.na(Miguel_bymonth$rain_month[i]) == TRUE){
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
        if (data_rain$year[r]==n_datos$año[i] && data_rain$month[r]== s #&& is.na(data_rain$miraflores_rain[r])==FALSE 
            && is.na(data_rain$UDEP_rain[r])==FALSE 
            #&&is.na(data_rain$chusis_rain[r])==FALSE 
            #&& is.na(data_rain$Bernal_rain[r])==FALSE
            #&&is.na(data_rain$mallares_rain[r])==FALSE 
            &&is.na(data_rain$cerritos_rain[r])==FALSE
            #&&is.na(data_rain$Esperanza_rain[r])==FALSE 
            && is.na(data_rain$miguel_rain[r])==TRUE){
          j=j+1
          
          model_SM18 <- lm(miguel_rain  ~   UDEP_rain+ cerritos_rain,
                           data = data_rain)
          
          data_predictora <- tibble(UDEP_rain = data_rain[r,]$UDEP_rain,
                                    #miraflores_rain = data_rain[r,]$miraflores_rain,
                                    #chusis_rain = data_rain[r,]$chusis_rain,
                                    #Bernal_rain = data_rain[r,]$Bernal_rain,
                                    cerritos_rain = data_rain[r,]$cerritos_rain,
                                    #mallares_rain = data_rain[r,]$mallares_rain,
                                    #Esperanza_rain = data_rain[r,]$Esperanza_rain
          )
          
          g[j] = predict(model_SM18, data_predictora)
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




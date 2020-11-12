#==================================================
# DESCRIPCIoN: Estimación de datos faltantes en la estacion miguel
#Udep y Senamhi
# AUTOR(ES): Nohelia e Isabella
#==================================================

## Establecer el Working Directory

setwd("C:/R/Tesis")
installed.packages("installr")

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
miss_var_summary (data_temp_min)
model_SMtmin_1 <- lm(miguel_temp_min  ~    miraflores_temp_min+
                        UDEP_temp_min,
                     data = data_temp_min)#0.9199

model_SMtmin_2 <-  lm(miguel_temp_min  ~   Esperanza_temp_min + mallares_temp_min + miraflores_temp_min+
                        chusis_temp_min,
                      data = data_temp_min)# 0.9466
model_SMtmin_3 <- lm(miguel_temp_min  ~   Esperanza_temp_min + miraflores_temp_min+
                       chusis_temp_min,
                     data = data_temp_min) #R 2 =0.9453
model_SMtmin_4 <-lm(miguel_temp_min  ~   Esperanza_temp_min + miraflores_temp_min+ mallares_temp_min,
                  data = data_temp_min) #0.9352
model_SMtmin_5 <-lm(miguel_temp_min ~   Esperanza_temp_min + chusis_temp_min+ mallares_temp_min,
                    data = data_temp_min) #0.9323
model_SMtmin_6 <-lm(miguel_temp_min  ~   chusis_temp_min + miraflores_temp_min+ mallares_temp_min,
                    data = data_temp_min) #0.9452
model_SMtmin_7 <-lm(miguel_temp_min  ~   Esperanza_temp_min + mallares_temp_min,
                    data = data_temp_min)#0.9141
model_SMtmin_8 <-lm(miguel_temp_min ~   Esperanza_temp_min + miraflores_temp_min,
                    data = data_temp_min) #0.9324
model_SMtmin_9 <-lm(miguel_temp_min  ~   Esperanza_temp_min + chusis_temp_min,
                    data = data_temp_min)#0.9179
model_SMtmin_10 <-lm(miguel_temp_min  ~   mallares_temp_min + miraflores_temp_min,
                    data = data_temp_min) #0.9287
model_SMtmin_11 <-lm(miguel_temp_min  ~   mallares_temp_min + chusis_temp_min,
                     data = data_temp_min) #0.9295
model_SMtmin_12 <-lm(miguel_temp_min  ~   mallares_temp_min + chusis_temp_min ,
                     data = data_temp_min) #0.9295
summary(model_SMtmin_12)
#Estimaciones con Cerritos




############Código de estimación###################
n=0  
n <- c()
m <- 0
k=1


for (j in 1971:2019){
  cont=0
  for (i in 1:length(Miguel_bymonth$year)){
    
    
    if(Miguel_bymonth$year[i] == j){
      
      if(is.na(Miguel_bymonth$min_temp_min[i]) == TRUE){
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


#library(dplyr)
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
        if (data_temp_min$year[r]==n_datos$año[i] && data_temp_min$month[r]== s 
            && is.na(data_temp_min$chusis_temp_min[r])==FALSE 
            #&& is.na(data_temp_min$UDEP_temp_min[r])==FALSE 
            #&&is.na(data_temp_min$miraflores_temp_min[r])==FALSE 
            #&& is.na(data_temp_min$Bernal_temp_min[r])==FALSE
            #&& is.na(data_temp_min$Esperanza_temp_min[r])==FALSE
            && is.na(data_temp_min$mallares_temp_min[r])==FALSE
           
            && is.na(data_temp_min$miguel_temp_min[r])==TRUE){
          j=j+1
          model_SMtmin_12 <-lm(miguel_temp_min  ~   mallares_temp_min + chusis_temp_min ,
                               data = data_temp_min)
          data_predictora <- tibble(#UDEP_temp_min = data_temp_min[r,]$UDEP_temp_min,
            chusis_temp_min = data_temp_min[r,]$chusis_temp_min,
            #miraflores_temp_min = data_temp_min[r,]$miraflores_temp_min,
            #Bernal_temp_min = data_temp_min[r,]$Bernal_temp_min,
            mallares_temp_min = data_temp_min[r,]$mallares_temp_min,
            #Esperanza_temp_min = data_temp_min[r,]$Esperanza_temp_min,
            
          )
          
          g[j] = predict(model_SMtmin_12, data_predictora)
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
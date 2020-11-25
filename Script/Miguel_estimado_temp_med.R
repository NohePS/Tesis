#==================================================
# DESCRIPCIoN: Estimación de datos faltantes en la estacion miguel
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
miss_var_summary (data_temp_med)
model_SMtmed_1 <- lm(miguel_temp_med  ~    miraflores_temp_med+
                       UDEP_temp_med,
                     data = data_temp_med)#0.9688

model_SMtmed_2 <-  lm(miguel_temp_med  ~   UDEP_temp_med + mallares_temp_med + chusis_temp_med+
                       chusis_temp_med,
                      data = data_temp_med)# 0.954
model_SMtmed_3 <- lm(miguel_temp_med  ~   Esperanza_temp_med + miraflores_temp_med+
                       chusis_temp_med,
                     data = data_temp_med) #R 2 =0.9765
#model_SMtmed_4 <-lm(miguel_temp_med  ~   Esperanza_temp_med + miraflores_temp_med+ mallares_temp_med,
#                    data = data_temp_med) #0.9776
model_SMtmed_5 <-lm(miguel_temp_med ~   Esperanza_temp_med + chusis_temp_med+ mallares_temp_med,
                    data = data_temp_med) #0.9642
#model_SMtmed_6 <-lm(miguel_temp_med  ~   chusis_temp_med + miraflores_temp_med+ mallares_temp_med,
#                    data = data_temp_med) #0.9452
model_SMtmed_6 <-lm(miguel_temp_med  ~   Esperanza_temp_med + mallares_temp_med,
                    data = data_temp_med)#0.9481
#model_SMtmed_7 <-lm(miguel_temp_med ~   Esperanza_temp_med + miraflores_temp_med,
 #                   data = data_temp_med) #0.9324
model_SMtmed_7 <-lm(miguel_temp_med  ~   Esperanza_temp_med + chusis_temp_med,
                    data = data_temp_med)#0.9508
model_SMtmed_8 <-lm(miguel_temp_med  ~   mallares_temp_med + miraflores_temp_med,
                     data = data_temp_med) #0.9773
model_SMtmed_9 <-lm(miguel_temp_med  ~   mallares_temp_med + chusis_temp_med,
                     data = data_temp_med) #0.9612
#model_SMtmed_10 <-lm(miguel_temp_med  ~   mallares_temp_med + chusis_temp_med ,
 #                    data = data_temp_med) #0.9295
summary(model_SMtmed_2)
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
      
      if(is.na(Miguel_bymonth$med_temp_med[i]) == TRUE){
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
        if (data_temp_med$year[r]==n_datos$año[i] && data_temp_med$month[r]== s 
            && is.na(data_temp_med$chusis_temp_med[r])==FALSE 
            && is.na(data_temp_med$UDEP_temp_med[r])==FALSE 
            #&&is.na(data_temp_med$miraflores_temp_med[r])==FALSE 
            #&& is.na(data_temp_med$Bernal_temp_med[r])==FALSE
            #&& is.na(data_temp_med$Esperanza_temp_med[r])==FALSE
            && is.na(data_temp_med$mallares_temp_med[r])==FALSE
            
            && is.na(data_temp_med$miguel_temp_med[r])==TRUE){
          j=j+1
          model_SMtmed_2 <-  lm(miguel_temp_med  ~   UDEP_temp_med + mallares_temp_med + chusis_temp_med+
                                  chusis_temp_med,
                                data = data_temp_med)
          data_predictora <- tibble(UDEP_temp_med = data_temp_med[r,]$UDEP_temp_med,
            chusis_temp_med = data_temp_med[r,]$chusis_temp_med,
            #miraflores_temp_med = data_temp_med[r,]$miraflores_temp_med,
            #Bernal_temp_med = data_temp_med[r,]$Bernal_temp_med,
            mallares_temp_med = data_temp_med[r,]$mallares_temp_med,
            #Esperanza_temp_med = data_temp_med[r,]$Esperanza_temp_med,
            
          )
          
          g[j] = predict(model_SMtmed_2, data_predictora)
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
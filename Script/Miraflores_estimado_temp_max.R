#==================================================
# DESCRIPCIoN: Estimación de datos faltantes en la estacion miraflores
#Udep y Senamhi
# AUTOR(ES): Nohelia e Isabella
#==================================================

## Establecer el Working Directory

setwd("C:/R/Tesis")
#installed.packages("installr")

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




#############Modelos de miraflores#########################################
miss_var_summary (data_temp_max)
model_Mtmax_1 <- lm(miraflores_temp_max  ~   Esperanza_temp_max + mallares_temp_max + chusis_temp_max+
                      miguel_temp_max + UDEP_temp_max,
                    data = data_temp_max)#0.9214
#model_Mtmax_2 <-  lm(miraflores_temp_max  ~   Esperanza_temp_max + mallares_temp_max + chusis_temp_max+
 #                    miguel_temp_max,
  #                data = data_temp_max)
model_Mtmax_2 <- lm(miraflores_temp_max  ~   Esperanza_temp_max + chusis_temp_max+
                      miguel_temp_max,
                    data = data_temp_max) # 0.9065
model_Mtmax_3 <-lm(miraflores_temp_max  ~   Esperanza_temp_max + chusis_temp_max+ mallares_temp_max,
                   data = data_temp_max) # 0.8865
model_Mtmax_4 <-lm(miraflores_temp_max ~   Esperanza_temp_max + miguel_temp_max+ mallares_temp_max,
                   data = data_temp_max) #0.9033
model_Mtmax_5 <-lm(miraflores_temp_max  ~   miguel_temp_max + chusis_temp_max+ mallares_temp_max,
                   data = data_temp_max) #0.8961
model_Mtmax_6 <-lm(miraflores_temp_max  ~   Esperanza_temp_max + mallares_temp_max,
                   data = data_temp_max)# 0.8498
model_Mtmax_7 <-lm(miraflores_temp_max ~   Esperanza_temp_max + chusis_temp_max,
                   data = data_temp_max) #0.8745
model_Mtmax_8 <-lm(miraflores_temp_max  ~   Esperanza_temp_max + miguel_temp_max,
                   data = data_temp_max)#0.9021
model_Mtmax_9 <-lm(miraflores_temp_max  ~   mallares_temp_max + chusis_temp_max,
                   data = data_temp_max) #0.8811
model_Mtmax_10 <-lm(miraflores_temp_max  ~   mallares_temp_max + miguel_temp_max,
                    data = data_temp_max) #0.8739
model_Mtmax_11 <-lm(miraflores_temp_max  ~   chusis_temp_max + miguel_temp_max,
                    data = data_temp_max) #0.9043
model_Mtmax_12 <-lm(miraflores_temp_max  ~   chusis_temp_max + UDEP_temp_max,
                    data = data_temp_max)#0.9085

summary(model_Mtmax_13)
#Estimaciones con Cerritos






############Código de estimación###################
n=0  
n <- c()
m <- 0
k=1


for (j in 1971:2019){
  cont=0
  for (i in 1:length(miraflores_bymonth$year)){
    
    
    if(miraflores_bymonth$year[i] == j){
      
      if(is.na(miraflores_bymonth$max_temp_max[i]) == TRUE){
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
        if (data_temp_max$year[r]==n_datos$año[i] && data_temp_max$month[r]== s 
            #&& is.na(data_temp_max$miguel_temp_max[r])==FALSE 
            && is.na(data_temp_max$UDEP_temp_max[r])==FALSE 
            &&is.na(data_temp_max$chusis_temp_max[r])==FALSE 
            #&& is.na(data_temp_max$Bernal_temp_max[r])==FALSE
            #&& is.na(data_temp_max$Esperanza_temp_max[r])==FALSE
            #&& is.na(data_temp_max$mallares_temp_max[r])==FALSE
            
            && is.na(data_temp_max$miraflores_temp_max[r])==TRUE){
          j=j+1
          model_Mtmax_12 <-lm(miraflores_temp_max  ~   chusis_temp_max + UDEP_temp_max,
                              data = data_temp_max)
          data_predictora <- tibble(UDEP_temp_max = data_temp_max[r,]$UDEP_temp_max,
            #miguel_temp_max = data_temp_max[r,]$miguel_temp_max,
            chusis_temp_max = data_temp_max[r,]$chusis_temp_max,
            #Bernal_temp_max = data_temp_max[r,]$Bernal_temp_max,
            #mallares_temp_max = data_temp_max[r,]$mallares_temp_max,
            #Esperanza_temp_max = data_temp_max[r,]$Esperanza_temp_max,
            
          )
          
          g[j] = predict(model_Mtmax_12, data_predictora)
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
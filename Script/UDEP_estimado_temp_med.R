#==================================================
# DESCRIPCIoN: Estimación de datos faltantes en la estacion UDEP
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




#############Modelos de UDEP#########################################
miss_var_summary (data_temp_med)
model_Utmed_1 <- lm(UDEP_temp_med  ~   Esperanza_temp_med  + miraflores_temp_med+ mallares_temp_med +
                     miguel_temp_med + chusis_temp_med,
                  data = data_temp_med)#0.9688
#model_Utmed_2 <- lm(UDEP_temp_med  ~   Esperanza_temp_med  + miraflores_temp_med +
 #                     miguel_temp_med + chusis_temp_med,
  #                  data = data_temp_med) #0.8394
#model_Utmed_2 <-  lm(UDEP_temp_med  ~   Esperanza_temp_med + mallares_temp_med + miraflores_temp_med+
 #                    miguel_temp_med,
  #               data = data_temp_med)
#model_Utmed_2 <- lm(UDEP_temp_med  ~   Esperanza_temp_med + miraflores_temp_med+
 #                     miguel_temp_med,
  #                  data = data_temp_med) #0.8154
model_Utmed_2 <-lm(UDEP_temp_med  ~   Esperanza_temp_med + miraflores_temp_med+ mallares_temp_med,
                  data = data_temp_med) #0.9678
model_Utmed_3 <-lm(UDEP_temp_med ~   Esperanza_temp_med + miguel_temp_med+ mallares_temp_med,
                 data = data_temp_med) #0.96
#model_Utmed_4 <-lm(UDEP_temp_med  ~   miguel_temp_med + miraflores_temp_med+ mallares_temp_med,
  #               data = data_temp_med) #0.7642
model_Utmed_4 <-lm(UDEP_temp_med  ~   Esperanza_temp_med + chusis_temp_med ,
                   data = data_temp_med)#0.9467
model_Utmed_5 <-lm(UDEP_temp_med  ~   Esperanza_temp_med + Bernal_temp_med ,
                   data = data_temp_med)#0.9463
model_Utmed_6 <-lm(UDEP_temp_med ~   Esperanza_temp_med + miraflores_temp_med,
                  data = data_temp_med) #0.9653
model_Utmed_7 <-lm(UDEP_temp_med  ~   Esperanza_temp_med + miguel_temp_med,
                   data = data_temp_med)#0.9592
model_Utmed_8 <-lm(UDEP_temp_med  ~   mallares_temp_med + miraflores_temp_med,
                  data = data_temp_med) #0.9586
model_Utmed_9 <-lm(UDEP_temp_med  ~   mallares_temp_med + miguel_temp_med,
                  data = data_temp_med) #0.9177
model_Utmed_10 <-lm(UDEP_temp_med  ~   miraflores_temp_med + chusis_temp_med ,
                  data = data_temp_med) #0.9543
summary(model_Utmed_10)
#Estimaciones con Cerritos



############Código de estimación###################
n=0  
n <- c()
m <- 0
k=1


for (j in 1971:2019){
  cont=0
  for (i in 1:length(data_UDEP_na$year)){
    
    
    if(data_UDEP_na$year[i] == j){
      
      if(is.na(data_UDEP_na$temp_med[i]) == TRUE){
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
            && is.na(data_temp_med$miguel_temp_med[r])==FALSE
            &&is.na(data_temp_med$miraflores_temp_med[r])==FALSE 
            #&& is.na(data_temp_med$Bernal_temp_med[r])==FALSE
            && is.na(data_temp_med$Esperanza_temp_med[r])==FALSE
            && is.na(data_temp_med$mallares_temp_med[r])==FALSE
            && is.na(data_temp_med$chusis_temp_med[r])==FALSE
            && is.na(data_temp_med$UDEP_temp_med[r])==TRUE){
          j=j+1
          model_Utmed_1 <- lm(UDEP_temp_med  ~   Esperanza_temp_med  + miraflores_temp_med+ mallares_temp_med +
                                miguel_temp_med + chusis_temp_med,
                              data = data_temp_med)
          data_predictora <- tibble(
            chusis_temp_med = data_temp_med[r,]$chusis_temp_med,
            miguel_temp_med = data_temp_med[r,]$miguel_temp_med,
            miraflores_temp_med = data_temp_med[r,]$miraflores_temp_med,
            #Bernal_temp_med = data_temp_med[r,]$Bernal_temp_med,
            mallares_temp_med = data_temp_med[r,]$mallares_temp_med,
            Esperanza_temp_med = data_temp_med[r,]$Esperanza_temp_med,
            
          )
          
          g[j] = predict(model_Utmed_1, data_predictora)
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



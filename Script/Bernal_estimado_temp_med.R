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

#############Modelos de miguel#########################################
miss_var_summary (data_temp_med)

model_Btmed_1 <- lm(Bernal_temp_med  ~    miraflores_temp_med+ chusis_temp_med,
                    data = data_temp_med)#0.9879

#model_Btmed_2 <- lm(Bernal_temp_med  ~    Esperanza_temp_med+
                     # chusis_temp_med,
                    #data = data_temp_med)#0.9154
model_Btmed_2 <- lm(Bernal_temp_med  ~    mallares_temp_med+
                      chusis_temp_med,
                    data = data_temp_med) #0.9848
model_Btmed_3 <- lm(Bernal_temp_med  ~    miguel_temp_med+ 
                      chusis_temp_med,
                    data = data_temp_med) #0.9287
model_Btmed_4 <- lm(Bernal_temp_med  ~    UDEP_temp_med+ 
                      mallares_temp_med,
                    data = data_temp_med) #0.9777
model_Btmed_5 <- lm(Bernal_temp_med  ~    miraflores_temp_med+ 
                      miguel_temp_med,
                    data = data_temp_med)#0.9856
model_Btmed_6 <- lm(Bernal_temp_med  ~    Esperanza_temp_med+ 
                      mallares_temp_med,
                    data = data_temp_med)#0.9702
model_Btmed_7 <- lm(Bernal_temp_med  ~    UDEP_temp_med+ 
                       miraflores_temp_med +chusis_temp_med  ,
                    data = data_temp_med)#0.9875
model_Btmed_8 <- lm(Bernal_temp_med  ~    miguel_temp_med+ 
                      Esperanza_temp_med,
                    data = data_temp_med)#0.989
#model_Btmed_9 <- lm(Bernal_temp_med  ~    miguel_temp_med+ 
 #                      mallares_temp_med,
  #                   data = data_temp_med)#0.8863
model_Btmed_10 <- lm(Bernal_temp_med  ~    UDEP_temp_med+ 
                       chusis_temp_med,
                     data = data_temp_med) #0.9835
model_Btmed_11 <- lm(Bernal_temp_med  ~    UDEP_temp_med+ 
                       miraflores_temp_med,
                     data = data_temp_med)#0.9821
summary(model_Btmed_11)
#Estimaciones con Cerritos




############Código de estimación###################
n=0  
n <- c()
m <- 0
k=1


for (j in 1971:2019){
  cont=0
  for (i in 1:length(Bernal_bymonth$year)){
    
    
    if(Bernal_bymonth$year[i] == j){
      
      if(is.na(Bernal_bymonth$med_temp_med[i]) == TRUE){
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
            #&& is.na(data_temp_med$chusis_temp_med[r])==FALSE 
            && is.na(data_temp_med$UDEP_temp_med[r])==FALSE 
            &&is.na(data_temp_med$miraflores_temp_med[r])==FALSE 
            #&& is.na(data_temp_med$miguel_temp_med[r])==FALSE
            #&& is.na(data_temp_med$Esperanza_temp_med[r])==FALSE
            #&& is.na(data_temp_med$mallares_temp_med[r])==FALSE
            
            && is.na(data_temp_med$Bernal_temp_med[r])==TRUE){
          j=j+1
          model_Btmed_11 <- lm(Bernal_temp_med  ~    UDEP_temp_med+ 
                                 miraflores_temp_med,
                               data = data_temp_med)
          data_predictora <- tibble(UDEP_temp_med = data_temp_med[r,]$UDEP_temp_med,
                                    #chusis_temp_med = data_temp_med[r,]$chusis_temp_med,
                                    miraflores_temp_med = data_temp_med[r,]$miraflores_temp_med,
                                    #miguel_temp_med = data_temp_med[r,]$miguel_temp_med,
                                    #mallares_temp_med = data_temp_med[r,]$mallares_temp_med,
                                    #Esperanza_temp_med = data_temp_med[r,]$Esperanza_temp_med,
                                    
          )
          
          g[j] = predict(model_Btmed_11, data_predictora)
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

write.xlsx(faltantes, file = "faltantes.xlsx", colNames = TRUE)


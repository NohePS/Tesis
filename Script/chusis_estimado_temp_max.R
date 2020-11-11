#==================================================
# DESCRIPCIoN: Estimación de datos faltantes en la estacion Chusis
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




#############Modelos de chusis#########################################
miss_var_summary (data_temp_max)
model_ctmax_1 <- lm(chusis_temp_max  ~   Esperanza_temp_max  + miraflores_temp_max+
                        UDEP_temp_max ,
               data = data_temp_max)#0.9036
#model_ctmax_2 <-  lm(chusis_temp_max  ~   Esperanza_temp_max + mallares_temp_max + miraflores_temp_max+
 #                      miguel_temp_max,
  #                   data = data_temp_max)
model_ctmax_2 <- lm(chusis_temp_max  ~   Esperanza_temp_max + miraflores_temp_max+
                       miguel_temp_max,
                      data = data_temp_max) #R 2 =0.9085
model_ctmax_3 <-lm(chusis_temp_max  ~   Esperanza_temp_max + miraflores_temp_max+ mallares_temp_max,
                   data = data_temp_max) #0.8915
model_ctmax_4 <-lm(chusis_temp_max ~   Esperanza_temp_max + miguel_temp_max+ mallares_temp_max,
                   data = data_temp_max) #0.8942
#model_ctmax_5 <-lm(chusis_temp_max  ~   miguel_temp_max + miraflores_temp_max+ mallares_temp_max,
 #                  data = data_temp_max) #0.9324
model_ctmax_6 <-lm(chusis_temp_max  ~   Esperanza_temp_max + mallares_temp_max,
                 data = data_temp_max)#0.8449
model_ctmax_7 <-lm(chusis_temp_max ~   Esperanza_temp_max + miraflores_temp_max,
                   data = data_temp_max) #0.8926
model_ctmax_8 <-lm(chusis_temp_max  ~   Esperanza_temp_max + miguel_temp_max,
                   data = data_temp_max)#0.8946
model_ctmax_9 <-lm(chusis_temp_max  ~   mallares_temp_max + miraflores_temp_max,
                   data = data_temp_max) #0.8671
model_ctmax_10 <-lm(chusis_temp_max  ~   mallares_temp_max + miguel_temp_max,
                   data = data_temp_max) #0.8422
model_ctmax_11 <-lm(chusis_temp_max  ~   miraflores_temp_max + miguel_temp_max,
                   data = data_temp_max) #0.8866
model_ctmax_12 <-lm(chusis_temp_max  ~   miraflores_temp_max + UDEP_temp_max,
                    data = data_temp_max) #0.8936
summary(model_ctmax_12)
#Estimaciones con Cerritos




summary(model_M1)

############Código de estimación###################
n=0  
n <- c()
m <- 0
k=1


for (j in 1971:2019){
  cont=0
  for (i in 1:length(chusis_bymonth$year)){
    
    
    if(chusis_bymonth$year[i] == j){
      
      if(is.na(chusis_bymonth$max_temp_max[i]) == TRUE){
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
            &&is.na(data_temp_max$miraflores_temp_max[r])==FALSE 
            #&& is.na(data_temp_max$Bernal_temp_max[r])==FALSE
            #&& is.na(data_temp_max$Esperanza_temp_max[r])==FALSE
            #&& is.na(data_temp_max$mallares_temp_max[r])==FALSE
            
            && is.na(data_temp_max$chusis_temp_max[r])==TRUE){
          j=j+1
          model_ctmax_12 <-lm(chusis_temp_max  ~   miraflores_temp_max + UDEP_temp_max,
                              data = data_temp_max)
          data_predictora <- tibble(UDEP_temp_max = data_temp_max[r,]$UDEP_temp_max,
                                    #miguel_temp_max = data_temp_max[r,]$miguel_temp_max,
                                    miraflores_temp_max = data_temp_max[r,]$miraflores_temp_max,
                                    #Bernal_temp_max = data_temp_max[r,]$Bernal_temp_max,
                                    #mallares_temp_max = data_temp_max[r,]$mallares_temp_max,
                                    #Esperanza_temp_max = data_temp_max[r,]$Esperanza_temp_max,
                                   
          )
          
          g[j] = predict(model_ctmax_12, data_predictora)
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
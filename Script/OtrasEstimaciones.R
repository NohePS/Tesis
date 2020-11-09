#Pruebas con estaciones Mallares y La esperanza

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

#model_B6.3 <- lm(Bernal_rain  ~   chusis_rain + Esperanza_rain+ mallares_rain + miraflores_rain + miguel_rain + UDEP_rain,
                 #data = data_rain)
summary(model_B6.3)
#model_B1.3 <- lm(Bernal_rain  ~   chusis_rain + Esperanza_rain + mallares_rain + miraflores_rain + miguel_rain,
                 #data = data_rain)
#model_B2.3 <- lm(Bernal_rain  ~   chusis_rain + Esperanza_rain + mallares_rain + miraflores_rain + UDEP_rain,
                 #data = data_rain)
#model_B3.3 <- lm(Bernal_rain  ~   chusis_rain + Esperanza_rain + mallares_rain + miguel_rain + UDEP_rain,
               #data = data_rain)
#model_B4.3 <- lm(Bernal_rain  ~   chusis_rain +Esperanza_rain + miraflores_rain + miguel_rain + UDEP_rain,
               #data = data_rain)
#model_B5.3 <- lm(Bernal_rain  ~   chusis_rain + mallares_rain + miraflores_rain + miguel_rain + UDEP_rain,
               #data = data_rain)
summary(model_B16.3)
#model_B7.3 <- lm(Bernal_rain  ~   chusis_rain +Esperanza_rain + miraflores_rain + miguel_rain ,
           #data = data_rain)
#model_B8.3 <- lm(Bernal_rain  ~   chusis_rain + mallares_rain + miraflores_rain + miguel_rain,
            #     data = data_rain)
#model_B9.3 <- lm(Bernal_rain  ~   chusis_rain + Esperanza_rain + mallares_rain + miraflores_rain,
             #    data = data_rain)
#model_B10.3 <- lm(Bernal_rain  ~   chusis_rain + Esperanza_rain + mallares_rain + miguel_rain,
              #   data = data_rain)
#model_B11.3 <- lm(Bernal_rain  ~   chusis_rain + Esperanza_rain + mallares_rain + UDEP_rain,
               #   data = data_rain)
#model_B12.3 <- lm(Bernal_rain  ~   chusis_rain + Esperanza_rain + miraflores_rain + UDEP_rain,
                #  data = data_rain)
#model_B13.3 <- lm(Bernal_rain  ~   chusis_rain + Esperanza_rain + UDEP_rain + miguel_rain,
                 # data = data_rain)
#model_B14.3 <- lm(Bernal_rain  ~   chusis_rain + miraflores_rain + mallares_rain + UDEP_rain,
                  #data = data_rain)
#model_B15.3 <- lm(Bernal_rain  ~   chusis_rain + UDEP_rain + mallares_rain + miguel_rain,
                  #data = data_rain)
#model_B17.3 <- lm(Bernal_rain  ~   chusis_rain + Esperanza_rain + mallares_rain,
                  #data = data_rain)
#model_B18.3 <- lm(Bernal_rain  ~   chusis_rain + Esperanza_rain + miraflores_rain,
                  #data = data_rain)
#model_B19.3 <- lm(Bernal_rain  ~   chusis_rain + Esperanza_rain + miguel_rain,
                  #data = data_rain)
#model_B20.3 <- lm(Bernal_rain  ~   chusis_rain + Esperanza_rain + UDEP_rain,
                  #data = data_rain)
#model_B21.3 <- lm(Bernal_rain  ~   chusis_rain + mallares_rain + miraflores_rain,
                  #data = data_rain)
#model_B22.3 <- lm(Bernal_rain  ~   chusis_rain + mallares_rain + miguel_rain,
                  #data = data_rain)
#model_B23.3 <- lm(Bernal_rain  ~   chusis_rain + mallares_rain + UDEP_rain,
                  #data = data_rain)
model_B24.3 <- lm(Bernal_rain  ~   Esperanza_rain + mallares_rain + miraflores_rain,
                  data = data_rain)
summary(model_B24.3)
#model_B25.3 <- lm(Bernal_rain  ~   Esperanza_rain + mallares_rain,
  #                data = data_rain)
#model_B26.3 <- lm(Bernal_rain  ~   Esperanza_rain + miraflores_rain,
                  #data = data_rain)
model_B27.3 <- lm(Bernal_rain  ~   Esperanza_rain + UDEP_rain,
                  data = data_rain)
#model_B28.3 <- lm(Bernal_rain  ~   Esperanza_rain + miguel_rain,
                  #data = data_rain)
#model_B29.3 <- lm(Bernal_rain  ~   Esperanza_rain + chusis_rain,
 #                 data = data_rain)
#model_B30.3 <- lm(Bernal_rain  ~   mallares_rain + miraflores_rain,
   #              data = data_rain)
#model_B31.3 <- lm(Bernal_rain  ~   mallares_rain + UDEP_rain,
 #                 data = data_rain)
#model_B32.3 <- lm(Bernal_rain  ~   mallares_rain + miguel_rain,
 #                 data = data_rain)
#model_B33.3 <- lm(Bernal_rain  ~   mallares_rain + chusis_rain,
  #                data = data_rain)




#Etmacion de Bernal
model_B8.3 <- lm(Bernal_rain  ~   chusis_rain + mallares_rain + miraflores_rain + miguel_rain,
                 data = data_rain)

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
        if (data_rain$year[r]==n_datos$año[i] && data_rain$month[r]== s 
            #&& is.na(data_rain$miguel_rain[r])==FALSE 
            #&& is.na(data_rain$UDEP_rain[r])==FALSE 
            #&&is.na(data_rain$miraflores_rain[r])==FALSE 
            #&& is.na(data_rain$chusis_rain[r])==FALSE
            && is.na(data_rain$Esperanza_rain[r])==FALSE
            #&& is.na(data_rain$mallares_rain[r])==FALSE
            && is.na(data_rain$Bernal_rain[r])==TRUE){
          j=j+1
          
          model_B24.3 <- lm(Bernal_rain  ~   Esperanza_rain + mallares_rain + miraflores_rain,
                            data = data_rain)
          data_predictora <- tibble(UDEP_rain = data_rain[r,]$UDEP_rain,
            #miguel_rain = data_rain[r,]$miguel_rain,
            #miraflores_rain = data_rain[r,]$miraflores_rain,
            #chusis_rain = data_rain[r,]$chusis_rain,
            #mallares_rain = data_rain[r, ]$mallares_rain,
            Esperanza_rain = data_rain[r, ]$Esperanza_rain
          )
          
          g[j] = predict(model_B27.3, data_predictora)
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





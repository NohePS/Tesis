#==================================================
# DESCRIPCIoN: Correlación y estimación de datos faltantes entre las 5 estaciones
#Udep y Senamhi
# AUTOR(ES): Nohelia e Isabella
#==================================================

## Establecer el Working Directory

setwd("C:/R/Tesis")
install.packages("gtools")

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

## Desarrollo -----------------------------------------------------------------------------
estaciones_year <- (1971:2019)
head(estaciones_year)

Bernal_t <-  Bernal_bymonth[85:672,]
Chusis_t <-  chusis_bymonth[85:672,]
chusis_rain_year <- chusis_byyear[8:56,]
Bernal_rain_year <- Bernal_byyear[8:56,]

df_estaciones <- data.frame(estaciones_year,
  chusis_rain_year$rain_month,Bernal_rain_year$rain_month, Miguel_byyear$rain_month, miraflores_byyear$rain_month,
  UDEP_byyear$rain_month, cerritos_byyear$rain_month )

#Estimación por año
cor_rain <- cor(data_rain[,3:10], use = "pairwise.complete.obs")



correlacion <- cor(data_rain[,3:7], use = "pairwise.complete.obs")

#Generar data frames por mes uniendo todas las estaciones 
#Enero 



#Estimar por mes Octubre del 2004 de chusis por Bernal

lm_Chusis_Bernal_octubre<- lm(Chusis_octubre$rain_month ~  Bernal_octubre$rain_month)
lm_Chusis_Bernal_octubre



#Estimas con funciwón Impute_lm
imp_Chusis_Bernal_octubre<- 
  Octubre %>%
  bind_shadow()%>%
  add_label_missings() %>%
  impute_lm(chusis_rain_oct ~  Bernal_rain_oct)
imp_Chusis_Bernal_octubre
# Using popdata, plot response vs. explanatory
ggplot(mi_df, aes(x =Bernal_bymonth$rain_month , y = chusis_bymonth$rain_month)) + 
  # Add a point layer
  geom_point() + 
  # Add a smooth trend layer, using lin. reg., no ribbon
  geom_smooth(method = "lm", se = FALSE) 



#Prueba para código predictor para años de estaciones con menos de 5 mese con NA

#for (i in names(Bernal_bymonth)){

#Data frame que contiene todoas las estaciones con el acumulado de lluvia por mes para cada año
data_rain <- data.frame(year = Miguel_bymonth$year , month = Miguel_bymonth$month, 
                        Bernal_rain = Bernal_t$rain_month, 
                        chusis_rain = Chusis_t$rain_month,
                        miraflores_rain = miraflores_bymonth$rain_month,
                        miguel_rain =  Miguel_bymonth$rain_month,
                        UDEP_rain = data_UDEP_na$rain,
                        Esperanza_rain = Esperanza_bymonth$rain_month,
                        mallares_rain = Mallares_bymonth$rain_month,
                        cerritos_rain = data_Cerritos_na$rain,
                        data_faltante)

data_temp_max <- data.frame(year = Miguel_bymonth$year, month = Miguel_bymonth$month, 
                        Bernal_temp_max = Bernal_t$max_temp_max, 
                        chusis_temp_max = Chusis_t$max_temp_max,
                        miraflores_temp_max = miraflores_bymonth$max_temp_max,
                        miguel_temp_max =  Miguel_bymonth$max_temp_max,
                        Esperanza_temp_max = Esperanza_bymonth$max_temp_max,
                        mallares_temp_max = Mallares_bymonth$max_temp_max,
                        UDEP_temp_max = data_UDEP_na[1:588,] $temp_max,
                        data_faltante)
cor_temp_max <- cor(data_temp_max[,3:9], use = "pairwise.complete.obs")
data_temp_min <- data.frame(year = Miguel_bymonth$year, month = Miguel_bymonth$month, 
                            Bernal_temp_min = Bernal_t$min_temp_min, 
                            chusis_temp_min = Chusis_t$min_temp_min,
                            miraflores_temp_min = miraflores_bymonth$min_temp_min,
                            miguel_temp_min =  Miguel_bymonth$min_temp_min,
                            Esperanza_temp_min = Esperanza_bymonth$min_temp_min,
                            mallares_temp_min = Mallares_bymonth$min_temp_min,
                            UDEP_temp_min = data_UDEP_na[1:588,]$temp_min,
                            data_faltante)

cor_temp_min <- cor(data_temp_min[,3:9], use = "pairwise.complete.obs")




#Modelos##############################


#model_x1 <- lm(x1 ~ x2 + x3 + x4, data = df)

y <- data_rain[13:588, ]$chusis_rain 
z <- data_rain[13:588, ]$miguel_rain
v <- data_rain[13:588, ]$miraflores_rain
#Prueba Walter
model_x1 <- lm(Bernal_rain  ~   UDEP_rain + miguel_rain + chusis_rain + miraflores_rain +0,
               data = data_rain) 
summary(model_x2)
model_x2 <- lm(chusis_rain  ~ miraflores_rain + miguel_rain + UDEP_rain,
               data = data_rain)
model_x3 <- lm(miguel_rain  ~  chusis_rain + miguel_rain ,
               data = data_rain)
model_x4 <- lm(miraflores_rain  ~ chusis_rain + miguel_rain + UDEP_rain,
               data = data_rain)
model_x5 <- lm(UDEP_rain  ~ chusis_rain + miguel_rain +  miraflores_rain,
               data = data_rain)
model_y1 <- lm(Bernal_rain  ~ miraflores_rain + UDEP_rain + 0,
               data = data_rain) 
model_y2 <- lm(chusis_rain  ~ miraflores_rain + UDEP_rain +0,
               data = data_rain)
model_y3 <- lm(miguel_rain  ~  chusis_rain + UDEP_rain +0,
               data = data_rain)
model_y4 <- lm(miraflores_rain  ~ chusis_rain + miguel_rain + UDEP_rain +0,
               data = data_rain)
model_y5 <- lm(UDEP_rain  ~ chusis_rain + miguel_rain +  miraflores_rain +0 ,
               data = data_rain)

anova(model_x1) 
model_x1
model_x2
model_x3
model_x4
model_x5
anova(model_x1) 
anova(model_x3) 
anova(model_x4) 
anova(model_x5) 

summary(model_x1)




cor
#############################
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
?slice
data_faltante <- c(1:588) 
data_faltante


library(dplyr)
n <-  data.frame(n)
n

valor<-c()
n_datos <- data.frame(meses_faltantes = n)
n_datos
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
n_datos



#s es los meses
PAP = 0
papa=c()
year<-0
h= c()
k=1
j=0
g = c()
data_predictora <- 0
for (i in 1:49) {
 # for (r in 1:672) { #Filas en los meses de lluvia
    
    r=479
  if (n_datos$valor[i]=="yes"){
      
   # for (s in 1:12){
      if (data_rain$year[r]==n_datos$año[i] && data_rain$month[r]== 11 && is.na(data_rain$miguel_rain[r])==FALSE && 
           is.na(data_rain$UDEP_rain[r])==FALSE && is.na(data_rain$miraflores_rain[r])==FALSE 
          && is.na(data_rain$chusis_rain[r])==FALSE&& is.na(data_rain$Bernal_rain[r])==TRUE){
        j=j+1
        
        model_x1 <- lm(Bernal_rain  ~   UDEP_rain + miguel_rain + chusis_rain + miraflores_rain,
                       data = data_rain)  
        data_predictora <- tibble(UDEP_rain = data_rain[r,]$UDEP_rain,
                                  miguel_rain = data_rain[r,]$miguel_rain,
                                  miraflores_rain = data_rain[r,]$miraflores_rain,
                                  chusis_rain = data_rain[r,]$chusis_rain)
        
       g = predict(model_x1, data_predictora)
        
       
      }else{
      
      }
    }
  
  # } 
      
    #}
  #k=k+1
  #print(h)
}
print(data_predictora) 
model_x1
r
h
PAP
papa
g


Datos_lm
model_x1 <- lm(Bernal_rain  ~ miguel_rain + UDEP_rain + chusis_rain,
               data = data_rain) 
summary(model_x1)
miss_var_summary(data_rain)
  
gsub("e", "", group)   

glimpse(Bernal_bymonth)
  
glimpse(k)


final <- data.frame(1971:2019,n)
final


#Bernal
dif <- (chusis_byyear$rain_month - Bernal_byyear$rain_month )/Bernal_byyear$rain_month
dif



write.xlsx(data_temp_min, file = "DataTmin_v1.xlsx", colNames = TRUE)


library(gtools)
#posibles combinaciones
N <- 7  # Número de elemento
alumnos <- c("bernal","chusis","miraflores","san_miguel","udep","esperanza","mallares") # Son los alumnos con id un número consecutivo
comb <- list()
j=1

  combinaciones <- combinations(N,i, alumnos)
  
  comb[[j]]= combinaciones
  
for (i in 3:7) {
  
  j=j+1
  
}


comb

for (i in 1:3) {
  
  if(comb[[i]][] =! "bernal"){
    comb[[]][]<-NULL
  }
}


comb[[1]][10]



#Para método de correlacion identificar aqueñños con menos de 5 meses faltantes
library(dplyr)
n <-  data.frame(n)
n

valor<-c()
n_datos <- data.frame(meses_faltantes = n)
n_datos
for (i in 1:49) {
  
  if (n[i,]>=1 && n[i,]<=5) {
    valor[i]="yes" 
  }else{
    valor[i]="no" 
  }
}
valor <-data.frame(valor)

n_datos <- mutate(n_datos,año=1971:2019, valor)
n_datos

rain_meses <- data.frame(enero , marzo, Abril, mayo, junio, julio, agosto,
                         setiembre, Octubre, Noviembre, Diciembre)
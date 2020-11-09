install.packages ('simputation')

#Imputar 
library(simputation)

# Impute air_temp and humidity with linear regression. Da valores imputados 
#pero aún pueden existir NA
formula <- air_temp + humidity ~ year + latitude + sea_surface_temp
tao_imp <- impute_lm(tao, formula)


#Segundo
# Initialize missing values with hot-deck
tao_imp <- hotdeck(tao)

# Create boolean masks for where air_temp and humidity are missing
missing_air_temp <- tao_imp$air_temp_imp
missing_humidity <- tao_imp$humidity_imp

for (i in 1:5) {
  # Set air_temp to NA in places where it was originally missing and re-impute it
  tao_imp$air_temp[missing_air_temp] <- NA
  tao_imp <- impute_lm(tao_imp, air_temp ~ year + latitude + sea_surface_temp + humidity)
  # Set humidity to NA in places where it was originally missing and re-impute it
  tao_imp$humidity[missing_humidity] <- NA
  tao_imp <- impute_lm(tao_imp, humidity ~ year + latitude + sea_surface_temp + air_temp)
}

#Tercer

# Initialize missing values with hot-deck
tao_imp <- hotdeck(tao)

# Create boolean masks for where air_temp and humidity are missing
missing_air_temp <- tao_imp$air_temp_imp
missing_humidity <- tao_imp$humidity_imp

for (i in 1:5) {
  # Set air_temp to NA in places where it was originally missing and re-impute it
  tao_imp$air_temp[missing_air_temp] <- NA
  tao_imp <- impute_lm(tao_imp, air_temp ~ year + latitude + sea_surface_temp + humidity)
  # Set humidity to NA in places where it was originally missing and re-impute it
  tao_imp$humidity[missing_humidity] <- NA
  tao_imp <- impute_lm(tao_imp, humidity ~ year + latitude + sea_surface_temp + air_temp)
}

#Falta agregar la funcion para ver cuantas iteraciones hacer
mapc <- function(a, b) {
  mean(abs(b - a) / a, na.rm = TRUE)
}


#4.0
# Load the missForest package
library(missForest)

# Impute biopics data using missForest
imp_res <- missForest(biopics, lewise = TRUE)

# Extract imputed data and check for missing values
imp_data <- imp_res$ximp
print(sum(is.na(imp_data)))

# Extract and print imputation errors
imp_err <- imp_res$OOBerror
print(imp_err)

# Set number of trees to 5 and number of variables used for splitting to 2
imp_res <- missForest(biopics, mtry = 2, ntree = 5)

# Print the resulting imputation errors
print(imp_res$OOBerror)
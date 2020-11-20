#####Climograma Bernal########
library(readxl)
library(ggplot2)
library(dplyr)

datos_falt <- read_excel("Data/Estimación_de_datos_faltantes.xlsx",
                         sheet = "UDEP")
datos_falt


datos_falt %>% 
  group_by(month) %>% 
  summarise(mean_temp_max = mean(temp_max),
            mean_temp_min = mean(temp_min),
            mean_rain = mean(rain))

colors <- c("Precipitación" = "#84c6ed", "T. máxima" = "red", "T. mínima" = "blue")

datos_falt %>% 
  group_by(month) %>% 
  summarise(mean_temp_max = mean(temp_max),
            mean_temp_min = mean(temp_min),
            mean_rain = mean(rain)) %>% 
  ggplot(aes(label = round(mean_temp_max, 1))) +
  geom_bar(aes(x = month, y = mean_rain/2, color = "Precipitación"), stat = "identity", 
           fill = "#84c6ed", alpha = .85) +
  geom_line(aes(x = month, y = mean_temp_max, color = "T. máxima")) +
  geom_line(aes(x = month, y = mean_temp_min, color = "T. mínima"))+
  geom_point(aes(x = month, y = mean_temp_max), size = 1.5)+
    geom_point(aes(x = month, y = mean_temp_min), size = 1.5)+
  scale_x_continuous(breaks = 1:12) +
  scale_y_continuous(
    breaks = seq(0,40 ,5),
    name = "Temperatura (°C)",
    sec.axis = sec_axis(~.*(2), name = "Precipitación (mm)" ,
                        breaks = seq(0,80, 10)
                        )
  ) +
  # geom_text(aes(x = month, y = mean_temp), nudge_y = 1) +
  labs(x = "Meses", title = "Climograma de la Estación Metereológica UDEP", 
       caption = "Fuente: Yo\n*Datos desde 1991 a 2019",
       color  = "Tipo") +
  scale_color_manual(values = colors) +
  theme_bw()
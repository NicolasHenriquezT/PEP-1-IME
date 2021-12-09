# Instalacion de paquetes a usar
if(!require(dplyr)){
  install.packages("dplyr", dependencies = TRUE)
  require(dplyr)
}

if(!require(tidyr)){
  install.packages("tidyr", dependencies = TRUE)
  require(tidyr)
}

if(!require(ggpubr)){
  install.packages("ggpubr", dependencies = TRUE)
  require(ggpubr)
}

# Se lee el archivo de entrada
# Al usar el comando View(datos) en la consola se puede observar que
# los tipos de variables se reflejan correctamente
datos <- read.csv(file.choose(), encoding = "UTF-8")

#¿Se encuestaron más o menos la misma cantidad de gente en cada provincia de la RM?
personasPorProvincia <- datos %>%  group_by(provincia) %>% mutate(cant = 1) %>% summarise(cantPersonas = sum(cant))

medidas_datos <- datos %>% summarise(Media_P = mean(cantPersonas),
                                     Mediana_P = median(cantPersonas),
                                     D)




data <-data.frame(data,stringsAsFactors = TRUE)
medidasEstadisticas <- group_by(data, provincia) %>%
  summarise(mean(ytot), median(ytot), mfv(ytot),
            sd(ytot), IQR(ytot), var(ytot))





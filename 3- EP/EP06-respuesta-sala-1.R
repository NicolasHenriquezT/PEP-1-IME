library(dplyr)
library(pwr)

# Integrantes
# Gonzalo Cuevas Matamala   - Rut: 19.721.859-2
# Nicolás Henríquez Turner  - Rut: 20.730.845-5
# Maximiliano Araya Poblete - Rut: 20.467.583-k
# Miguel Salinas González   - Rut: 20.215.515-4

##############################################################################                                                                            
#  ____    ____    _____    ____   _   _   _   _   _____      _        _     #
# |  _ \  |  _ \  | ____|  / ___| | | | | | \ | | |_   _|    / \      / |    #
# | |_) | | |_) | |  _|   | |  _  | | | | |  \| |   | |     / _ \     | |    #
# |  __/  |  _ <  | |___  | |_| | | |_| | | |\  |   | |    / ___ \    | |    #
# |_|     |_| \_\ |_____|  \____|  \___/  |_| \_|   |_|   /_/   \_\   |_|    #                                                                    
##############################################################################   
# Estudios previos habían determinado que la incidencia de cáncer oral en la población 
# general que bebe regularmente entre 10 y 44 ml de alcohol era de 50%. ¿Respaldan estos
# datos tal estimación? 

# Para este caso, se han definido las siguientes hipótesis:

# Hipótesis Nula (H0)

# H0: Las incidencias de cáncer oral en la población general que bebe regularmente entre 
#     10 y 44 ml de alcohol es de un 50%.

# Hipótesis alternativa (HA)

# HA: Las incidencias de cáncer oral en la poblacion general que bebe regularmente entre
#     10 y 44 ml de alcohol es distinta de un 50%. 

# Matemáticamente las hipótesis formuladas quedan como:
# Denotando como p a la proporción de todas las instancias que se encuentran en el 50%, el 
# cual es considerado como el valor hipotético. representado como p0 = 0.5
# H0: p  = p0
# HA: p != p0

# Como no se conoce la probabilidad de éxito p de la población, se usará el estimador
# puntual pˆ, el cual permite representar la proporción de éxito de la muestra.

# Datos conocidos
n <- 200
casosCancer <- 109
casosControl <- 91
datos <- c(casosCancer, casosControl)  
media <- mean(datos)
p_exito <- (casosCancer / n) 
alfa <- 0.05
valor_nulo <- 0.5

# Se construye el intervalo de confianza
error_est <- sqrt((p_exito * (1 - p_exito)) / n)
Z_critico <- qnorm(alfa / 2, lower.tail = FALSE)
inferior <- p_exito - Z_critico * error_est
superior <- p_exito + Z_critico * error_est
cat("Intervalo de confianza =[", inferior,", ", superior, "]\n", sep="")

# Prueba de hipótesis
error_est_hip <- sqrt((valor_nulo * (1 - valor_nulo)) / n)
Z <- (p_exito - valor_nulo) / error_est_hip
p <- 2 * pnorm(Z, lower.tail = FALSE)       
cat("Hipótesis alternativa bilateral\n")
cat("Z =", Z, "\n")
cat("p =", p)

# Como los datos fueron escogidos al azar, podemos suponer que las observaciones
# son independientes, ademas estas representan menos del 10% del total de la poblacion. 

# Si consideramos que el valor nulo es verdadero, tenemos que los exitos y fracasos
# estan representados de la siguiente forma:
# éxitos => 0.5 * 200 = 100    
# fracasos => (1 - 0.5) * 200 = 100
# Como ambos valores son mayores a 10, se verifica la condición de éxito-fracaso.

# Como la distribucion muestral de pˆ cumple las condiciones anteriores, podemos decir
# que es cercana a la normalidad con media µ = p.

# Conclusión 
# El intervalo de confianza obtenido ([0.475986, 0.614014]) nos permite afirmar con
# 95% de confianza que la proporción de instancias del problema se encuentra entre
# 47,6% y 61,4%. Ademas el valor de p es de 0.2, el cual es mayor que nuestro nivel de
# significancia, por lo que no hay evidencia suficiente para rechazar H0, es decir, los 
# datos respaldan la estimación realizada.


##############################################################################                                                                            
#  ____    ____    _____    ____   _   _   _   _   _____      _        ____  #
# |  _ \  |  _ \  | ____|  / ___| | | | | | \ | | |_   _|    / \      |___ \ #
# | |_) | | |_) | |  _|   | |  _  | | | | |  \| |   | |     / _ \       __) |#
# |  __/  |  _ <  | |___  | |_| | | |_| | | |\  |   | |    / ___ \     / __/ #
# |_|     |_| \_\ |_____|  \____|  \___/  |_| \_|   |_|   /_/   \_\   |_____|#
##############################################################################                                                                           
# Según estos datos, ¿da lo mismo beber de 10 a 44 ml de alcohol diariamente que hacerlo
# con 45 o más ml?

# Definiendo intervalo 1 como el intervalo de alcohol diario que va entre los 10 a
# 44ml e intervalo 2 como el intervalo de alcohol diario que va desde los 45ml o más,
# se definen las siguientes hipótesis:

# Hipótesis Nula (H0)

# H0: No existe diferencia en la tasa de cáncer oral entre el intervalo 1 y el 
#     intervalo 2

# Hipótesis alternativa (HA)

# HA: Las tasas de cáncer oral entre el intervalo 1 y el intervalo 2 son diferentes

# Matemáticamente las hipótesis formuladas quedan como:
# Denotando como p1 y p2 a las proporciones de los intervalos 1 y 2, respectivamente,
# que sufren de cáncer oral (casos de cáncer oral):
# H0: p1 - p2 = 0
# HA: p1 - p2 != 0

# Datos conocidos
n1 <- 200     # n del intervalo 10-44
n2 <- 349     # n del intervalo 45 o mas
n3<-c(n1, n2)

exito1 <- 109
exito2 <- 242
exitos<-c(exito1,exito2)

alfa <- 0.05
valor_nulo <- 0

# Prueba de Wilson
prueba <- prop.test(exitos,n = n3,alternative ="two.sided",
                    conf.level = 1 - alfa)

# Resultados de la prueba:

# Proporciones de éxito
p1 <- prueba$estimate[1] 
p2 <- prueba$estimate[2]

# Diferencia
diferencia <- p1 - p2

# Intervalo de confianza
inferior <- prueba$conf.int[1]
superior <- prueba$conf.int[2]
cat("Intervalo de confianza =[", inferior,", ", superior, "]\n", sep="")

# Prueba de hipótesis
error_est_hip <- prueba$statistic 
Z <- (diferencia - valor_nulo) / error_est_hip 
p <- prueba$p.value
cat("Hipótesis alternativa bilateral\n") 
cat("p =", p, "\n")
cat("Z =", Z)

# Conclusión:
# Se puede afirmar con 95% de confianza que la diferencia en la tasa de cáncer oral entre
# el intervalo 1 y el intervalo 2 se encuentra entre ([-0.2366214,-0.0601981]) es decir,
# puede variar entre -23.66% y -6.02%. Adicionalmente, el valor p obtenido (p = 0.0006924446)
# es por mucho menor al nivel de significación (a = 0.05), por lo que existe evidencia
# fuerte en favor de HA, permitiendo rechazar la hipótesis nula en favor de la hipótesis
# alternativa.



##############################################################################                                                                           
#  ____    ____    _____    ____   _   _   _   _   _____      _        _____ #
# |  _ \  |  _ \  | ____|  / ___| | | | | | \ | | |_   _|    / \      |___ / #
# | |_) | | |_) | |  _|   | |  _  | | | | |  \| |   | |     / _ \       |_ \ #
# |  __/  |  _ <  | |___  | |_| | | |_| | | |\  |   | |    / ___ \     ___) |#
# |_|     |_| \_\ |_____|  \____|  \___/  |_| \_|   |_|   /_/   \_\   |____/ #
##############################################################################                                                                           
# Suponiendo que la diferencia en la proporción de personas que desarrollan la enfermedad entre 
# quienes beben de 10 a 44 ml de alcohol por día y aquellos que beben 45 o más ml al día es de 0.15.
# ¿Cuánta gente deberíamos monitorear para obtener un intervalo de confianza del 95% y poder 
# estadístico de 90%? Si se intenta mantener aproximadamente la misma proporción de gente estudiada 
# en cada caso.

# Definiendo como p1 y p2 a las proporciones de personas que beben alcohol entre los 10
# a 44ml y 45ml o mas, respectivamente, se definen las siguientes hipótesis:

# Hipótesis Nula (H0)

# H0: La diferencia entre las proporciones de quienes beben de 10 a 44ml de alcohol por
#     día (p1) y aquellos que beben 45 o más ml (p2) es de 0.15.

# Hipótesis alternativa (HA)

# H0: La diferencia entre las proporciones de quienes beben de 10 a 44ml de alcohol por 
#     día (p1) y aquellos que beben 45 o más ml (p2) es distinta de 0.15.

# Matemáticamente las hipótesis formuladas quedan como:
# H0 p1 - p2  = 0.15
# H1 p1 - p2 != 0.15

# Datos conocidos
diferencia <- 0.15
poder <- 0.9
alfa <- 0.05

# Se utiliza la función pwr.2p.test, ya que se intenta mantener la misma cantidad de
# personas en ambas muestras.
resultado <- pwr.2p.test(h = diferencia,
                         n = NULL,
                         sig.level = alfa, 
                         power = poder,
                         alternative = 'two.side')$n 

n <- ceiling(resultado)
print(n)

# Conclusión:
# Como fue demostrado en los ejercicios anteriores, las proporciones cumplen con el 
# requisito de ser independientes y para este caso, se asume que siguen una distribución normal.
# Por último se necesita monitorear un total de 934 personas para mantener
# aproximadamente la misma proporción de gente estudiada en cada caso.

# ----------------------------------------------------
# Ejemplo en R: Implementación del Método Bootstrap
# y la Prueba T Bootstrap
# ----------------------------------------------------

# Cargar paquetes necesarios
library(tidyverse)

# Fijar la semilla para la generación de números aleatorios, 
# lo cual permite reproducibilidad de los resultados
set.seed(123)

# Definir parámetros para la generación de la muestra
n <- 100  # Tamaño de la muestra
mu <- 100 # Media poblacional
sd <- 10  # Desviación estándar poblacional

# Generación de una muestra aleatoria de tamaño n a partir de una distribución normal
x <- rnorm(n, mu, sd)

# Estimación de la media muestral
(xbar <- mean(x)) 

# Cálculo del intervalo de confianza del 95% (IC 95%) usando la teoría clásica
alpha <- 0.05 # Nivel de significancia
# Cálculo del valor crítico de la distribución t-Student para el intervalo de confianza
(t_c <- qt(alpha / 2, n - 1, lower.tail = FALSE))
# Cálculo del intervalo de confianza teórico basado en la distribución t
(ic_teo <- xbar + c(-1, 1) * t_c * sd(x) / sqrt(n))

# Cálculo del intervalo de confianza utilizando el método Bootstrap
B <- 100000 # Número de muestras bootstrap
# Generación de las muestras bootstrap (con reemplazo) en una matriz de tamaño n x B
x_boot <- matrix(sample(x, n * B, replace = TRUE), n, B) # Sacando mis muestras
# Cálculo de las medias bootstrap para cada muestra
x_bars <- colSums(x_boot) / n # Calculando las medias bootstrap
# Cálculo del intervalo de confianza para las medias bootstrap
ic_boots <- quantile(x_bars, c(alpha / 2, 1 - alpha / 2))

# Mostrar resultados de los intervalos de confianza calculados
ic_teo   # Intervalo de confianza utilizando el método teórico
ic_boots # Intervalo de confianza utilizando el método bootstrap

# Bajo la asunción de normalidad, la estadística T se distribuye según t-Student(n-1)
# Calculamos T para cada muestra bootstrap

# Calcular las varianzas muestrales para cada muestra bootstrap
x_vars <- apply(x_boot, 2, var)
# Calcular la estadística T bootstrap para cada muestra
t_boot <- (x_bars - xbar) * sqrt(n) / sqrt(x_vars)

# Graficar la distribución de T bootstrap y comparar con la distribución teórica de T
data.frame(t = t_boot, density_teo = dt(t_boot, n - 1)) %>%
    ggplot(aes(x = t, y = ..density..)) +
    geom_histogram(aes(col = "Dist Bootstrap", fill = "Dist Bootstrap")) +
    geom_point(aes(y = density_teo), col = "black")

#### Pruebas para la media
xbar # Mostrar la media muestral

# Formulación de hipótesis:
# H_0: mu = 100 (hipótesis nula) vs H_a: mu > 100 (hipótesis alternativa)

# Prueba clásica suponiendo normalidad:
# Se rechaza H_0 si T > t_0
# Para calcular el p-valor:
# P[T > t(obs)] bajo H_0: mu = 100, donde T sigue una distribución t-Student con (n-1) grados de libertad
(t_obs <- (xbar - 100) / sqrt(var(x) / n)) # Estadístico de prueba
(pvalor <- pt(t_obs, n - 1, lower.tail = FALSE)) # p-valor: 0.26
# El p-valor es mayor que 0.05, por lo que no hay suficiente evidencia para rechazar H_0
# Esto indica que no se puede rechazar la hipótesis de que la media sea 100

# Si no se desea suponer normalidad, se realiza la prueba mediante Bootstrap
# Calcular el p-valor utilizando la distribución de t_boot
sum(t_boot >= t_obs) / B # Proporción de valores bootstrap mayores o iguales a t_obs

# Ejemplo adicional utilizando otro conjunto de datos
library(resampledata)
data(Bangladesh)
(n <- nrow(Bangladesh)) # Número de observaciones en el conjunto de datos
# 271, lo que es una muestra considerablemente grande. 
# Por el Teorema del Límite Central (TLC), los resultados probablemente sean similares.

# Mostrar histograma del contenido de arsénico en los datos de Bangladesh
hist(Bangladesh$Arsenic)
mean(Bangladesh$Arsenic) # Media del arsénico

# Gráfico Q-Q para evaluar la normalidad de los datos
ars <- Bangladesh$Arsenic
ars_est <- (ars - mean(ars)) / sd(ars) # Normalización de los datos
qqnorm(ars_est) # Graficar cuantiles observados vs cuantiles teóricos
abline(a = 0, b = 1, col = "red") # Línea de referencia que indica normalidad

# Comparación con el gráfico Q-Q de la muestra inicial
# Se espera que los datos se alineen con la línea roja si fueran normales
qqnorm((x - xbar) / sqrt(var(x)))
abline(a = 0, b = 1, col = "red")

# Los datos parecen no ser normales, pero dado el tamaño de la muestra (grande),
# se puede aplicar la prueba t clásica como aproximación gracias al TLC

# Prueba clásica para la media del arsénico:
# H_0: mu = 100 vs H_a: mu > 100
(xbar <- mean(ars)) # Media muestral: 125
(n <- length(ars)) # Tamaño de la muestra: 271
(sd <- sd(ars)) # Desviación estándar muestral
(T <- (xbar - 100) / (sd / sqrt(n))) # Estadístico de prueba T

# El valor del estadístico T indica la distancia estándar entre la media muestral y la media nula (100)
# Si T es grande, estamos muy lejos de la media nula

# Calcular el p-valor para determinar la significancia del resultado
pt(T, n - 1, lower.tail = FALSE) # p-valor: 0.08150503
# El p-valor es del 8%, mayor que 5%, por lo que no hay suficiente evidencia para rechazar H_0
# Esto sugiere que el resultado observado podría ser debido al azar



# Alternativa: Método Bootstrap
# Metodología no paramétrica que no asume normalidad
# La distribución muestral se toma como la muestra real, y de ahí se obtiene información sobre
# la distribución del estadístico de interés


B <- 10000 # Numero de muestras bootstrap (Queremos que sea grande para obtener una buena aproximación)
n <- length(ars) # Tamaño de la muestra de arsénico (queremos que sea igual que la mustra)
# Generar muestras bootstrap a partir de los datos del arsénico
x_boot <- matrix(sample(ars, n * B, replace = TRUE), n, B)

x_bars <- colMeans(x_boot) #La media de cada muestra bootstrap (cada columna)
x_sds <- apply(x_boot, 2, sd) #La desviacion estandar de cada muestra bootstrap (cada columna)

# Calcular la estadística T bootstrap para cada muestra
#Tengo que cambiar el 100 por la media de la muestra
#Porque bajo el mundo bootstrap, la media de la muestra es la media poblacional (mu=xbar)
#xbar es la media muetral
xbar <- mean(ars) # Media muestral: 125
t_boot <- (x_bars - xbar) / (x_sds / sqrt(n)) #Estadístico de prueba para cada muestra bootstrap

# Calcular el p-valor utilizando la distribución bootstrap
(p_val <- (sum(t_boot >= T)) / B) # p-valor: 0.055

# Graficar la distribución bootstrap y comparar con la distribución teórica t-Student
data.frame(t = t_boot) %>% 
    ggplot(aes(x = t)) +
    geom_histogram(aes(y = ..density.., fill = "Dist Bootstrap")) +
    geom_vline(xintercept = T, col = "black", lty = 2, lwd = 2) +
    stat_function(aes(color = "Dist Bajo Normalidad"), fun = dt,
    args = list(df = n - 1), color = "skyblue", lwd = 1.5)







1










# Prueba Bootstrap
# Metodología no paramétrica
B <- 100000 # Tamaño de la muestra bootstrap








x_boot <- matrix(sample(ars, n * B, replace = TRUE), n, B)
x_bars <- colMeans(x_boot)
x_sds <- apply(x_boot, 2, sd)
t_boot <- (x_bars - xbar) / (x_sds / sqrt(n))

# Cálculo del p-valor bootstrap
(p_val <- sum(t_boot >= T) / B) # 0.05269

# Gráfico de la distribución bootstrap y la comparación con la distribución teórica
data.frame(t = t_boot) %>% 
    ggplot(aes(x = t)) +
    geom_histogram(aes(y = ..density.., fill = "Dist Bootstrap")) +
    geom_vline(xintercept = T, col = "black", lty = 2, lwd = 2) +
    stat_function(aes(color = "Dist Bajo Normalidad"), fun = dt, args = list(df = n - 1), color = "skyblue", lwd = 1.5)

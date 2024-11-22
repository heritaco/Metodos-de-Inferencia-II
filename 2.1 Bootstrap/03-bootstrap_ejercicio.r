"""
En este código, realizamos una prueba t de una muestra para verificar si la media de una variable es igual a 100.
Primero, calculamos el estadístico t observado y el p-valor para la prueba t clásica asumiendo normalidad.
Luego, realizamos una prueba ¡bootstrap! para calcular el p-valor sin asumir normalidad en los datos.
"""

library(resampledata)  # Cargamos la biblioteca con los datos de muestra
data(Bangladesh)       # Cargamos el conjunto de datos de Bangladesh

# Número de observaciones en el conjunto de datos de Bangladesh
num_observaciones <- nrow(Bangladesh)  # Hay 271 observaciones en total

# Vemos las primeras observaciones del conjunto de datos para inspección
head(Bangladesh)


"""
----------------------------------- ¿Es normal? -----------------------------------

Una forma de saber si una variable sigue una distribución normal es mediante un
histograma y un gráfico QQ-Plot. Un histograma nos da una idea visual de la
distribución de los datos. Un gráfico QQ-Plot compara los cuantiles de los datos
con los cuantiles de una distribución normal. Si los datos siguen una
distribución normal, los puntos en el gráfico QQ-Plot deberían seguir una línea recta.
"""

# Extraemos la variable "Arsenic" del conjunto de datos
arsenico <- Bangladesh$Arsenic
arsenico
# Histograma de la variable "Arsenic"
# Observamos la distribución del arsénico, parece que no sigue una distribución normal
hist(arsenico)

# Gráfico QQ-Plot para verificar si los datos se ajustan a una distribución normal
qqnorm(arsenico)

# La línea del gráfico muestra que los datos no son normales
# Normalizamos la variable "Arsenic" para intentar ajustarla a una distribución normal
arsenico_normalizado <- (arsenico - mean(arsenico)) / sd(arsenico) 

# Gráfico QQ-Plot para los datos estandarizados (normalizados)
qqnorm(arsenico_normalizado)
abline(0, 1, col = "red")  # Agregamos una línea de referencia (pendiente 1, intercepto 0)
# Como los datos no se ajustan a la línea roja, no siguen una distribución normal

"""
----------------------------------- Prueba t de una muestra -----------------------------------

Vamos a realizar una prueba t de una muestra para verificar si la media del arsénico
es igual a 100. La hipótesis nula es que la media del arsénico es 100. La hipótesis
alternativa es que la media del arsénico no es 100. 
"""

# Calculamos la media del arsénico
media_arsenico <- mean(arsenico)
media_arsenico

# Número de observaciones
n_muestras <- length(arsenico)
n_muestras

# Calculamos la desviación estándar del arsénico
desviacion_arsenico <- sd(arsenico)
desviacion_arsenico

# Calculamos el estadístico T para una prueba t de una muestra
# Hipótesis nula: la media del arsénico es 100
estadistico_T <- (media_arsenico - 100) / (desviacion_arsenico / sqrt(n_muestras))
estadistico_T

# Calculamos el p-valor para la prueba t
# Bajo normalidad, el estadístico t sigue una distribución t con n-1 grados de libertad.
p_valor <- pt(estadistico_T, df = n_muestras - 1, lower.tail = FALSE)
p_valor

# Con un p-valor de 8%, no podemos rechazar la hipótesis nula bajo un nivel de significancia del 5%
# Esto significa que la probabilidad de obtener un valor tan extremo o más extremo que el observado
# bajo la suposición de que la media del arsénico es 100, es del 8%.

# Si el arsénico sigue una distribución normal, la probabilidad de obtener un valor extremo es del 8%.
# Pero, ¿qué sucede si el arsénico no es normal? Vamos a hacer una prueba bootstrap.


"""
----------------------------------- Prueba Bootstrap -----------------------------------

En la prueba bootstrap, generamos muestras de bootstrap a partir de los datos originales
y calculamos el estadístico de interés para cada muestra de bootstrap. Luego, calculamos
el p-valor como la proporción de estadísticos de bootstrap que son iguales o más extremos
que el estadístico observado. No asumimos ninguna distribución en la prueba bootstrap.
"""

# Número de muestras bootstrap
num_muestras_bootstrap <- 10000

# Generamos una matriz de muestras bootstrap
# Cada columna representa una muestra bootstrap obtenida con reemplazo
muestras_bootstrap <- matrix(sample(arsenico, n_muestras * num_muestras_bootstrap, replace = TRUE), 
                             nrow = n_muestras, ncol = num_muestras_bootstrap)

# Calculamos las medias de cada muestra bootstrap
medias_bootstrap <- colMeans(muestras_bootstrap)

# Calculamos las varianzas de cada muestra bootstrap
varianzas_bootstrap <- apply(muestras_bootstrap, 2, var)

# Calculamos los estadísticos t de bootstrap
# En bootstrap, la media es la media empírica de las muestras.
t_bootstrap <- (medias_bootstrap - media_arsenico) / (sqrt(varianzas_bootstrap) / sqrt(n_muestras))
t_bootstrap
# Calculamos el estadístico t observado para la muestra original
estadistico_T_observado <- estadistico_T

# Calculamos el p-valor usando el método bootstrap
p_valor_bootstrap <- mean(t_bootstrap >= estadistico_T_observado)
p_valor_bootstrap

# El p-valor bootstrap nos da una idea de si los resultados son extremos bajo la hipótesis nula en un contexto no paramétrico.

"""
Conclusión: La prueba bootstrap nos permite calcular el p-valor sin asumir normalidad en los datos.
En este caso, el p-valor bootstrap es del 5%, lo que sugiere que la media del arsénico no es 100.
"""

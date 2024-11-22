# a) Calcular los valores de los rangos
library(dplyr)

# Crear el dataframe
data <- data.frame(
    ID = 1:45,
    Genero = c(
        rep(1, 25), rep(2, 20)
    ),
    Puntaje = c(
        5,1,1,3,4,5,2,4,3,3,1,3,3,5,2,2,4,2,2,2,1,5,4,5,3,
        5,4,2,3,5,2,4,5,4,2,1,5,4,4,5,3,3,4,2,1
    ),
    Rango = c(
        40.5,10.5,10.5,19.5,30,40.5,10.5,30,19.5,19.5,3.5,19.5,19.5,40.5,10.5,
        10.5,30,10.5,10.5,10.5,3.5,40.5,30,10.5,40.5,
        19.5,40.5,30,10.5,19.5,40.5,30,3.5,40.5,30,3.5,40.5,30,30,40.5,
        19.5,19.5,19.5,30,10.5,3.5
    )
)

# Calcular los rangos
data <- data %>%
    group_by(Genero) %>%
    mutate(Rango_calculado = rank(-Puntaje, ties.method = "average"))

# b) Interpretar los rangos
# (Interpretación manual requerida)

# c) Plantear la hipótesis
# H0: No hay diferencia en las puntuaciones de compra entre hombres y mujeres
# H1: Los hombres tienden a comprar menos que las mujeres

# d) Prueba de hipótesis
resultado <- t.test(Puntaje ~ Genero, data = data, alternative = "less")
print(resultado)

# e) Prueba de hipótesis manual

# Separar los datos por género
hombres <- subset(data, Genero == 1)$Puntaje
mujeres <- subset(data, Genero == 2)$Puntaje

# Calcular estadísticas descriptivas
n1 <- length(hombres)
n2 <- length(mujeres)
mean1 <- mean(hombres)
mean2 <- mean(mujeres)
sd1 <- sd(hombres)
sd2 <- sd(mujeres)

# Calcular el estadístico t
se <- sqrt((sd1^2 / n1) + (sd2^2 / n2))
t_stat <- (mean1 - mean2) / se

# Grados de libertad usando la aproximación de Welch
df <- ((sd1^2 / n1) + (sd2^2 / n2))^2 / 
    ((sd1^2 / n1)^2 / (n1 - 1) + (sd2^2 / n2)^2 / (n2 - 1))

# Calcular el valor p
p_value <- pt(t_stat, df, lower.tail = TRUE)

# Imprimir resultados
cat("t =", t_stat, "\ndf =", df, "\np-value =", p_value, "\n")
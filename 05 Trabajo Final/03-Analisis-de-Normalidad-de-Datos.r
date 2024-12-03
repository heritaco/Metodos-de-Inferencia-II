# Cargar librerías necesarias
library(ggplot2)
library(stats)
library(nortest)

# Establecer el directorio de trabajo
setwd("C:/Heri/GitHub/Metodos de Inferencia II/05 Trabajo Final")
datos <- read.csv("normality-test.csv", header = TRUE)

# a) Visualización de la Distribución

# Histograma con densidad normal estimada
ggplot(datos, aes(x = x)) +
    geom_histogram(aes(y = ..density..), binwidth = 1, fill = "lightblue", color = "black") +
    stat_function(fun = dnorm, args = list(mean = mean(datos$x), sd = sd(datos$x)), color = "red") +
    labs(title = "Histograma con Densidad Normal Estimada", x = "x", y = "Densidad") +
    theme_minimal()

# Función de Distribución Empírica (ECDF)
ggplot(datos, aes(x = x)) +
    stat_ecdf(geom = "step") +
    stat_function(fun = pnorm, args = list(mean = mean(datos$x), sd = sd(datos$x)), color = "red") +
    labs(title = "Función de Distribución Empírica vs Normal", x = "x", y = "Probabilidad Acumulada") +
    theme_minimal()

# Gráfico QQ
ggplot(datos, aes(sample = x)) +
    stat_qq() +
    stat_qq_line(color = "red") +
    labs(title = "Gráfico QQ", x = "Cuantiles Teóricos", y = "Cuantiles Muestrales") +
    theme_minimal()

# b) Partición del Espacio y Prueba de Bondad de Ajuste

# Definir particiones
cantidad_clases <- 5
breaks <- quantile(datos$x, probs = seq(0, 1, length.out = cantidad_clases + 1))
observados <- table(cut(datos$x, breaks = breaks, include.lowest = TRUE))
esperados <- diff(pnorm(breaks, mean = mean(datos$x), sd = sd(datos$x))) * length(datos$x)

# Prueba de chi-cuadrada
chisq.test(observados, p = diff(pnorm(breaks, mean = mean(datos$x), sd = sd(datos$x))))

# Histograma con particiones
ggplot(datos, aes(x = x)) +
    geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
    geom_vline(xintercept = breaks, color = "red", linetype = "dashed") +
    labs(title = "Histograma con Particiones", x = "x", y = "Frecuencia") +
    theme_minimal()

# c) Pruebas de Normalidad

# Prueba de Kolmogorov-Smirnov
ks_result <- ks.test(datos$x, "pnorm", mean = mean(datos$x), sd = sd(datos$x))
ks_result

# Cálculo manual de D- y D+
ecdf_func <- ecdf(datos$x)
d_plus <- max(ecdf_func(datos$x) - pnorm(datos$x, mean = mean(datos$x), sd = sd(datos$x)))
d_minus <- max(pnorm(datos$x, mean = mean(datos$x), sd = sd(datos$x)) - (ecdf_func(datos$x) - 1/length(datos$x)))
d_plus
d_minus

# Gráfico de distancias
plot(ecdf_func, main = "D+ y D- sobre la ECF", xlab = "x", ylab = "F(x)")
curve(pnorm(x, mean = mean(datos$x), sd = sd(datos$x)), add = TRUE, col = "red")
abline(v = datos$x[which.max(ecdf_func(datos$x) - pnorm(datos$x, mean = mean(datos$x), sd = sd(datos$x)))], col = "blue", lty = 2)
abline(v = datos$x[which.max(pnorm(datos$x, mean = mean(datos$x), sd = sd(datos$x)) - (ecdf_func(datos$x) - 1/length(datos$x)))], col = "green", lty = 2)

# Comparación con funciones de prueba
lillie_result <- lillie.test(datos$x)
shapiro_result <- shapiro.test(datos$x)

ks_result
lillie_result
shapiro_result









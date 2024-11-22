# Cargar librerías necesarias
if(!require("ggplot2")) install.packages("ggplot2", dependencies = TRUE)
if(!require("nortest")) install.packages("nortest", dependencies = TRUE)

# Cargar datos
data <- read.csv("C:/Heri/GitHub/Inferencia II/2.3 Xi Cuadrada/normality_test.csv")

# Visualización de los datos
# Histograma con curva de densidad
ggplot(data, aes(x = x)) +
  geom_histogram(aes(y = ..density..), bins = 10, fill = "skyblue", color = "black") +
  geom_density(color = "darkblue", size = 1) +
  labs(title = "Histograma y Curva de Densidad",
       x = "Valores de x",
       y = "Densidad")

# Gráfico Q-Q para comparar con una distribución normal
ggplot(data, aes(sample = x)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "Gráfico Q-Q",
       x = "Cuantiles Teóricos",
       y = "Cuantiles de la Muestra")

# Prueba de Shapiro-Wilk (para muestras pequeñas, n < 50)
shapiro_test <- shapiro.test(data$x)
cat("Resultado de la Prueba de Shapiro-Wilk:\n", shapiro_test, "\n")

# Prueba de Anderson-Darling (para muestras más grandes)
ad_test <- ad.test(data$x)
cat("Resultado de la Prueba de Anderson-Darling:\n", ad_test, "\n")

# Prueba de Chi-Cuadrada
# Para la prueba de chi-cuadrada, dividimos los datos en intervalos
# Primero definimos el número de intervalos
k <- floor(sqrt(nrow(data))) # Regla aproximada de Sturges
breaks <- seq(min(data$x), max(data$x), length.out = k + 1)
observed_freq <- table(cut(data$x, breaks = breaks))
expected_freq <- dnorm((breaks[-1] + breaks[-length(breaks)]) / 2,
                       mean = mean(data$x),
                       sd = sd(data$x)) * diff(breaks) * nrow(data)
chi_squared <- sum((observed_freq - expected_freq)^2 / expected_freq)
p_value <- pchisq(chi_squared, df = k - 1, lower.tail = FALSE)
p_value

cat("Resultado de la Prueba de Chi-Cuadrada:\n")
cat("Estadístico Chi-Cuadrada:", chi_squared, "\n")
cat("Valor p:", p_value, "\n")

# Interpretación
if(p_value < 0.05) {
  cat("Conclusión: Los datos no siguen una distribución normal (nivel de significancia 0.05).\n")
} else {
  cat("Conclusión: No se puede rechazar la hipótesis nula; los datos podrían seguir una distribución normal.\n")
}

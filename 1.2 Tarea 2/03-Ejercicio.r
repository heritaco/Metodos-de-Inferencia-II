# Definir los parámetros
mu_0 <- 0  # Valor bajo H0
sigma <- 1  # Desviación estándar conocida
alpha_levels <- c(0.05, 0.01)  # Niveles de significancia
n_values <- c(100, 1000, 10000)  # Tamaños de muestra
mu_values <- seq(-0.5, 1.5, by = 0.01)  # Valores de mu para la función potencia

# Función para calcular la potencia
calculate_power <- function(n, alpha, mu) {
  z_alpha <- qnorm(1 - alpha)  # Valor crítico para el nivel de significancia dado
  power <- 1 - pnorm(z_alpha - sqrt(n) * (mu - mu_0) / sigma)
  return(power)
}

# Graficar la función potencia para cada combinación de n y alpha
par(mfrow = c(2, 3), mar = c(4, 4, 2, 1) + 0.1, oma = c(0, 0, 2, 0))  # Crear una matriz de gráficos 2x3 con márgenes ajustados

for (alpha in alpha_levels) {
  for (n in n_values) {
    power_values <- sapply(mu_values, calculate_power, n = n, alpha = alpha)
    
    plot(mu_values, power_values, type = "l", lwd = 2, col = "#1f77b4",
         xlab = expression(mu), ylab = expression(beta(mu)),
         main = paste("Función Potencia (n =", n, ", alpha =", alpha, ")"),
         cex.main = 1.2, cex.lab = 1.1, cex.axis = 1)
    grid(col = "gray", lty = "dotted")
    abline(h = alpha, col = "#ff7f0e", lty = 2, lwd = 2)
    text(mu_0, alpha + 0.05, labels = expression(alpha), col = "#ff7f0e", pos = 4, cex = 1.1)
  }
}

# Título general para todos los gráficos
mtext("Gráficos de la Función Potencia", outer = TRUE, cex = 1.5)
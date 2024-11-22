# Definir parámetros
n <- 100  # Tamaño de la muestra
alpha <- 0.05  # Nivel de significancia
num_simulations <- 10000  # Número de simulaciones
lambda_values <- seq(0.5, 2, by=0.1)  # Valores de lambda para la función potencia

# Calcular los puntos críticos c1 y c2 bajo H0 (lambda = 1)
T_null <- replicate(num_simulations, sum(rexp(n, rate = 1)))
c1 <- quantile(T_null, alpha / 2)
c2 <- quantile(T_null, 1 - alpha / 2)

# Inicializar un vector para almacenar la potencia
power <- numeric(length(lambda_values))

# Calcular la función potencia para cada valor de lambda
for (i in 1:length(lambda_values)) {
  lambda <- lambda_values[i]
  
  # Simular la estadística T bajo lambda
  T_sim <- replicate(num_simulations, sum(rexp(n, rate = lambda)))
  
  # Calcular la potencia como la proporción de veces que T cae en la región crítica
  power[i] <- mean(T_sim < c1 | T_sim > c2)
} 

par(mfrow = c(1,1))  # Ajustar márgenes

# Configurar los parámetros de la gráfica
par(mar = c(5, 5, 4, 2) + 0.1)  # Ajustar márgenes

# Graficar la función potencia
plot(lambda_values, power, type = "b", pch = 16, col = "#1f77b4",
     xlab = expression(lambda), ylab = "Potencia",
     main = "Función Potencia para la Prueba Insesgada",
     cex.main = 1.2, cex.lab = 1.1, cex.axis = 1)
grid(col = "gray", lty = "dotted")
abline(h = alpha, col = "#ff7f0e", lty = 2, lwd = 2)
text(1, alpha + 0.05, labels = expression(alpha), col = "#ff7f0e", pos = 4, cex = 1.1)
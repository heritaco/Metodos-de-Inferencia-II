# Parámetros
n <- 10  # Tamaño de la muestra
alpha <- 0.05  # Nivel de significancia

# Calcular el valor crítico k
k <- qgamma(1 - alpha, shape = n, scale = 1)

# Mostrar el resultado
cat("El valor crítico k para n =", n, "y alpha =", alpha, "es:", k, "\n")



# Definir los parámetros
n <- 30          # Número de lanzamientos
x <- 19          # Número de 1 obtenidos
p0 <- 1/2        # Probabilidad bajo H0 (dado justo)
p1 <- 2/3        # Probabilidad bajo H1 (dado sesgado)
alpha <- 0.05    # Nivel de significancia

# Función de verosimilitud bajo H0
L0 <- dbinom(x, size = n, prob = p0)

# Función de verosimilitud bajo H1
L1 <- dbinom(x, size = n, prob = p1)

# Razón de verosimilitud
LR <- L1 / L0

# Determinar el umbral k usando el nivel de significancia
# Queremos encontrar el valor de k tal que P(LR > k | H0) = 0.05
# Esto se hace buscando k en donde la probabilidad acumulada bajo H0 es 0.05
k <- quantile(rbinom(10000, size = n, prob = p0), 0.95) # Simulación para obtener k

# Comparar LR con k
reject_H0 <- LR > k

# Resultados
cat("Razón de verosimilitud (LR):", LR, "\n")
cat("Umbral k para nivel de significancia del 5%:", k, "\n")
if (reject_H0) {
  cat("Se rechaza H0: Existe evidencia significativa para concluir que el dado es sesgado.\n")
} else {
  cat("No se rechaza H0: No hay evidencia significativa para concluir que el dado es sesgado.\n")
}



# Definir el tamaño de la muestra y el valor de theta bajo H0
n <- 100
theta_h0 <- 1
alpha <- 0.005

# Simular la muestra de una distribución Beta(theta, 1) bajo H0
set.seed(123) # Para reproducibilidad
X <- rbeta(n, theta_h0, 1)

# Transformación Yi = -log(Xi)
Y <- -log(X)

# Estadística suficiente T = sum(Yi)
T <- sum(Y)

# Determinar el valor crítico k tal que P(T > k | θ = 1) = 0.005
k <- qgamma(1 - alpha, shape = n, rate = theta_h0)

# Calcular la probabilidad de error tipo II para θ = 2
theta_h1 <- 2
beta_error <- pgamma(k, shape = n, rate = theta_h1)

# Simulaciones para estimar la probabilidad de error tipo II
n_sim <- 10000
T_sim <- replicate(n_sim, sum(-log(rbeta(n, theta_h1, 1))))
simulated_beta_error <- mean(T_sim <= k)

# Resultados
cat("Valor de k (umbral):", k, "\n")
cat("Probabilidad de error tipo II calculada:", beta_error, "\n")
cat("Probabilidad de error tipo II estimada por simulación:", simulated_beta_error, "\n")



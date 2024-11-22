library(tidyverse)

n <- 30
mu <- 100
s <- 10
x <- rnorm(n, mu, s)

# Estimate the mean of x
mean_x <- mean(x)

# IC 95%
alpha <- 0.05

t_critical <- qt(alpha / 2, df = n - 1, lower.tail = FALSE)

interval_of_confidence <- mean_x + c(-1, 1) * t_critical * sd(x) / sqrt(n)
print(interval_of_confidence)

# ----------------------------- Bootstrap -----------------------------

# Bootstrap es un método no paramétrico para estimar la distribución de un estadístico de interés (como la media) 
# a partir de los datos observados (sin asumir una distribución específica)

# IC With bootstrap

B <- 10000 # Number of bootstrap samples

# bootstrap_samples <- replicate(B, sample(x, n, replace = TRUE)) # Bootstrap samples

# R trabaja más rápido si se hace la matriz de una vez
bootstrap_faster <- matrix(sample(x, n * B, replace = TRUE), nrow = n, ncol = B)    # Faster way to do bootstrap
x_vectors <- bootstrap_faster

bootstrap_samples <- B

# A cada una le tengo que sacar la media
means <- colMeans(bootstrap_faster)

# Histograms
hist(means, breaks = 30, col = "lightblue", border = "white", main = "Bootstrap distribution of the mean", xlab = "Mean")
hist(x, col = "#AAAAAA", border = "white", main = "Original X", xlab = "Mean")

IC_bootstrap <- quantile(means, c(alpha / 2, 1 - alpha / 2))

IC_bootstrap
# v.s
interval_of_confidence

# The both, interval of confidence and IC with bootstrap are similar
# But the first method is asumed that the data is normal distributed
# The second method is more robust :)

# No solo se parecen las IC, sino que también las distribuciones teóricas y las de bootstrap

# Bajo normalidad [T = (x - mu) / (sd / sqrt(n))] ~ t(n - 1)
# Para cada muestra de bootstrap, se puede calcular el T y obtener un IC

# En Bootstrap, la media es la media empirica de las muestras de bootstrap
# Entonces cada estadístico de bootstrap es una media muestral
# Y la distribución de los estadísticos de bootstrap es la distribución de las medias muestrales
# Por lo tanto, el IC de bootstrap es un IC de las medias muestrales
    # F ~ (x barra), en mi munndo de bootstrap, la media de la distribución es la media de los datos
    # x_1 -> t_1 = (x_1\barra - x\barra) / (sd\gorrito / sqrt(n))

# En resumen, el IC de bootstrap es un IC de las medias muestrales

# Calcular las varianzas de los estadísticos de bootstrap
var_means <- apply(x_vectors, 2, var)

t_bootstrap <- (means - mean_x) / (sqrt(var_means) / sqrt(n))

# Plotting with ggplot2
data.frame(t = t_bootstrap) %>%
  ggplot(aes(x = t)) +
  geom_histogram(aes(y = ..density.., fill = "Dist Bootstrap"), bins = 30, alpha = 0.6, color = "white") +
  geom_density(aes(col = "Dist Bootstrap"), lwd = 2) +
  geom_vline(xintercept = quantile(t_bootstrap, c(0.025, 0.975)), linetype = "dashed", color = "black") +
  labs(title = "Bootstrap Distribution of the t-statistic",
       x = "t values",
       y = "Density",
       fill = "Legend",
       col = "Legend") +
  theme_minimal() +
  theme(legend.position = "top")

# ----------------------------- Pruebas para la media -----------------------------

mean
# H0: mu = 100 vs H1: mu != 100

# Prueba clásica suponiendo normalidad
# Rechazo si T > t_critical
# Podemos calcular el p-valor
    # IP[T > t | H_0: mu = 100] = 1 - P[T < t] = 1 - pt(t, df = n - 1)
    # T = (x - mu) / (sd / sqrt(n))
    # T = (means - 100) / (sd / sqrt(n))
t_observado <- (mean_x - 100) / (sd(x) / sqrt(n))
t_observado

pvalor <- 1 - pt(t_observado, df = n - 1)
pvalor # 0.3797
# Como pvalor > alpha, no rechazo H0
    # No puedo rechazar que la media sea 100

# ----------------------------- Si no quiero suponer normalidad -----------------------------
#                               haciendo la prueba por bootstrap

# Calculo el estadístico de bootstrap para cada muestra (ya lo hicimos)
# t_bootstrap

# Calculo el p-valor
    # IP[T > t | aproximación de t_bootstrap] 
pvalor_bootstrap <- mean(t_bootstrap >= t_observado)
pvalor_bootstrap # 0.3834

# Como pvalor > alpha, no rechazo H0
    # No puedo rechazar que la media sea 100

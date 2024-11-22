import scipy.stats as stats

# Parámetros de la distribución
n = 20
beta = 5

# Encontrar el percentil 5%
alpha = 0.05
t = stats.gamma.ppf(alpha, a=n, scale=beta)

print(t)

antes <- c(6.42, 6.76, 6.56, 4.80, 8.43, 7.49, 8.05, 5.05, 5.77, 3.91, 6.77, 6.44, 6.17, 7.67, 7.34, 6.85, 5.13, 5.73)
despues4 <- c(5.83, 6.20, 5.83, 4.27, 7.71, 7.12, 7.25, 4.63, 5.31, 3.70, 6.15, 5.59, 5.56, 7.11, 6.84, 6.40, 4.52, 5.13)

# Prueba no paramétrica adecuada: Prueba de signos
signs <- antes - despues4

n <- length(signs)

W <- n*(n+1)/2

E_x <- n*(n+1)/4
V_x <- n*(n+1)*(2*n+1)/24

z <- (W - E_x) / sqrt(V_x)

p_value <- 2 * (1 - pnorm(abs(z)))

cat("WSRT Manual:\n  W =", W, "\n  E[x] = ", E_x, "\n  V[x] = ", V_x, "\n  Z = ", z,  "\n  p-valor =", p_value, "\n")

# Prueba de Wilcoxon
wilcox_test <- wilcox.test(antes, despues4, paired = TRUE, alternative = "two.sided")

# Mostrar resultados de la Prueba de Wilcoxon
cat("Prueba de Wilcoxon:\n")
print(wilcox_test)

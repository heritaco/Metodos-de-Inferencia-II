# Datos
sujeto <- 1:18
antes <- c(6.42, 6.76, 6.56, 4.80, 8.43, 7.49, 8.05, 5.05, 5.77, 3.91, 6.77, 6.44, 6.17, 7.67, 7.34, 6.85, 5.13, 5.73)
despues4 <- c(5.83, 6.20, 5.83, 4.27, 7.71, 7.12, 7.25, 4.63, 5.31, 3.70, 6.15, 5.59, 5.56, 7.11, 6.84, 6.40, 4.52, 5.13)

# Prueba no paramétrica adecuada: Prueba de signos
signs <- antes - despues4
positivo <- sum(signs > 0)
negativo <- sum(signs < 0)
n <- positivo + negativo
p_value_sign <- 2 * min(positivo, negativo) / n

# Mostrar resultados de la Prueba de Signos
cat("Prueba de Signos:\n")
cat("Positivos:", positivo, "\n")
cat("Negativos:", negativo, "\n")
cat("Valor p:", p_value_sign, "\n\n")

# Prueba de Wilcoxon
wilcox_test <- wilcox.test(antes, despues4, paired = TRUE, alternative = "two.sided")

# Mostrar resultados de la Prueba de Wilcoxon
cat("Prueba de Wilcoxon:\n")
print(wilcox_test)
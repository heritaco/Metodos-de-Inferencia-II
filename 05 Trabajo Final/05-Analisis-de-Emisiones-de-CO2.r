# a) Filtrar los datos para incluir solo las observaciones desde 1930 en adelante
data <- read.csv("CO2_EMISSONS.CSV", stringsAsFactors = FALSE)
data_filtered <- subset(data, Year >= 1930)

# b) Remover todas las observaciones que son 0 y todos los datos vacíos
data_filtered[data_filtered == 0] <- NA
data_clean <- na.omit(data_filtered)

# c) Escoger dos países y realizar la prueba de Mann-Whitney
usa <- data_clean$USA
canada <- data_clean$Canada

# d) Cálculos manuales para estadístico y valor p usando aproximación normal
n1 <- length(usa)
n2 <- length(canada)
rank_all <- rank(c(usa, canada))
R1 <- sum(rank_all[1:n1])
U1 <- R1 - n1*(n1 + 1)/2
mu_U <- n1 * n2 / 2
sigma_U <- sqrt(n1 * n2 * (n1 + n2 + 1) / 12)
z <- (U1 - mu_U) / sigma_U
p_value <- 1 - pnorm(z)

# e) Comparar con wilcox.test
wilcox_result <- wilcox.test(usa, canada, alternative = "greater")
# Comentario: Comparar z y p_value con wilcox_result$statistic y wilcox_result$p.value

# f) Prueba t para diferencia de medias
t_test <- t.test(usa, canada, alternative = "greater", var.equal = FALSE)
# Verificar supuestos: normalidad y homogeneidad de varianzas
# Si no se cumplen, comparar con Mann-Whitney y t Bootstrap

# g) Prueba t de manera manual
mean_usa <- mean(usa)
mean_canada <- mean(canada)
var_usa <- var(usa)
var_canada <- var(canada)

t_stat <- (mean_usa - mean_canada) / sqrt(var_usa/n1 + var_canada/n2)
df <- (var_usa/n1 + var_canada/n2)^2 / ((var_usa^2)/(n1^2*(n1-1)) + (var_canada^2)/(n2^2*(n2-1))))
p_manual <- 1 - pt(t_stat, df)

# Resultado
t_stat
df
p_manual
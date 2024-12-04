# a) Filtrar los datos para incluir solo las observaciones desde 1930 en adelante
setwd("C:/Heri/GitHub/Metodos de Inferencia II/05 Trabajo Final")
data <- read.csv("CO2-emissions.csv", stringsAsFactors = FALSE)
data_filtered <- subset(data, Year >= 1930)

rank_function <- function(list1, list2) {
  
  combined <- c(list1, list2)
  sorted_combined <- sort(combined)
  
  sorted_list1 <- sort(list1)
  sorted_list2 <- sort(list2)
  
  matrix1 <- data.frame(Value = sorted_list1, Type = "x", stringsAsFactors = FALSE)
  matrix2 <- data.frame(Value = sorted_list2, Type = "y", stringsAsFactors = FALSE)
  
  matrix <- rbind(matrix1, matrix2)
  matrix <- matrix[order(matrix$Value), ]
  
  sorted_matrix <- matrix
  sorted_matrix$Rank <- 1:nrow(sorted_matrix)
  
  sorted_matrix$Count <- ifelse(sorted_matrix$Type == "x", 
                                sapply(1:nrow(sorted_matrix), function(i) sum(sorted_matrix$Type[1:(i-1)] == "y" & sorted_matrix$Value[1:(i-1)] < sorted_matrix$Value[i])), 
                                0)
  
  cat("Value\tType\tRank\tCount\n")
  apply(sorted_matrix, 1, function(row) {
    cat(paste(row["Value"], row["Type"], row["Rank"], row["Count"], sep = "\t"), "\n")
  })
  
  sum_ranks <- sum(sorted_matrix$Rank[sorted_matrix$Type == "x"])
  sum_counts <- sum(sorted_matrix$Count[sorted_matrix$Type != "-"])
  
  cat("\nsum_ranks list1 > list2 =", sum_ranks, "\n")
  cat("sum_counts =", sum_counts, "\n")

  return(sum_counts)
} 

# Wilcoxon Rank Sum Test

usa <- data_filtered$USA
usa

canada <- data_filtered$Canada
canada

rank_function(usa, canada)

sum_ranks <- rank_function(usa, canada)
sum_ranks

n1 <- length(usa)
n1

n2 <- length(canada)
n2

m_U <- n1 * n2 / 2
m_U

sigma_U <- sqrt(n1 * n2 * (n1 + n2 + 1) / 12)

z <- (sum_ranks - m_U) / sigma_U
z

p_value <- 1 - pnorm(z)
p_value

cat("Prueba Wilcoxon manual:\n W_x =", sum_ranks, "\n", "p-valor =", p_value, "\n")

# e) Comparar con wilcox.test
wilcox.test(usa, canada, alternative = "greater")
# Comentario: Comparar z y p_value con wilcox_result$statistic y wilcox_result$p.value

# f) Prueba t para diferencia de medias
t.test(usa, canada, alternative = "greater", var.equal = FALSE)
# Verificar supuestos: normalidad y homogeneidad de varianzas
# Si no se cumplen, comparar con Mann-Whitney y t Bootstrap

# g) Prueba t de manera manual
mean_usa <- mean(usa)
mean_canada <- mean(canada)
var_usa <- var(usa)
var_canada <- var(canada)
t_stat <- (mean_usa - mean_canada) / sqrt(var_usa/n1 + var_canada/n2)
df <- (var_usa/n1 + var_canada/n2)^2 / ((var_usa^2)/(n1^2*(n1-1)) + (var_canada^2)/(n2^2*(n2-1)))
p_manual <- 1 - pt(t_stat, df)

# Resultado
t_stat
df
p_manual

cat("Prueba t manual:\n t =", t_stat, "\ndf =", df , "\np-valor =", p_manual, "\n", "  Media EEUU =", mean_usa, "\n", "  Media Canada =", mean_canada, "\n")
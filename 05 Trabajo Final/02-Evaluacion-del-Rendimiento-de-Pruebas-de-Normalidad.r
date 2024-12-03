# 02.r

library(nortest)
library(moments)
library(ggplot2)

set.seed(123)

# Define parameters
sample_sizes <- c(30, 120, 480)
B <- 10000
alpha <- 0.10

# Initialize results
results <- list(
    H0_true = list(),
    H0_false = list()
)

# Simulation function
simulate_tests <- function(n, dist, test_funcs, B, alpha) {
    rejections <- matrix(0, nrow = B, ncol = length(test_funcs))
    colnames(rejections) <- names(test_funcs)
    
    for (i in 1:B) {
        if (dist == "normal") {
            data <- rnorm(n)
        } else {
            data <- rt(n, df = 5)
        }
        for (test in names(test_funcs)) {
            test_result <- tryCatch(test_funcs[[test]](data), error = function(e) NULL)
            if (!is.null(test_result) && test_result$p.value < alpha) {
                rejections[i, test] <- 1
            }
        }
    }
    colMeans(rejections)
}

library(nortest)   # For lillie.test, cvm.test, ad.test
library(stats)     # For shapiro.test

# Define tests
test_functions <- list(
    LT = lillie.test,
    CVMT = cvm.test,
    AD = ad.test,
    SFT = shapiro.test
)

# Run simulations for H0_true
for (n in sample_sizes) {
    results$H0_true[[as.character(n)]] <- simulate_tests(n, "normal", test_functions, B, alpha)
}

# Create tables
library(knitr)
kable(as.data.frame(results$H0_true), digits = 3, caption = "H0 True: Proportion of Rejections")
kable(as.data.frame(results$H0_false), digits = 3, caption = "H0 False: Proportion of Rejections")

# Plot p-values distribution (example for one n)
n_example <- 30
p_values_true <- replicate(B, {
    data <- rnorm(n_example)
    sapply(test_functions, function(f) f(data)$p.value)
})
p_values_false <- replicate(B, {
    data <- rt(n_example, df = 5)
    sapply(test_functions, function(f) f(data)$p.value)
})

p_df_true <- data.frame(t(p_values_true))
p_df_true$Test <- rownames(p_df_true)
p_df_true_melt <- reshape2::melt(p_df_true, id.vars = "Test")

p_df_false <- data.frame(t(p_values_false))
p_df_false$Test <- rownames(p_df_false)
p_df_false_melt <- reshape2::melt(p_df_false, id.vars = "Test")

ggplot(p_df_true_melt, aes(x = value, fill = Test)) +
    geom_density(alpha = 0.5) +
    labs(title = paste("P-Values Distribution H0 True n =", n_example), x = "P-Value")

ggplot(p_df_false_melt, aes(x = value, fill = Test)) +
    geom_density(alpha = 0.5) +
    labs(title = paste("P-Values Distribution H0 False n =", n_example), x = "P-Value")

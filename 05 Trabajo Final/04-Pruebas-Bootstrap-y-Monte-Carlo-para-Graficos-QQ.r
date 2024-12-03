# Alternativa Monte Carlo y Bootstrap para Pruebas de Bondad de Ajuste en Gráficos QQ

set.seed(123)
library(ggplot2)

# Parámetros
n <- 30
M <- 1000
B <- 1000
alpha <- 0.01

# Funciones para generar cuantiles teóricos
qq_theoretical <- function(data, dist = "gamma") {
    n <- length(data)
    sorted_data <- sort(data)
    if (dist == "gamma") {
        shape <- 2
        rate <- 1
        return(qgamma((1:n)/(n + 1), shape=shape, rate=rate))
    } else if (dist == "lnorm") {
        meanlog <- 0
        sdlog <- 1
        return(qlnorm((1:n)/(n + 1), meanlog=meanlog, sdlog=sdlog))
    }
}

# Alternativa Monte Carlo
monte_carlo_bands <- function(data, dist, M, alpha) {
    n <- length(data)
    theoretical <- qq_theoretical(data, dist)
    mc_quantiles <- matrix(0, nrow=M, ncol=n)
    
    for(i in 1:M) {
        if(dist == "gamma"){
            sim <- rgamma(n, shape=2, rate=1)
        } else if(dist == "lnorm"){
            sim <- rlnorm(n, meanlog=0, sdlog=1)
        }
        mc_quantiles[i, ] <- sort(sim)
    }
    
    lower <- apply(mc_quantiles, 2, quantile, probs=alpha/2)
    upper <- apply(mc_quantiles, 2, quantile, probs=1 - alpha/2)
    return(data.frame(theoretical, lower, upper))
}

# Alternativa Bootstrap
bootstrap_bands <- function(data, dist, B, alpha) {
    n <- length(data)
    sorted_data <- sort(data)
    bootstrap_quantiles <- matrix(0, nrow=B, ncol=n)
    
    for(i in 1:B) {
        indices <- sample(1:n, size=n, replace=TRUE)
        sample_data <- data[indices]
        bootstrap_quantiles[i, ] <- sort(sample_data)
    }
    
    lower <- apply(bootstrap_quantiles, 2, quantile, probs=alpha/2)
    upper <- apply(bootstrap_quantiles, 2, quantile, probs=1 - alpha/2)
    theoretical <- qq_theoretical(data, dist)
    return(data.frame(theoretical, lower, upper))
}

# Simulación para Gamma
sample_gamma <- rgamma(n, shape=2, rate=1)
qq_data_gamma <- data.frame(
    observed = sort(sample_gamma),
    theoretical = qq_theoretical(sample_gamma, "gamma")
)

mc_bands_gamma <- monte_carlo_bands(sample_gamma, "gamma", M, alpha)
bootstrap_bands_gamma <- bootstrap_bands(sample_gamma, "gamma", B, alpha)

# Gráfico QQ con Monte Carlo
ggplot(qq_data_gamma, aes(x=observed, y=theoretical)) +
    geom_point() +
    geom_line(aes(y=mc_bands_gamma$lower), linetype="dashed", color="blue") +
    geom_line(aes(y=mc_bands_gamma$upper), linetype="dashed", color="blue") +
    geom_abline(intercept=0, slope=1) +
    labs(title="QQ Plot Gamma - Monte Carlo", x="Cuantiles Observados", y="Cuantiles Teóricos")

# Gráfico QQ con Bootstrap
ggplot(qq_data_gamma, aes(x=observed, y=theoretical)) +
    geom_point() +
    geom_line(aes(y=bootstrap_bands_gamma$lower), linetype="dashed", color="red") +
    geom_line(aes(y=bootstrap_bands_gamma$upper), linetype="dashed", color="red") +
    geom_abline(intercept=0, slope=1) +
    labs(title="QQ Plot Gamma - Bootstrap", x="Cuantiles Observados", y="Cuantiles Teóricos")

# Simulación para Lognormal
sample_lnorm <- rlnorm(n, meanlog=0, sdlog=1)
qq_data_lnorm <- data.frame(
    observed = sort(sample_lnorm),
    theoretical = qq_theoretical(sample_lnorm, "lnorm")
)

mc_bands_lnorm <- monte_carlo_bands(sample_lnorm, "lnorm", M, alpha)
bootstrap_bands_lnorm <- bootstrap_bands(sample_lnorm, "lnorm", B, alpha)

# Gráfico QQ con Monte Carlo
ggplot(qq_data_lnorm, aes(x=observed, y=theoretical)) +
    geom_point() +
    geom_line(aes(y=mc_bands_lnorm$lower), linetype="dashed", color="blue") +
    geom_line(aes(y=mc_bands_lnorm$upper), linetype="dashed", color="blue") +
    geom_abline(intercept=0, slope=1) +
    labs(title="QQ Plot Lognormal - Monte Carlo", x="Cuantiles Observados", y="Cuantiles Teóricos")

# Gráfico QQ con Bootstrap
ggplot(qq_data_lnorm, aes(x=observed, y=theoretical)) +
    geom_point() +
    geom_line(aes(y=bootstrap_bands_lnorm$lower), linetype="dashed", color="red") +
    geom_line(aes(y=bootstrap_bands_lnorm$upper), linetype="dashed", color="red") +
    geom_abline(intercept=0, slope=1) +
    labs(title="QQ Plot Lognormal - Bootstrap", x="Cuantiles Observados", y="Cuantiles Teóricos")




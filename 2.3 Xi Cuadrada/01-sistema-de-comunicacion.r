"""
Típicamente, en el conteo de errores de un sistema de comunicación,
se asume que los errores se distribuyen de acuerdo a una distribución de Poisson.
En un experimento se contaron los errores en 170 intervalos de tiempo y se
obtuvieron los siguientes resultados:

Ningún error:        44
Un error:            52
Dos errores:         36
Tres errores:        20
Cuatro errores:      12
Cinco errores:        6
Seis errores:         0
Siete errores:        1

Número de errores:    0  1  2  3  4  5  6  7
Frecuencia observada: 44 52 36 20 12 5  0  1
"""

observados = c(44, 52, 36, 20, 12, 5, 0, 1)

# Buscar valores observados
x_barra = sum(0:7 * observados) / 170
lambda_gorrito = x_barra
lambda_gorrito

# Valores observados para xi, (tienen que ser mayores a 5)
observado = c(44, 52, 36, 20, 12, 6)

# Valores para los cuales queremos calcular la probabilidad
x <- 0:4
probabilidades <- dpois(x, lambda)
probabilidades

# Valor límite
x <- 4
probabilidad_mayor_que_4 <- ppois(x, lambda, lower.tail = FALSE)
probabilidad_mayor_que_4

# Concatenar los valores de las probabilidades
probabilidades <- c(probabilidades, probabilidad_mayor_que_4)
probabilidades

"""
Redondemos? el p-valor cambia dependiendo de si redondeamos o no
p-valor-redondeado      = 0.3707383
p-valor-no-redondeado   = 0.2677756
"""

# Calcular los valores esperados
esperado = probabilidades * sum(observado)
esperado

# Calcular el valor chi cuadrado
valor_chi_cuadrado = (observado - esperado)^2 / esperado
valor_chi_cuadrado

# Sumar los valores
suma_chi_cuadrado = sum(valor_chi_cuadrado)
suma_chi_cuadrado

# Calcular los grados de libertad
grados_libertad = length(observado) - 1
grados_libertad

# Calcular el p-valor
p_valor = pchisq(suma_chi_cuadrado, grados_libertad, lower.tail = FALSE)
p_valor

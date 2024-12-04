# ID Genero Puntaje Rango ID Genero Puntaje Rango ID Genero Puntaje Rango
# 1 1 5 40.5    16 1 2 10.5 31 2 5 40.5
# 2 1 1 3.5     17 1 4 30 32 2 4 30
# 3 1 1 3.5     18 1 2 10.5 33 2 2 10.5
# 4 1 3 19.5    19 1 2 10.5 34 2 4 30
# 5 1 4 30      20 1 2 10.5 35 2 4 30
# 6 1 5 40.5    21 1 1 3.5 36 2 4 30
# 7 1 2 10.5    22 1 5 40.5 37 2 5 40.5
# 8 1 4 30      23 1 4 30 38 2 4 30
# 9 1 3 19.5    24 1 1 3.5 39 2 5 40.5
# 10 1 3 19.5   25 1 3 19.5 40 2 3 19.5
# 11 1 1 3.5    26 2 5 40.5 41 2 3 19.5
# 12 1 3 19.5   27 2 4 30 42 2 3 19.5
# 13 1 3 19.5   28 2 5 40.5 43 2 4 30
# 14 1 5 40.5   29 2 3 19.5 44 2 2 10.5
# 15 1 2 10.5   30 2 5 40.5 45 2 1 3.5

Rango_1 = c(40.5, 3.5, 3.5, 19.5, 30, 40.5, 10.5, 30, 19.5, 19.5, 3.5, 19.5, 19.5, 40.5, 10.5, 10.5, 30, 10.5, 10.5, 10.5, 3.5, 40.5, 30, 10.5)
Rango_2 = c(40.5, 19.5, 40.5, 30, 10.5, 19.5, 40.5, 30, 3.5, 40.5, 30, 3.5, 40.5, 30, 30, 40.5, 19.5, 19.5, 19.5, 30, 10.5)

Sum_r1 = sum(Rango_1)
Sum_r2 = sum(Rango_2)

n1 = length(Rango_1)
n2 = length(Rango_2)

cat("Valor de n_x:", n1)
cat("Valor de n_y:", n2)

cat("Valor de W:", Sum_r1)


E_x = n1 * (n1 + n2 + 1) / 2
Var_x = n1 * n2 * (n1 + n2 + 1) / 12

cat("Valor de E_x:", E_x)
cat("Valor de Var_x:", Var_x)
cat("W ~ N(", E_x, ",", Var_x, ")")

U_x = (Sum_r1 - E_x) / sqrt(Var_x)

cat("Valor de Z:", U_x)


p_value = pnorm(U_x)
p_value

cat("Valor de p:", p_value)

cat("Valor de n_x:", n1, "\nValor de n_y:", n2, "\nValor de W:", Sum_r1, "\nValor de E_x:", E_x, "\nValor de Var_x:", Var_x, "\nW ~ N(", E_x, ",", Var_x, ")\nValor de Z:", U_x, "\nValor de p:", p_value)


cat("n_x:", n1, "\n_y:", n2, "\nW:", Sum_r1, "\nE[X]:", E_x, "\n\Var[X]:", Var_x, "\nW ~ N(", E_x, ",", Var_x, ")\nZ:", U_x, "\np:", p_value)
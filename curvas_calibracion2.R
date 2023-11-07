library(ggplot2)

# Datos de concentración conocida y propiedad medida
concentracion <- c(1, 2, 3, 4, 5)
propiedad_medida <- c(10, 20, 30, 40, 50)

# Realizar un ajuste lineal
ajuste_lineal <- lm(propiedad_medida ~ concentracion)

# Obtener los parámetros de la ecuación lineal
pendiente <- coef(ajuste_lineal)[2]
ordenada_al_origen <- coef(ajuste_lineal)[1]

# Crear un dataframe con los datos y la curva de calibración
df <- data.frame(Concentracion = concentracion, Medida = propiedad_medida)
df$Curva_Calibracion <- predict(ajuste_lineal, newdata = df)

# Crear el gráfico utilizando ggplot2
grafico <- ggplot(data = df, aes(x = Concentracion, y = Medida)) +
  geom_point() +  # Puntos de datos
  geom_line(aes(x = Concentracion, y = Curva_Calibracion), color = "red") +  # Curva de calibración
  labs(title = "Curva de Calibración (Ajuste Lineal)",
       x = "Concentración",
       y = "Propiedad Medida")

# Mostrar el gráfico
print(grafico)


#------------------------------------------------------------------------------
library(ggplot2)

# Datos de concentración conocida y propiedad medida
concentracion <- c(1, 2, 3, 4, 5)
propiedad_medida <- c(10, 20, 30, 40, 50)

# Realizar un ajuste no lineal
ajuste_no_lineal <- nls(propiedad_medida ~ a * exp(b * concentracion), start = list(a = 1, b = 1))

# Obtener los parámetros de la ecuación no lineal
parametros <- coef(ajuste_no_lineal)

# Crear un dataframe con los datos y la curva de calibración
df <- data.frame(Concentracion = concentracion, Medida = propiedad_medida)
df$Curva_Calibracion <- parametros["a"] * exp(parametros["b"] * df$Concentracion)

# Crear el gráfico utilizando ggplot2
grafico <- ggplot(data = df, aes(x = Concentracion, y = Medida)) +
  geom_point() +  # Puntos de datos
  geom_line(aes(x = Concentracion, y = Curva_Calibracion), color = "red") +  # Curva de calibración
  labs(title = "Curva de Calibración (Ajuste No Lineal)",
       x = "Concentración",
       y = "Propiedad Medida")

# Mostrar el gráfico
print(grafico)

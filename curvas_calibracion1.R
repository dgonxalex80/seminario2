library(ggplot2)
library(nls2)

# Datos de concentración conocida y propiedad medida
concentracion <- c(1, 2, 3, 4, 5)
propiedad_medida <- c(10, 20, 30, 40, 50)

# Realizar un ajuste no lineal a una función exponencial
ajuste_no_lineal <- nls(propiedad_medida ~ a * exp(b * concentracion), start = list(a = 1, b = 1))

# Obtener los parámetros ajustados
parametros <- coef(ajuste_no_lineal)

# Prueba de hipótesis para los parámetros
summary(ajuste_no_lineal)

# Intervalos de confianza para los parámetros
confint(ajuste_no_lineal)

# Gráfico de datos y curva de calibración
df <- data.frame(Concentracion = concentracion, Medida = propiedad_medida)
df$Curva_Calibracion <- parametros["a"] * exp(parametros["b"] * df$Concentracion)

grafico <- ggplot(data = df, aes(x = Concentracion, y = Medida)) +
  geom_point() +  # Puntos de datos
  geom_line(aes(x = Concentracion, y = Curva_Calibracion), color = "red") +  # Curva de calibración
  labs(title = "Curva de Calibración (Ajuste No Lineal)",
       x = "Concentración",
       y = "Propiedad Medida")

print(grafico)

# Validación cruzada simple
set.seed(123)  # Establece una semilla para la reproducibilidad
indices <- sample(1:5)  # Genera índices aleatorios para división de datos
datos_entrenamiento <- df[indices != 5, ]
datos_prueba <- df[indices == 5, ]

ajuste_validacion <- nls(Medida ~ a * exp(b * Concentracion), data = datos_entrenamiento, start = list(a = 1, b = 1))
parametros_validacion <- coef(ajuste_validacion)

# Predicción en datos de prueba
predicciones <- predict(ajuste_validacion, newdata = datos_prueba)

# Evaluación de la capacidad de predicción
error_cuadratico_medio <- sqrt(mean((datos_prueba$Medida - predicciones)^2))
cat("Error cuadrático medio en datos de prueba:", error_cuadratico_medio, "\n")


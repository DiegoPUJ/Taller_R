# Simulación de base de datos con 60 clientes
set.seed(42)

salario <- c(19388, 49898, 28219, 83601, 29736, 50235, 45976, sample(18000:85000, 53, replace = TRUE))
industria <- c(1, 2, 0, 0, 0, 1, 0, sample(c(0,1,2), 53, replace = TRUE, prob = c(0.5,0.3,0.2)))
ocupacion <- c(0, 0, 3, 5, 4, 1, 2, sample(0:5, 53, replace = TRUE))

datos <- data.frame(Salario = salario, Industria = industria, Ocupacion = ocupacion)

# Ver los primeros registros
head(datos)

# Punto 3: Tabla de frecuencias y gráfico circular para Ocupación

# Crear la tabla de frecuencias para la variable Ocupacion (con mayúscula)
tabla_ocupacion <- table(datos$Ocupacion)
print(tabla_ocupacion)

# Calcular los porcentajes para cada categoría
porcentajes <- round(prop.table(tabla_ocupacion) * 100, 2)
print(porcentajes)

# Graficar el diagrama circular (pastel)
pie(tabla_ocupacion,
    labels = paste(names(tabla_ocupacion), porcentajes, "%"),
    main = "Distribución de Ocupación")



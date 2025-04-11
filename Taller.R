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
    
# Punto 4: Tabla de frecuencias y gráfico de barras para Industria
# Crear la tabla de frecuencias para la variable Industria
tabla_industria <- table(datos$Industria)
print(tabla_industria)

# Calcular los porcentajes
porcentaje_industria <- round(prop.table(tabla_industria) * 100, 2)
print(porcentaje_industria)

# Etiquetas con nombre completo para las barras
nombres_industria <- c("Otra", "Manufactura", "Construcción")
names(tabla_industria) <- nombres_industria

# Diagrama de barras
barplot(tabla_industria,
        col = "steelblue",
        main = "Distribución por Tipo de Industria",
        xlab = "Tipo de Industria",
        ylab = "Frecuencia",
        ylim = c(0, max(tabla_industria) + 5))

# Punto 5: Tabla de frecuencias y histograma para la variable cuantitativa SALARIO

# Crear intervalos de clases para los salarios (8 clases por defecto)
tabla_salario <- cut(datos$Salario, breaks = 8)
tabla_frecuencia_salario <- table(tabla_salario)
print(tabla_frecuencia_salario)

# Calcular porcentajes por intervalo
porcentaje_salario <- round(prop.table(tabla_frecuencia_salario) * 100, 2)
print(porcentaje_salario)

# Histograma
hist(datos$Salario,
     breaks = 8,
     col = "lightgreen",
     main = "Histograma de Salario Anual",
     xlab = "Salario (USD)",
     ylab = "Frecuencia")


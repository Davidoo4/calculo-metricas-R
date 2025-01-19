# 1. Leer el fichero de entrada
leer_numeros <- function(nombre_fichero) {
  if (!file.exists(nombre_fichero)) {
    stop(paste("El archivo", nombre_fichero, "no existe."))
  }
  numeros <- scan(nombre_fichero, what = integer(), quiet = TRUE)
  return(numeros)
}

# 2. Calcular estadísticas
calcular_estadisticas <- function(numeros) {
  media <- mean(numeros)
  mediana <- median(numeros)
  desviacion_estandar <- sd(numeros)
  return(list(media = media, mediana = mediana, desviacion_estandar = desviacion_estandar))
}

# 3. Manejar valores atípicos
verificar_variabilidad <- function(desviacion_estandar) {
  if (desviacion_estandar > 10) {
    mensaje <- "Alta variabilidad: La desviación estándar es mayor a 10."
  } else {
    mensaje <- "Baja variabilidad: La desviación estándar es menor o igual a 10."
  }
  return(mensaje)
}

# 4. Aplicar una función con la familia sapply
calcular_cuadrados <- function(numeros) {
  cuadrados <- sapply(numeros, function(x) x^2)
  return(cuadrados)
}

# 5. Escribir los resultados en un fichero de salida
escribir_resultados <- function(nombre_salida, estadisticas, mensaje, cuadrados) {
  resultados <- c(
    "Resultados del análisis:",
    paste("Media:", round(estadisticas$media, 2)),
    paste("Mediana:", estadisticas$mediana),
    paste("Desviación estándar:", round(estadisticas$desviacion_estandar, 2)),
    mensaje,
    "\nCuadrados de los números:",
    paste(cuadrados, collapse = ", ")
  )
  writeLines(resultados, con = nombre_salida)
  cat("Resultados guardados en", nombre_salida, "\n")
}

# 6. Ejecutar el script
nombre_fichero <- "numeros.txt"  # Reemplazar con la ruta real al archivo
nombre_salida <- "resultados.txt"

# Leer los números
numeros <- leer_numeros(nombre_fichero)

# Calcular estadísticas
estadisticas <- calcular_estadisticas(numeros)

# Verificar variabilidad
mensaje <- verificar_variabilidad(estadisticas$desviacion_estandar)

# Calcular cuadrados
cuadrados <- calcular_cuadrados(numeros)

# Escribir los resultados
escribir_resultados(nombre_salida, estadisticas, mensaje, cuadrados)

#Función de densidad de la distribución Triangular

dtriang <- function(x, min, max, mode) {
  #Validaciones
  if (any(min >= max)) stop("min debe ser menor que max")
  if (any(mode < min | mode > max)) stop("mode debe estar entre min y max")

  #Cálculo de la densidad según cada tramo
  dens <- ifelse(x < min | x > max, 0,
                 ifelse(x <= mode, 2 * (x - min) / ((max - min) * (mode - min)),
                        2 * (max - x) / ((max - min) * (max - mode))))
  return(dens)
}

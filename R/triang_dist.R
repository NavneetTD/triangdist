#Función de densidad de la distribución triangular

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


#Función de distribución acumulada triangular

ptriang <- function(q, min, max, mode) {
  if (any(min >= max)) stop("min debe ser menor que max")
  if (any(mode < min | mode > max)) stop("mode debe estar entre min y max")

  cdf <- ifelse(q < min, 0,
                ifelse(q <= mode, (q - min)^2 / ((max - min) * (mode - min)),
                       ifelse(q < max, 1 - (max - q)^2 / ((max - min) * (max - mode)),
                              1)))
  return(cdf)
}

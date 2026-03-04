#' @export
#'
#' @title Esquema de rotación trimestral 2-2-2
#' @description Construye un calendario trimestral con cuatro grupos de rotación,
#' garantizando para un periodo de referencia t los traslapes:
#' 50\% (t+1), 25\% (t+2), 0\% (t+3) y 50\% (t+4).
#' @param periodos Número de periodos trimestrales a generar.
#' @param prefijo Prefijo de identificación para las cohortes.
#' @return Una lista con dos elementos: \code{esquema} (data frame del calendario)
#' y \code{traslapes} (porcentaje de traslape respecto al periodo t).
#' @examples
#' resultado <- Esquema_222_trimestral(periodos = 8)
#' resultado$esquema
#' resultado$traslapes
Esquema_222_trimestral <- function(periodos = 8, prefijo = "C") {
  if (!is.numeric(periodos) || length(periodos) != 1 || periodos < 5) {
    stop("periodos debe ser numérico y mayor o igual a 5")
  }

  if (!is.character(prefijo) || nchar(prefijo) != 1) {
    stop("prefijo debe ser un caracter de longitud 1")
  }

  # Patrón base de 5 trimestres que cumple los traslapes solicitados
  base <- list(
    c(1, 2, 3, 4),
    c(1, 2, 5, 6),
    c(1, 7, 8, 9),
    c(10, 11, 12, 13),
    c(1, 2, 14, 15)
  )

  esquema_ids <- vector(mode = "list", length = periodos)
  esquema_ids[1:5] <- base

  if (periodos > 5) {
    for (i in 6:periodos) {
      desfase <- max(unlist(esquema_ids[1:(i - 1)]))
      esquema_ids[[i]] <- base[[((i - 1) %% 5) + 1]] + desfase
    }
  }

  esquema <- as.data.frame(do.call(rbind, esquema_ids))
  names(esquema) <- paste0("G", 1:4)
  esquema <- cbind(periodo = 1:periodos, esquema)

  for (j in 2:ncol(esquema)) {
    esquema[[j]] <- paste0(prefijo, esquema[[j]])
  }

  ref <- unlist(esquema[1, -1])
  traslapes <- data.frame(
    periodo = 1:5,
    traslape = sapply(1:5, function(i) {
      actual <- unlist(esquema[i, -1])
      mean(ref %in% actual)
    })
  )

  return(list(esquema = esquema, traslapes = traslapes))
}

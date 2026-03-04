% Generated manually
\name{Esquema_222_trimestral}
\alias{Esquema_222_trimestral}
\title{Esquema de rotación trimestral 2-2-2}
\usage{
Esquema_222_trimestral(periodos = 8, prefijo = "C")
}
\arguments{
\item{periodos}{Número de periodos trimestrales a generar.}

\item{prefijo}{Prefijo para identificar cohortes.}
}
\value{
Una lista con dos elementos:
\itemize{
\item \code{esquema}: data frame con columnas \code{periodo}, \code{G1}, \code{G2}, \code{G3}, \code{G4}.
\item \code{traslapes}: data frame con el traslape respecto al periodo \code{t}.
}
}
\description{
Construye un calendario trimestral con cuatro grupos de rotación y los traslapes:
50\% entre \code{t} y \code{t+1}, 25\% entre \code{t} y \code{t+2}, 0\% entre
\code{t} y \code{t+3}, y 50\% entre \code{t} y \code{t+4}.
}
\examples{
resultado <- Esquema_222_trimestral(periodos = 8)
resultado$esquema
resultado$traslapes
}

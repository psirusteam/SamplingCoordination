#' @export
#' @importFrom stats runif
#' @importFrom dplyr arrange
#' @importFrom dplyr mutate
#' @importFrom magrittr %>%
#' @importFrom dplyr desc
#'
#' @title
#' Generation of permanent, colocated, Poisson and Pareto random numbers
#' @description
#' Generates, for \code{N} units, the permanent random numbers
#' \eqn{\xi_k \sim U(0,1)} and the colocated random numbers
#' \eqn{(R_k - \epsilon)/N}, where \eqn{R_k} is the rank of \eqn{\xi_k} and
#' \eqn{\epsilon \sim U(0,1)} is a single random number independent of the
#' \eqn{\xi_k} (Brewer, Early and Joyce, 1972). When a size measure
#' \code{xk} is given, it also computes the sequential Poisson number
#' \eqn{\xi_k / (N p_k)}, with \eqn{p_k = x_k / \sum x_k} (Ohlsson, 1998),
#' or, with \code{Pareto = TRUE}, the Pareto number
#' \eqn{[\xi_k / (1 - \xi_k)] / [\pi_k / (1 - \pi_k)]}, with \eqn{\pi_k}
#' proportional to \code{xk} for a sample of size \code{n} (Rosen, 1997).
#' In all cases the sample is formed by the units with the smallest numbers.
#'
#' For a PSU frame stored in a \code{data.frame}, with strata, see
#' \code{\link{generate_random_frame}}.
#'
#' @return
#' A list with:
#' \describe{
#'   \item{\code{Xi_Perman}}{Permanent random numbers.}
#'   \item{\code{Xi_Coloc}}{Colocated random numbers.}
#'   \item{\code{Xi_pipt}}{Sequential Poisson numbers; only when \code{xk}
#'     is given and \code{Pareto = FALSE}.}
#'   \item{\code{Xi_Pareto}}{Pareto numbers; only when \code{xk} and
#'     \code{n} are given and \code{Pareto = TRUE}.}
#' }
#' @author Jose Fernando Zea Castro <jfzeac at unal.edu.co>,
#'   Hugo Andres Gutierrez Rojas <andres.gutierrez at cepal.org>,
#'   Stalyn Yasid Guerrero Gomez <syguerrerog at unal.edu.co>
#' @param N Number of units.
#' @param seed Seed for the random number generation.
#' @param xk Optional numeric vector of length \code{N} with the size measure
#'   of each unit (e.g. number of dwellings). If \code{NULL}, only the
#'   permanent and colocated numbers are returned.
#' @param Pareto Logical. If \code{TRUE}, the Pareto number is computed
#'   instead of the sequential Poisson number. Requires \code{xk} and
#'   \code{n}.
#' @param n Sample size used to compute the inclusion probabilities
#'   \eqn{\pi_k} of the Pareto number.
#' @seealso \code{\link{generate_random_frame}}
#'
#' @examples
#' # Permanent and colocated numbers for 5 units
#' generate_random(N = 5, seed = 12345)
#'
#' # With a size measure: sequential Poisson number
#' generate_random(N = 5, seed = 12345, xk = c(50, 40, 70, 30, 90))
#'
#' # Pareto number for a sample of 3 units
#' generate_random(N = 5, seed = 12345, xk = c(50, 40, 70, 30, 90),
#'                 Pareto = TRUE, n = 3)

generate_random <- function(N,
                            seed,
                            xk = NULL,
                            Pareto = FALSE,
                            n = NULL) {
  set.seed(seed)
  Xi_Perman <- runif(N)
  
  # epsilon must be independent of Xi_Perman (Brewer, Early and Joyce, 1972):
  # draw it after the permanent numbers, without resetting the seed.
  epsilon <- runif(1)
  
  Xi_Coloc <- (rank(Xi_Perman) - epsilon) / N
  
  salida <- list(Xi_Perman, Xi_Coloc)
  names(salida) <- c("Xi_Perman", "Xi_Coloc")
  
  if (isTRUE(Pareto) &
      (!is.null(xk) &
       is.null(n)) |
      (is.null(xk) & !is.null(n)))
    stop("Enter n and the vector xk")
  
  if (!is.null(xk)) {
    if (length(xk) != N)
      stop("Enter a vector xk of the same length as N")
    
    pk <- xk / sum(xk)
    Xi_ppt <- Xi_Perman / (N * pk)
    
    if (isTRUE(Pareto)) {
      pi_k <- TeachingSampling::PikPPS(n, x = xk)
      Xi_Pareto <- (Xi_Perman / (1 - Xi_Perman)) / (pi_k / (1 - pi_k))
      salida <- list(Xi_Perman, Xi_Coloc, Xi_Pareto)
      names(salida) <- c("Xi_Perman", "Xi_Coloc", "Xi_Pareto")
    } else {
      salida <- list(Xi_Perman, Xi_Coloc, Xi_ppt)
      names(salida) <- c("Xi_Perman", "Xi_Coloc", "Xi_pipt")
    }
  }
  
  return(salida)
}
#' @export
#'
#' @title
#' Coordinated simple random sampling (illustration)
#' @description
#' Selects \code{Q} coordinated simple random samples without replacement
#' (MAS, \emph{muestreo aleatorio simple}) of sizes \code{vctr_n} from the
#' units \code{1:N}, using one permanent random number \eqn{\xi} generated
#' with \code{\link{generate_random}}. Sample 1 contains the
#' \code{vctr_n[1]} units with the smallest \eqn{\xi}. Sample \eqn{i}
#' contains the \code{vctr_n[i]} units with the smallest
#' \eqn{(\xi + a_i) \bmod 1}, where
#' \itemize{
#'   \item \code{type = "negative"}: \eqn{a_i = (i - 1)/Q}, so the samples
#'     start at different points of the interval (0, 1) and overlap as little
#'     as possible;
#'   \item \code{type = "positive"}: \eqn{a_i = 0}, so all samples start
#'     at the same point and overlap as much as possible.
#' }
#'
#' This function is illustrative: it works on simulated units \code{1:N}
#' and is useful to teach how coordination by shifting a common permanent
#' random number works. To coordinate samples on a real PSU frame see
#' \code{\link{select_coordinated_PSUs}} and \code{\link{overlap_sample}}.
#' @return
#' A data frame sorted by \code{unit} with the columns \code{unit}
#' (1 to \code{N}), \code{xi_P} (permanent random number), \code{s1}
#' and, for \eqn{i = 2, \ldots, Q}, \code{xi_P<i>} (shifted number) and
#' \code{s<i>} (1 if the unit is in sample \eqn{i}, 0 otherwise).
#' @author Jose Fernando Zea Castro <jfzeac at unal.edu.co>, Hugo Andres
#'   Gutierrez Rojas <andres.gutierrez at cepal.org>, Stalyn Yasid Guerrero
#'   Gomez <syguerrerog at unal.edu.co>
#' @param type \code{"negative"} (default) or \code{"positive"}.
#' @param Q Number of samples (Q >= 2). Default 2.
#' @param N Population size. Default 10.
#' @param vctr_n Numeric vector of length \code{Q} with the size of each
#'   sample.
#' @param seed Seed for the permanent random number. Default 12345.
#'
#' @seealso \code{\link{generate_random}}, \code{\link{select_coordinated_PSUs}}
#' @examples
#' # Three samples with negative coordination
#' SI_coord(type = "negative", Q = 3, N = 100, vctr_n = c(10, 20, 12),
#'          seed = 12345)

SI_coord <- function(type = "negative",
                     Q = 2,
                     N = 10,
                     vctr_n,
                     seed = 12345) {
  xi_P <- generate_random(N = N, seed = seed)$Xi_Perman
  
  if (type == "negative") {
    vctr_a <- rep(NA_real_, Q)
    vctr_a[1] <- 0
    
    for (i in 1:Q) {
      vctr_a[i] <- vctr_a[1] + (i - 1) / Q
    }
    
    df <- data.frame(unit = 1:N, xi_P) %>% arrange(xi_P) %>%
      mutate(s1 = c(rep(1, vctr_n[1]),  rep(0, N - vctr_n[1])))
    
    for (i in 2:Q) {
      df[[paste0("xi_P", i)]] <- (df[["xi_P"]] + vctr_a[i]) %% 1
      df <- df[order(df[[paste0("xi_P", i)]]),]
      df[[paste0("s", i)]] <-
        c(rep(1, vctr_n[i]), rep(0, N - vctr_n[i]))
    }
  }
  
  if (type == "positive") {
    vctr_a <- rep(NA_real_, Q)
    vctr_a[1] <- 0
    
    for (i in 1:Q) {
      vctr_a[i] <- vctr_a[1] + 0
    }
    
    df <- data.frame(unit = 1:N, xi_P) %>% arrange(xi_P) %>%
      mutate(s1 = c(rep(1, vctr_n[1]),  rep(0, N - vctr_n[1])))
    
    for (i in 2:Q) {
      df[[paste0("xi_P", i)]] <- (df[["xi_P"]] + vctr_a[i]) %% 1
      df <- df[order(df[[paste0("xi_P", i)]]),]
      df[[paste0("s", i)]] <-
        c(rep(1, vctr_n[i]), rep(0, N - vctr_n[i]))
    }
  }
  
  df <- df %>% arrange(unit)
  return(df)
}
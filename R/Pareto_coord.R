#' @export
#'
#' @title
#' Coordinated Pareto sampling (illustration)
#' @description
#' Selects \code{Q} coordinated Pareto \eqn{\pi}ps samples (Rosen, 1997)
#' of sizes \code{vctr_n} from the units \code{1:N}. The Pareto number of
#' each unit is computed by \code{\link{generate_random}} from a permanent
#' random number, the size measure \code{xk} and the sample size \code{n}
#' used for the inclusion probabilities. Sample 1 contains the
#' \code{vctr_n[1]} units with the smallest Pareto number (largest for
#' \code{type = "positive"}). Sample \eqn{i} contains the
#' \code{vctr_n[i]} units with the smallest (Pareto number \eqn{+ a_i})
#' \eqn{\bmod 1}, with \eqn{a_i = (i - 1)/Q} for \code{type = "negative"}
#' and \eqn{a_i = 0} for \code{type = "positive"}.
#'
#' This function is illustrative: it works on simulated units \code{1:N}
#' and is useful to teach how coordination by shifting a common permanent
#' random number works. To coordinate samples on a real PSU frame see
#' \code{\link{select_coordinated_PSUs}} and \code{\link{overlap_sample}}.
#' @return
#' A data frame sorted by \code{unit} with the columns \code{unit},
#' \code{xi_pareto} (Pareto number), \code{s1} and, for
#' \eqn{i = 2, \ldots, Q}, \code{xi_pareto<i>} (shifted number) and
#' \code{s<i>} (1 if the unit is in sample \eqn{i}, 0 otherwise).
#' @author Jose Fernando Zea Castro <jfzeac at unal.edu.co>, Hugo Andres
#'   Gutierrez Rojas <andres.gutierrez at cepal.org>, Stalyn Yasid Guerrero
#'   Gomez <syguerrerog at unal.edu.co>
#' @param type \code{"negative"} (default) or \code{"positive"}.
#' @param Q Number of samples (Q >= 2). Default 2.
#' @param N Population size. Default 10.
#' @param vctr_n Numeric vector of length \code{Q} with the size of each
#'   sample.
#' @param xk Numeric vector of length \code{N} with the size measure of each
#'   unit.
#' @param n Sample size used to compute the inclusion probabilities
#'   \eqn{\pi_k} proportional to \code{xk}.
#' @param seed Seed for the permanent random number. Default 12345.
#'
#' @seealso \code{\link{generate_random}}, \code{\link{select_coordinated_PSUs}}
#' @examples
#' pareto_coord(type = "negative",
#'              Q = 3, N = 16, vctr_n = c(3, 3, 3),
#'              xk = c(170, 180, 198, 173, 184, 179, 170, 190, 162, 159, 166,
#'                     190, 220, 240, 150, 270),
#'              n = 3, seed = 1234567)
#'
#' pareto_coord(type = "positive", Q = 2, N = 10, vctr_n = c(3, 3),
#'              xk = c(198, 173, 184, 179, 170, 190, 162, 159, 166, 190),
#'              n = 3, seed = 12345)

pareto_coord <-
  function(type = "negative",
           Q = 2,
           N = 10,
           vctr_n,
           xk,
           n,
           seed = 12345) {
    xi_pareto <-
      generate_random(N = N,
                      seed = seed,
                      xk,
                      Pareto = TRUE,
                      n)$Xi_Pareto
    
    
    if (type == "negative") {
      vctr_a <- rep(NA_real_, Q)
      vctr_a[1] <- 0
      
      for (i in 1:Q) {
        vctr_a[i] <- vctr_a[1] + (i - 1) / Q
      }
      
      df <-
        data.frame(unit = 1:N, xi_pareto) %>% arrange(xi_pareto) %>%
        mutate(s1 = c(rep(1, vctr_n[1]),  rep(0, N - vctr_n[1])))
      
      for (i in 2:Q) {
        df[[paste0("xi_pareto", i)]] <- (df[["xi_pareto"]] + vctr_a[i]) %% 1
        df <- df[order(df[[paste0("xi_pareto", i)]]),]
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
      
      df <-
        data.frame(unit = 1:N, xi_pareto) %>% arrange(desc(xi_pareto)) %>%
        mutate(s1 = c(rep(1, vctr_n[1]),  rep(0, N - vctr_n[1])))
      
      for (i in 2:Q) {
        df[[paste0("xi_pareto", i)]] <- (df[["xi_pareto"]] + vctr_a[i]) %% 1
        df <- df[order(df[[paste0("xi_pareto", i)]]),]
        df[[paste0("s", i)]] <-
          c(rep(1, vctr_n[i]), rep(0, N - vctr_n[i]))
      }
    }
    
    df <- df %>% arrange(unit)
    return(df)
  }
#' @export
#' @import tidyverse
#' @import TeachingSampling
#'
#' @title
#' Generate a correlated Poisson process
#' @description
#' This function generates a correlated Poisson process based on the given parameters.
#' @return A data frame with columns:
#' \item{unit}{An integer vector representing the unit ID.}
#' \item{xi_pps}{A numeric vector representing the generated random numbers.}
#' \item{s1, s2, ..., sQ}{A binary vector representing the points in each quantile.}
#' @author Jose Fernando Zea Castro <jfzeac at unal.edu.co>, Hugo Andres Gutierrez Rojas <andres.gutierrez at cepal.org>, Stalyn Yasid Guerrero Gomez <syguerrerog at unal.edu.co>  
#' @param type A string specifying the type of type to be used: "negative" or "positive". Default is "negative".
#' @param Q An integer specifying the number of quantiles to be used. Default is 2.
#' @param N An integer specifying the number of units. Default is 10.
#' @param vctr_n A numeric vector of length Q specifying the number of points in each quantile.
#' @param xk A numeric scalar specifying the Poisson parameter.
#' @param seed An integer specifying the seed for the random number generator. Default is 12345.
#'
#' @seealso \code{\link{Generate_random}}
#' @examples
#' Poisson_coord(type = "positive", Q = 2, N = 10, vctr_n = c(3,3),
#'                xk = c(198, 173, 184, 179, 170, 190, 162, 159, 166, 190),
#'                               seed = 12345)
#'
#'

Poisson_coord <-
  function(type = "negative",
           Q = 2,
           N = 10,
           vctr_n,
           xk,
           seed = 12345) {
    xi_pps <- Generate_random(N = N, seed = seed,  xk)$Xi_pipt
    
    if (type == "negative") {
      vctr_a <- rep(NA_real_, Q)
      vctr_a[1] <- 0
      
      for (i in 1:Q) {
        vctr_a[i] <- vctr_a[1] + (i - 1) / Q
      }
      
      
      df <- data.frame(unit = 1:N, xi_pps) %>% arrange(xi_pps) %>%
        mutate(s1 = c(rep(1, vctr_n[1]),  rep(0, N - vctr_n[1])))
      
      for (i in 2:Q) {
        df[[paste0("xi_pps", i)]] <- (df[["xi_pps"]] + vctr_a[i]) %% 1
        df <- df[order(df[[paste0("xi_pps", i)]]),]
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
      
      
      df <- data.frame(unit = 1:N, xi_pps) %>% arrange(xi_pps) %>%
        mutate(s1 = c(rep(1, vctr_n[1]),  rep(0, N - vctr_n[1])))
      
      for (i in 2:Q) {
        df[[paste0("xi_pps", i)]] <- (df[["xi_pps"]] + vctr_a[i]) %% 1
        df <- df[order(df[[paste0("xi_pps", i)]]),]
        df[[paste0("s", i)]] <-
          c(rep(1, vctr_n[i]), rep(0, N - vctr_n[i]))
      }
    }
    
    df <- df %>% arrange(unit)
    return(df)
  }

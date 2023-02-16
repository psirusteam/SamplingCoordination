#' @export
#' 
#' @title
#' Coordinated Simple Random Sampling
#' @description 
#' The function f_MAS_coord generates data that follows the Modified Atkinson-Schönberg (MAS) coordinated sampling scheme. It produces a data frame with information about the sample selection from a population of size N using Q categories. The coordination can be either negative or positive. The function uses the genera_alea function to generate the random numbers.
#' @return 
#' The function returns a data frame with the following columns:
#' \itemize{
#' \item unit: the unit identifier, from 1 to N
#' \item xi_P: the random number generated from the genera_alea function
#' \item s1 to sQ: binary variables indicating whether the unit is selected or not, for each category Q.
#' }
#' @author Hugo Andres Gutierrez Rojas <andres.gutierrez at cepal.org>, Jose Fernando Zea Castro <jfzeac at unal.edu.co>, Stalyn Yasid Guerrero Gomez <syguerrerog at unal.edu.co>  
#' @param coordination a character string, either "negative" or "positive", indicating the type of coordination (default is "negativa")
#' @param Q an integer indicating the number of categories (default is 2)
#' @param N an integer indicating the population size (default is 10)
#' @param vctr_n a vector containing the number of units to be selected from each category
#' @param seed an integer value that sets the seed for the random number generator (default is 12345)
#'
#' @seealso \code{\link{genera_alea}}
#' @examples 
#' f_MAS_coord(coordination = "negative",
#'                               Q = 3, N = 100, vctr_n = c(10, 20, 12),
#'                               seed = 12345)
#' 
#'                                                             
f_MAS_coord <- function(coordination = "negative", Q = 2, N = 10, vctr_n, 
                        seed = 12345){
  
  xi_P <- genera_alea(N = N, seed = seed)$Xi_Perman
  
  
  if(coordination == "negative"){
    vctr_a <- rep(NA_real_, Q)
    vctr_a[1] <- 0
    
    for(i in 1:Q){
      vctr_a[i] <- vctr_a[1] + (i-1) / Q
    }
    
    df <- data.frame(unit = 1:N, xi_P) %>% arrange(xi_P) %>%
      mutate(s1 = c(rep(1, vctr_n[1]),  rep(0, N - vctr_n[1])))
    
    for(i in 2:Q){
      df[[paste0("xi_P", i)]] <- (df[["xi_P"]] + vctr_a[i]) %% 1
      df <- df[order(df[[paste0("xi_P", i)]]), ]
      df[[paste0("s", i)]] <- c(rep(1, vctr_n[i]), rep(0, N - vctr_n[i]))
    }
    
    
  }
  
  
  
  if(coordination == "positive"){
    vctr_a <- rep(NA_real_, Q)
    vctr_a[1] <- 0
    
    for(i in 1:Q){
      vctr_a[i] <- vctr_a[1] + 0
    }
    
  df <- data.frame(unit = 1:N, xi_P) %>% arrange(xi_P) %>%
      mutate(s1 = c(rep(1, vctr_n[1]),  rep(0, N - vctr_n[1])))
    
    for(i in 2:Q){
      df[[paste0("xi_P", i)]] <- (df[["xi_P"]] + vctr_a[i]) %% 1
      df <- df[order(df[[paste0("xi_P", i)]]), ]
      df[[paste0("s", i)]] <- c(rep(1, vctr_n[i]), rep(0, N - vctr_n[i]))
    }
    
    
    
  }
  
  df <- df %>% arrange(unit)
  return(df)
}

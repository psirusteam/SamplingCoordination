#' @export
#' 
#' @title
#' Generate a correlated Pareto process
#' @description 
#' This function generates a data frame with Pareto distributed random points. The points are ordered and assigned to Q clusters using a given coordination method.
#' @return A data frame with the generated points and their corresponding clusters.
#' @author Hugo Andres Gutierrez Rojas <andres.gutierrez at cepal.org>, Jose Fernando Zea Castro <jfzeac at unal.edu.co>, Stalyn Yasid Guerrero Gomez <syguerrerog at unal.edu.co>  
#' @param coordination A character string specifying the coordination method. The default is "negative".
#' @param Q An integer specifying the number of clusters. The default is 2.
#' @param N An integer specifying the number of points to generate. The default is 10.
#' @param vctr_n A numeric vector specifying the number of points in each cluster. The default is NULL.
#' @param xk A numeric vector containing the parameters of the Pareto distribution. The default is NULL.
#' @param n An integer representing the Pareto shape parameter. The default is NULL.
#' @param seed An integer specifying the seed to use for the random number generator. The default is 12345.
#'

#' @seealso \code{\link{genera_alea}}
#' @examples
#' f_Pareto_coord(coordination = "negative",
#'  Q = 3, N = 16, vctr_n = c(3,3, 3), 
#'  xk = c(170, 180, 198, 173, 184, 179, 170, 190, 162, 159, 166,
#'   190, 220, 240, 150, 270), n = 3, seed = 1234567)
#'   f_Pareto_coord(coordination = "positive", Q = 2, N = 10, vctr_n = c(3,3), 
#'   xk = c(198, 173, 184, 179, 170, 190, 162, 159, 166, 190),
#'   n = 3, seed = 12345)


f_Pareto_coord <- function(coordination = "negative", Q = 2, N = 10, vctr_n, 
                           xk, n,
                           seed = 12345){
  
  
  xi_pareto <- genera_alea(N = N, seed = seed,  xk, pereto_method = TRUE, n)$Xi_Pareto
  
  
  if(coordination == "negative"){
    vctr_a <- rep(NA_real_, Q)
    vctr_a[1] <- 0
    
    for(i in 1:Q){
      vctr_a[i] <- vctr_a[1] + (i-1) / Q
    }
    
    
    
    df <- data.frame(unit = 1:N, xi_pareto) %>% arrange(xi_pareto) %>%
      mutate(s1 = c(rep(1, vctr_n[1]),  rep(0, N - vctr_n[1])))
    
    for(i in 2:Q){
      df[[paste0("xi_pareto", i)]] <- (df[["xi_pareto"]] + vctr_a[i]) %% 1
      df <- df[order(df[[paste0("xi_pareto", i)]]), ]
      df[[paste0("s", i)]] <- c(rep(1, vctr_n[i]), rep(0, N - vctr_n[i]))
    }
    
    
  }
  
  
  
  if(coordination == "positive"){
    vctr_a <- rep(NA_real_, Q)
    vctr_a[1] <- 0
    
    for(i in 1:Q){
      vctr_a[i] <- vctr_a[1] + 0
    }
    
    
  
    df <- data.frame(unit = 1:N, xi_pareto) %>% arrange(desc(xi_pareto)) %>%
      mutate(s1 = c(rep(1, vctr_n[1]),  rep(0, N - vctr_n[1])))
    
    for(i in 2:Q){
      df[[paste0("xi_pareto", i)]] <- (df[["xi_pareto"]] + vctr_a[i]) %% 1
      df <- df[order(df[[paste0("xi_pareto", i)]]), ]
      df[[paste0("s", i)]] <- c(rep(1, vctr_n[i]), rep(0, N - vctr_n[i]))
    }
    
    
    
  }
  
  df <- df %>% arrange(unit)
 return(df)
}

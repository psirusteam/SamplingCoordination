#' @import dplyr
#' @export

#' @title
#' Generation of random samples.
#' @description 
#' This function generates random numbers using two different methods: Permutation 
#' and "colocated" random numbers. Additionally, it can generate Pareto and PPS 
#' random numbers if the corresponding parameters are specified.
#' @return 
#' The random number generation returns the permanent and placed random numbers.
#' @author Hugo Andres Gutierrez Rojas <andres.gutierrez@cepal.org>, 
#' Jose Fernando Zea Castro <jfzeac@unal.edu.co>,
#' Stalyn Yasid Guerrero Gomez <syguerrerog@unal.edu.co> 
#' @param N: number of random numbers to generate
#' @param semilla: seed for the random number generation
#' @param xk: vector of weights for PPS or Pareto distributions. If NULL, the 
#' function will not generate PPS or Pareto random numbers.
#' @param indica_pareto: a boolean indicating whether to generate Pareto random 
#' numbers (TRUE) or not (FALSE)
#' @param n: a parameter for the Pareto distribution. If NULL, the function will
#'  not generate Pareto random numbers.
#' @examples 
#' #We want 5 random numbers:
#' genera_alea(N = 5, semilla = 12345)
#' # In case there is an auxiliary variable, the program returns the Pareto random numbers:
#' genera_alea(N = 5, semilla = 12345, xk = c(50, 40, 70, 30, 90))
#' genera_alea(N = 5, semilla = 12345, xk = c(50, 40, 70, 30, 90), indica_pareto = T, n = 3)

genera_alea <- function(N, semilla, xk = NULL, indica_pareto = F, n = NULL){
  
  set.seed(semilla)  
  Xi_Perman <- runif(N)  
  
  set.seed(semilla)  
  epsilon <-  runif(1)
  
  Xi_Coloc <- (rank(Xi_Perman) - epsilon) / N
  
  salida <- list(Xi_Perman, Xi_Coloc)
  names(salida) <- c("Xi_Perman", "Xi_Coloc")
  
  if(isTRUE(indica_pareto) & (!is.null(xk) & is.null(n)) | (is.null(xk) & !is.null(n))) stop("Ingrese n y el vector xk")
  
  if(!is.null(xk)){
    if(length(xk) != N) stop("Enter a vector xk of the same length as N")
    
    # Pipt no depende de n
    pk = xk / sum(xk)
    Xi_ppt <-  Xi_Perman / (N * pk)
    
    if(isTRUE(indica_pareto)){
      
      # Pareto (si depende de n)
      pi_k <- TeachingSampling::PikPPS(n, x = xk)
      Xi_Pareto <- (Xi_Perman / (1 - Xi_Perman)) / (pi_k / (1 - pi_k))  
      salida <- list(Xi_Perman, Xi_Coloc, Xi_Pareto)
      names(salida) <- c("Xi_Perman", "Xi_Coloc", "Xi_Pareto")
      
    } else { # # Pipt no depende de n
      salida <- list(Xi_Perman, Xi_Coloc, Xi_ppt)
      names(salida) <- c("Xi_Perman", "Xi_Coloc", "Xi_pipt")
    }
  }
  
  return(salida)
}
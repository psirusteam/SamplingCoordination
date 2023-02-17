#' @export
#' @importFrom stats runif
#' @importFrom dplyr arrange
#' @importFrom dplyr mutate
#' @importFrom magrittr %>%
#' @importFrom dplyr desc
#' 
#' @title
#' Generation of random samples.
#' @description 
#' This function generates random numbers using two different methods: Permutation and "colocated" random numbers. Additionally, it can generate Pareto and PPS random numbers if the corresponding parameters are specified.
#' @return 
#' The random number generation returns the permanent and placed random numbers.
#' @author Jose Fernando Zea Castro <jfzeac at unal.edu.co>, Hugo Andres Gutierrez Rojas <andres.gutierrez at cepal.org>, Stalyn Yasid Guerrero Gomez <syguerrerog at unal.edu.co>  
#' @param N number of random numbers to generate
#' @param seed seed for the random number generation
#' @param xk vector of weights for PPS or Pareto distributions. If NULL, the function will not generate PPS or Pareto random numbers.
#' @param Pareto a boolean indicating whether to generate Pareto random numbers (TRUE) or not (FALSE)
#' @param n a parameter for the Pareto distribution. If NULL, the function will not generate Pareto random numbers.
#' 
#' @examples 
#' #We want 5 random numbers:
#' Generate_random(N = 5, seed = 12345)
#' # In case there is an auxiliary variable, the program returns the Pareto random numbers:
#' Generate_random(N = 5, seed = 12345, xk = c(50, 40, 70, 30, 90))
#' Generate_random(N = 5, seed = 12345, xk = c(50, 40, 70, 30, 90), Pareto = TRUE, n = 3)

Generate_random <-
  function(N,
           seed,
           xk = NULL,
           Pareto = FALSE,
           n = NULL) {
    set.seed(seed)
    Xi_Perman <- runif(N)
    
    set.seed(seed)
    epsilon <-  runif(1)
    
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
      
      
      pk = xk / sum(xk)
      Xi_ppt <-  Xi_Perman / (N * pk)
      
      if (isTRUE(Pareto)) {
        pi_k <- TeachingSampling::PikPPS(n, x = xk)
        Xi_Pareto <-
          (Xi_Perman / (1 - Xi_Perman)) / (pi_k / (1 - pi_k))
        salida <- list(Xi_Perman, Xi_Coloc, Xi_Pareto)
        names(salida) <- c("Xi_Perman", "Xi_Coloc", "Xi_Pareto")
        
      } else {
        salida <- list(Xi_Perman, Xi_Coloc, Xi_ppt)
        names(salida) <- c("Xi_Perman", "Xi_Coloc", "Xi_pipt")
      }
    }
    
    return(salida)
  }

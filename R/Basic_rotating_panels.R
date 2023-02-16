#' @export
#' 
#' @title Basic Rotating Panels
#' @description A function that generates a data frame with the configuration of rotating panels.
#' @author Hugo Andres Gutierrez Rojas <andres.gutierrez at cepal.org>, Jose Fernando Zea Castro <jfzeac at unal.edu.co>, Stalyn Yasid Guerrero Gomez <syguerrerog at unal.edu.co>  
#' @return A data frame with the configuration of rotating panels.
#' @param A Integer value for the length of the active period
#' @param B Integer value for the length of the inactive period
#' @param C Integer value for the number of repetitions
#' @examples
#' Basic_rotating_panels(A = 4, B = 8, C = 4) 
#' Basic_rotating_panels(A = 2, B = 2, C = 2) 
#' Basic_rotating_panels(A = 5, B = 0, C = 0)  
#' Basic_rotating_panels(A = 4, B = 0, C = 0) 
#' Basic_rotating_panels(A = 2, B = 0, C = 0)  
 

Basic_rotating_panels <- function(A, B, C) {
  
  if (!is.numeric(A) || !is.numeric(B) || !is.numeric(C)) {
    stop("A, B, and C must be numeric")
  }
  
  length_period <- 2 * (A + B) 
  number_panels <- 1 + (B / A) 
  
  if (B == 0) {
    length_period <- A + B
    number_panels <- 1
  }
  
  basic_cycle <- rep(1:number_panels, rep(A, number_panels))
  basic_cycle_rep <- rep(basic_cycle, 2)
  
  if (B == 0) {
    basic_cycle_rep <- basic_cycle
  }
  
  num_columnas <- A
  
  lista <- vector(mode = "list", length = num_columnas)
  lista[[1]] <- basic_cycle_rep
  for (i in 1:(A - 1)) {
    lista[[i + 1]] <-
      c(dplyr::lead(basic_cycle_rep, i)[-(length_period:(length_period - (i -
                                                                             1)))],
        rep(number_panels + 1, i))
  }
  
  df <- as.data.frame(do.call(cbind, lista))
  return(df)
}

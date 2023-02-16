#' @importFrom dplyr bind_rows
#' @export
#' 
#' @title 
#' Rotating Panels
#' @description 
#' Generate a sequence of rotating panels according to the input parameters.
#' @return a data frame with \code{period} rows and a column for each panel
#' @author Hugo Andres Gutierrez Rojas <andres.gutierrez at cepal.org>, Jose Fernando Zea Castro <jfzeac at unal.edu.co>, Stalyn Yasid Guerrero Gomez <syguerrerog at unal.edu.co>  
#' @param A An integer indicating the length of the first panel.
#' @param B An integer indicating the length of the subsequent panels.
#' @param C An integer indicating the number of repetitions of the panels.
#' @param period An integer indicating the total number of periods required (>= 0).
#' @param value_initial A character indicating the initial value to be used in the sequence.
#'
#' @examples
#' Rotating_panels(5, 0, 0, 40)
#' Rotating_panels(5, 0, 0, 40, "A")

Rotating_panels <- function(A, B, C, period,
                            value_initial = "A") {
  
  if (!is.numeric(A) || !is.numeric(B) || !is.numeric(C)) {
    stop("A, B, and C must be numeric")
  }
 
 if (!is.character(value_initial) || nchar(value_initial) != 1 || !grepl("[A-Z]", value_initial)) {
    stop("value_initial must be a character string of length 1 that corresponds to a letter in the English alphabet")
  }
  
  Basic_panels <-  Basic_rotating_panels(A, B, C)
  num_basic_period <- nrow(Basic_panels)
  
  if (period < num_basic_period) {
    result_panels <- Basic_panels[1:period, ]
  }
  
  if (period == num_basic_period) {
    result_panels <- Basic_rotating_panels(A, B, C)
  }
  
  if ((period >  num_basic_period) &
      (period %% num_basic_period) == 0) {
    repetition <- period / num_basic_period
    
    Basic_panels <- Basic_rotating_panels(A, B, C)
    result_panels <- vector(mode = "list", length = repetition)
    result_panels[[1]] <- Basic_panels
    
    for (i in 1:(repetition - 1)) {
      valorSum <- max(result_panels[[i]][, 1])
      result_panels[[i + 1]] <- Basic_panels + valorSum
    }
    result_panels <- bind_rows(result_panels)
    
  }
  
  if ((period >  num_basic_period) &
      (period %% num_basic_period) > 0) {
    repetition <- ceiling(period / num_basic_period)
    
    Basic_panels <- Basic_rotating_panels(A, B, C)
    result_panels <- vector(mode = "list", length = repetition)
    result_panels[[1]] <- Basic_panels
    
    for (i in 1:(repetition - 1)) {
      valorSum <- max(result_panels[[i]][, 1])
      result_panels[[i + 1]] <- Basic_panels + valorSum
    }
    result_panels <- bind_rows(result_panels)
    result_panels <- result_panels[1:period, ]
  }
  
  
  posicion_value_initial <- which(LETTERS == value_initial)
  letras <- LETTERS[-(1:(posicion_value_initial - 1))]
  if (value_initial == "A")
    letras <- LETTERS
  
  names(result_panels) <- letras[1:ncol(result_panels)]
  
  for (i in 1:nrow(result_panels)) {
    for (j in 1:ncol(result_panels)) {
      result_panels[i, j] <- paste0(colnames(result_panels)[j],
                                      result_panels[i, j])
    }
  }
  
  return(result_panels) 
}


#' @importFrom dplyr bind_rows
#' @export
#' 
#' @title 
#' Rotating Panels
#' @description 
#' Generate a sequence of rotating panels according to the input parameters.
#' This function supports rotating panel schemes such as 4(0)0 (scheme 400) and
#' 5(0)0 (scheme 500), where \code{A} defines the number of consecutive periods
#' a unit is observed, \code{B} the number of periods it rests, and \code{C} the
#' number of times the observation-rest cycle repeats. For example, scheme 500
#' is obtained with \code{A = 5, B = 0, C = 0}, and scheme 400 with
#' \code{A = 4, B = 0, C = 0}.
#' 
#' @return a data frame with \code{period} rows and a column for each panel
#' 
#' @author Jose Fernando Zea Castro <jfzeac at unal.edu.co>,
#'   Hugo Andres Gutierrez Rojas <andres.gutierrez at cepal.org>,
#'   Stalyn Yasid Guerrero Gomez <syguerrerog at unal.edu.co>,
#'   Yury Vanessa Ochoa Montes <yury.ochoa at urosario.edu.co>
#'   
#' @param A An integer indicating the number of consecutive periods a unit is observed.
#'   For scheme 500 use \code{A = 5}; for scheme 400 use \code{A = 4}.
#' @param B An integer indicating the number of periods a unit rests between observation cycles.
#'   For schemes 400 and 500 use \code{B = 0}.
#' @param C An integer indicating the number of times the observation-rest cycle repeats.
#'   For schemes 400 and 500 use \code{C = 0}.
#' @param period An integer indicating the total number of periods required (>= 0).
#' @param value_initial A character indicating the initial value to be used in the sequence.
#'
#' @examples
#' # Scheme 500: units observed 5 consecutive periods, no rest
#' rotating_panels(5, 0, 0, 40)
#' rotating_panels(5, 0, 0, 40, "A")
#' # Scheme 400: units observed 4 consecutive periods, no rest
#' rotating_panels(4, 0, 0, 40)
rotating_panels <- function(A, B, C, period,
                            value_initial = "A") {
  
  if (!is.numeric(A) || !is.numeric(B) || !is.numeric(C)) {
    stop("A, B, and C must be numeric")
  }
  
  if (!is.character(value_initial) || nchar(value_initial) != 1 || !grepl("[A-Z]", value_initial)) {
    stop("value_initial must be a character string of length 1 that corresponds to a letter in the English alphabet")
  }
  
  Basic_panels <-  basic_rotating_panels(A, B, C)
  num_basic_period <- nrow(Basic_panels)
  
  if (period < num_basic_period) {
    result_panels <- Basic_panels[1:period, ]
  }
  
  if (period == num_basic_period) {
    result_panels <- basic_rotating_panels(A, B, C)
  }
  
  if ((period >  num_basic_period) &
      (period %% num_basic_period) == 0) {
    repetition <- period / num_basic_period
    
    Basic_panels <- basic_rotating_panels(A, B, C)
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
    
    Basic_panels <- basic_rotating_panels(A, B, C)
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
#' @export
#' 
#' @title
#' Sample size calculation for each panel
#' @description
#' This function calculates the sample size of each panel given the sample size of the PSU stratum and the number of panels per PSU.
#' @return A vector with the following elements:
#' \describe{
#' \item{Num_large_panels}{Number of panels in large groups.}
#' \item{Num_normal_panels}{Number of panels in normal groups.}
#' \item{Num_panels}{Total number of panels.}
#' \item{Num_PSU_large_panels}{Number of PSUs in large groups.}
#' \item{Num_PSU_normal_panels}{Number of PSUs in normal groups.}
#' }
#' @author Jose Fernando Zea Castro <jfzeac at unal.edu.co>, Hugo Andres Gutierrez Rojas <andres.gutierrez at cepal.org>, Stalyn Yasid Guerrero Gomez <syguerrerog at unal.edu.co>   
#' @param sample_size_PSU_stratum Sample size of the PSU stratum.
#' @param Num_panels_period Number of panels per PSU.
#'
#' @examples
#' Sample_size_panels(sample_size_PSU_stratum = 400, Num_panels_period = 4)
#'
#' @export
#' 
Sample_size_panels <-
  function(sample_size_PSU_stratum,
           Num_panels_period) {
    Num_PSU_normal_panels <-
      floor((sample_size_PSU_stratum / Num_panels_period))
    num_large_groups <-
      sample_size_PSU_stratum - (Num_PSU_normal_panels * Num_panels_period)
    
    resultado <-
      c(
        num_large_groups,
        Num_panels_period - num_large_groups,
        Num_panels_period,
        Num_PSU_normal_panels + 1,
        Num_PSU_normal_panels
      )
    names(resultado) <-
      c(
        "Num_large_panels",
        "Num_normal_panels",
        "Num_panels",
        "Num_PSU_large_panels",
        "Num_PSU_normal_panels"
      )
    resultado
  }

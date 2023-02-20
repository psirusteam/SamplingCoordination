#' @export
#' 
#' @title
#' Function to calculate the number of PSUs in each panel
#' @description This function calculates the number of panels that must be included in each PSU (Primary Sampling Unit) in order to perform a stratified sampling.
#' @return A named numeric vector with the following elements:
#' \describe{
#'   \item{Num_large_panels}{The number of large panels.}
#'   \item{Num_normal_panels}{The number of normal panels.}
#'   \item{Num_panels}{The total number of panels.}
#'   \item{Num_PSU_large_panels}{The number of PSUs for the large panels.}
#'   \item{Num_PSU_normal_panels}{The number of PSUs for the normal panels.}
#'   }
#' @author Jose Fernando Zea Castro <jfzeac at unal.edu.co>, Hugo Andres Gutierrez Rojas <andres.gutierrez at cepal.org>, Stalyn Yasid Guerrero Gomez <syguerrerog at unal.edu.co>  
#' @param num_PSU_strata Numeric vector with the number of strata per PSU.
#' @param total_num_panels Numeric scalar with the total number of panels.
#' 
#' @examples
#' Redistribution_PSU(num_PSU_strata = 50, total_num_panels = 10)
#' 
Redistribution_PSU <-
  function(num_PSU_strata, total_num_panels) {
    PSU_num_normal_panels <-
      floor((num_PSU_strata / total_num_panels))
    num_large_groups <-
      num_PSU_strata - (PSU_num_normal_panels * total_num_panels)
    prue <-  num_large_groups * (PSU_num_normal_panels + 1) +
      (total_num_panels - num_large_groups) * PSU_num_normal_panels
    result <-
      c(
        num_large_groups,
        total_num_panels - num_large_groups,
        total_num_panels,
        PSU_num_normal_panels + 1,
        PSU_num_normal_panels
      )
    names(result) <-
      c(
        "Num_large_panels",
        "Num_normal_panels",
        "Num_panels",
        "Num_PSU_large_panels",
        "Num_PSU_normal_panels"
      )
    result
  }

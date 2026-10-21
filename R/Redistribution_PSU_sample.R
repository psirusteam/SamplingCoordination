#' @export
#'
#' @title
#' Distribute the PSU sample of a stratum across the panels of a period
#' @description
#' Splits the sample size of a stratum among the panels visited in one period
#' so that panel sample sizes differ by at most one PSU. It is the same
#' computation as \code{\link{redistribution_PSU}}, applied to the sample
#' size instead of the population count, and is used by
#' \code{\link{panel_sampling}}.
#'
#' @return A named numeric vector with \code{Num_large_panels},
#'   \code{Num_normal_panels}, \code{Num_panels}, \code{Num_PSU_large_panels}
#'   and \code{Num_PSU_normal_panels} (see \code{\link{redistribution_PSU}}).
#' @author Jose Fernando Zea Castro <jfzeac at unal.edu.co>, Hugo Andres
#'   Gutierrez Rojas <andres.gutierrez at cepal.org>, Stalyn Yasid Guerrero
#'   Gomez <syguerrerog at unal.edu.co>
#' @param sample_size_PSU_stratum Number of PSUs to select in the stratum.
#' @param Num_panels_period Number of panels visited in each period.
#' @seealso \code{\link{redistribution_PSU}}, \code{\link{panel_sampling}}
#'
#' @examples
#' # 109 PSUs in a period with 15 panels: 4 panels of 8 and 11 panels of 7
#' redistribution_PSU_sample(sample_size_PSU_stratum = 109,
#'                           Num_panels_period = 15)
redistribution_PSU_sample <-
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
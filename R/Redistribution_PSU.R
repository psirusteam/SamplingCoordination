#' @export
#'
#' @title
#' Distribute a number of PSUs across panels as evenly as possible
#' @description
#' Splits \code{num_PSU_strata} PSUs into \code{total_num_panels} panels so
#' that panel sizes differ by at most one PSU. If the division is not exact,
#' the remainder \eqn{r = N - K \lfloor N/K \rfloor} goes to \eqn{r} "large"
#' panels with \eqn{\lfloor N/K \rfloor + 1} PSUs, and the other \eqn{K - r}
#' "normal" panels receive \eqn{\lfloor N/K \rfloor} PSUs.
#'
#' It is used by \code{\link{panel_sampling}} to distribute the PSUs of a
#' stratum in the population across all panels. The same computation applied
#' to the sample size is \code{\link{redistribution_PSU_sample}}.
#'
#' @return A named numeric vector with:
#' \describe{
#'   \item{Num_large_panels}{Number of panels that receive one extra PSU.}
#'   \item{Num_normal_panels}{Number of panels with the base size.}
#'   \item{Num_panels}{Total number of panels.}
#'   \item{Num_PSU_large_panels}{PSUs in each large panel.}
#'   \item{Num_PSU_normal_panels}{PSUs in each normal panel.}
#' }
#' @author Jose Fernando Zea Castro <jfzeac at unal.edu.co>, Hugo Andres
#'   Gutierrez Rojas <andres.gutierrez at cepal.org>, Stalyn Yasid Guerrero
#'   Gomez <syguerrerog at unal.edu.co>
#' @param num_PSU_strata Number of PSUs to distribute (e.g. the PSUs of a
#'   stratum in the population).
#' @param total_num_panels Number of panels.
#' @seealso \code{\link{redistribution_PSU_sample}}, \code{\link{panel_sampling}}
#'
#' @examples
#' # 53 PSUs in 10 panels: 3 panels with 6 PSUs and 7 panels with 5 PSUs
#' redistribution_PSU(num_PSU_strata = 53, total_num_panels = 10)
redistribution_PSU <-
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
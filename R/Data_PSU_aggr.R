#' @title
#' PSU counts and sample sizes by stratum
#' @description
#' Population count of PSUs and sample size for each of the 14 strata of
#' \code{\link{Data_PSU}}. \code{N_PSU} counts original PSUs; since every PSU
#' appears in \code{Data_PSU} split into two parts, a stratum has
#' \code{2 * N_PSU} rows there. In total there are 6,710 PSUs and a sample of
#' 915 PSUs.
#' @format
#' A data frame with 14 rows and 4 columns:
#' \describe{
#'   \item{straum_id}{Stratum identifier (character), as in \code{Data_PSU}.}
#'   \item{straum_name}{Name of the stratum.}
#'   \item{N_PSU}{Number of PSUs in the population.}
#'   \item{n_PSU}{Number of PSUs to select in the sample.}
#' }
#' @seealso \code{\link{Data_PSU}}
#' @examples
#' data(Data_PSU_aggr)
#' Data_PSU_aggr
#' colSums(Data_PSU_aggr[, c("N_PSU", "n_PSU")])
"Data_PSU_aggr"

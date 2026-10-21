#' @title
#' Example PSU frame
#' @description
#' Example frame of primary sampling units (PSUs) distributed in 14 strata.
#' Each of the 6,710 original PSUs is split into two parts, identified by the
#' suffixes \code{_1} and \code{_2} of \code{PSU_id} (for instance
#' \code{"1_1"} and \code{"1_2"}), so the frame has 13,420 rows. Both parts of
#' a PSU always belong to the same stratum. \code{\link{Data_PSU_aggr}} gives,
#' for each stratum, the number of original PSUs (\code{N_PSU}) and the
#' sample size; therefore the number of rows of a stratum in
#' \code{Data_PSU} is \code{2 * N_PSU}.
#' @format
#' A data frame with 13,420 rows and 2 columns:
#' \describe{
#'   \item{straum_id}{Stratum identifier (character).}
#'   \item{PSU_id}{Identifier of the PSU part, \code{"<PSU>_<part>"}.}
#' }
#' @seealso \code{\link{Data_PSU_aggr}}
#' @examples
#' data(Data_PSU)
#' head(Data_PSU)
#' # Rows per stratum (two parts per PSU)
#' table(Data_PSU$straum_id)
"Data_PSU"

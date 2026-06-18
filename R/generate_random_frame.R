#' @export
#' @importFrom stats runif
#' @importFrom dplyr group_by mutate ungroup select
#' @importFrom magrittr %>%
#' @importFrom rlang enquo quo_name
#'
#' @title
#' Generation of permanent random numbers for sample coordination (data frame interface).
#'
#' @description
#' Generates random numbers for coordinated sampling and appends them as
#' columns to the PSU frame. Unlike \code{\link{generate_random}}, this function
#' works directly with a data frame containing PSU identifiers, strata, and size
#' variables. The type of random number computed depends on the \code{method} argument:
#' \itemize{
#'   \item \strong{MAS}: only the permanent random number \eqn{\xi_i^P}.
#'   \item \strong{Coloc}: permanent and colocated random numbers.
#'   \item \strong{Pareto}: permanent random number, inclusion probability
#'     \eqn{\pi_i = n\_sample \times p_i}, and Pareto score
#'     \eqn{\xi_i^{par} = (\xi_i^P / (1 - \xi_i^P)) / (\pi_i / (1 - \pi_i))}.
#'     Requires \code{size_var} and \code{n_sample}. Note that \eqn{\pi_i < 1} must
#'     hold for all units.
#'   \item \strong{Poisson}: permanent random number and Poisson sequential
#'     score \eqn{\xi_i^{pps} = \xi_i^P / (N_h \times p_i)},
#'     where \eqn{p_i = x_k / \sum_h x_k}. Requires \code{size_var} only.
#' }
#' When \code{strata} is supplied, \eqn{p_i}, \eqn{\pi_i}, and the coordination
#' scores are computed \strong{within each stratum} independently, so that
#' \eqn{\sum_{i \in h} p_i = 1} for each stratum \eqn{h}.
#'
#' If \code{permanent_random} is provided, it is used as the permanent random
#' number \eqn{\xi_i^P} instead of generating a new one internally.
#' The permanent random number is always returned in the output frame as
#' \code{Xi_Perman}, regardless of whether it was generated internally or
#' supplied by the user.
#'
#' @return
#' The input \code{data} frame with additional columns appended:
#' \describe{
#'   \item{\code{Xi_Perman}}{Permanent random number \eqn{\xi_i^P \sim U(0,1)}.
#'     Always returned, whether generated internally or supplied via
#'     \code{permanent_random}.}
#'   \item{\code{Xi_Coloc}}{Colocated random number. Returned for
#'     \code{method = "Coloc"}.}
#'   \item{\code{p_i}}{Size-proportional weight \eqn{p_i = x_k / \sum_h x_k},
#'     computed within each stratum when \code{strata} is provided. Returned for
#'     \code{method = "Pareto"} or \code{"Poisson"}.}
#'   \item{\code{pi_i}}{Inclusion probability \eqn{\pi_i = n\_sample \times p_i},
#'     computed within each stratum when \code{strata} is provided. Returned for
#'     \code{method = "Pareto"} only.}
#'   \item{\code{Xi_Pareto}}{Pareto coordination score
#'     \eqn{\xi_i^{par} = (\xi_i^P/(1-\xi_i^P)) / (\pi_i/(1-\pi_i))}.
#'     Returned for \code{method = "Pareto"}.}
#'   \item{\code{Xi_Poisson}}{Poisson sequential score
#'     \eqn{\xi_i^{pps} = \xi_i^P / (N_h \times p_i)}, where \eqn{N_h} is
#'     the number of PSUs in the stratum. Returned for
#'     \code{method = "Poisson"}.}
#' }
#'
#' @note
#' To ensure unique permanent random numbers across strata when iterating
#' with a loop, set the seed once outside the loop and let the internal
#' \code{runif} calls continue the sequence:
#' \preformatted{
#' set.seed(12345)
#' resultados <- list()
#' for (est in unique(frame$strata)) {
#'   df <- frame[frame$strata == est, ]
#'   resultados[[est]] <- generate_random_frame(
#'     data     = df,
#'     id_psu   = psu,
#'     method   = "Pareto",
#'     size_var = dwellings,
#'     n_sample = 3
#'   )
#' }
#' dplyr::bind_rows(resultados)
#' }
#'
#' @author Hugo Andres Gutierrez Rojas <andres.gutierrez at cepal.org>,
#'   Yury Vanessa Ochoa Montes <yury.ochoa at urosario.edu.co>
#'
#' @param data A \code{data.frame} or \code{tibble} containing the PSU frame.
#' @param id_psu Unquoted name of the PSU identifier column in \code{data}.
#' @param method Character string indicating which random number to compute.
#'   One of \code{"MAS"}, \code{"Coloc"}, \code{"Pareto"}, or
#'   \code{"Poisson"}. Default is \code{"MAS"}.
#' @param size_var Unquoted name of the numeric column in \code{data} with the
#'   PSU size measure (e.g., number of dwellings or households). Required
#'   when \code{method = "Pareto"} or \code{method = "Poisson"}.
#' @param n_sample Integer. Number of PSUs to be selected. Required when
#'   \code{method = "Pareto"} to compute \eqn{\pi_i = n\_sample \times p_i}.
#'   Must satisfy \eqn{n\_sample \times p_i < 1} for all units.
#' @param strata Unquoted name of the stratum variable in \code{data}. When
#'   supplied, \eqn{p_i} and all derived scores are computed independently
#'   within each stratum. If \code{NULL} (default), computations are performed
#'   over the full frame.
#' @param permanent_random Optional numeric vector of length \code{nrow(data)}
#'   with pre-existing permanent random numbers in the open interval \eqn{(0,1)}.
#'   When supplied, these values are used as \eqn{\xi_i^P} instead of generating
#'   new ones internally. This enables sample coordination across surveys
#'   that share the same permanent random numbers (e.g., a \code{nap} column
#'   from a previous survey frame). The values are always returned in the output
#'   as \code{Xi_Perman}.
#'
#' @seealso \code{\link{generate_random}} for a simpler vector-based interface.
#'
#' @examples
#' frame <- data.frame(
#'   psu       = paste0("PSU", formatC(1:20, width = 2, flag = "0")),
#'   strata    = rep(c("A", "B"), each = 10),
#'   dwellings = c(50, 80, 120, 90, 110, 70, 95, 130, 60, 85,
#'                 75, 100, 55, 140, 88, 92, 65, 115, 78, 105)
#' )
#'
#' # MAS
#' set.seed(12345)
#' generate_random_frame(data = frame, id_psu = psu)
#'
#' # Colocated
#' set.seed(12345)
#' generate_random_frame(data = frame, id_psu = psu, method = "Coloc")
#'
#' # Poisson by stratum
#' set.seed(12345)
#' generate_random_frame(data = frame, id_psu = psu,
#'                       method = "Poisson", size_var = dwellings, strata = strata)
#'
#' # Pareto by stratum
#' set.seed(12345)
#' generate_random_frame(data = frame, id_psu = psu,
#'                       method = "Pareto", size_var = dwellings,
#'                       n_sample = 3, strata = strata)
#'
#' # Pareto using pre-existing permanent random numbers
#' frame$nap <- runif(20)
#' generate_random_frame(data = frame, id_psu = psu,
#'                       method = "Pareto", size_var = dwellings,
#'                       n_sample = 3, strata = strata,
#'                       permanent_random = frame$nap)
generate_random_frame <- function(data,
                                  id_psu,
                                  method           = "MAS",
                                  size_var         = NULL,
                                  n_sample         = NULL,
                                  strata           = NULL,
                                  permanent_random = NULL) {
  
  # -- 0. Input validation ---------------------------------------------------
  if (!is.data.frame(data))
    stop("`data` must be a data.frame or tibble.", call. = FALSE)
  
  id_psu_nm <- quo_name(enquo(id_psu))
  if (!id_psu_nm %in% names(data))
    stop("PSU identifier '", id_psu_nm, "' not found in `data`.",
         call. = FALSE)
  
  method <- match.arg(method, choices = c("MAS", "Coloc", "Pareto", "Poisson"))
  
  size_var_expr <- substitute(size_var)
  strata_expr   <- substitute(strata)
  
  if (method %in% c("Pareto", "Poisson") && is.null(size_var_expr))
    stop("`size_var` is required when method = '", method, "'.",
         call. = FALSE)
  
  if (method == "Pareto") {
    if (is.null(n_sample))
      stop("`n_sample` is required when method = 'Pareto'.", call. = FALSE)
    if (!is.numeric(n_sample) || length(n_sample) != 1 || n_sample < 1)
      stop("`n_sample` must be a single positive integer.", call. = FALSE)
  }
  
  if (!is.null(strata_expr)) {
    strata_nm <- quo_name(enquo(strata))
    if (!strata_nm %in% names(data))
      stop("Stratum variable '", strata_nm, "' not found in `data`.",
           call. = FALSE)
  }
  
  N <- nrow(data)
  
  # -- 1. Permanent random number --------------------------------------------
  if (!is.null(permanent_random)) {
    if (!is.numeric(permanent_random) || length(permanent_random) != N)
      stop("`permanent_random` must be a numeric vector of length nrow(data).",
           call. = FALSE)
    if (any(permanent_random <= 0 | permanent_random >= 1, na.rm = TRUE))
      stop("`permanent_random` values must be strictly in (0, 1).",
           call. = FALSE)
    Xi_Perman <- permanent_random
  } else {
    Xi_Perman <- runif(N)
  }
  data$Xi_Perman <- Xi_Perman
  
  # -- 2. Colocated ----------------------------------------------------------
  if (method == "Coloc") {
    epsilon       <- runif(1)
    data$Xi_Coloc <- (base::rank(Xi_Perman) - epsilon) / N
  }
  
  # -- 3. Pareto or Poisson --------------------------------------------------
  if (method %in% c("Pareto", "Poisson")) {
    
    size_vec <- eval(size_var_expr, envir = data)
    
    if (!is.numeric(size_vec))
      stop("`size_var` must be a numeric column.", call. = FALSE)
    if (any(size_vec <= 0, na.rm = TRUE))
      stop("`size_var` must contain positive values only.", call. = FALSE)
    
    data$.size_var <- size_vec
    
    if (!is.null(strata_expr)) {
      strata_nm <- quo_name(enquo(strata))
      data <- data %>%
        group_by(.data[[strata_nm]]) %>%
        mutate(
          p_i  = .size_var / sum(.size_var, na.rm = TRUE),
          .N_h = dplyr::n()
        ) %>%
        ungroup()
    } else {
      data <- data %>%
        mutate(
          p_i  = .size_var / sum(.size_var, na.rm = TRUE),
          .N_h = N
        )
    }
    
    if (method == "Pareto") {
      data <- data %>%
        mutate(
          pi_i      = round(TeachingSampling::PikPPS(n = n_sample, x = .size_var), 5),
          Xi_Pareto = (Xi_Perman / (1 - Xi_Perman)) / (pi_i / (1 - pi_i))
        ) %>%
        select(-p_i)
      if (any(data$pi_i >= 1, na.rm = TRUE))
        warning("Some units have pi_i >= 1. Consider reducing n_sample or ",
                "reviewing size_var values.", call. = FALSE)
    }
    
    if (method == "Poisson") {
      data <- data %>%
        mutate(Xi_Poisson = Xi_Perman / (.N_h * p_i))
    }
    
    data$.size_var <- NULL
    data$.N_h      <- NULL
  }
  
  return(data)
}
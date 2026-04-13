#' @export
#' @importFrom stats runif rank
#' @importFrom dplyr group_by mutate ungroup
#' @importFrom magrittr %>%
#' @importFrom rlang enquo quo_name
#'
#' @title
#' Generation of permanent random numbers for sample coordination.
#'
#' @description
#' Generates random numbers for coordinated sampling and appends them as
#' columns to the PSU frame. The type of random number computed depends on
#' the `method` argument:
#' \itemize{
#'   \item \strong{MAS}: only the permanent random number \eqn{\xi_i^P}.
#'   \item \strong{Coloc}: permanent and colocated random numbers.
#'   \item \strong{Pareto}: permanent random number, inclusion probability
#'     \eqn{\pi_i = n\_sample \times p_i}, and Pareto score
#'     \eqn{\xi_i^{par} = (\xi_i^P / (1 - \xi_i^P)) / (\pi_i / (1 - \pi_i))}.
#'     Requires `size_var` and `n_sample`. Note that \eqn{\pi_i < 1} must
#'     hold for all units.
#'   \item \strong{Poisson}: permanent random number and Poisson sequential
#'     score \eqn{\xi_i^{pps} = \xi_i^P / (N_h \times p_i)},
#'     where \eqn{p_i = x_k / \sum_h x_k}. Requires `size_var` only.
#' }
#' When `strata` is supplied, \eqn{p_i}, \eqn{\pi_i}, and the coordination
#' scores are computed \strong{within each stratum} independently, so that
#' \eqn{\sum_{i \in h} p_i = 1} for each stratum \eqn{h}.
#'
#' @return
#' The input `data` frame with additional columns appended:
#' \describe{
#'   \item{`Xi_Perman`}{Permanent random number \eqn{\xi_i^P \sim U(0,1)}.
#'     Always returned.}
#'   \item{`Xi_Coloc`}{Colocated random number. Returned for
#'     \code{method = "Coloc"}.}
#'   \item{`p_i`}{Size-proportional weight \eqn{p_i = x_k / \sum_h x_k},
#'     computed within each stratum when `strata` is provided. Returned for
#'     \code{method = "Pareto"} or \code{"Poisson"}.}
#'   \item{`pi_i`}{Inclusion probability \eqn{\pi_i = n\_sample \times p_i},
#'     computed within each stratum when `strata` is provided. Returned for
#'     \code{method = "Pareto"} only.}
#'   \item{`Xi_Pareto`}{Pareto coordination score
#'     \eqn{\xi_i^{par} = (\xi_i^P/(1-\xi_i^P)) / (\pi_i/(1-\pi_i))}.
#'     Returned for \code{method = "Pareto"}.}
#'   \item{`Xi_Poisson`}{Poisson sequential score
#'     \eqn{\xi_i^{pps} = \xi_i^P / (N_h \times p_i)}, where \eqn{N_h} is
#'     the number of PSUs in the stratum. Returned for
#'     \code{method = "Poisson"}.}
#' }
#'
#' @author Hugo Andres Gutierrez Rojas <andres.gutierrez at cepal.org>,
#'   Yury Vanessa Ochoa Montes <yury.ochoa at urosario.edu.co>
#'
#' @param data A `data.frame` or `tibble` containing the PSU frame.
#' @param id_psu Unquoted name of the PSU identifier column in `data`.
#' @param seed Integer seed for reproducible random number generation.
#' @param method Character string indicating which random number to compute.
#'   One of \code{"MAS"}, \code{"Coloc"}, \code{"Pareto"}, or
#'   \code{"Poisson"}. Default is \code{"MAS"}.
#' @param size_var Unquoted name of the numeric column in `data` with the
#'   PSU size measure (e.g., number of dwellings or households). Required
#'   when \code{method = "Pareto"} or \code{method = "Poisson"}.
#' @param n_sample Integer. Number of PSUs to be selected. Required when
#'   \code{method = "Pareto"} to compute \eqn{\pi_i = n\_sample \times p_i}.
#'   Must satisfy \eqn{n\_sample \times p_i < 1} for all units.
#' @param strata Unquoted name of the stratum variable in `data`. When
#'   supplied, \eqn{p_i} and all derived scores are computed independently
#'   within each stratum. If `NULL` (default), computations are performed
#'   over the full frame.
#'
#' @examples
#' library(tidyverse)
#' set.seed(1)
#' frame <- tibble(
#'   psu       = paste0("PSU", str_pad(1:20, 2, pad = "0")),
#'   strata    = rep(c("A", "B"), each = 10),
#'   dwellings = sample(50:150, 20, replace = TRUE)
#' )
#'
#' # MAS: only permanent random number
#' Generate_random_2(data = frame, id_psu = psu, seed = 12345, method = "MAS")
#'
#' # Colocated random number
#' Generate_random_2(data = frame, id_psu = psu, seed = 12345, method = "Coloc")
#'
#' # Poisson by stratum
#' Generate_random_2(data = frame, id_psu = psu, seed = 12345,
#'                 method = "Poisson", size_var = dwellings, strata = strata)
#'
#' # Pareto by stratum (requires n_sample)
#' Generate_random_2(data = frame, id_psu = psu, seed = 12345,
#'                 method = "Pareto", size_var = dwellings,
#'                 n_sample = 3, strata = strata)

Generate_random_2 <- function(data,
                            id_psu,
                            seed,
                            method   = "MAS",
                            size_var = NULL,
                            n_sample = NULL,
                            strata   = NULL) {
  
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
  
  # -- 1. Permanent random number (always) -----------------------------------
  set.seed(seed)
  Xi_Perman      <- runif(N)
  data$Xi_Perman <- Xi_Perman
  
  # -- 2. Colocated ----------------------------------------------------------
  if (method == "Coloc") {
    set.seed(seed)
    epsilon       <- runif(1)
    data$Xi_Coloc <- (rank(Xi_Perman) - epsilon) / N
  }
  
  # -- 3. Pareto or Poisson --------------------------------------------------
  if (method %in% c("Pareto", "Poisson")) {
    
    size_vec <- eval(size_var_expr, envir = data)
    
    if (!is.numeric(size_vec))
      stop("`size_var` must be a numeric column.", call. = FALSE)
    if (any(size_vec <= 0, na.rm = TRUE))
      stop("`size_var` must contain positive values only.", call. = FALSE)
    
    data$.size_var <- size_vec
    
    # -- compute p_i, pi_i, scores within stratum or globally ---------------
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
          pi_i      = n_sample * p_i,
          Xi_Pareto = (Xi_Perman / (1 - Xi_Perman)) / (pi_i / (1 - pi_i))
        )
      if (any(data$pi_i >= 1, na.rm = TRUE))
        warning("Some units have pi_i >= 1. Consider reducing n_sample or ",
                "reviewing size_var values.", call. = FALSE)
    }
    
    if (method == "Poisson") {
      data <- data %>%
        mutate(
          Xi_Poisson = Xi_Perman / (.N_h * p_i)
        )
    }
    
    # remove internal helper columns
    data$.size_var <- NULL
    data$.N_h      <- NULL
  }
  
  return(data)
}
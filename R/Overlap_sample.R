#' @export
#' @importFrom dplyr left_join mutate bind_rows if_else
#' @importFrom rlang enquo quo_name sym
#' @importFrom magrittr %>%
#'
#' @title
#' Overlapped Sample Selection Between Two Consecutive Periods
#'
#' @description
#' Selects a coordinated sample between two consecutive periods guaranteeing
#' a desired overlap rate. Within each stratum, PSUs from the previous period
#' are first retained (up to the overlap target), then the remaining PSUs are
#' selected from those not previously sampled. Both groups are ordered by
#' `sort_var` (the permanent random number or coordination score already
#' computed by [generate_random_frame()]) — no new random numbers are generated inside
#' this function.
#'
#' @details
#' Within each stratum \eqn{h} the target number of retained PSUs is
#' \eqn{n_m = round(n_h \times overlap)} (R's \code{round()}, which rounds
#' halves to the even number, e.g. \code{round(2.5) = 2}), capped at the
#' number of PSUs in the previous sample. The remaining \eqn{n_u = n_h - n_m}
#' PSUs are new. If there are not enough new PSUs, more previous PSUs are
#' retained to complete \eqn{n_h}. If the stratum cannot reach \eqn{n_h} at
#' all, every available PSU is selected and a warning is issued.
#'
#' \code{NA} values in `prev_sample` are treated as \code{0} (not in the
#' previous sample). PSUs whose stratum has no row in `n_h` are kept in the
#' output with \code{in_sample = 0}.
#'
#' @return
#' The input `psu_frame` (same rows, same order) with additional columns:
#' \describe{
#'   \item{`n_h`}{Required sample size of the stratum (\code{NA} if the
#'     stratum is not in `n_h`).}
#'   \item{`n_m`}{Number of PSUs retained from the previous period in the stratum.}
#'   \item{`n_u`}{Number of new PSUs selected in the stratum.}
#'   \item{`in_sample`}{Integer (1/0): 1 if the PSU is selected in the current
#'     period, 0 otherwise.}
#'   \item{`type`}{Character: \code{"retained"} for PSUs kept from the previous
#'     period, \code{"new"} for newly selected PSUs, \code{NA} for non-selected.}
#' }
#'
#' @param psu_frame A `data.frame` or `tibble` containing the PSU frame with
#'   the coordination scores already computed (output of [generate_random_frame()]).
#' @param strata Unquoted name of the stratum variable in `psu_frame`.
#' @param id_psu Unquoted name of the PSU identifier variable in `psu_frame`.
#' @param n_h A `data.frame` or `tibble` with two columns: the stratum variable
#'   (same name as in `psu_frame`) and `n_h` with the sample size per stratum.
#' @param overlap Numeric value between 0 and 1 indicating the desired overlap
#'   proportion (e.g., `0.38` = 38\%).
#' @param prev_sample Unquoted name of the dichotomous variable (1/0) in
#'   `psu_frame` indicating whether the PSU was selected in the previous period.
#' @param method Character string: one of \code{"MAS"}, \code{"Pareto"}, or
#'   \code{"Poisson"}. Used only for the summary message — the actual selection
#'   in all cases orders by `sort_var` ascending and takes the first units.
#' @param sort_var Unquoted name of the coordination score column in
#'   `psu_frame` (e.g., `Xi_Poisson`, `Xi_Pareto`, `Xi_Perman`).
#'
#'
#' @examples
#' library(dplyr)
#' data(Data_PSU)
#' set.seed(2026)
#'
#' # Two strata of Data_PSU with a permanent random number
#' frame <- Data_PSU %>%
#'   filter(straum_id %in% c("111", "321")) %>%
#'   generate_random_frame(id_psu = PSU_id, strata = straum_id)
#'
#' # Previous period: 20 PSUs per stratum, selected with the permanent
#' # number shifted by 0.5
#' frame <- frame %>%
#'   group_by(straum_id) %>%
#'   mutate(prev = as.integer(rank((Xi_Perman + 0.5) %% 1) <= 20)) %>%
#'   ungroup()
#'
#' # Current period: 20 PSUs per stratum keeping 75% of the previous sample
#' result <- overlap_sample(
#'   psu_frame   = frame,
#'   strata      = straum_id,
#'   id_psu      = PSU_id,
#'   n_h         = data.frame(straum_id = c("111", "321"), n_h = c(20, 20)),
#'   overlap     = 0.75,
#'   prev_sample = prev,
#'   sort_var    = Xi_Perman
#' )
#'
#' # 15 retained and 5 new PSUs per stratum
#' result %>%
#'   filter(in_sample == 1) %>%
#'   count(straum_id, type)
overlap_sample <- function(psu_frame,
                           strata,
                           id_psu,
                           n_h,
                           overlap,
                           prev_sample,
                           method   = "MAS",
                           sort_var) {
  
  # ── 0. Capture NSE arguments ──────────────────────────────────────────────
  strata_nm   <- quo_name(enquo(strata))
  id_psu_nm   <- quo_name(enquo(id_psu))
  ant_nm      <- quo_name(enquo(prev_sample))
  sort_var_nm <- quo_name(enquo(sort_var))
  
  # ── 1. Input validation ───────────────────────────────────────────────────
  if (!is.data.frame(psu_frame))
    stop("`psu_frame` must be a data.frame or tibble.", call. = FALSE)
  
  if (!is.data.frame(n_h))
    stop("`n_h` must be a data.frame or tibble with columns '",
         strata_nm, "' and 'n_h'.", call. = FALSE)
  
  if (!strata_nm %in% names(psu_frame))
    stop("Stratum variable '", strata_nm, "' not found in `psu_frame`.",
         call. = FALSE)
  
  if (!id_psu_nm %in% names(psu_frame))
    stop("PSU id variable '", id_psu_nm, "' not found in `psu_frame`.",
         call. = FALSE)
  
  if (!ant_nm %in% names(psu_frame))
    stop("Previous sample variable '", ant_nm, "' not found in `psu_frame`.",
         call. = FALSE)
  
  if (!all(unique(psu_frame[[ant_nm]]) %in% c(0L, 1L, 0, 1, NA)))
    stop("`", ant_nm, "` must be a dichotomous variable with values 0 and 1.",
         call. = FALSE)
  
  if (!is.numeric(overlap) || length(overlap) != 1 || overlap < 0 || overlap > 1)
    stop("`overlap` must be a single numeric value between 0 and 1.",
         call. = FALSE)
  
  method <- match.arg(method, choices = c("MAS", "Pareto", "Poisson"))
  
  if (sort_var_nm == "NULL")
    stop("`sort_var` is required",
         call. = FALSE)
  
  if (!sort_var_nm %in% names(psu_frame))
    stop("Sort variable '", sort_var_nm, "' not found in `psu_frame`.",
         call. = FALSE)
  
  if (!strata_nm %in% names(n_h) || !"n_h" %in% names(n_h))
    stop("`n_h` must have columns '", strata_nm, "' and 'n_h'.",
         call. = FALSE)
  
  if (anyDuplicated(n_h[[strata_nm]]) > 0)
    stop("`n_h` must have one row per stratum.", call. = FALSE)

  if ("n_h" %in% names(psu_frame))
    stop("`psu_frame` already has a column named 'n_h'; rename it first.",
         call. = FALSE)

  # ── 2. Prepare frame: keep row order, NA in prev_sample -> 0 ─────────────
  psu_frame$.row_id <- seq_len(nrow(psu_frame))
  psu_frame[[ant_nm]][is.na(psu_frame[[ant_nm]])] <- 0L

  psu_frame <- psu_frame %>%
    left_join(n_h[, c(strata_nm, "n_h")], by = strata_nm)

  no_size <- is.na(psu_frame$n_h)
  if (any(no_size))
    warning(sum(no_size), " PSU(s) belong to strata with no match in `n_h`; ",
            "they are returned with in_sample = 0.", call. = FALSE)

  # ── 3. Process stratum by stratum ─────────────────────────────────────────
  strata_list <- unique(psu_frame[[strata_nm]][!no_size])
  strata_list <- strata_list[!is.na(strata_list)]
  shortfall   <- character(0)

  results <- lapply(strata_list, function(h) {

    sub <- psu_frame[!no_size & !is.na(psu_frame[[strata_nm]]) &
                       psu_frame[[strata_nm]] == h, , drop = FALSE]
    nh  <- sub$n_h[1]

    # Split into previous sample and new candidates (prev_sample has no NA)
    in_prev <- sub[[ant_nm]] == 1
    prev    <- sub[in_prev,  , drop = FALSE]
    pool    <- sub[!in_prev, , drop = FALSE]

    # Number to retain and number of new PSUs
    n_m <- min(round(nh * overlap), nrow(prev))
    n_u <- nh - n_m

    # Not enough new PSUs: retain more previous PSUs to complete n_h
    if (n_u > nrow(pool)) {
      n_u <- nrow(pool)
      n_m <- min(nh - n_u, nrow(prev))
    }

    if (n_m + n_u < nh)
      shortfall <<- c(shortfall, paste0(h, " (", n_m + n_u, "/", nh, ")"))

    # ── 3a. Retain from previous sample: order by sort_var, take first n_m ──
    retained_ids <- prev[[id_psu_nm]][order(prev[[sort_var_nm]])][seq_len(n_m)]

    # ── 3b. Select new PSUs: order by sort_var, take first n_u ───────────────
    new_ids <- pool[[id_psu_nm]][order(pool[[sort_var_nm]])][seq_len(n_u)]

    # ── 3c. Tag each PSU: in_sample = 1/0, type = retained/new/NA ───────────
    sub %>%
      mutate(
        n_m       = n_m,
        n_u       = n_u,
        in_sample = if_else(
          !!sym(id_psu_nm) %in% c(retained_ids, new_ids), 1L, 0L
        ),
        type = if_else(
          !!sym(id_psu_nm) %in% retained_ids, "retained",
          if_else(!!sym(id_psu_nm) %in% new_ids, "new", NA_character_)
        )
      )
  })

  # ── 4. Bind strata results + PSUs without sample size, restore order ──────
  unmatched <- psu_frame[no_size | is.na(psu_frame[[strata_nm]]), , drop = FALSE]
  if (nrow(unmatched) > 0) {
    unmatched$n_m       <- NA_real_
    unmatched$n_u       <- NA_real_
    unmatched$in_sample <- 0L
    unmatched$type      <- NA_character_
  }

  output <- bind_rows(c(results, list(unmatched)))
  output <- output[order(output$.row_id), , drop = FALSE]
  output$.row_id <- NULL
  rownames(output) <- NULL

  if (length(shortfall) > 0)
    warning("Not enough PSUs to reach n_h in ", length(shortfall),
            " stratum/strata (selected/required): ",
            paste(shortfall, collapse = ", "), call. = FALSE)

  # ── 5. Summary message ────────────────────────────────────────────────────
  total_sel      <- sum(output$in_sample,          na.rm = TRUE)
  total_retained <- sum(output$type == "retained", na.rm = TRUE)
  total_new      <- sum(output$type == "new",      na.rm = TRUE)
  real_overlap   <- if (total_sel > 0) {
    round(total_retained / total_sel * 100, 1)
  } else {
    0
  }

  message(
    "=== overlap_sample summary ===\n",
    "  Total PSUs selected : ", total_sel,      "\n",
    "  Retained (overlap)  : ", total_retained, "\n",
    "  New PSUs            : ", total_new,      "\n",
    "  Realized overlap    : ", real_overlap,   "%\n",
    "  Method              : ", method
  )

  return(output)
}

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
#' computed by [Generate_random_2()]) — no new random numbers are generated inside
#' this function.
#'
#' @return
#' The input `psu_frame` with additional columns:
#' \describe{
#'   \item{`n_m`}{Number of PSUs retained from the previous period in the stratum.}
#'   \item{`n_u`}{Number of new PSUs selected in the stratum.}
#'   \item{`in_sample`}{Integer (1/0): 1 if the PSU is selected in the current
#'     period, 0 otherwise.}
#'   \item{`type`}{Character: \code{"retained"} for PSUs kept from the previous
#'     period, \code{"new"} for newly selected PSUs, \code{NA} for non-selected.}
#' }
#'
#' @param psu_frame A `data.frame` or `tibble` containing the PSU frame with
#'   the coordination scores already computed (output of [Generate_random_2()]).
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
#' \dontrun{
#' # Step 1: generate coordination scores
#' frame <- Generate_random_2(
#'   data     = psu_frame,
#'   id_psu   = cod_estab,
#'   seed     = 2025,
#'   method   = "Poisson",
#'   size_var = n_students,
#'   strata   = strata
#' )
#'
#' # Step 2: select overlapped sample
#' result <- Overlap_sample(
#'   psu_frame   = frame,
#'   strata      = strata,
#'   id_psu      = cod_estab,
#'   n_h         = n_h_df,
#'   overlap     = 0.38,
#'   prev_sample = in_sample_2025,
#'   method      = "Poisson",
#'   sort_var    = Xi_Poisson
#' )
#'
#' # PSUs selected in current period
#' result %>% filter(in_sample == 1L)
#' }

Overlap_sample <- function(psu_frame,
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
  
  # ── 2. Merge sample sizes into the frame ──────────────────────────────────
  psu_frame <- psu_frame %>%
    left_join(n_h, by = strata_nm)
  
  if (any(is.na(psu_frame$n_h)))
    warning("Some strata in `psu_frame` have no match in `n_h`. ",
            "Those PSUs will be excluded from selection.", call. = FALSE)
  
  # ── 3. Process stratum by stratum ─────────────────────────────────────────
  strata_list <- unique(psu_frame[[strata_nm]])
  strata_list <- strata_list[!is.na(strata_list)]
  
  results <- lapply(strata_list, function(h) {
    
    sub  <- psu_frame[psu_frame[[strata_nm]] == h & !is.na(psu_frame$n_h), ]
    nh   <- unique(sub$n_h)
    
    # Split into previous sample and new candidates
    prev <- sub[sub[[ant_nm]] == 1, ]
    pool <- sub[sub[[ant_nm]] == 0, ]
    # Number to retain and number of new PSUs
    n_m <- min(floor(nh * overlap), nrow(prev))
    n_u <- nh - n_m
    
    if (n_u > nrow(pool)) {
      n_u <- nrow(pool)
      n_m <- nh - n_u
    }
    
    # ── 3a. Retain from previous sample: order by sort_var, take first n_m ──
    retained_ids <- character(0)
    if (n_m > 0 && nrow(prev) > 0) {
      prev_ord     <- prev[order(prev[[sort_var_nm]]), ]
      retained_ids <- prev_ord[[id_psu_nm]][seq_len(n_m)]
    }   
    
    # ── 3b. Select new PSUs: order by sort_var, take first n_u ───────────────
    new_ids <- character(0)
    if (n_u > 0 && nrow(pool) > 0) {
      pool_ord <- pool[order(pool[[sort_var_nm]]), ]
      n_sel    <- min(n_u, nrow(pool_ord))
      new_ids  <- pool_ord[[id_psu_nm]][seq_len(n_sel)]
      
    } else if (n_u > 0 && nrow(pool) == 0) {
      warning("Stratum '", h, "': no PSUs available for new selection. ",
              "Overlap may be lower than requested.", call. = FALSE)
    }
    
    # ── 3c. Tag each PSU: in_sample = 1/0, type = retained/new/NA ───────────
    sub <- sub %>%
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
    sub
  })
  
  # ── 4. Bind all strata results ────────────────────────────────────────────
  output <- bind_rows(results)
  
  # ── 5. Summary message ────────────────────────────────────────────────────
  total_sel      <- sum(output$in_sample,            na.rm = TRUE)
  total_retained <- sum(output$type == "retained",   na.rm = TRUE)
  total_new      <- sum(output$type == "new",        na.rm = TRUE)
  real_overlap   <- if (total_sel > 0) {
    round(total_retained / total_sel * 100, 1)
  } else {
    0
  }
  
  message(
    "=== Overlap_sample summary ===\n",
    "  Total PSUs selected : ", total_sel,      "\n",
    "  Retained (overlap)  : ", total_retained, "\n",
    "  New PSUs            : ", total_new,      "\n",
    "  Realized overlap    : ", real_overlap,   "%\n",
    "  Method              : ", method
  )
  
  return(output)
}

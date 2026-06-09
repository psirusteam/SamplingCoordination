#' @export
#' @importFrom dplyr count arrange mutate distinct filter select left_join bind_rows across starts_with group_by ungroup coalesce
#'
#' @title
#' Sample size allocation by mini-panel and socioeconomic stratum via IPFP
#'
#' @description
#' Given a PSU frame with socioeconomic strata and a monthly sample allocation
#' table, this function:
#' \enumerate{
#'   \item Distributes each monthly sample (\code{n_month_column}) across
#'     \code{n_panels_column} mini-panels using floor + remainder allocation.
#'     Mini-panel numbering is continuous across months within the quarter
#'     (e.g. month1 \eqn{\to} mp1\ldots mp4, month2 \eqn{\to} mp5\ldots mp8,
#'     month3 \eqn{\to} mp9\ldots mp12 when \code{n_panels_column = 4}).
#'   \item Computes SES proportions from the frame and distributes the
#'     quarterly total proportionally across SES levels.
#'   \item Applies IPFP (\code{TeachingSampling::IPFP}) to allocate PSUs
#'     jointly by mini-panel and SES stratum, respecting both marginals.
#' }
#' Both the continuous (raw) and rounded IPFP solutions are returned.
#'
#' @return
#' A list with two elements:
#' \describe{
#'   \item{\code{ipfp_continuous}}{A \code{data.frame} with the continuous IPFP
#'     allocation. Columns: \code{geo_column}, \code{month_column},
#'     \code{mini_panel}, one column per SES level (\code{SES_<value>}),
#'     \code{total_mp}, \code{total_month}, and \code{total_quarter}.}
#'   \item{\code{ipfp_rounded}}{Same structure with SES allocations rounded
#'     to integers.}
#' }
#' If \code{keep_intermediates = TRUE}, the list also contains:
#' \describe{
#'   \item{\code{panel_sizes}}{Mini-panel sizes per month and geographic stratum.}
#'   \item{\code{ses_proportions}}{SES proportions and quarterly expected totals
#'     per geographic stratum.}
#' }
#'
#' @author Yury Vanessa Ochoa Montes <yury.ochoa at urosario.edu.co>
#'
#' @param frame A \code{data.frame} or \code{tibble} with the PSU frame.
#'   Must contain at least the columns referenced by \code{geo_column} and
#'   \code{ses_column}.
#' @param sample_table A \code{data.frame} or \code{tibble} with one row per
#'   geographic stratum and month. Must contain: \code{geo_column},
#'   \code{month_column} (month identifier), \code{n_month_column} (sample size
#'   for that month), and \code{n_panels_column} (number of mini-panels for that
#'   month).
#' @param geo_column Character. Name of the geographic stratum column in both
#'   \code{frame} and \code{sample_table}. Default \code{"geo_stratum"}.
#' @param ses_column Character. Name of the socioeconomic stratum column in
#'   \code{frame}. Default \code{"ses_stratum"}.
#' @param month_column Character. Name of the month identifier column in
#'   \code{sample_table}. Default \code{"month"}.
#' @param n_month_column Character. Name of the monthly sample size column in
#'   \code{sample_table}. Default \code{"n_month"}.
#' @param n_panels_column Character. Name of the column in \code{sample_table}
#'   with the number of mini-panels per month for each stratum.
#'   Default \code{"n_panels"}.
#' @param keep_intermediates Logical. If \code{TRUE}, returns \code{panel_sizes}
#'   and \code{ses_proportions} in the output list. Default \code{FALSE}.
#'
#' @seealso \code{\link{rotating_panels}}, \code{\link{assign_PSUs_to_panels}},
#'   \code{\link{generate_random_frame}}
#'
#' @examples
#' \dontrun{
#' # sample_table (long format — one row per stratum x month):
#' #   geo_stratum  month    n_month  n_panels
#' #   "01_1"       month1        29         4
#' #   "01_1"       month2        29         4
#' #   "01_1"       month3        30         4
#' #   "01_4"       month1         4         1
#' #   "01_4"       month2         4         1
#' #   "01_4"       month3         5         1
#'
#' result <- panel_sample_size_nse(
#'   frame              = psu_frame,
#'   sample_table       = monthly_allocation,
#'   geo_column         = "geo_stratum",
#'   ses_column         = "ses_stratum",
#'   month_column       = "month",
#'   n_month_column     = "n_month",
#'   n_panels_column    = "n_panels",
#'   keep_intermediates = TRUE
#' )
#'
#' result$ipfp_continuous
#' result$ipfp_rounded
#' result$panel_sizes       # only when keep_intermediates = TRUE
#' result$ses_proportions   # only when keep_intermediates = TRUE
#' }
panel_sample_size_nse <- function(frame,
                                  sample_table,
                                  geo_column         = "geo_stratum",
                                  ses_column         = "ses_stratum",
                                  month_column       = "month",
                                  n_month_column     = "n_month",
                                  n_panels_column    = "n_panels",
                                  keep_intermediates = FALSE) {
  
  # ── 0. Validate ───────────────────────────────────────────────────────────
  stopifnot(
    is.data.frame(frame),
    is.data.frame(sample_table),
    is.character(geo_column),      length(geo_column)      == 1,
    is.character(ses_column),      length(ses_column)      == 1,
    is.character(month_column),    length(month_column)    == 1,
    is.character(n_month_column),  length(n_month_column)  == 1,
    is.character(n_panels_column), length(n_panels_column) == 1,
    is.logical(keep_intermediates)
  )
  
  required_frame  <- c(geo_column, ses_column)
  required_sample <- c(geo_column, month_column, n_month_column, n_panels_column)
  
  missing_frame  <- setdiff(required_frame,  names(frame))
  missing_sample <- setdiff(required_sample, names(sample_table))
  
  if (length(missing_frame) > 0)
    stop("Missing columns in `frame`: ",
         paste(missing_frame, collapse = ", "), call. = FALSE)
  
  if (length(missing_sample) > 0)
    stop("Missing columns in `sample_table`: ",
         paste(missing_sample, collapse = ", "), call. = FALSE)
  
  if (any(sample_table[[n_panels_column]] < 1, na.rm = TRUE))
    stop("`", n_panels_column, "` must be >= 1 for every row.", call. = FALSE)
  
  # ── 1. Stratum-level info ─────────────────────────────────────────────────
  # Keep only rows with valid monthly sample size
  sample_clean <- sample_table %>%
    filter(
      !is.na(.data[[n_month_column]]),
      .data[[n_month_column]] > 0
    )
  
  # Quarterly total per geographic stratum
  stratum_info <- sample_clean %>%
    group_by(.data[[geo_column]]) %>%
    mutate(n_quarter = sum(.data[[n_month_column]], na.rm = TRUE)) %>%
    ungroup() %>%
    select(dplyr::all_of(c(geo_column, n_panels_column, month_column)),
           n_quarter) %>%
    distinct()
  
  geos <- unique(sample_clean[[geo_column]])
  
  # ── 2. Internal helper — distribute monthly n across mini-panels ──────────
  # Mini-panel numbering is continuous across months within the quarter.
  # offset accumulates the panel count of previous months.
  .distribute_panels <- function(n_month, n_mp, offset) {
    n_mp         <- as.integer(n_mp)
    n_mp_base    <- floor(n_month / n_mp)
    remainder    <- n_month %% n_mp
    n_per_mp     <- rep(n_mp_base, n_mp)
    if (remainder > 0)
      n_per_mp[seq_len(remainder)] <- n_per_mp[seq_len(remainder)] + 1L
    data.frame(
      mini_panel  = paste0("mp", seq_len(n_mp) + offset),
      n_mp        = n_per_mp,
      total_month = n_month
    )
  }
  
  # ── 3. Mini-panel sizes per stratum-month ─────────────────────────────────
  # n_panels_column can vary by month within the same stratum
  # (e.g. month1 = 1, month2 = 1, month3 = 2 for a 4-panel scheme),
  # so offset accumulates the actual panel count of each previous month.
  panel_sizes <- bind_rows(lapply(geos, function(geo) {
    rows   <- sample_clean[sample_clean[[geo_column]] == geo, ]
    rows   <- rows[order(rows[[month_column]]), ]
    offset <- 0L
    
    bind_rows(lapply(seq_len(nrow(rows)), function(i) {
      n_mp_i <- as.integer(rows[[n_panels_column]][i])
      out    <- .distribute_panels(rows[[n_month_column]][i], n_mp_i, offset)
      out[[geo_column]]      <- geo
      out[[n_panels_column]] <- n_mp_i
      out[[month_column]]    <- rows[[month_column]][i]
      offset                <<- offset + n_mp_i
      out
    }))
  })) %>%
    select(dplyr::all_of(
      c(geo_column, n_panels_column, month_column,
        "mini_panel", "n_mp", "total_month")
    ))
  
  # ── 4. Internal helper — SES proportions and quarterly expected totals ─────
  .calc_ses_proportions <- function(df_stratum, n_quarter) {
    n_quarter <- as.numeric(n_quarter)
    
    ses_prop <- df_stratum %>%
      count(.data[[ses_column]], name = "n_frame") %>%
      arrange(.data[[ses_column]]) %>%
      mutate(
        prop         = n_frame / sum(n_frame),
        expected_ses = floor(prop * n_quarter)
      )
    
    remainder <- n_quarter - sum(ses_prop$expected_ses)
    if (remainder > 0) {
      ord     <- order(ses_prop$expected_ses == 0, ses_prop$prop,
                       decreasing = TRUE)
      top_ses <- ord[seq_len(remainder)]
      ses_prop$expected_ses[top_ses] <- ses_prop$expected_ses[top_ses] + 1L
    }
    
    stopifnot(sum(ses_prop$expected_ses) == n_quarter)
    ses_prop %>% mutate(n_quarter = n_quarter)
  }
  
  # ── 5. SES proportions per geographic stratum ─────────────────────────────
  ses_proportions <- bind_rows(lapply(geos, function(geo) {
    inf        <- stratum_info[stratum_info[[geo_column]] == geo, ]
    df_stratum <- frame[frame[[geo_column]] == geo, ]
    
    out <- .calc_ses_proportions(df_stratum, inf$n_quarter[1])
    out[[geo_column]]      <- geo
    out[[n_panels_column]] <- inf[[n_panels_column]][1]
    out
  })) %>%
    select(dplyr::all_of(
      c(geo_column, n_panels_column, ses_column,
        "n_frame", "prop", "expected_ses")
    ))
  
  # ── 6. IPFP per geographic stratum ────────────────────────────────────────
  ipfp_results <- lapply(geos, function(geo) {
    mp_geo  <- panel_sizes[panel_sizes[[geo_column]] == geo, ]
    ses_geo <- ses_proportions[ses_proportions[[geo_column]] == geo, ]
    
    if (nrow(ses_geo) == 0 || nrow(mp_geo) == 0) return(NULL)
    
    # Row marginals: one per (month x mini_panel)
    Row.knw <- setNames(
      mp_geo$n_mp,
      paste0(mp_geo[[month_column]], "_", mp_geo$mini_panel)
    )
    
    # Column marginals: one per SES level, rescaled to match row total
    Col.knw <- setNames(
      ses_geo$expected_ses,
      paste0("SES_", ses_geo[[ses_column]])
    )
    Col.knw <- Col.knw * (sum(Row.knw) / sum(Col.knw))
    
    mat <- TeachingSampling::IPFP(
      outer(Row.knw, Col.knw) / sum(Row.knw),
      Col.knw,
      Row.knw
    )[seq_along(Row.knw), seq_along(Col.knw), drop = FALSE]
    
    mat_rounded <- round(mat)
    
    .to_df <- function(m, type) {
      as.data.frame(m) %>%
        mutate(month_mp = rownames(m), !!geo_column := geo, .type = type)
    }
    
    bind_rows(.to_df(mat, "continuous"), .to_df(mat_rounded, "rounded"))
  })
  
  # ── 7. Reshape and assemble final output ──────────────────────────────────
  ipfp_raw <- bind_rows(ipfp_results) %>%
    separate(
      month_mp,
      into = c(month_column, "mini_panel"),
      sep  = "_(?=mp)",
      fill = "right"
    ) %>%
    mutate(
      mini_panel   = coalesce(mini_panel, "mp1"),
      total_mp     = rowSums(across(starts_with("SES_")), na.rm = TRUE)
    ) %>%
    group_by(.data[[geo_column]], .data[[month_column]], .type) %>%
    mutate(total_month = sum(total_mp)) %>%
    ungroup() %>%
    group_by(.data[[geo_column]], .type) %>%
    mutate(total_quarter = sum(total_mp)) %>%
    ungroup() %>%
    left_join(
      stratum_info %>%
        select(dplyr::all_of(c(geo_column, n_panels_column))) %>%
        distinct(),
      by = geo_column
    ) %>%
    select(
      dplyr::all_of(c(geo_column, n_panels_column, month_column)),
      mini_panel,
      starts_with("SES_"),
      total_mp, total_month, total_quarter,
      .type
    )
  
  # ── 8. Build output list ──────────────────────────────────────────────────
  out <- list(
    ipfp_continuous = dplyr::filter(ipfp_raw, .type == "continuous") %>%
      select(-.type),
    ipfp_rounded    = dplyr::filter(ipfp_raw, .type == "rounded") %>%
      select(-.type)
  )
  
  if (keep_intermediates) {
    out$panel_sizes      <- panel_sizes
    out$ses_proportions  <- ses_proportions
  }
  
  out
}
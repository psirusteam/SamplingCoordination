#' @export
#' @importFrom dplyr bind_rows left_join mutate select distinct any_of rename filter group_by summarise first n
#' @importFrom tidyr pivot_wider pivot_longer
#' @importFrom tibble tibble
#'
#' @title
#' Build the final PSU-by-period panel matrix
#'
#' @description
#' Combines the outputs of \code{\link{assign_PSUs_to_panels}} and
#' \code{\link{cyclic_panel_adjustment}} into a single PSU frame, then
#' cross-joins it with the theoretical rotation reference to produce a wide
#' matrix where each row is a PSU and each column is a period
#' (\code{T1_M1}, \code{T1_M2}, \ldots, \code{T<period>_M3}). Cells contain
#' the mini-panel label the PSU belongs to in that period, or \code{"0"} if
#' the PSU is not active in that period.
#'
#' A verification summary of the number of active PSUs per geographic stratum
#' and quarter is always returned alongside the matrix.
#'
#' @return
#' A list with two elements:
#' \describe{
#'   \item{\code{panel_matrix}}{A \code{data.frame} in wide format with columns
#'     \code{geo_column}, \code{ses_column}, \code{psu_column}, and one column
#'     per period (\code{T1_M1} \ldots \code{T<period>_M3}).}
#'   \item{\code{verification}}{A \code{data.frame} with the number of active
#'     PSUs per geographic stratum and quarter (wide format). Useful for
#'     validating that realised sample sizes match the design.}
#' }
#'
#' @author Yury Vanessa Ochoa Montes <yury.ochoa at urosario.edu.co>
#'
#' @param assigned_frame A \code{data.frame} output of
#'   \code{\link{assign_PSUs_to_panels}}. Must contain \code{geo_column},
#'   \code{ses_column}, \code{psu_column}, and \code{panel_column}.
#' @param cyclic_frame A \code{data.frame} output of
#'   \code{\link{cyclic_panel_adjustment}}. Must contain \code{geo_column},
#'   \code{ses_column}, \code{psu_column}, and \code{cyclic_panel_column}.
#' @param rotation_schemes A named list of theoretical rotation matrices, each
#'   being a \code{data.frame} with columns \code{quarter_label},
#'   \code{month_label}, and \code{theoretical_panel} (the mini-panel label
#'   active in that quarter-month combination). Names must match the scheme
#'   labels used in \code{sample_table}. Pass \code{NULL} as the value for any
#'   scheme that has no rotation matrix (e.g. \code{"no_rotation"}).
#'   Example:
#'   \code{list("12_panels" = rotation_12, "4_panels" = rotation_4,
#'   "no_rotation" = NULL)}.
#' @param sample_table A \code{data.frame} with at least \code{geo_column} and
#'   \code{scheme_column}, used to map each geographic stratum to its rotation
#'   scheme.
#' @param period Integer. Total number of quarters in the design (e.g. 48).
#' @param geo_column Character. Name of the geographic stratum column in all
#'   input frames. Default \code{"geo_stratum"}.
#' @param ses_column Character. Name of the socioeconomic stratum column.
#'   Default \code{"ses_stratum"}.
#' @param psu_column Character. Name of the PSU identifier column.
#'   Default \code{"psu_id"}.
#' @param panel_column Character. Name of the assigned panel column in
#'   \code{assigned_frame}. Default \code{"panel"}.
#' @param cyclic_panel_column Character. Name of the cyclic panel column in
#'   \code{cyclic_frame}. Default \code{"panel_cyclic"}.
#' @param scheme_column Character. Name of the rotation scheme column in
#'   \code{sample_table}. Default \code{"scheme"}.
#' @param quarter_label Character. Name of the quarter label column in each
#'   rotation matrix inside \code{rotation_schemes}.
#'   Default \code{"quarter"}.
#' @param month_label Character. Name of the month label column in each
#'   rotation matrix inside \code{rotation_schemes}.
#'   Default \code{"month"}.
#' @param theoretical_panel Character. Name of the theoretical mini-panel
#'   column in each rotation matrix inside \code{rotation_schemes}.
#'   Default \code{"theoretical_panel"}.
#' @param no_rotation_label Character. Scheme name used to identify strata
#'   with no rotation. For these strata synthetic panel labels are generated
#'   automatically (\code{X}, \code{Y}, \code{Z} per quarter).
#'   Default \code{"no_rotation"}.
#'
#' @seealso \code{\link{assign_PSUs_to_panels}},
#'   \code{\link{cyclic_panel_adjustment}},
#'   \code{\link{panel_sample_size_nse}}
#'
#' @examples
#' \dontrun{
#' result <- build_panel_matrix(
#'   assigned_frame      = assigned_psus,
#'   cyclic_frame        = cyclic_psus,
#'   rotation_schemes    = list(
#'     "12_panels"   = rotation_12,
#'     "4_panels"    = rotation_4,
#'     "no_rotation" = NULL
#'   ),
#'   sample_table        = monthly_allocation,
#'   period              = 48,
#'   geo_column          = "geo_stratum",
#'   ses_column          = "ses_stratum",
#'   psu_column          = "psu_id",
#'   panel_column        = "panel",
#'   cyclic_panel_column = "panel_cyclic",
#'   scheme_column       = "scheme",
#'   quarter_label       = "quarter",
#'   month_label         = "month",
#'   theoretical_panel   = "theoretical_panel",
#'   no_rotation_label   = "no_rotation"
#' )
#'
#' result$panel_matrix
#' result$verification
#'
#' # Export
#' writexl::write_xlsx(
#'   result$panel_matrix,
#'   "Output/selected_psus_intercensal.xlsx"
#' )
#' }
build_panel_matrix <- function(assigned_frame,
                               cyclic_frame,
                               rotation_schemes,
                               sample_table,
                               period,
                               geo_column          = "geo_stratum",
                               ses_column          = "ses_stratum",
                               psu_column          = "psu_id",
                               panel_column        = "panel",
                               cyclic_panel_column = "panel_cyclic",
                               scheme_column       = "scheme",
                               quarter_label       = "quarter",
                               month_label         = "month",
                               theoretical_panel   = "theoretical_panel",
                               no_rotation_label   = "no_rotation") {
  
  # ── 0. Validate ───────────────────────────────────────────────────────────
  stopifnot(
    is.data.frame(assigned_frame),
    is.data.frame(cyclic_frame),
    is.list(rotation_schemes),
    !is.null(names(rotation_schemes)),
    is.data.frame(sample_table),
    is.numeric(period), length(period) == 1, period >= 1,
    is.character(geo_column),          length(geo_column)          == 1,
    is.character(ses_column),          length(ses_column)          == 1,
    is.character(psu_column),          length(psu_column)          == 1,
    is.character(panel_column),        length(panel_column)        == 1,
    is.character(cyclic_panel_column), length(cyclic_panel_column) == 1,
    is.character(scheme_column),       length(scheme_column)       == 1,
    is.character(quarter_label),       length(quarter_label)       == 1,
    is.character(month_label),         length(month_label)         == 1,
    is.character(theoretical_panel),   length(theoretical_panel)   == 1,
    is.character(no_rotation_label),   length(no_rotation_label)   == 1
  )
  
  required_assigned <- c(geo_column, ses_column, psu_column, panel_column)
  required_cyclic   <- c(geo_column, ses_column, psu_column, cyclic_panel_column)
  required_sample   <- c(geo_column, scheme_column)
  
  missing_assigned <- setdiff(required_assigned, names(assigned_frame))
  missing_cyclic   <- setdiff(required_cyclic,   names(cyclic_frame))
  missing_sample   <- setdiff(required_sample,   names(sample_table))
  
  if (length(missing_assigned) > 0)
    stop("Missing columns in `assigned_frame`: ",
         paste(missing_assigned, collapse = ", "), call. = FALSE)
  
  if (length(missing_cyclic) > 0)
    stop("Missing columns in `cyclic_frame`: ",
         paste(missing_cyclic, collapse = ", "), call. = FALSE)
  
  if (length(missing_sample) > 0)
    stop("Missing columns in `sample_table`: ",
         paste(missing_sample, collapse = ", "), call. = FALSE)
  
  # ── 1. Combine assigned and cyclic frames ─────────────────────────────────
  combined_frame <- assigned_frame %>%
    select(dplyr::all_of(
      c(geo_column, ses_column, psu_column, panel_column)
    )) %>%
    bind_rows(
      cyclic_frame %>%
        select(dplyr::all_of(
          c(geo_column, ses_column, psu_column, cyclic_panel_column)
        )) %>%
        rename(!!panel_column := !!cyclic_panel_column)
    )
  
  # ── 2. Build rotation reference table ────────────────────────────────────
  # Strata with no rotation scheme get synthetic X/Y/Z labels per quarter
  no_rot_ref <- tibble(
    !!quarter_label     := rep(paste0("Q", seq_len(period)), each = 3),
    !!month_label       := rep(paste0("M", 1:3), times = period),
    !!theoretical_panel := paste0(
      rep(c("X", "Y", "Z"), times = period),
      rep(seq_len(period), each = 3)
    ),
    !!scheme_column     := no_rotation_label
  )
  
  rotation_ref <- bind_rows(
    lapply(names(rotation_schemes), function(nm) {
      rot <- rotation_schemes[[nm]]
      if (is.null(rot)) return(NULL)
      rot %>%
        mutate(!!scheme_column := nm) %>%
        select(dplyr::all_of(
          c(scheme_column, quarter_label, month_label, theoretical_panel)
        ))
    }),
    no_rot_ref
  )
  
  # ── 3. Ordered period labels ──────────────────────────────────────────────
  period_order <- paste0("T", rep(seq_len(period), each = 3), "_M", 1:3)
  
  # ── 4. Join scheme, rotation reference, and build period column ───────────
  scheme_map <- sample_table %>%
    select(dplyr::all_of(c(geo_column, scheme_column))) %>%
    distinct()
  
  panel_matrix <- combined_frame %>%
    left_join(scheme_map, by = geo_column) %>%
    left_join(
      rotation_ref,
      by           = c(setNames(theoretical_panel, panel_column),
                       scheme_column),
      relationship = "many-to-many"
    ) %>%
    mutate(
      .period = paste0(
        "T", gsub("\\D+", "", .data[[quarter_label]]),
        "_M", gsub("\\D+", "", .data[[month_label]])
      )
    ) %>%
    select(dplyr::all_of(c(geo_column, ses_column, psu_column)),
           .period, !!panel_column) %>%
    distinct() %>%
    pivot_wider(
      names_from  = .period,
      values_from = !!panel_column,
      values_fn   = first,
      values_fill = "0"
    ) %>%
    select(dplyr::all_of(c(geo_column, ses_column, psu_column)),
           any_of(period_order))
  
  # ── 5. Verification summary ───────────────────────────────────────────────
  verification <- panel_matrix %>%
    pivot_longer(
      cols      = any_of(period_order),
      names_to  = ".period",
      values_to = ".panel"
    ) %>%
    filter(.panel != "0") %>%
    mutate(.quarter = gsub("_M\\d+", "", .period)) %>%
    group_by(.data[[geo_column]], .quarter) %>%
    summarise(n_psus = n(), .groups = "drop") %>%
    pivot_wider(
      names_from  = .quarter,
      values_from = n_psus,
      values_fill = 0L
    )
  
  # ── 6. Output ─────────────────────────────────────────────────────────────
  list(
    panel_matrix = panel_matrix,
    verification = verification
  )
}
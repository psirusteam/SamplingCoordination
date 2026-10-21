#' @export
#' @importFrom dplyr filter mutate select distinct arrange group_by summarise left_join bind_rows across all_of coalesce pull rowwise n_distinct
#' @importFrom tibble tibble
#'
#' @title
#' Cyclic adjustment to complete incomplete or missing mini-panels
#'
#' @description
#' After the initial PSU assignment, some mini-panels may be incomplete
#' (fewer PSUs than demanded) or entirely absent. This function completes
#' incomplete panels first, then fills missing panels, taking PSUs cyclically
#' by stratum (ordered by \code{order_column}) without resetting the index
#' between panels.
#'
#' @param assigned_frame data.frame output of \code{assign_PSUs_to_panels()},
#'   with columns \code{geo_column}, \code{stratum_column}, \code{PSU_column},
#'   \code{order_column}, \code{scheme_column}, and \code{panel}.
#' @param demand_table data.frame with columns \code{geo_column},
#'   \code{stratum_column}, \code{panel_letter}, and \code{n_assigned}.
#' @param panels_by_scheme Named list mapping each scheme to its theoretical
#'   panel vector. E.g.:
#'   \code{list("12_minipaneles" = mp_teoricos_12, "4_minipaneles" = mp_teoricos_4)}.
#' @param scheme_column Character. Name of the scheme column. Default \code{"scheme"}.
#' @param geo_column Character. Name of the geo stratum column. Default \code{"geo"}.
#' @param stratum_column Character. Name of the socioeconomic (SES) stratum column. Default \code{"stratum"}.
#' @param PSU_column Character. Name of the PSU identifier column. Default \code{"PSU"}.
#' @param order_column Character. Name of the ordering column. Default \code{"rank"}.
#'
#' @return A tibble with columns \code{geo_column}, \code{PSU_column},
#'   \code{stratum_column}, and \code{panel_cyclic} containing only the
#'   PSUs added by the cyclic adjustment. If no panel needs adjustment, a
#'   message is shown and a tibble with the same columns and zero rows is
#'   returned, so it can still be passed to \code{\link{build_panel_matrix}}.
#'
#' @author Yury Vanessa Ochoa
#' @seealso \code{\link{assign_PSUs_to_panels}},
#'   \code{\link{build_panel_matrix}} for the complete workflow.
#'
#' @examples
#' # Same toy frame as in assign_PSUs_to_panels(), with geographic stratum
#' # and rotation scheme
#' psu_frame <- data.frame(
#'   geo         = "01",
#'   scheme      = "AB",
#'   PSU         = sprintf("PSU%02d", 1:11),
#'   stratum     = rep(c("low", "high"), times = c(8, 3)),
#'   rank        = c(0.12, 0.85, 0.33, 0.07, 0.61, 0.48, 0.92, 0.25,
#'                   0.71, 0.16, 0.54)
#' )
#' demand <- data.frame(
#'   geo          = "01",
#'   stratum      = rep(c("low", "high"), each = 2),
#'   panel_letter = rep(c("A", "B"), times = 2),
#'   n_assigned   = c(2, 1, 1, 1)
#' )
#' sequence_AB <- c("A1", "B1", "A2", "B2")
#'
#' assigned <- assign_PSUs_to_panels(
#'   DF = psu_frame, stratum_column = "stratum", PSU_column = "PSU",
#'   order_column = "rank", demand_table = demand,
#'   panels_sequence = sequence_AB
#' )
#'
#' # Panel B2 has no "high" PSU: it is completed reusing the PSUs of that
#' # stratum cyclically, in order of rank
#' cyclic_panel_adjustment(
#'   assigned_frame   = assigned,
#'   demand_table     = demand,
#'   panels_by_scheme = list(AB = sequence_AB)
#' )
#' @export
cyclic_panel_adjustment <- function(assigned_frame, demand_table, panels_by_scheme,
                                    scheme_column  = "scheme",
                                    geo_column     = "geo",
                                    stratum_column = "stratum",
                                    PSU_column     = "PSU",
                                    order_column   = "rank") {
  
  # ── 0. Validate ───────────────────────────────────────────────────────────
  stopifnot(
    is.data.frame(assigned_frame),
    is.data.frame(demand_table),
    is.list(panels_by_scheme),
    all(c(geo_column, stratum_column, PSU_column, order_column, scheme_column, "panel") %in% names(assigned_frame)),
    all(c(geo_column, stratum_column, "panel_letter", "n_assigned") %in% names(demand_table))
  )
  
  # ── 1. n_real: PSUs assigned by geo x scheme x panel x stratum ───────────
  n_real <- assigned_frame %>%
    dplyr::filter(!is.na(panel)) %>%
    dplyr::mutate(panel_letter = gsub("[0-9]+", "", panel)) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(
      c(geo_column, scheme_column, "panel", "panel_letter", stratum_column)
    ))) %>%
    dplyr::summarise(n_real = dplyr::n_distinct(.data[[PSU_column]]),
                     .groups = "drop")
  
  # ── 2. panel_status: incomplete panels with shortfall by SES ─────────────
  # Expand to all SES levels in demand_table to detect SES with no PSUs
  panel_status <- assigned_frame %>%
    dplyr::filter(!is.na(panel)) %>%
    dplyr::mutate(panel_letter = gsub("[0-9]+", "", panel)) %>%
    dplyr::distinct(.data[[geo_column]], .data[[scheme_column]], panel, panel_letter) %>%
    dplyr::left_join(
      demand_table %>%
        dplyr::select(dplyr::all_of(c(geo_column, stratum_column, "panel_letter", "n_assigned"))) %>%
        dplyr::distinct(),
      by = c(geo_column, "panel_letter")
    ) %>%
    dplyr::left_join(
      n_real,
      by = c(geo_column, scheme_column, "panel", "panel_letter", stratum_column)
    ) %>%
    dplyr::mutate(
      n_real  = dplyr::coalesce(n_real, 0L),
      missing = n_assigned - n_real
    ) %>%
    dplyr::filter(missing > 0)
  
  # ── 3. Strata that need adjustment ───────────────────────────────────────
  geos_to_adjust <- assigned_frame %>%
    dplyr::distinct(.data[[geo_column]], .data[[scheme_column]]) %>%
    dplyr::filter(.data[[geo_column]] %in% {
      incomplete_geos <- unique(panel_status[[geo_column]])
      missing_geos <- assigned_frame %>%
        dplyr::distinct(.data[[geo_column]], .data[[scheme_column]]) %>%
        dplyr::rowwise() %>%
        dplyr::filter(length(setdiff(
          panels_by_scheme[[.data[[scheme_column]]]],
          unique(assigned_frame$panel[assigned_frame[[geo_column]] == .data[[geo_column]]])
        )) > 0) %>%
        dplyr::pull(.data[[geo_column]])
      unique(c(incomplete_geos, missing_geos))
    })
  
  if (nrow(geos_to_adjust) == 0) {
    message("No geo requires cyclic adjustment.")
    # Empty result with the expected columns, so it can be passed to
    # build_panel_matrix() without errors
    empty <- dplyr::as_tibble(
      assigned_frame[0, c(geo_column, PSU_column, stratum_column), drop = FALSE]
    )
    empty$panel_cyclic <- character(0)
    return(empty)
  }
  
  # ── 4. Cyclic assignment by geo ──────────────────────────────────────────
  dplyr::bind_rows(lapply(seq_len(nrow(geos_to_adjust)), function(g) {
    
    geo         <- geos_to_adjust[[geo_column]][g]
    scheme_geo  <- geos_to_adjust[[scheme_column]][g]
    mp_sequence <- panels_by_scheme[[scheme_geo]]
    
    incomplete <- panel_status %>%
      dplyr::filter(.data[[geo_column]] == geo) %>%
      dplyr::distinct(panel) %>%
      dplyr::pull()
    
    absent <- setdiff(
      mp_sequence,
      unique(assigned_frame$panel[assigned_frame[[geo_column]] == geo])
    )
    
    panels_to_fill <- unique(c(incomplete, absent))
    if (length(panels_to_fill) == 0) return(NULL)
    
    psus_by_stratum <- assigned_frame %>%
      dplyr::filter(.data[[geo_column]] == geo) %>%
      dplyr::arrange(.data[[stratum_column]], .data[[order_column]]) %>%
      dplyr::group_by(.data[[stratum_column]]) %>%
      dplyr::summarise(psus = list(.data[[PSU_column]]), .groups = "drop")
    
    idx_stratum <- setNames(rep(1L, nrow(psus_by_stratum)),
                            psus_by_stratum[[stratum_column]])
    
    dplyr::bind_rows(lapply(panels_to_fill, function(panel) {
      
      letter <- gsub("[0-9]+", "", panel)
      
      # Handle incomplete and missing panels separately
      needed <- if (panel %in% incomplete) {
        # CASE 1: incomplete panel — take only the shortfall by SES
        panel_status %>%
          dplyr::filter(.data[[geo_column]] == geo, panel == !!panel) %>%
          dplyr::select(dplyr::all_of(c(stratum_column, "missing"))) %>%
          dplyr::rename(n_take = missing)
      } else {
        # CASE 2: missing panel — take the full demand by SES
        demand_table %>%
          dplyr::filter(.data[[geo_column]] == geo, panel_letter == letter) %>%
          dplyr::select(dplyr::all_of(c(stratum_column, "n_assigned"))) %>%
          dplyr::distinct() %>%
          dplyr::rename(n_take = n_assigned)
      }
      
      needed <- needed %>% dplyr::filter(n_take > 0)
      
      if (nrow(needed) == 0) return(NULL)
      
      dplyr::bind_rows(lapply(seq_len(nrow(needed)), function(i) {
        
        stratum <- needed[[stratum_column]][i]
        n_take  <- needed$n_take[i]
        psus    <- psus_by_stratum$psus[psus_by_stratum[[stratum_column]] == stratum][[1]]
        
        if (is.null(psus)) return(NULL)
        
        indices <- ((idx_stratum[stratum] - 1 + seq_len(n_take) - 1) %% length(psus)) + 1
        idx_stratum[stratum] <<- ((idx_stratum[stratum] - 1 + n_take) %% length(psus)) + 1
        
        dplyr::tibble(
          !!geo_column     := geo,
          !!PSU_column     := psus[indices],
          !!stratum_column := stratum,
          panel_cyclic     = panel
        )
      }))
    }))
  }))
}

#' Assign PSUs to mini-panels by stratum
#'
#' @description
#' Assigns PSUs to mini-panels sequentially following a user-defined theoretical
#' panel sequence. Units are assigned in order of \code{order_column} (e.g. a
#' Pareto rank) respecting the demand per stratum from \code{demand_table}.
#' Panels are interleaved by period (A1, B1, C1, A2, B2, C2...) so all letters
#' are filled before advancing to the next period. When PSUs are exhausted in a
#' stratum that stratum is skipped without affecting others. When a panel has no
#' demand in \code{demand_table} it is skipped entirely — so the sequence
#' defines the theoretical ceiling and the demand table controls how far the
#' assignment actually reaches.
#'
#' @param DF data.frame with at least \code{stratum_column}, \code{PSU_column},
#'   and \code{order_column}.
#' @param stratum_column Character. Name of the stratum column in \code{DF}.
#' @param PSU_column Character. Name of the PSU identifier column in \code{DF}.
#' @param order_column Character. Name of the column used to sort PSUs within
#'   each stratum before assignment (e.g. a Pareto rank).
#' @param demand_table data.frame with columns: \code{stratum_column},
#'   \code{panel_letter} (e.g. \code{"A"}, \code{"B"}, \code{"X"}), and
#'   \code{n_assigned} (integer). Only rows with \code{n_assigned > 0} are
#'   used. Controls how far the assignment reaches within
#'   \code{panels_sequence}.
#' @param panels_sequence Character vector with the full theoretical panel
#'   sequence (e.g. \code{c("A1","B1","C1","A2","B2","C2",...)}). Defines
#'   the ceiling — panels with no demand in \code{demand_table} are skipped
#'   automatically. Typically built as:
#'   \code{paste0(rep(letters, times = period), rep(1:period, each = n_letters))}.
#'
#' @return \code{DF} with an additional column \code{panel} containing the
#'   assigned mini-panel label (e.g. \code{"A1"}, \code{"D4"}).
#'   PSUs not assigned to any panel receive \code{NA}.
#'
#' @author Yury Vanessa Ochoa
#'
#' @seealso \code{\link{cyclic_panel_adjustment}} to complete the panels
#'   that remain incomplete, and \code{\link{build_panel_matrix}} for the
#'   complete workflow on \code{\link{Data_PSU}}.
#'
#' @examples
#' # 11 PSUs in two SES strata, ordered by a random number
#' psu_frame <- data.frame(
#'   psu_id      = sprintf("PSU%02d", 1:11),
#'   ses_stratum = rep(c("low", "high"), times = c(8, 3)),
#'   rank        = c(0.12, 0.85, 0.33, 0.07, 0.61, 0.48, 0.92, 0.25,
#'                   0.71, 0.16, 0.54)
#' )
#'
#' # PSUs demanded by each mini-panel letter and SES stratum
#' demand <- data.frame(
#'   ses_stratum  = rep(c("low", "high"), each = 2),
#'   panel_letter = rep(c("A", "B"), times = 2),
#'   n_assigned   = c(2, 1, 1, 1)
#' )
#'
#' assigned <- assign_PSUs_to_panels(
#'   DF              = psu_frame,
#'   stratum_column  = "ses_stratum",
#'   PSU_column      = "psu_id",
#'   order_column    = "rank",
#'   demand_table    = demand,
#'   panels_sequence = c("A1", "B1", "A2", "B2")
#' )
#' assigned[order(assigned$ses_stratum, assigned$rank), ]
#'
#' # The "high" stratum runs out of PSUs: panel B2 gets none there
#' table(assigned$ses_stratum, assigned$panel, useNA = "ifany")
#' @export
assign_PSUs_to_panels <- function(DF,
                                  stratum_column,
                                  PSU_column,
                                  order_column,
                                  demand_table,
                                  panels_sequence) {
  
  # ── 0. Validate ───────────────────────────────────────────────────────────
  stopifnot(
    is.data.frame(DF),
    is.data.frame(demand_table),
    stratum_column %in% names(DF),
    PSU_column     %in% names(DF),
    order_column   %in% names(DF),
    all(c(stratum_column, "panel_letter", "n_assigned") %in% names(demand_table)),
    is.character(panels_sequence),
    length(panels_sequence) > 0
  )
  
  # ── 1. Normalise types ────────────────────────────────────────────────────
  DF <- DF %>%
    dplyr::mutate(
      .stratum = as.character(.data[[stratum_column]]),
      .psu     = as.character(.data[[PSU_column]])
    )
  
  demand_table <- demand_table %>%
    dplyr::mutate(
      .stratum   = as.character(.data[[stratum_column]]),
      n_assigned = as.integer(round(n_assigned))
    ) %>%
    dplyr::filter(n_assigned > 0)
  
  if (nrow(demand_table) == 0)
    return(DF %>%
             dplyr::mutate(panel = NA_character_) %>%
             dplyr::select(-.stratum, -.psu))
  
  # ── 2. Ordered PSU list per stratum ───────────────────────────────────────
  psus_by_stratum <- DF %>%
    dplyr::arrange(.data$.stratum, .data[[order_column]]) %>%
    dplyr::group_by(.data$.stratum) %>%
    dplyr::summarise(psus = list(.psu), .groups = "drop")
  
  idx_stratum <- setNames(
    rep(1L, nrow(psus_by_stratum)),
    psus_by_stratum$.stratum
  )
  
  # ── 3. Iterate panels_sequence — stops naturally where IPFP has no demand ─
  result <- dplyr::bind_rows(lapply(panels_sequence, function(panel) {
    
    letter <- gsub("[0-9]+", "", panel)
    
    demand <- demand_table %>%
      dplyr::filter(panel_letter == letter) %>%
      dplyr::select(.stratum, n_assigned)
    
    # No demand for this letter in IPFP → skip panel entirely
    if (nrow(demand) == 0) return(NULL)
    
    dplyr::bind_rows(lapply(seq_len(nrow(demand)), function(i) {
      
      stratum <- demand$.stratum[i]
      n_take  <- demand$n_assigned[i]
      psus    <- psus_by_stratum$psus[psus_by_stratum$.stratum == stratum][[1]]
      idx     <- idx_stratum[stratum]
      
      # PSUs exhausted for this stratum → skip, others unaffected
      if (is.null(psus) || idx > length(psus)) return(NULL)
      
      end                   <- min(idx + n_take - 1L, length(psus))
      idx_stratum[stratum] <<- end + 1L
      
      data.frame(
        .psu     = psus[idx:end],
        .stratum = stratum,
        panel    = panel,
        stringsAsFactors = FALSE
      )
    }))
  }))
  
  # ── 4. Join back ──────────────────────────────────────────────────────────
  DF %>%
    dplyr::left_join(result, by = c(".psu", ".stratum")) %>%
    dplyr::select(-.psu, -.stratum)
}
#' Package-level imports
#'
#' @importFrom stats setNames
#' @importFrom rlang :=
#' @importFrom tidyr separate
#' @keywords internal
#' @noRd
NULL

utils::globalVariables(c(
  # panel_sampling (legacy)
  "unit", "random", "Panels", "stratum",
  "random_sel", "NpobPSUPanels", "nPSUPanels", "n",
  # panel_sample_size_nse
  "mini_panel", "total_month", "total_quarter",
  "n_quarter", "expected_ses", "n_frame", ".type", "month_mp",
  # build_panel_matrix
  ".period", ".panel", ".quarter", "n_psus",
  # cyclic_panel_adjustment
  "panel_cyclic", "panel_letter", "n_real", "missing", "n_take",
  # assign_PSUs_to_panels
  "panel", "n_assigned", ".stratum", ".psu",
  # generate_random_frame
  ".size_var", "pi_i", "p_i", ".N_h",
  # panel_sample_size_nse
  "prop", "total_mp"
))


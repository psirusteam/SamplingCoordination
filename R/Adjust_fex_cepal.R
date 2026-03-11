#' @export
#' @import dplyr
#' @import rlang

#' @title Adjust expansion factors for household surveys (CEPAL method)
#' @description
#' Applies the standard CEPAL workflow to adjust household expansion factors (FEX):
#' design weight, adjustment for unknown eligibility, exclusion of ineligible units,
#' and nonresponse adjustment through a logistic response-propensity model.
#'
#' @param data A data frame with household-level records.
#' @param household_id Unquoted column name with the unique household identifier.
#' @param strata Unquoted column name with the sampling stratum.
#' @param disposition_code Unquoted column name with fieldwork result category.
#' Valid categories are: `"ER"`, `"ENR"`, `"IN"`, and `"UNK"`.
#' @param major_domain Unquoted column name for major administrative division (DAM).
#' @param area Unquoted column name for geographic area.
#' @param pi_second_stage Unquoted column name with second-stage inclusion probability.
#' @param pi_first_stage Unquoted column name with first-stage inclusion probability.
#' @param keep_steps Logical. If `TRUE`, returns all intermediate datasets and the final
#' dataset in a list. If `FALSE`, returns only the final dataset.
#' @param step_by_step Logical. If `TRUE`, pauses after each stage until pressing Enter.
#' @param view_steps Logical. If `TRUE`, opens each stage dataset with `View()`
#' in interactive sessions.
#'
#' @return
#' A data frame with adjusted weights for responding eligible households (`ER`) including:
#' `d_1k`, `a_b`, `d_2k`, `d_3k`, `I_k`, `D_k`, `phi_k`, and `d_4k`.
#' If `keep_steps = TRUE`, returns a list with `step_a`, `step_b`, `step_c`, `step_d`,
#' `summary_a`, `summary_b`, `summary_c`, `summary_d`, and `base_ER_final`.
#'

#' @author Yury Vanessa Ochoa
#' @examples
#' # Minimal reproducible example
#' toy_data <- data.frame(
#'   hh_id = 1:8,
#'   stratum = c("S1", "S1", "S1", "S1", "S2", "S2", "S2", "S2"),
#'   result = c("ER", "ENR", "UNK", "IN", "ER", "ENR", "ER", "UNK"),
#'   dam = c("D1", "D1", "D1", "D1", "D2", "D2", "D2", "D2"),
#'   area = c("U", "R", "U", "R", "U", "R", "U", "R"),
#'   pi2 = rep(0.5, 8),
#'   pi1 = rep(0.5, 8)
#' )
#'
#' Adjust_fex_cepal(
#'   data = toy_data,
#'   household_id = hh_id,
#'   strata = stratum,
#'   disposition_code = result,
#'   major_domain = dam,
#'   area = area,
#'   pi_second_stage = pi2,
#'   pi_first_stage = pi1,
#'   keep_steps = FALSE,
#'   step_by_step = TRUE,
#'   view_steps = TRUE
#' )
Adjust_fex_cepal <- function(data,
                             household_id,
                             strata,
                             disposition_code,
                             major_domain,
                             area,
                             pi_second_stage,
                             pi_first_stage,
                             keep_steps = TRUE,
                             step_by_step = TRUE,
                             view_steps = TRUE) {

  household_id <- enquo(household_id)
  strata <- enquo(strata)
  disposition_code <- enquo(disposition_code)
  major_domain <- enquo(major_domain)
  area <- enquo(area)
  pi_second_stage <- enquo(pi_second_stage)
  pi_first_stage <- enquo(pi_first_stage)

  required_vars <- c(
    as_name(household_id),
    as_name(strata),
    as_name(disposition_code),
    as_name(major_domain),
    as_name(area),
    as_name(pi_second_stage),
    as_name(pi_first_stage)
  )

  missing_vars <- setdiff(required_vars, names(data))
  if (length(missing_vars) > 0) {
    stop(
      paste0("Missing required variables in `data`: ", paste(missing_vars, collapse = ", ")),
      call. = FALSE
    )
  }

  valid_disposition_codes <- c("ER", "ENR", "IN", "UNK")
  observed_disposition_codes <- unique(data[[as_name(disposition_code)]])
  if (!all(observed_disposition_codes %in% valid_disposition_codes)) {
    stop(
      paste0(
        "`disposition_code` contains invalid categories. Allowed values are: ",
        paste(valid_disposition_codes, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  if (any(data[[as_name(pi_second_stage)]] <= 0, na.rm = TRUE) ||
      any(data[[as_name(pi_first_stage)]] <= 0, na.rm = TRUE)) {
    stop("Inclusion probabilities must be greater than zero.", call. = FALSE)
  }

  build_summary <- function(df, fex_var) {
    summary_domain <- df %>%
      group_by(!!major_domain) %>%
      summarise(FEX = sum(.data[[fex_var]], na.rm = TRUE), .groups = "drop")

    list(
      national_sum = sum(df[[fex_var]], na.rm = TRUE),
      by_major_domain = summary_domain
    )
  }

  show_stage <- function(stage_label, df, summary_obj) {
    cat("\n====================================================\n")
    cat("Expansion factor diagnostics -", stage_label, "\n")
    cat("====================================================\n")
    cat("\nNational sum:\n")
    print(summary_obj$national_sum)
    cat("\nSum by major domain:\n")
    print(summary_obj$by_major_domain)

    if (view_steps) {
      View(df, title = paste("Adjust_fex_cepal -", stage_label))
    }

    if (step_by_step) {
      readline(prompt = paste0("Press Enter to continue to the next step after this ", stage_label, "... "))
    }

    invisible(NULL)
  }

  households <- data
  steps <- list()

  households <- households %>%
    mutate(d_1k = 1 / ((!!pi_second_stage) * (!!pi_first_stage)))
  summary_a <- build_summary(households, "d_1k")
  show_stage("A. Basic design weight", households, summary_a)
  steps$step_a <- households
  steps$summary_a <- summary_a

  adjustment_eligibility <- households %>%
    group_by(!!strata) %>%
    summarise(
      num = sum(d_1k, na.rm = TRUE),
      den = sum(d_1k[(!!disposition_code) %in% c("ER", "ENR", "IN")], na.rm = TRUE),
      a_b = num / den,
      .groups = "drop"
    )

  households <- households %>%
    left_join(adjustment_eligibility, by = as_name(strata)) %>%
    mutate(d_2k = if_else((!!disposition_code) %in% "UNK", 0, a_b * d_1k))
  summary_b <- build_summary(households, "d_2k")
  show_stage("B. Unknown eligibility adjustment", households, summary_b)
  steps$step_b <- households
  steps$summary_b <- summary_b

  households <- households %>%
    mutate(d_3k = if_else((!!disposition_code) %in% c("UNK", "IN"), 0, d_2k))
  summary_c <- build_summary(households, "d_3k")
  show_stage("C. Excluding ineligible units", households, summary_c)
  steps$step_c <- households
  steps$summary_c <- summary_c

  households <- households %>%
    mutate(
      I_k = if_else((!!disposition_code) %in% c("ER", "ENR"), 1, 0),
      D_k = if_else((!!disposition_code) == "ER", 1, 0),
      .major_domain = as.factor(!!major_domain),
      .area = as.factor(!!area),
      .strata = as.factor(!!strata)
    )

  model_data <- households %>% filter(I_k == 1)
  if (nrow(model_data) == 0) {
    stop("No eligible units (ER or ENR) available for nonresponse adjustment.", call. = FALSE)
  }

  if (length(unique(model_data$D_k)) < 2) {
    stop("`D_k` has no variation among eligible units.", call. = FALSE)
  }

  response_model <- glm(
    D_k ~ .major_domain + .area + .strata,
    data = model_data,
    family = binomial(link = "logit")
  )

  households <- households %>%
    mutate(
      phi_k = predict(response_model, newdata = households, type = "response"),
      d_4k = if_else(D_k == 1, d_3k / phi_k, 0)
    )
  summary_d <- build_summary(households, "d_4k")
  show_stage("D. Nonresponse adjustment", households, summary_d)
  steps$step_d <- households
  steps$summary_d <- summary_d

  final_data <- households %>%
    filter(D_k == 1) %>%
    select(
      !!household_id,
      !!strata,
      !!disposition_code,
      d_1k, a_b, d_2k, d_3k, I_k, D_k, phi_k, d_4k
    )

  if (keep_steps) {
    return(c(steps, list(base_ER_final = final_data)))
  }

  final_data
}

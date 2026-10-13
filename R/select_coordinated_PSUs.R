#' @export
#' @importFrom dplyr arrange filter group_by left_join mutate select summarise ungroup row_number n
#' @importFrom rlang .data
#'
#' @title
#' Coordinated PSU selection against previous samples (negative or positive)
#'
#' @description
#' Selects PSUs for a new survey while controlling the overlap with one or
#' more previously selected samples. Previous samples enter as 0/1 indicator
#' columns of the frame (\code{1} = the PSU belongs to that sample).
#' Within each stratum, PSUs are ranked by:
#' \enumerate{
#'   \item their overlap score, i.e. the number of previous samples that
#'     contain the PSU (ascending for \code{type = "negative"}, descending for
#'     \code{type = "positive"});
#'   \item any user-supplied ordering number (\code{order_column}), ascending
#'     or descending.
#' }
#' The first \eqn{n_h} PSUs of the resulting ranking are selected in each
#' stratum \eqn{h}. Optionally, the selected PSUs are spread across periods
#' (e.g. quarters) following the period sizes given in \code{n}.
#'
#' @details
#' \strong{Why this function.} \code{\link{SI_coord}} and
#' \code{\link{pareto_coord}} coordinate samples by shifting one common
#' permanent random number (\eqn{(\xi + a) \bmod 1}), which requires that all
#' surveys share the same random number, the same frame and the same
#' selection scheme. In practice, surveys such as a Labour Force Survey (ECE),
#' a Household Survey (EH) and a Budget Survey (EPF) are drawn with
#' \emph{different} sorting numbers (e.g. different Pareto numbers
#' \eqn{\xi_k (1 - \pi_k) / (\pi_k (1 - \xi_k))} computed with
#' survey-specific inclusion probabilities, sample sizes and strata). In that
#' setting a shift of a common number cannot guarantee the desired overlap.
#' \code{select_coordinated_PSUs()} works directly on the outcome of the
#' previous selections, so each survey can keep its own ordering number:
#' \itemize{
#'   \item \code{type = "negative"}: PSUs outside every previous sample are
#'     taken first; PSUs in fewer previous samples are preferred to PSUs in
#'     more of them.
#'   \item \code{type = "positive"}: PSUs in previous samples are taken first;
#'     PSUs shared by more previous samples are preferred.
#' }
#'
#' \strong{Rules.} With \code{rule = "prioritize"} (default) the preferred
#' PSUs are used first and the rest of the stratum completes the sample only
#' when they are not enough. With \code{rule = "strict"} only the preferred
#' PSUs are eligible (overlap score \code{0} for negative coordination,
#' \code{> 0} for positive coordination); if they are not enough, the
#' stratum is reported as \code{"SHORTFALL"} and a warning is issued.
#'
#' \strong{Inclusion probabilities.} Coordination changes the effective
#' inclusion probabilities of the new sample relative to those implied by
#' \code{order_column} alone. The verification table reports how many PSUs
#' were affected so the user can assess the impact on the design weights.
#'
#' @return
#' A list with two elements:
#' \describe{
#'   \item{\code{frame}}{The input \code{frame} (same rows and order) with the
#'     added columns \code{overlap_score} (number of previous samples that
#'     contain the PSU), \code{rank_<sample_name>} (rank among eligible PSUs
#'     of the stratum; \code{NA} if not eligible or the stratum has no
#'     sample size), \code{<sample_name>} (0/1 selection indicator) and, when
#'     \code{period_column} is given, \code{period_column} with the period
#'     assigned to each selected PSU (\code{NA} otherwise).}
#'   \item{\code{verification}}{A \code{data.frame} with one row per stratum:
#'     \code{n_required}, \code{n_frame}, \code{n_preferred} (PSUs with the
#'     preferred overlap status), \code{n_selected},
#'     \code{n_not_preferred} (selected PSUs outside the preferred group),
#'     one column \code{overlap_<col>} per previous sample with the number of
#'     selected PSUs that belong to it, \code{shortfall} and \code{status}
#'     (\code{"OK"}, \code{"COMPLETED"} when non-preferred PSUs had to be
#'     used, or \code{"SHORTFALL"}).}
#' }
#'
#' @author Yury Vanessa Ochoa Montes <yury.ochoa at urosario.edu.co>
#'
#' @param frame A \code{data.frame} or \code{tibble} with one row per PSU.
#' @param n Sample size. One of:
#'   \itemize{
#'     \item a single number, when \code{stratum_column = NULL};
#'     \item a named numeric vector (names = strata), when
#'       \code{stratum_column} is given;
#'     \item a \code{data.frame} with the columns \code{stratum_column} (if
#'       given), \code{n_column} and, optionally, \code{period_column}
#'       (one row per stratum x period).
#'   }
#'   Sizes must be non-negative integers.
#' @param prev_columns Character vector with the names of the 0/1 columns in
#'   \code{frame} that flag the previous samples to coordinate with
#'   (e.g. \code{c("sample_ECE_y1", "sample_EH")}). \code{NA} is treated as
#'   \code{0}.
#' @param order_column Character. Name of the numeric column used to rank PSUs
#'   within each stratum (e.g. a Pareto number, a permanent random number,
#'   a size measure).
#' @param decreasing Logical. If \code{FALSE} (default) PSUs are ranked by
#'   \code{order_column} in ascending order (smallest first, as in Pareto or
#'   permanent random number sampling); if \code{TRUE}, in descending order.
#' @param type Character. \code{"negative"} (default) to minimize the overlap
#'   with the previous samples, \code{"positive"} to maximize it.
#' @param rule Character. \code{"prioritize"} (default) or \code{"strict"}.
#'   See Details.
#' @param stratum_column Character or \code{NULL}. Name of the stratum column
#'   in \code{frame} (and in \code{n} when it is a \code{data.frame}).
#'   \code{NULL} (default) selects from the whole frame.
#' @param n_column Character. Name of the sample size column when \code{n} is
#'   a \code{data.frame}. Default \code{"n"}.
#' @param period_column Character or \code{NULL}. Name of the period column in
#'   \code{n} (e.g. \code{"quarter"}). When given, the selected PSUs of each
#'   stratum are assigned to periods in rank order, following the order in
#'   which the periods appear in \code{n}. Default \code{NULL}.
#' @param psu_column Character or \code{NULL}. Name of the PSU identifier
#'   column; only used to check for duplicated PSUs. Default \code{NULL}.
#' @param sample_name Character. Name of the output selection column.
#'   Default \code{"sample_new"}.
#'
#' @seealso \code{\link{generate_random_frame}}, \code{\link{panel_sample_size_nse}},
#'   \code{\link{SI_coord}}, \code{\link{pareto_coord}}, \code{\link{overlap_sample}}
#'
#' @examples
#' set.seed(123)
#' frame <- data.frame(
#'   psu       = paste0("PSU", 1:40),
#'   stratum   = rep(c("A", "B"), each = 20),
#'   xi_new    = runif(40),                  # ordering number of the new survey
#'   sample_s1 = rbinom(40, 1, 0.3),         # previous survey 1 (0/1)
#'   sample_s2 = rbinom(40, 1, 0.2)          # previous survey 2 (0/1)
#' )
#'
#' # Negative coordination with two previous surveys, sizes by stratum
#' res <- select_coordinated_PSUs(
#'   frame          = frame,
#'   n              = c(A = 5, B = 4),
#'   prev_columns   = c("sample_s1", "sample_s2"),
#'   order_column   = "xi_new",
#'   stratum_column = "stratum",
#'   psu_column     = "psu",
#'   sample_name    = "sample_s3"
#' )
#' res$verification
#'
#' # Positive coordination, descending order, no strata
#' select_coordinated_PSUs(
#'   frame        = frame,
#'   n            = 6,
#'   prev_columns = "sample_s1",
#'   order_column = "xi_new",
#'   decreasing   = TRUE,
#'   type         = "positive"
#' )$verification
#'
#' # Sizes by stratum x quarter (e.g. rounded IPFP output in long format)
#' sizes <- data.frame(
#'   stratum = rep(c("A", "B"), each = 2),
#'   quarter = rep(c("Q1", "Q2"), 2),
#'   n       = c(3, 2, 2, 2)
#' )
#' res_q <- select_coordinated_PSUs(
#'   frame          = frame,
#'   n              = sizes,
#'   prev_columns   = c("sample_s1", "sample_s2"),
#'   order_column   = "xi_new",
#'   stratum_column = "stratum",
#'   period_column  = "quarter"
#' )
#' table(res_q$frame$stratum, res_q$frame$quarter)
select_coordinated_PSUs <- function(frame,
                                    n,
                                    prev_columns,
                                    order_column,
                                    decreasing     = FALSE,
                                    type           = c("negative", "positive"),
                                    rule           = c("prioritize", "strict"),
                                    stratum_column = NULL,
                                    n_column       = "n",
                                    period_column  = NULL,
                                    psu_column     = NULL,
                                    sample_name    = "sample_new") {

  type <- match.arg(type)
  rule <- match.arg(rule)

  # -- 0. Validate -----------------------------------------------------------
  stopifnot(
    is.data.frame(frame),
    is.character(prev_columns), length(prev_columns) >= 1,
    is.character(order_column), length(order_column) == 1,
    is.logical(decreasing),     length(decreasing)   == 1,
    is.character(sample_name),  length(sample_name)  == 1,
    is.null(stratum_column) || (is.character(stratum_column) && length(stratum_column) == 1),
    is.null(period_column)  || (is.character(period_column)  && length(period_column)  == 1),
    is.null(psu_column)     || (is.character(psu_column)     && length(psu_column)     == 1)
  )

  required_frame <- c(prev_columns, order_column, stratum_column, psu_column)
  missing_frame  <- setdiff(required_frame, names(frame))
  if (length(missing_frame) > 0)
    stop("Missing columns in `frame`: ",
         paste(missing_frame, collapse = ", "), call. = FALSE)

  rank_name <- paste0("rank_", sample_name)
  new_cols  <- c("overlap_score", rank_name, sample_name, period_column)
  clash     <- intersect(new_cols, names(frame))
  if (length(clash) > 0)
    stop("`frame` already contains output column(s): ",
         paste(clash, collapse = ", "),
         ". Rename them or change `sample_name`.", call. = FALSE)

  if (!is.numeric(frame[[order_column]]))
    stop("`", order_column, "` must be numeric.", call. = FALSE)

  if (!is.null(psu_column) && anyDuplicated(frame[[psu_column]]) > 0)
    stop("Duplicated PSUs in `", psu_column, "`.", call. = FALSE)

  if (!is.null(period_column) && !is.data.frame(n))
    stop("`period_column` requires `n` to be a data.frame.", call. = FALSE)

  # -- 1. Sample size table (.stratum, .n) -----------------------------------
  if (is.data.frame(n)) {
    req_n  <- c(stratum_column, n_column, period_column)
    miss_n <- setdiff(req_n, names(n))
    if (length(miss_n) > 0)
      stop("Missing columns in `n`: ", paste(miss_n, collapse = ", "), call. = FALSE)
    n_long <- data.frame(
      .stratum = if (is.null(stratum_column)) "all" else as.character(n[[stratum_column]]),
      .n       = n[[n_column]],
      stringsAsFactors = FALSE
    )
    if (!is.null(period_column)) n_long$.period <- n[[period_column]]
  } else if (is.numeric(n)) {
    if (is.null(stratum_column)) {
      if (length(n) != 1)
        stop("Without `stratum_column`, `n` must be a single number.", call. = FALSE)
      n_long <- data.frame(.stratum = "all", .n = n, stringsAsFactors = FALSE)
    } else {
      if (is.null(names(n)) || any(names(n) == ""))
        stop("With `stratum_column`, a numeric `n` must be named by stratum.", call. = FALSE)
      n_long <- data.frame(.stratum = names(n), .n = unname(n), stringsAsFactors = FALSE)
    }
  } else {
    stop("`n` must be a number, a named numeric vector or a data.frame.", call. = FALSE)
  }

  if (any(is.na(n_long$.n)) || any(n_long$.n < 0) ||
      any(abs(n_long$.n - round(n_long$.n)) > 1e-8))
    stop("Sample sizes must be non-negative integers (round IPFP output first).",
         call. = FALSE)
  n_long$.n <- as.integer(round(n_long$.n))

  if (is.null(period_column) && anyDuplicated(n_long$.stratum) > 0)
    stop("`n` has more than one row per stratum; supply `period_column`.", call. = FALSE)

  n_tab <- n_long %>%
    group_by(.data$.stratum) %>%
    summarise(.n_total = sum(.data$.n), .groups = "drop")

  # -- 2. Overlap score and ranking key --------------------------------------
  prev_mat <- vapply(prev_columns, function(v) {
    x <- suppressWarnings(as.numeric(frame[[v]]))
    x[is.na(x)] <- 0
    as.integer(x > 0)
  }, integer(nrow(frame)))
  prev_mat <- matrix(prev_mat, nrow = nrow(frame), dimnames = list(NULL, prev_columns))

  work <- data.frame(
    .row           = seq_len(nrow(frame)),
    .stratum       = if (is.null(stratum_column)) "all" else as.character(frame[[stratum_column]]),
    overlap_score  = as.integer(rowSums(prev_mat)),
    .key           = if (decreasing) -frame[[order_column]] else frame[[order_column]],
    stringsAsFactors = FALSE
  )

  # Preferred PSUs: free PSUs (negative) or PSUs already sampled (positive)
  work$.preferred <- if (type == "negative") work$overlap_score == 0 else work$overlap_score > 0
  # Score key: fewer overlaps first (negative) / more overlaps first (positive)
  work$.score_key <- if (type == "negative") work$overlap_score else -work$overlap_score
  work$.eligible  <- if (rule == "strict") work$.preferred else TRUE

  in_demand <- work$.stratum %in% n_tab$.stratum[n_tab$.n_total > 0]
  if (any(is.na(work$.key[in_demand & work$.eligible])))
    stop("`", order_column, "` has NA values for eligible PSUs.", call. = FALSE)

  # -- 3. Rank within stratum and select -------------------------------------
  work <- work %>%
    left_join(n_tab, by = ".stratum") %>%
    arrange(.data$.stratum, !.data$.eligible, .data$.score_key, .data$.key, .data$.row) %>%
    group_by(.data$.stratum) %>%
    mutate(
      .rank = ifelse(.data$.eligible & !is.na(.data$.n_total),
                     cumsum(.data$.eligible), NA_integer_),
      .sel  = as.integer(!is.na(.data$.rank) & .data$.rank <= .data$.n_total)
    ) %>%
    ungroup() %>%
    arrange(.data$.row)

  # -- 4. Output frame -------------------------------------------------------
  out <- frame
  out[["overlap_score"]] <- work$overlap_score
  out[[rank_name]]       <- as.integer(work$.rank)
  out[[sample_name]]     <- work$.sel

  if (!is.null(period_column)) {
    slots <- n_long[n_long$.n > 0, , drop = FALSE]
    slots <- data.frame(
      .stratum = rep(slots$.stratum, slots$.n),
      .period  = rep(slots$.period,  slots$.n),
      stringsAsFactors = FALSE
    )
    slots$.rank <- stats::ave(seq_len(nrow(slots)), slots$.stratum, FUN = seq_along)
    key_work  <- paste(work$.stratum, work$.rank, sep = "\r")
    key_slots <- paste(slots$.stratum, slots$.rank, sep = "\r")
    per <- slots$.period[match(key_work, key_slots)]
    per[work$.sel == 0] <- NA
    out[[period_column]] <- per
  }

  # -- 5. Verification -------------------------------------------------------
  sel <- work$.sel == 1
  strata_all <- union(n_tab$.stratum, unique(work$.stratum))
  verification <- data.frame(stratum = strata_all, stringsAsFactors = FALSE)
  verification$n_required <- n_tab$.n_total[match(strata_all, n_tab$.stratum)]
  verification$n_required[is.na(verification$n_required)] <- 0L
  cnt <- function(cond) as.integer(tapply(cond, factor(work$.stratum, levels = strata_all), sum))
  cnt0 <- function(x) { x[is.na(x)] <- 0L; x }
  verification$n_frame         <- cnt0(cnt(rep(TRUE, nrow(work))))
  verification$n_preferred     <- cnt0(cnt(work$.preferred))
  verification$n_selected      <- cnt0(cnt(sel))
  verification$n_not_preferred <- cnt0(cnt(sel & !work$.preferred))
  for (v in prev_columns) {
    verification[[paste0("overlap_", v)]] <- cnt0(cnt(sel & prev_mat[, v] == 1))
  }
  verification$shortfall <- verification$n_required - verification$n_selected
  verification$status <- ifelse(verification$shortfall > 0, "SHORTFALL",
                         ifelse(verification$n_not_preferred > 0, "COMPLETED", "OK"))
  if (!is.null(stratum_column)) names(verification)[1] <- stratum_column
  verification <- verification[verification$n_required > 0, , drop = FALSE]
  rownames(verification) <- NULL

  if (any(verification$shortfall > 0))
    warning(sum(verification$shortfall > 0),
            " stratum/strata without enough eligible PSUs. See `$verification`.",
            call. = FALSE)

  list(frame = out, verification = verification)
}

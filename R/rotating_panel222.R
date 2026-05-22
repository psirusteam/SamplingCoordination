#' Generate a 2-2-2 Rotating Panel
#'
#' @description
#' Generates a sequence of rotating panels following the 2-2-2 scheme. In this scheme,
#' each unit is observed for 2 consecutive periods, rests for 2 periods, and is observed
#' again for 2 more periods. The panel guarantees a 50\% overlap between contiguous periods.
#'
#' @param n_periods Integer. Total number of periods required (>= 1).
#' @param value_initial A character indicating the initial letter to be used for the panel
#'   columns. Default is \code{"A"}.
#'
#' @return A data frame with \code{n_periods} rows and 4 columns, one per rotating subpanel.
#'   Column names correspond to consecutive letters starting from \code{value_initial}.
#' @author Jose Fernando Zea Castro <jfzeac at unal.edu.co>,
#'   Hugo Andres Gutierrez Rojas <andres.gutierrez at cepal.org>,
#'   Stalyn Yasid Guerrero Gomez <syguerrerog at unal.edu.co>,
#'   Yury Vanessa Ochoa Montes <yury.ochoa at urosario.edu.co>
#'
#' @examples
#' rotating_panel222(40)
#' rotating_panel222(40, value_initial = "A")
#' rotating_panel222(5, value_initial = "A")
#'
#' @export
rotating_panel222 <- function(n_periods, value_initial = "A") {

  if (!is.character(value_initial) || nchar(value_initial) != 1 || !grepl("[A-Z]", value_initial)) {
    stop("value_initial must be a single uppercase letter.")
  }

  # Always compute at least 8 periods (minimum required for the 2-2-2 structure),
  # then trim to the requested n_periods at the end
  n_compute <- max(n_periods, 8)

  vctr_rep1stLetterCols <- 1:4

  df_possibleScenarios2ndVisit <- utils_admissibleScenarios222(
    n_periods = n_compute,
    vctr_rep1stLetterCols = vctr_rep1stLetterCols,
    str_blockInitLetter = value_initial
  )

  vctr_rep2ndLetterCols <- df_possibleScenarios2ndVisit[1, ]

  block <- utils_minimal_block222(
    n_periods = n_compute,
    vctr_rep1stLetterCols = vctr_rep1stLetterCols,
    vctr_rep2ndLetterCols = vctr_rep2ndLetterCols,
    str_blockInitLetter = value_initial
  )

  block <- as.data.frame(block)
  block <- block[1:n_periods, ]

  posicion <- which(LETTERS == value_initial)
  colnames(block) <- LETTERS[posicion:(posicion + 3)]
  row.names(block) <- 1:n_periods

  block
}


#' Compute Admissible Scenarios for the 2-2-2 Rotative Panel
#'
#' @description
#' Computes admissible scenarios for the second letter columns in a 2-2-2 rotative panel
#' by evaluating all combinations and retaining those that pass the global overlap test
#' at 100\%.
#'
#' @param n_periods Integer. Number of periods. Default is 40.
#' @param vctr_rep1stLetterCols Integer vector. Repetitions of the first letter of each column.
#'   Default is \code{1:4}.
#' @param str_blockInitLetter Character. Initial letter of the block.
#'
#' @return A data frame with the admissible combinations of second letter column repetitions.
#'
#' @author Jose Fernando Zea Castro <jfzeac at unal.edu.co>,
#'   Hugo Andres Gutierrez Rojas <andres.gutierrez at cepal.org>,
#'   Stalyn Yasid Guerrero Gomez <syguerrerog at unal.edu.co>,
#'   Yury Vanessa Ochoa Montes <yury.ochoa at urosario.edu.co>
#'
#' @examples
#' utils_admissibleScenarios222(n_periods = 40, vctr_rep1stLetterCols = 1:4,
#'                              str_blockInitLetter = "A")
#'
#' @export
utils_admissibleScenarios222 <- function(n_periods = 40,
                                         vctr_rep1stLetterCols = 1:4,
                                         str_blockInitLetter) {
  grid_2ndLetter <- expand.grid(c(1, 3), c(1, 3), c(1, 3), c(1, 3))
  lst_possibleOutcomes <- vector(mode = "list", nrow(grid_2ndLetter))

  for (i in 1:nrow(grid_2ndLetter)) {
    lst_possibleOutcomes[[i]] <- utils_minimal_block222(
      n_periods = n_periods,
      vctr_rep1stLetterCols = vctr_rep1stLetterCols,
      vctr_rep2ndLetterCols = grid_2ndLetter[i, ],
      str_blockInitLetter
    )
  }

  vctr_globalTest <- rep(NA_real_, nrow(grid_2ndLetter))
  for (i in 1:nrow(grid_2ndLetter)) {
    vctr_globalTest[i] <- utils_test_contigous222(lst_possibleOutcomes[[i]])$globalTest
  }

  df_possibleOutcome <- grid_2ndLetter[which(vctr_globalTest == 100), ]
  df_possibleOutcome
}


#' Generate a Minimal Block for the 2-2-2 Rotative Panel
#'
#' @description
#' Generates the minimal repeating block of a 2-2-2 rotative panel for a specified
#' number of periods, given the repetition vectors for first and second letter columns
#' and the block's initial letter.
#'
#' @param n_periods Integer. Number of periods.
#' @param vctr_rep1stLetterCols Integer vector. Repetitions of the first letter of each column.
#' @param vctr_rep2ndLetterCols Integer vector. Repetitions of the second letter of each column.
#' @param str_blockInitLetter Character. Initial letter of the block.
#'
#' @return A character matrix of dimension \code{n_periods x 4} with panel labels.
#'
#' @author Jose Fernando Zea Castro <jfzeac at unal.edu.co>,
#'   Hugo Andres Gutierrez Rojas <andres.gutierrez at cepal.org>,
#'   Stalyn Yasid Guerrero Gomez <syguerrerog at unal.edu.co>,
#'   Yury Vanessa Ochoa Montes <yury.ochoa at urosario.edu.co>
#'  
#' @examples
#' utils_minimal_block222(40, 1:4, c(1, 3, 1, 3), "A")
#'
#' @export
utils_minimal_block222 <- function(n_periods, vctr_rep1stLetterCols,
                                    vctr_rep2ndLetterCols,
                                    str_blockInitLetter) {
  init_letter <- which(LETTERS == toupper(str_blockInitLetter))
  vctr_letras_iniciales <- LETTERS[init_letter:(init_letter + 3)]

  matrix_out <- matrix(NA, nrow = n_periods, ncol = length(vctr_rep1stLetterCols))

  # Row 1
  matrix_out[1, ] <- c(1, 1, 1, 1)

  # Rows 2-6: initial structure based on vctr_rep1stLetterCols
  matrix_out[2:6, which(vctr_rep1stLetterCols == 1)] <- c(1, NA, NA, 1, 1)
  matrix_out[2:6, which(vctr_rep1stLetterCols == 2)] <- c(NA, NA, 1, 1, NA)
  matrix_out[2:6, which(vctr_rep1stLetterCols == 3)] <- c(1, NA, NA, NA, NA)
  matrix_out[2:6, which(vctr_rep1stLetterCols == 4)] <- c(NA, NA, NA, NA, NA)

  # Row 2
  for (j in 1:4) {
    if (is.na(matrix_out[2, j])) matrix_out[2, j] <- 2
  }

  # Row 3
  for (j in 1:4) {
    if (matrix_out[1, j] == matrix_out[2, j]) {
      matrix_out[3, j] <- matrix_out[2, j] + 1
    } else {
      matrix_out[3, j] <- matrix_out[2, j]
    }
  }

  # Row 4
  for (j in 1:4) {
    if (is.na(matrix_out[4, j]) && matrix_out[2, j] != matrix_out[3, j]) {
      matrix_out[4, j] <- matrix_out[3, j]
    }
    if (is.na(matrix_out[4, j]) && matrix_out[2, j] == matrix_out[3, j] &&
        vctr_rep1stLetterCols[j] %in% 1:2) {
      matrix_out[4, j] <- matrix_out[1, j]
    }
    if (is.na(matrix_out[4, j]) && matrix_out[2, j] == matrix_out[3, j] &&
        vctr_rep1stLetterCols[j] %in% 3:4) {
      matrix_out[4, j] <- matrix_out[3, j] + 1
    }
  }

  # Row 5
  for (j in 1:4) {
    if (is.na(matrix_out[5, j]) && matrix_out[3, j] != matrix_out[4, j]) {
      matrix_out[5, j] <- matrix_out[4, j]
    }
    if (is.na(matrix_out[5, j]) && matrix_out[3, j] == matrix_out[4, j] &&
        vctr_rep1stLetterCols[j] %in% 1:2) {
      matrix_out[5, j] <- matrix_out[2, j]
    }
    if (is.na(matrix_out[5, j]) && matrix_out[3, j] == matrix_out[4, j] &&
        vctr_rep1stLetterCols[j] %in% 3:4) {
      matrix_out[5, j] <- matrix_out[4, j] + 1
    }
  }

  # Row 6
  for (j in 1:4) {
    if (is.na(matrix_out[6, j]) && matrix_out[4, j] != matrix_out[5, j]) {
      matrix_out[6, j] <- matrix_out[5, j]
    }
    if (is.na(matrix_out[6, j]) && matrix_out[4, j] == matrix_out[5, j] &&
        vctr_rep2ndLetterCols[j] == 1) {
      matrix_out[6, j] <- matrix_out[3, j]
    }
    if (is.na(matrix_out[6, j]) && matrix_out[4, j] == matrix_out[5, j] &&
        vctr_rep2ndLetterCols[j] == 3) {
      matrix_out[6, j] <- matrix_out[5, j] + 1
    }
  }

  # Row 7
  for (j in 1:4) {
    if (is.na(matrix_out[7, j]) && matrix_out[5, j] != matrix_out[6, j]) {
      matrix_out[7, j] <- matrix_out[6, j]
    }
    if (is.na(matrix_out[7, j]) && matrix_out[5, j] == matrix_out[6, j] &&
        vctr_rep2ndLetterCols[j] == 1) {
      matrix_out[7, j] <- matrix_out[4, j]
    }
    if (is.na(matrix_out[7, j]) && matrix_out[5, j] == matrix_out[6, j] &&
        vctr_rep2ndLetterCols[j] == 3) {
      matrix_out[7, j] <- matrix_out[6, j] + 1
    }
  }

  # Rows 8 onwards
  for (i in 8:n_periods) {
    for (j in 1:4) {
      if (is.na(matrix_out[i, j]) && matrix_out[i - 2, j] != matrix_out[i - 1, j]) {
        matrix_out[i, j] <- matrix_out[i - 1, j]
      }
      if (is.na(matrix_out[i, j]) && matrix_out[i - 2, j] == matrix_out[i - 1, j] &&
          matrix_out[i - 3, j] == matrix_out[i - 7, j]) {
        matrix_out[i, j] <- matrix_out[i - 1, j] + 1
      }
      if (is.na(matrix_out[i, j]) && matrix_out[i - 2, j] == matrix_out[i - 1, j] &&
          matrix_out[i - 3, j] != matrix_out[i - 7, j]) {
        matrix_out[i, j] <- matrix_out[i - 3, j]
      }
    }
  }

  # Add letter prefix to each column
  matrix_out2 <- matrix_out
  for (i in 1:4) {
    matrix_out2[, i] <- paste0(vctr_letras_iniciales[i], matrix_out[, i])
  }

  matrix_out2
}


#' Create a Matrix of Common Elements Between Rows
#'
#' @description
#' Creates a square matrix where entry \code{[i, j]} contains the number of common
#' elements between row \code{i} and row \code{j} of the input matrix.
#'
#' @param matrix_data A matrix or data frame.
#'
#' @return A square numeric matrix of dimension \code{nrow(matrix_data) x nrow(matrix_data)}.
#'
#' @author Jose Fernando Zea Castro <jfzeac at unal.edu.co>,
#'   Hugo Andres Gutierrez Rojas <andres.gutierrez at cepal.org>,
#'   Stalyn Yasid Guerrero Gomez <syguerrerog at unal.edu.co>,
#'   Yury Vanessa Ochoa Montes <yury.ochoa at urosario.edu.co>
#'
#' @examples
#' mat <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), nrow = 5, ncol = 2)
#' utils_create_common_elements_matrix(mat)
#'
#' @export
utils_create_common_elements_matrix <- function(matrix_data) {
  utils_common_elements_between_rows <- function(row1, row2) {
    length(intersect(row1, row2))
  }

  matrix_data <- as.matrix(matrix_data)
  n <- nrow(matrix_data)
  common_elements_matrix <- matrix(0, nrow = n, ncol = n)

  for (i in 1:n) {
    for (j in 1:n) {
      common_elements_matrix[i, j] <- utils_common_elements_between_rows(
        matrix_data[i, ], matrix_data[j, ]
      )
    }
  }

  return(common_elements_matrix)
}


#' Test Overlap Properties of a 2-2-2 Rotative Panel
#'
#' @description
#' Verifies that a 2-2-2 rotative panel block satisfies the required overlap properties:
#' \itemize{
#'   \item Contiguous periods: 50\% overlap.
#'   \item Two periods apart: 0\% overlap.
#'   \item Three periods apart: 25\% overlap.
#'   \item Four periods apart: 50\% overlap.
#' }
#'
#' @param block A matrix or data frame representing the rotative panel block.
#'
#' @return A named list with overlap vectors, proportions, individual test percentages,
#'   and a \code{globalTest} value (average of all four tests, 100 = fully valid).
#'
#' @author Jose Fernando Zea Castro <jfzeac at unal.edu.co>,
#'   Hugo Andres Gutierrez Rojas <andres.gutierrez at cepal.org>,
#'   Stalyn Yasid Guerrero Gomez <syguerrerog at unal.edu.co>,
#'   Yury Vanessa Ochoa Montes <yury.ochoa at urosario.edu.co>
#'
#' @examples
#' block <- utils_minimal_block222(40, 1:4, c(1, 3, 1, 3), "A")
#' utils_test_contigous222(block)
#'
#' @export
utils_test_contigous222 <- function(block) {
  num_Panels <- ncol(block)
  common_elements_matrix <- utils_create_common_elements_matrix(block)

  contiguous <- NA
  for (i in 1:(nrow(common_elements_matrix) - 1)) {
    contiguous[i] <- common_elements_matrix[i, i + 1]
    names(contiguous)[i] <- paste0(i, "-", i + 1)
  }
  prop_contiguous <- contiguous / num_Panels
  indica_contiguous <- 100 * sum(prop_contiguous == 0.5) / length(contiguous)

  contiguousEach2 <- NA
  for (i in 1:(nrow(common_elements_matrix) - 2)) {
    contiguousEach2[i] <- common_elements_matrix[i, i + 2]
    names(contiguousEach2)[i] <- paste0(i, "-", i + 2)
  }
  prop_contiguousEach2 <- contiguousEach2 / num_Panels
  indica_contiguousEach2 <- 100 * sum(prop_contiguousEach2 == 0) / length(prop_contiguousEach2)

  contiguousEach3 <- NA
  for (i in 1:(nrow(common_elements_matrix) - 3)) {
    contiguousEach3[i] <- common_elements_matrix[i, i + 3]
    names(contiguousEach3)[i] <- paste0(i, "-", i + 3)
  }
  prop_contiguousEach3 <- contiguousEach3 / num_Panels
  indica_contiguousEach3 <- 100 * sum(prop_contiguousEach3 == 0.25) / length(contiguousEach3)

  contiguousEach4 <- NA
  for (i in 1:(nrow(common_elements_matrix) - 4)) {
    contiguousEach4[i] <- common_elements_matrix[i, i + 4]
    names(contiguousEach4)[i] <- paste0(i, "-", i + 4)
  }
  prop_contiguousEach4 <- contiguousEach4 / num_Panels
  indica_contiguousEach4 <- 100 * sum(prop_contiguousEach4 == 0.5) / length(contiguousEach4)

  outcome <- list(
    block, common_elements_matrix,
    contiguous, prop_contiguous, indica_contiguous,
    contiguousEach2, prop_contiguousEach2, indica_contiguousEach2,
    contiguousEach3, prop_contiguousEach3, indica_contiguousEach3,
    contiguousEach4, prop_contiguousEach4, indica_contiguousEach4,
    (indica_contiguous + indica_contiguousEach2 +
       indica_contiguousEach3 + indica_contiguousEach4) / 4
  )

  names(outcome) <- c(
    "Panel", "Rows_CommonElements",
    "commonRows", "prop_commonRows", "test_commonRows",
    "commonRowsEach2", "prop_commonRowsEach2", "test_commonRowsEach2",
    "commonRowsEach3", "prop_commonRowsEach3", "test_commonRowsEach3",
    "commonRowsEach4", "prop_commonRowsEach4", "test_commonRowsEach4",
    "globalTest"
  )

  outcome
}

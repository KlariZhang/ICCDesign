# File: utils-data.R
# Description: Data preprocessing and validation for ICC analysis
# Author: [Clare Gao]
# Date: 2026-5-6
# Dependencies: base R (no additional packages required)

#==============================================================================#
# Function 1: icc_preprocess_data
# Core data cleaning, validation and standardization for ICC calculations
#==============================================================================#
#' Preprocess and Validate Data for ICC Analysis
#'
#' @description
#' Performs standardized data cleaning, format conversion, and legality validation
#' for raw input data. Provides a unified valid data input for all ICC calculation
#' functions to avoid repetitive validation code.
#'
#' @param data A data frame or matrix. Rows represent subjects, columns represent raters/
#'   repeated measurements. Must contain only numeric values.
#' @param na.rm Logical. Default is \code{TRUE}. If \code{TRUE}, remove rows with
#'   missing values; if \code{FALSE}, retain missing values and return a warning.
#'
#' @return A named list containing:
#' \describe{
#'   \item{data_matrix}{Numeric matrix. Standardized numeric matrix (no missing values
#'     if \code{na.rm = TRUE}).}
#'   \item{n}{Integer. Number of valid subjects (rows).}
#'   \item{k}{Integer. Number of raters/repeated measurements (columns).}
#'   \item{warning_msg}{Character or \code{NULL}. Warning message for missing values,
#'     \code{NULL} if no missing values.}
#'   \item{error_msg}{Character or \code{NULL}. Error message for invalid data,
#'     \code{NULL} if data is valid.}
#' }
#'
#' @keywords internal
#' @export
icc_preprocess_data <- function(data, na.rm = TRUE) {
  # 1. Initialize output list with default values
  result <- list(
    data_matrix = NULL,
    n = NULL,
    k = NULL,
    warning_msg = NULL,
    error_msg = NULL
  )

  #----------------------------------------------------------------------------#
  # Step 1: Validate input data type (must be data frame or matrix)
  #----------------------------------------------------------------------------#
  if (!is.data.frame(data) && !is.matrix(data)) {
    result$error_msg <- "Invalid input: 'data' must be a data frame or matrix."
    return(result)
  }

  #----------------------------------------------------------------------------#
  # Step 2: Convert to matrix and validate all values are numeric
  #----------------------------------------------------------------------------#
  data_matrix <- as.matrix(data)
  if (!is.numeric(data_matrix)) {
    result$error_msg <- "Invalid data: All values must be numeric (non-numeric values detected)."
    return(result)
  }

  #----------------------------------------------------------------------------#
  # Step 3: Validate row and column count (minimum 2 rows and 2 columns)
  #----------------------------------------------------------------------------#
  original_rows <- nrow(data_matrix)
  original_cols <- ncol(data_matrix)
  if (original_rows < 2 || original_cols < 2) {
    result$error_msg <- sprintf(
      "Invalid dimensions: Data requires at least 2 subjects (rows) and 2 raters (columns). Current: %d rows, %d columns.",
      original_rows, original_cols
    )
    return(result)
  }

  #----------------------------------------------------------------------------#
  # Step 4: Handle missing values
  #----------------------------------------------------------------------------#
  na_count <- sum(is.na(data_matrix))
  processed_matrix <- data_matrix

  if (na_count > 0) {
    if (na.rm) {
      # Remove rows with missing values
      processed_matrix <- stats::na.omit(processed_matrix)
      valid_rows <- nrow(processed_matrix)

      # Re-validate row count after removing NA
      if (valid_rows < 2) {
        result$error_msg <- sprintf(
          "Insufficient valid data: After removing rows with missing values, only %d valid subject(s) remain (minimum 2 required).",
          valid_rows
        )
        return(result)
      }

      result$warning_msg <- sprintf(
        "Missing values detected: Total %d missing values removed. %d rows deleted, %d valid rows remaining.",
        na_count, original_rows - valid_rows, valid_rows
      )
    } else {
      # Retain NA values and issue warning
      result$warning_msg <- sprintf(
        "Missing values detected: Total %d missing values retained. This may cause errors in ICC calculations.",
        na_count
      )
    }
  }

  #----------------------------------------------------------------------------#
  # Step 5: Populate final results
  #----------------------------------------------------------------------------#
  result$data_matrix <- processed_matrix
  result$n <- nrow(processed_matrix)
  result$k <- ncol(processed_matrix)

  # Return standardized preprocessed result
  return(result)
}

#==============================================================================#
# Function 2: icc_check_design
# Validate core ICC design parameters and generate warnings/tips
#==============================================================================#
#' Validate ICC Design Parameters
#'
#' @description
#' Validates the legality of 4 core user-input design parameters, intercepts
#' invalid inputs in advance, and generates standardized warnings and tips
#' to avoid runtime errors during ICC calculation.
#'
#' @param same_raters Logical. Required. Are all subjects measured by the same
#'   group of raters? TRUE = yes, FALSE = no.
#' @param rater_effect Character. Optional (NULL). Rater effect type: "random" or "fixed".
#'   Not required when \code{same_raters = FALSE}.
#' @param rating_type Character. Required. Type of rating used: "single" or "average".
#' @param agreement_type Character. Optional (NULL). Agreement type: "absolute" or "consistency".
#'   Not required when \code{same_raters = FALSE}.
#' @param k Integer. Required. Number of raters (columns of the data), from \code{icc_preprocess_data}.
#'
#' @return A named list containing:
#' \describe{
#'   \item{is_valid}{Logical. Whether the design parameters are valid.}
#'   \item{error_msg}{Character or NULL. Error message for invalid parameters, NULL if valid.}
#'   \item{warning_msg}{Character or NULL. Warning message for non-recommended scenarios, NULL if none.}
#'   \item{tip_msg}{Character or NULL. Tip message for automatic mapping scenarios, NULL if none.}
#' }
#'
#' @keywords internal
#' @references
#' McGraw, K. O., & Wong, S. P. (1996). Forming inferences about some
#' intraclass correlation coefficients. Psychological Methods, 1(1), 30-46.
icc_check_design <- function(same_raters, rater_effect = NULL,
                             rating_type, agreement_type = NULL, k) {
  # 1. Initialize output result
  result <- list(
    is_valid = FALSE,
    error_msg = NULL,
    warning_msg = NULL,
    tip_msg = NULL
  )

  #----------------------------------------------------------------------------#
  # Step 1: Basic mandatory parameter validation
  #----------------------------------------------------------------------------#
  # Check same_raters: must be a single logical value
  if (missing(same_raters) || !is.logical(same_raters) || length(same_raters) != 1) {
    result$error_msg <- "Invalid parameter: 'same_raters' must be a single logical value (TRUE/FALSE)."
    return(result)
  }

  # Check rating_type: required, must be single character
  if (missing(rating_type) || !is.character(rating_type) || length(rating_type) != 1) {
    result$error_msg <- "Invalid parameter: 'rating_type' must be a single character: 'single' or 'average'."
    return(result)
  }

  # Check rating_type allowed values
  if (!rating_type %in% c("single", "average")) {
    result$error_msg <- "Invalid 'rating_type': must be either 'single' or 'average'."
    return(result)
  }

  # Check k: must be integer >= 2
  if (missing(k) || !is.numeric(k) || length(k) != 1 || k < 2 || k != as.integer(k)) {
    result$error_msg <- "Invalid parameter: 'k' must be an integer greater than or equal to 2."
    return(result)
  }

  #----------------------------------------------------------------------------#
  # Step 2: One-way model validation (same_raters = FALSE)
  #----------------------------------------------------------------------------#
  if (!same_raters) {
    # For one-way model: rater_effect and agreement_type must NOT be provided
    if (!is.null(rater_effect) || !is.null(agreement_type)) {
      result$error_msg <- "Invalid input: One-way model does NOT require specifying 'rater_effect' or 'agreement_type'."
      return(result)
    }

    # All checks passed for one-way model
    result$is_valid <- TRUE
    return(result)
  }

  #----------------------------------------------------------------------------#
  # Step 3: Two-way model validation (same_raters = TRUE)
  #----------------------------------------------------------------------------#
  if (same_raters) {
    # Check required parameters for two-way model
    if (is.null(rater_effect) || is.null(agreement_type)) {
      result$error_msg <- "Missing parameters: Two-way model requires both 'rater_effect' and 'agreement_type'."
      return(result)
    }

    # Validate rater_effect
    if (!rater_effect %in% c("random", "fixed")) {
      result$error_msg <- "Invalid 'rater_effect': must be either 'random' or 'fixed'."
      return(result)
    }

    # Validate agreement_type
    if (!agreement_type %in% c("absolute", "consistency")) {
      result$error_msg <- "Invalid 'agreement_type': must be either 'absolute' or 'consistency'."
      return(result)
    }

    # All parameter checks passed for two-way model
    result$is_valid <- TRUE

    #--------------------------------------------------------------------------#
    # Generate tip/warning messages for special two-way scenarios
    #--------------------------------------------------------------------------#
    # Tip: Random effects + consistency (auto-mapped to ICC(3,x))
    if (rater_effect == "random" && agreement_type == "consistency") {
      result$tip_msg <- sprintf(
        "Tip: Random effects + consistency combination. Automatically mapped to ICC(3,%s) (mixed effects model) in practice.",
        ifelse(rating_type == "single", "1", "k")
      )
    }

    # Warning: Fixed effects + absolute agreement (not recommended)
    if (rater_effect == "fixed" && agreement_type == "absolute") {
      result$warning_msg <- sprintf(
        "WARNING: Fixed effects + absolute agreement is rarely used and hard to interpret. Recommended: Random effects + absolute agreement (ICC(2,%s)).",
        ifelse(rating_type == "single", "1", "k")
      )
    }
  }

  # Return final validation result
  return(result)
}



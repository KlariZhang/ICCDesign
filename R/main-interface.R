# File: main-interface.R
# Description: Top-level user interface functions for ICC analysis
# Author: [Ziyu Liu]
# Date: [2026-04-27]
# Dependencies: icc_preprocess_data, icc_check_design, and core ICC functions

#==============================================================================#
# Function 1: icc_map_design_to_icc
# Core decision mapping: 4 design questions -> ICC function
#==============================================================================#
#' Map Design Parameters to ICC Type
#'
#' @description
#' Maps user's 4 design questions to the corresponding ICC core calculation
#' function, full name, and any necessary warnings or tips.
#'
#' @param same_raters Logical. Are all subjects measured by the same group of raters?
#' @param rater_effect Character. "random" or "fixed". Ignored if \code{same_raters = FALSE}.
#' @param rating_type Character. "single" (single rating) or "average" (average of k ratings).
#' @param agreement_type Character. "absolute" (absolute agreement) or "consistency" (consistency).
#'   Ignored if \code{same_raters = FALSE}.
#'
#' @return A named list containing:
#' \describe{
#'   \item{icc_func_name}{Character. Name of the core ICC calculation function.}
#'   \item{icc_full_name}{Character. Full name of the ICC type.}
#'   \item{icc_code}{Character. ICC code (e.g., "1,1").}
#'   \item{warning_msg}{Character or NULL. Warning message for not recommended combinations.}
#'   \item{tip_msg}{Character or NULL. Tip message for automatically mapped combinations.}
#' }
#'
#' @keywords internal
#' @references
#' McGraw, K. O., & Wong, S. P. (1996). Forming inferences about some
#' intraclass correlation coefficients. Psychological Methods, 1(1), 30-46.
icc_map_design_to_icc <- function(same_raters, rater_effect, rating_type, agreement_type) {
  # Initialize output
  result <- list(
    icc_func_name = NULL,
    icc_full_name = NULL,
    icc_code = NULL,
    warning_msg = NULL,
    tip_msg = NULL
  )

  #----------------------------------------------------------------------------#
  # Branch 1: One-way random effects (different raters per subject)
  #----------------------------------------------------------------------------#
  if (!same_raters) {
    if (rating_type == "single") {
      result$icc_func_name <- "icc_calc_1_1"
      result$icc_full_name <- "ICC(1,1) One-way random, single rating, absolute agreement"
      result$icc_code <- "1,1"
    } else if (rating_type == "average") {
      result$icc_func_name <- "icc_calc_1_k"
      result$icc_full_name <- "ICC(1,k) One-way random, average of k ratings, absolute agreement"
      result$icc_code <- "1,k"
    }
    return(result)
  }

  #----------------------------------------------------------------------------#
  # Branch 2: Two-way models (same raters for all subjects)
  #----------------------------------------------------------------------------#
  if (same_raters) {
    # Case A: Random effects + Absolute agreement
    if (rater_effect == "random" && agreement_type == "absolute") {
      if (rating_type == "single") {
        result$icc_func_name <- "icc_calc_2_1"
        result$icc_full_name <- "ICC(2,1) Two-way random, single rating, absolute agreement"
        result$icc_code <- "2,1"
      } else if (rating_type == "average") {
        result$icc_func_name <- "icc_calc_2_k"
        result$icc_full_name <- "ICC(2,k) Two-way random, average of k ratings, absolute agreement"
        result$icc_code <- "2,k"
      }
    }

    # Case B: Fixed effects + Consistency
    else if (rater_effect == "fixed" && agreement_type == "consistency") {
      if (rating_type == "single") {
        result$icc_func_name <- "icc_calc_3_1"
        result$icc_full_name <- "ICC(3,1) Two-way mixed, single rating, consistency"
        result$icc_code <- "3,1"
      } else if (rating_type == "average") {
        result$icc_func_name <- "icc_calc_3_k"
        result$icc_full_name <- "ICC(3,k) Two-way mixed, average of k ratings, consistency"
        result$icc_code <- "3,k"
      }
    }

    # Case C: Random effects + Consistency (auto-map to ICC(3,1)/ICC(3,k))
    else if (rater_effect == "random" && agreement_type == "consistency") {
      if (rating_type == "single") {
        result$icc_func_name <- "icc_calc_2_1_consistency"
        result$icc_full_name <- "ICC(2,1,consistency) Two-way random, single rating, consistency (mapped to ICC(3,1))"
        result$icc_code <- "2,1,consistency"
      } else if (rating_type == "average") {
        result$icc_func_name <- "icc_calc_2_k_consistency"
        result$icc_full_name <- "ICC(2,k,consistency) Two-way random, average of k ratings, consistency (mapped to ICC(3,k))"
        result$icc_code <- "2,k,consistency"
      }
      result$tip_msg <- paste(
        "Random effects + consistency combination specified.",
        "In practice, this is automatically mapped to the mixed effects model (ICC(3,x)).",
        "See framework documentation for details."
      )
    }

    # Case D: Fixed effects + Absolute agreement (NOT RECOMMENDED)
    else if (rater_effect == "fixed" && agreement_type == "absolute") {
      if (rating_type == "single") {
        result$icc_func_name <- "icc_calc_3_1_absolute"
        result$icc_full_name <- "ICC(3,1,absolute) Two-way mixed, single rating, absolute agreement (NOT RECOMMENDED)"
        result$icc_code <- "3,1,absolute"
      } else if (rating_type == "average") {
        result$icc_func_name <- "icc_calc_3_k_absolute"
        result$icc_full_name <- "ICC(3,k,absolute) Two-way mixed, average of k ratings, absolute agreement (NOT RECOMMENDED)"
        result$icc_code <- "3,k,absolute"
      }
      result$warning_msg <- paste(
        "WARNING: Fixed effects + absolute agreement combination is rarely used",
        "and difficult to interpret. It is strongly recommended to use",
        "the random effects + absolute agreement model (ICC(2,x)) instead.",
        "See framework documentation for details."
      )
    }
  }

  return(result)
}

#==============================================================================#
# Function 2: icc_evaluate
# Evaluate ICC reliability based on Koo & Li (2016) criteria
#==============================================================================#
#' Evaluate ICC Reliability
#'
#' @description
#' Evaluates the reliability of an ICC result based on the 95% confidence
#' interval lower bound, following the criteria of Koo & Li (2016).
#'
#' @param icc_result List. Output from a core ICC calculation function.
#'
#' @return A named list with elements:
#' \itemize{
#'   \item icc_code: Character. ICC code.
#'   \item point_est: Numeric. ICC point estimate.
#'   \item ci_lower: Numeric. 95% CI lower bound.
#'   \item rating_en: Character. English reliability rating.
#'   \item rating_cn: Character. Chinese reliability rating.
#'   \item explanation: Character. Interpretation text.
#' }
#'
#' @keywords internal
#' @references
#' Koo, T. K., & Li, M. Y. (2016). A guideline of selecting and reporting
#' intraclass correlation coefficients for reliability research. Journal of
#' Chiropractic Medicine, 15(2), 155-163.
icc_evaluate <- function(icc_result) {
  # Extract values
  icc_code <- icc_result$icc_code
  point_est <- icc_result$point_est
  ci_lower <- icc_result$ci_lower
  ci_upper <- icc_result$ci_upper

  # Determine rating based on CI lower bound
  if (ci_lower < 0.5) {
    rating_en <- "Poor"
    rating_cn <- "Poor"
  } else if (ci_lower >= 0.5 && ci_lower < 0.75) {
    rating_en <- "Moderate"
    rating_cn <- "Moderate"
  } else if (ci_lower >= 0.75 && ci_lower < 0.9) {
    rating_en <- "Good"
    rating_cn <- "Good"
  } else {
    rating_en <- "Excellent"
    rating_cn <- "Excellent"
  }

  # Build explanation
  explanation <- paste0(
    "Based on the 95% CI lower bound (", sprintf("%.3f", ci_lower), "), ",
    "the reliability is rated as '", rating_en, "' (", rating_cn, "). "
  )

  # Add cross-interval warning if needed
  if (ci_lower < 0.75 && ci_upper >= 0.75) {
    explanation <- paste0(
      explanation,
      "Note: The confidence interval spans multiple rating categories, ",
      "indicating uncertainty in the reliability estimate. ",
      "Consider increasing the sample size for a more precise estimate."
    )
  }

  # Return result
  list(
    icc_code = icc_code,
    point_est = point_est,
    ci_lower = ci_lower,
    rating_en = rating_en,
    rating_cn = rating_cn,
    explanation = explanation
  )
}

#==============================================================================#
# Function 3: icc_generate_report
# Generate standardized publication-ready report
#==============================================================================#
#' Generate ICC Report
#'
#' @description
#' Generates a standardized, publication-ready report of ICC results,
#' supporting text, Markdown, and HTML formats.
#'
#' @param icc_result List. Output from a core ICC calculation function.
#' @param evaluation List. Output from \code{icc_evaluate}.
#' @param format Character. Output format: "text" (default), "markdown", or "html".
#'
#' @return Character. Formatted report text.
#'
#' @keywords internal
icc_generate_report <- function(icc_result, evaluation, format = "text") {
  # Extract all necessary information
  icc_full_name <- icc_result$icc_type
  icc_code <- icc_result$icc_code
  point_est <- icc_result$point_est
  ci_level <- icc_result$ci_level
  ci_lower <- icc_result$ci_lower
  ci_upper <- icc_result$ci_upper
  f_test_null <- icc_result$F_test_null
  rating_en <- evaluation$rating_en
  rating_cn <- evaluation$rating_cn
  explanation <- evaluation$explanation

  # Build report components
  header <- "=== ICC Analysis Report ==="
  model_section <- paste0("Model Type: ", icc_full_name)
  result_section <- paste0(
    "Results:\n",
    "  ICC Point Estimate: ", sprintf("%.4f", point_est), "\n",
    "  ", sprintf("%.0f%%", ci_level * 100), " Confidence Interval: [",
    sprintf("%.4f", ci_lower), ", ", sprintf("%.4f", ci_upper), "]"
  )

  test_section <- if (!is.null(f_test_null)) {
    paste0(
      "Hypothesis Test (H0: ICC = 0):\n",
      "  F-statistic: ", sprintf("%.4f", f_test_null$F_stat), "\n",
      "  DF1: ", f_test_null$df1, ", DF2: ", f_test_null$df2, "\n",
      "  p-value: ", if (!is.null(f_test_null$p_value)) sprintf("%.4f", f_test_null$p_value) else "N/A"
    )
  } else {
    "Hypothesis Test: Not available"
  }

  rating_section <- paste0(
    "Reliability Rating:\n",
    "  Category: ", rating_en, " (", rating_cn, ")\n",
    "  Interpretation: ", explanation
  )

  # Combine based on format
  if (format == "text") {
    report <- paste0(
      header, "\n\n",
      model_section, "\n\n",
      result_section, "\n\n",
      test_section, "\n\n",
      rating_section
    )
  } else if (format == "markdown") {
    report <- paste0(
      "# ICC Analysis Report\n\n",
      "## Model Type\n", icc_full_name, "\n\n",
      "## Results\n",
      "- ICC Point Estimate: ", sprintf("%.4f", point_est), "\n",
      "- ", sprintf("%.0f%%", ci_level * 100), " CI: [",
      sprintf("%.4f", ci_lower), ", ", sprintf("%.4f", ci_upper), "]\n\n",
      "## Hypothesis Test\n",
      "- F-statistic: ", sprintf("%.4f", f_test_null$F_stat), "\n",
      "- DF: (", f_test_null$df1, ", ", f_test_null$df2, ")\n",
      "- p-value: ", if (!is.null(f_test_null$p_value)) sprintf("%.4f", f_test_null$p_value) else "N/A", "\n\n",
      "## Reliability Rating\n",
      "- **", rating_en, "** (", rating_cn, ")\n",
      "- ", explanation
    )
  } else if (format == "html") {
    report <- paste0(
      "<h1>ICC Analysis Report</h1>",
      "<h2>Model Type</h2><p>", icc_full_name, "</p>",
      "<h2>Results</h2>",
      "<ul>",
      "<li>ICC Point Estimate: <strong>", sprintf("%.4f", point_est), "</strong></li>",
      "<li>", sprintf("%.0f%%", ci_level * 100), " CI: [",
      sprintf("%.4f", ci_lower), ", ", sprintf("%.4f", ci_upper), "]</li>",
      "</ul>",
      "<h2>Hypothesis Test</h2>",
      "<ul>",
      "<li>F-statistic: ", sprintf("%.4f", f_test_null$F_stat), "</li>",
      "<li>DF: (", f_test_null$df1, ", ", f_test_null$df2, ")</li>",
      "<li>p-value: ", if (!is.null(f_test_null$p_value)) sprintf("%.4f", f_test_null$p_value) else "N/A", "</li>",
      "</ul>",
      "<h2>Reliability Rating</h2>",
      "<p><strong>", rating_en, "</strong> (", rating_cn, ")</p>",
      "<p>", explanation, "</p>"
    )
  }

  return(report)
}

#==============================================================================#
# Function 4: icc_calc (TOP-LEVEL MAIN FUNCTION)
# Complete end-to-end ICC analysis workflow
#==============================================================================#
#' Calculate Intraclass Correlation Coefficient (ICC)
#'
#' @description
#' Top-level main function for complete ICC analysis. Users only need to
#' provide raw data and answer 4 design questions. The function automatically
#' handles data preprocessing, parameter validation, ICC type mapping,
#' core calculation, reliability evaluation, and report generation.
#'
#' @param data Data frame or matrix. Raw data where rows = subjects,
#'   columns = raters/measurements.
#' @param same_raters Logical. Are all subjects measured by the same group
#'   of raters?
#' @param rater_effect Character. "random" or "fixed". Ignored if
#'   \code{same_raters = FALSE}.
#' @param rating_type Character. "single" (single rating) or "average"
#'   (average of k ratings).
#' @param agreement_type Character. "absolute" (absolute agreement) or
#'   "consistency" (consistency). Ignored if \code{same_raters = FALSE}.
#' @param alpha Numeric. Significance level for confidence interval,
#'   default 0.05.
#' @param rho0 Numeric. Optional null hypothesis value for non-zero test,
#'   default NULL.
#' @param interaction Logical. Whether to include interaction term in
#'   two-way models, default TRUE.
#' @param na.rm Logical. Whether to automatically remove rows with missing
#'   values, default TRUE.
#' @param verbose Logical. Whether to automatically print warnings, tips,
#'   and the report to the console, default TRUE.
#'
#' @return A named list containing:
#' \describe{
#'   \item{data_summary}{List. Data preprocessing summary.}
#'   \item{icc_result}{List. Full ICC calculation results.}
#'   \item{evaluation}{List. Reliability evaluation results.}
#'   \item{report}{Character. Standardized report text.}
#'   \item{warning_msg}{Character or NULL. Warning message.}
#'   \item{tip_msg}{Character or NULL. Tip message.}
#' }
#'
#' @export
#' @references
#' McGraw, K. O., & Wong, S. P. (1996). Forming inferences about some
#' intraclass correlation coefficients. Psychological Methods, 1(1), 30-46.
#'
#' Koo, T. K., & Li, M. Y. (2016). A guideline of selecting and reporting
#' intraclass correlation coefficients for reliability research. Journal of
#' Chiropractic Medicine, 15(2), 155-163.
#'
#' @examples
#' \dontrun{
#' # Example 1: One-way random effects, single rating
#' data <- matrix(rnorm(100), nrow = 20, ncol = 5)
#' result <- icc_calc(data, same_raters = FALSE, rating_type = "single")
#'
#' # Example 2: Two-way random effects, average rating, absolute agreement
#' result <- icc_calc(data, same_raters = TRUE, rater_effect = "random",
#'                    rating_type = "average", agreement_type = "absolute")
#' }
icc_calc <- function(data, same_raters, rater_effect = NULL,
                     rating_type, agreement_type = NULL,
                     alpha = 0.05, rho0 = NULL, interaction = TRUE,
                     na.rm = TRUE, verbose = TRUE) {
  #----------------------------------------------------------------------------#
  # Step 1: Data Preprocessing
  #----------------------------------------------------------------------------#
  data_summary <- icc_preprocess_data(data, na.rm = na.rm)
  data_matrix <- data_summary$data_matrix

  #----------------------------------------------------------------------------#
  # Step 2: Design Parameter Validation
  #----------------------------------------------------------------------------#
  design_check <- icc_check_design(
    same_raters = same_raters,
    rater_effect = rater_effect,
    rating_type = rating_type,
    agreement_type = agreement_type,
    k = data_summary$k
  )

  #----------------------------------------------------------------------------#
  # Step 3: Map Design to ICC Function
  #----------------------------------------------------------------------------#
  mapping <- icc_map_design_to_icc(
    same_raters = same_raters,
    rater_effect = rater_effect,
    rating_type = rating_type,
    agreement_type = agreement_type
  )

  warning_msg <- mapping$warning_msg
  tip_msg <- mapping$tip_msg

  #----------------------------------------------------------------------------#
  # Step 4: Core ICC Calculation
  #----------------------------------------------------------------------------#
  # Dynamically get the function
  icc_func <- get(mapping$icc_func_name, envir = asNamespace("ICCDesign"))

  # Call the function
  icc_result <- icc_func(
    data_matrix = data_matrix,
    alpha = alpha,
    rho0 = rho0,
    interaction = interaction
  )

  #----------------------------------------------------------------------------#
  # Step 5: Reliability Evaluation
  #----------------------------------------------------------------------------#
  evaluation <- icc_evaluate(icc_result)

  #----------------------------------------------------------------------------#
  # Step 6: Generate Report
  #----------------------------------------------------------------------------#
  report <- icc_generate_report(icc_result, evaluation, format = "text")

  #----------------------------------------------------------------------------#
  # Step 7: Console Output (if verbose = TRUE)
  #----------------------------------------------------------------------------#
  if (verbose) {
    # Print warning if exists
    if (!is.null(warning_msg)) {
      message("WARNING: ", warning_msg)
      message()
    }

    # Print tip if exists
    if (!is.null(tip_msg)) {
      message("NOTE: ", tip_msg)
      message()
    }

    # Print report
    cat(report)
  }

  #----------------------------------------------------------------------------#
  # Step 8: Return Complete Result
  #----------------------------------------------------------------------------#
  result <- list(
    data_summary = data_summary,
    icc_result = icc_result,
    evaluation = evaluation,
    report = report,
    warning_msg = warning_msg,
    tip_msg = tip_msg
  )

  # Set class for pretty printing
  class(result) <- "icc_result"

  return(result)
}

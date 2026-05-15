# File: core-icc.R
# Description: Core calculation functions for all 10 ICC types
#              (6 common + 4 supplementary)
# Author: [Ziyu Liu]
# Date: [20260421]
# Dependencies: All functions rely on internal utility functions from
#               utils-data.R and utils-calc.R

#==============================================================================#
# Helper: Internal function to assemble standardized output (DRY principle)
# Not exported, only used within this file
#==============================================================================#
.assemble_icc_output <- function(icc_type_full, icc_type_code,
                                 anova_res, point_est, ci_res,
                                 f_test_null, f_test_rho0 = NULL,
                                 warning_msg = NULL, tip_msg = NULL) {
  list(
    icc_type = icc_type_full,
    icc_code = icc_type_code,
    point_est = point_est,
    ci_level = ci_res$ci_level,
    ci_lower = ci_res$ci_lower,
    ci_upper = ci_res$ci_upper,
    F_test_null = f_test_null,
    F_test_rho0 = f_test_rho0,
    anova_summary = anova_res[c("MSR", "MSC", "MSE", "MSW", "df1", "df2", "df3", "n", "k")],
    warning_msg = warning_msg,
    tip_msg = tip_msg
  )
}

#==============================================================================#
# ICC(1,1): One-way random effects, single rating, absolute agreement
#==============================================================================#
#' Calculate ICC(1,1)
#'
#' @description
#' Calculates the Intraclass Correlation Coefficient (ICC) for a one-way
#' random effects model using a single rater/measurement, focusing on
#' absolute agreement.
#'
#' @param data_matrix A standardized numeric matrix from \code{icc_preprocess_data}.
#'   Rows = subjects, columns = raters/measurements.
#' @param alpha Significance level for confidence interval, default 0.05.
#' @param rho0 Optional null hypothesis value for non-zero test, default NULL.
#' @param interaction Logical, whether to include interaction term (ignored for one-way).
#'
#' @return A standardized list containing ICC results, see package documentation for details.
#' @keywords internal
#' @references
#' McGraw, K. O., & Wong, S. P. (1996). Forming inferences about some
#' intraclass correlation coefficients. Psychological Methods, 1(1), 30-46.
icc_calc_1_1 <- function(data_matrix, alpha = 0.05, rho0 = NULL, interaction = TRUE) {
  # 1. Calculate one-way ANOVA
  anova_res <- icc_calc_anova(data_matrix, model_type = "oneway")

  # 2. Calculate point estimate
  point_est <- icc_tool_point(anova_res, icc_type = "1,1")

  # 3. Calculate confidence interval
  ci_res <- icc_tool_ci(anova_res, icc_type = "1,1", point_est = point_est, alpha = alpha)

  # 4. Calculate F-test for H0: ICC = 0
  f_test_null <- icc_calc_f_test(anova_res, icc_type = "1,1", rho0 = 0, alpha = alpha)

  # 5. Calculate F-test for H0: ICC = rho0 (if provided)
  f_test_rho0 <- if (!is.null(rho0)) {
    icc_calc_f_test(anova_res, icc_type = "1,1", rho0 = rho0, alpha = alpha)
  } else {
    NULL
  }

  # 6. Assemble and return standardized output
  .assemble_icc_output(
    icc_type_full = "ICC(1,1) One-way random, single rating, absolute agreement",
    icc_type_code = "1,1",
    anova_res = anova_res,
    point_est = point_est,
    ci_res = ci_res,
    f_test_null = f_test_null,
    f_test_rho0 = f_test_rho0,
    warning_msg = NULL,
    tip_msg = NULL
  )
}

#==============================================================================#
# ICC(1,k): One-way random effects, average of k ratings, absolute agreement
#==============================================================================#
#' Calculate ICC(1,k)
#'
#' @description
#' Calculates the Intraclass Correlation Coefficient (ICC) for a one-way
#' random effects model using the average of k raters/measurements, focusing
#' on absolute agreement.
#'
#' @inheritParams icc_calc_1_1
#' @return A standardized list containing ICC results.
#' @keywords internal
#' @references
#' McGraw, K. O., & Wong, S. P. (1996).
icc_calc_1_k <- function(data_matrix, alpha = 0.05, rho0 = NULL, interaction = TRUE) {
  anova_res <- icc_calc_anova(data_matrix, model_type = "oneway")
  point_est <- icc_tool_point(anova_res, icc_type = "1,k")
  ci_res <- icc_tool_ci(anova_res, icc_type = "1,k", point_est = point_est, alpha = alpha)
  f_test_null <- icc_calc_f_test(anova_res, icc_type = "1,k", rho0 = 0, alpha = alpha)
  f_test_rho0 <- if (!is.null(rho0)) icc_calc_f_test(anova_res, icc_type = "1,k", rho0 = rho0, alpha = alpha) else NULL

  .assemble_icc_output(
    icc_type_full = "ICC(1,k) One-way random, average of k ratings, absolute agreement",
    icc_type_code = "1,k",
    anova_res = anova_res,
    point_est = point_est,
    ci_res = ci_res,
    f_test_null = f_test_null,
    f_test_rho0 = f_test_rho0,
    warning_msg = NULL,
    tip_msg = NULL
  )
}

#==============================================================================#
# ICC(2,1): Two-way random effects, single rating, absolute agreement
#==============================================================================#
#' Calculate ICC(2,1)
#'
#' @description
#' Calculates the Intraclass Correlation Coefficient (ICC) for a two-way
#' random effects model using a single rater, focusing on absolute agreement.
#'
#' @inheritParams icc_calc_1_1
#' @return A standardized list containing ICC results.
#' @keywords internal
#' @references
#' McGraw, K. O., & Wong, S. P. (1996).
icc_calc_2_1 <- function(data_matrix, alpha = 0.05, rho0 = NULL, interaction = TRUE) {
  anova_res <- icc_calc_anova(data_matrix, model_type = "twoway", interaction = interaction)
  point_est <- icc_tool_point(anova_res, icc_type = "2,1")
  ci_res <- icc_tool_ci(anova_res, icc_type = "2,1", point_est = point_est, alpha = alpha)
  f_test_null <- icc_calc_f_test(anova_res, icc_type = "2,1", rho0 = 0, alpha = alpha)
  f_test_rho0 <- if (!is.null(rho0)) icc_calc_f_test(anova_res, icc_type = "2,1", rho0 = rho0, alpha = alpha) else NULL

  .assemble_icc_output(
    icc_type_full = "ICC(2,1) Two-way random, single rating, absolute agreement",
    icc_type_code = "2,1",
    anova_res = anova_res,
    point_est = point_est,
    ci_res = ci_res,
    f_test_null = f_test_null,
    f_test_rho0 = f_test_rho0,
    warning_msg = NULL,
    tip_msg = NULL
  )
}

#==============================================================================#
# ICC(2,k): Two-way random effects, average of k ratings, absolute agreement
#==============================================================================#
#' Calculate ICC(2,k)
#'
#' @description
#' Calculates the Intraclass Correlation Coefficient (ICC) for a two-way
#' random effects model using the average of k raters, focusing on absolute agreement.
#'
#' @inheritParams icc_calc_1_1
#' @return A standardized list containing ICC results.
#' @keywords internal
#' @references
#' McGraw, K. O., & Wong, S. P. (1996).
icc_calc_2_k <- function(data_matrix, alpha = 0.05, rho0 = NULL, interaction = TRUE) {
  anova_res <- icc_calc_anova(data_matrix, model_type = "twoway", interaction = interaction)
  point_est <- icc_tool_point(anova_res, icc_type = "2,k")
  ci_res <- icc_tool_ci(anova_res, icc_type = "2,k", point_est = point_est, alpha = alpha)
  f_test_null <- icc_calc_f_test(anova_res, icc_type = "2,k", rho0 = 0, alpha = alpha)
  f_test_rho0 <- if (!is.null(rho0)) icc_calc_f_test(anova_res, icc_type = "2,k", rho0 = rho0, alpha = alpha) else NULL

  .assemble_icc_output(
    icc_type_full = "ICC(2,k) Two-way random, average of k ratings, absolute agreement",
    icc_type_code = "2,k",
    anova_res = anova_res,
    point_est = point_est,
    ci_res = ci_res,
    f_test_null = f_test_null,
    f_test_rho0 = f_test_rho0,
    warning_msg = NULL,
    tip_msg = NULL
  )
}

#==============================================================================#
# ICC(3,1): Two-way mixed effects, single rating, consistency
#==============================================================================#
#' Calculate ICC(3,1)
#'
#' @description
#' Calculates the Intraclass Correlation Coefficient (ICC) for a two-way
#' mixed effects model using a single rater, focusing on consistency.
#'
#' @inheritParams icc_calc_1_1
#' @return A standardized list containing ICC results.
#' @keywords internal
#' @references
#' McGraw, K. O., & Wong, S. P. (1996).
icc_calc_3_1 <- function(data_matrix, alpha = 0.05, rho0 = NULL, interaction = TRUE) {
  anova_res <- icc_calc_anova(data_matrix, model_type = "twoway", interaction = interaction)
  point_est <- icc_tool_point(anova_res, icc_type = "3,1")
  ci_res <- icc_tool_ci(anova_res, icc_type = "3,1", point_est = point_est, alpha = alpha)
  f_test_null <- icc_calc_f_test(anova_res, icc_type = "3,1", rho0 = 0, alpha = alpha)
  f_test_rho0 <- if (!is.null(rho0)) icc_calc_f_test(anova_res, icc_type = "3,1", rho0 = rho0, alpha = alpha) else NULL

  .assemble_icc_output(
    icc_type_full = "ICC(3,1) Two-way mixed, single rating, consistency",
    icc_type_code = "3,1",
    anova_res = anova_res,
    point_est = point_est,
    ci_res = ci_res,
    f_test_null = f_test_null,
    f_test_rho0 = f_test_rho0,
    warning_msg = NULL,
    tip_msg = NULL
  )
}

#==============================================================================#
# ICC(3,k): Two-way mixed effects, average of k ratings, consistency
#==============================================================================#
#' Calculate ICC(3,k)
#'
#' @description
#' Calculates the Intraclass Correlation Coefficient (ICC) for a two-way
#' mixed effects model using the average of k raters, focusing on consistency.
#'
#' @inheritParams icc_calc_1_1
#' @return A standardized list containing ICC results.
#' @keywords internal
#' @references
#' McGraw, K. O., & Wong, S. P. (1996).
icc_calc_3_k <- function(data_matrix, alpha = 0.05, rho0 = NULL, interaction = TRUE) {
  anova_res <- icc_calc_anova(data_matrix, model_type = "twoway", interaction = interaction)
  point_est <- icc_tool_point(anova_res, icc_type = "3,k")
  ci_res <- icc_tool_ci(anova_res, icc_type = "3,k", point_est = point_est, alpha = alpha)
  f_test_null <- icc_calc_f_test(anova_res, icc_type = "3,k", rho0 = 0, alpha = alpha)
  f_test_rho0 <- if (!is.null(rho0)) icc_calc_f_test(anova_res, icc_type = "3,k", rho0 = rho0, alpha = alpha) else NULL

  .assemble_icc_output(
    icc_type_full = "ICC(3,k) Two-way mixed, average of k ratings, consistency",
    icc_type_code = "3,k",
    anova_res = anova_res,
    point_est = point_est,
    ci_res = ci_res,
    f_test_null = f_test_null,
    f_test_rho0 = f_test_rho0,
    warning_msg = NULL,
    tip_msg = NULL
  )
}

#==============================================================================#
# Supplementary ICC: ICC(2,1,consistency)
# Maps to ICC(3,1) in practice with a tip message
#==============================================================================#
#' Calculate ICC(2,1,consistency) [Supplementary]
#'
#' @description
#' Calculates ICC for two-way random effects, single rating, consistency.
#' Note: In practice, this maps to ICC(3,1) (mixed effects model).
#'
#' @inheritParams icc_calc_1_1
#' @return A standardized list containing ICC results.
#' @keywords internal
#' @references
#' McGraw, K. O., & Wong, S. P. (1996).
icc_calc_2_1_consistency <- function(data_matrix, alpha = 0.05, rho0 = NULL, interaction = TRUE) {
  anova_res <- icc_calc_anova(data_matrix, model_type = "twoway", interaction = interaction)
  point_est <- icc_tool_point(anova_res, icc_type = "3,1") # Use same formula as ICC(3,1)
  ci_res <- icc_tool_ci(anova_res, icc_type = "3,1", point_est = point_est, alpha = alpha)
  f_test_null <- icc_calc_f_test(anova_res, icc_type = "3,1", rho0 = 0, alpha = alpha)
  f_test_rho0 <- if (!is.null(rho0)) icc_calc_f_test(anova_res, icc_type = "3,1", rho0 = rho0, alpha = alpha) else NULL

  tip_msg <- paste(
    "Random effects + consistency combination specified.",
    "In practice, this is automatically mapped to ICC(3,1) (mixed effects model).",
    "See framework documentation for details."
  )

  .assemble_icc_output(
    icc_type_full = "ICC(2,1,consistency) Two-way random, single rating, consistency (mapped to ICC(3,1))",
    icc_type_code = "2,1,consistency",
    anova_res = anova_res,
    point_est = point_est,
    ci_res = ci_res,
    f_test_null = f_test_null,
    f_test_rho0 = f_test_rho0,
    warning_msg = NULL,
    tip_msg = tip_msg
  )
}

#==============================================================================#
# Supplementary ICC: ICC(2,k,consistency)
# Maps to ICC(3,k) in practice with a tip message
#==============================================================================#
#' Calculate ICC(2,k,consistency) [Supplementary]
#'
#' @description
#' Calculates ICC for two-way random effects, average of k ratings, consistency.
#' Note: In practice, this maps to ICC(3,k) (mixed effects model).
#'
#' @inheritParams icc_calc_1_1
#' @return A standardized list containing ICC results.
#' @keywords internal
#' @references
#' McGraw, K. O., & Wong, S. P. (1996).
icc_calc_2_k_consistency <- function(data_matrix, alpha = 0.05, rho0 = NULL, interaction = TRUE) {
  anova_res <- icc_calc_anova(data_matrix, model_type = "twoway", interaction = interaction)
  point_est <- icc_tool_point(anova_res, icc_type = "3,k")
  ci_res <- icc_tool_ci(anova_res, icc_type = "3,k", point_est = point_est, alpha = alpha)
  f_test_null <- icc_calc_f_test(anova_res, icc_type = "3,k", rho0 = 0, alpha = alpha)
  f_test_rho0 <- if (!is.null(rho0)) icc_calc_f_test(anova_res, icc_type = "3,k", rho0 = rho0, alpha = alpha) else NULL

  tip_msg <- paste(
    "Random effects + consistency combination specified.",
    "In practice, this is automatically mapped to ICC(3,k) (mixed effects model).",
    "See framework documentation for details."
  )

  .assemble_icc_output(
    icc_type_full = "ICC(2,k,consistency) Two-way random, average of k ratings, consistency (mapped to ICC(3,k))",
    icc_type_code = "2,k,consistency",
    anova_res = anova_res,
    point_est = point_est,
    ci_res = ci_res,
    f_test_null = f_test_null,
    f_test_rho0 = f_test_rho0,
    warning_msg = NULL,
    tip_msg = tip_msg
  )
}

#==============================================================================#
# Supplementary ICC: ICC(3,1,absolute)
# Not recommended, provides a warning message
#==============================================================================#
#' Calculate ICC(3,1,absolute) [Supplementary]
#'
#' @description
#' Calculates ICC for two-way mixed effects, single rating, absolute agreement.
#' Note: This combination is rarely used and not recommended.
#'
#' @inheritParams icc_calc_1_1
#' @return A standardized list containing ICC results.
#' @keywords internal
#' @references
#' McGraw, K. O., & Wong, S. P. (1996).
icc_calc_3_1_absolute <- function(data_matrix, alpha = 0.05, rho0 = NULL, interaction = TRUE) {
  anova_res <- icc_calc_anova(data_matrix, model_type = "twoway", interaction = interaction)
  point_est <- icc_tool_point(anova_res, icc_type = "2,1") # Use same formula as ICC(2,1)
  ci_res <- icc_tool_ci(anova_res, icc_type = "2,1", point_est = point_est, alpha = alpha)
  f_test_null <- icc_calc_f_test(anova_res, icc_type = "2,1", rho0 = 0, alpha = alpha)
  f_test_rho0 <- if (!is.null(rho0)) icc_calc_f_test(anova_res, icc_type = "2,1", rho0 = rho0, alpha = alpha) else NULL

  warning_msg <- paste(
    "WARNING: Fixed effects + absolute agreement combination is rarely used",
    "and difficult to interpret. It is strongly recommended to use",
    "ICC(2,1) (random effects + absolute agreement) instead.",
    "See framework documentation for details."
  )

  .assemble_icc_output(
    icc_type_full = "ICC(3,1,absolute) Two-way mixed, single rating, absolute agreement (NOT RECOMMENDED)",
    icc_type_code = "3,1,absolute",
    anova_res = anova_res,
    point_est = point_est,
    ci_res = ci_res,
    f_test_null = f_test_null,
    f_test_rho0 = f_test_rho0,
    warning_msg = warning_msg,
    tip_msg = NULL
  )
}

#==============================================================================#
# Supplementary ICC: ICC(3,k,absolute)
# Not recommended, provides a warning message
#==============================================================================#
#' Calculate ICC(3,k,absolute) [Supplementary]
#'
#' @description
#' Calculates ICC for two-way mixed effects, average of k ratings, absolute agreement.
#' Note: This combination is rarely used and not recommended.
#'
#' @inheritParams icc_calc_1_1
#' @return A standardized list containing ICC results.
#' @keywords internal
#' @references
#' McGraw, K. O., & Wong, S. P. (1996).
icc_calc_3_k_absolute <- function(data_matrix, alpha = 0.05, rho0 = NULL, interaction = TRUE) {
  anova_res <- icc_calc_anova(data_matrix, model_type = "twoway", interaction = interaction)
  point_est <- icc_tool_point(anova_res, icc_type = "2,k")
  ci_res <- icc_tool_ci(anova_res, icc_type = "2,k", point_est = point_est, alpha = alpha)
  f_test_null <- icc_calc_f_test(anova_res, icc_type = "2,k", rho0 = 0, alpha = alpha)
  f_test_rho0 <- if (!is.null(rho0)) icc_calc_f_test(anova_res, icc_type = "2,k", rho0 = rho0, alpha = alpha) else NULL

  warning_msg <- paste(
    "WARNING: Fixed effects + absolute agreement combination is rarely used",
    "and difficult to interpret. It is strongly recommended to use",
    "ICC(2,k) (random effects + absolute agreement) instead.",
    "See framework documentation for details."
  )

  .assemble_icc_output(
    icc_type_full = "ICC(3,k,absolute) Two-way mixed, average of k ratings, absolute agreement (NOT RECOMMENDED)",
    icc_type_code = "3,k,absolute",
    anova_res = anova_res,
    point_est = point_est,
    ci_res = ci_res,
    f_test_null = f_test_null,
    f_test_rho0 = f_test_rho0,
    warning_msg = warning_msg,
    tip_msg = NULL
  )
}

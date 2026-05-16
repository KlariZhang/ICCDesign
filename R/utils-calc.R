# File: utils-calc.R
# Description: Core ANOVA calculation for all ICC models (one-way & two-way)
# Author: [Clare Gao]
# Date: 2026-5-6
# Dependencies: base R (no additional packages required)

#==============================================================================#
# Function 1: icc_calc_anova
# Core ANOVA computation for ICC: unified one-way/two-way calculation
#==============================================================================#
#' Calculate ANOVA for ICC Models
#'
#' @description
#' Unified function to compute mean squares and degrees of freedom for
#' one-way random and two-way random/mixed ANOVA models. Serves as the
#' foundational calculation for all ICC types, eliminating redundant code.
#'
#' @param data_matrix Numeric matrix. Standardized data matrix from \code{icc_preprocess_data}.
#' @param model_type Character. Model type: "oneway" (one-way random) or "twoway" (two-way).
#' @param interaction Logical. Default \code{TRUE}. Whether to include subject×rater interaction
#'   term for two-way models (follows standard literature settings). If FALSE, rater effect
#'   is pooled into the residual term.
#'
#' @return A named list containing ANOVA results:
#' \describe{
#'   \item{MSR}{Numeric. Mean square for subjects (rows).}
#'   \item{MSC}{Numeric. Mean square for raters (columns) (NULL for one-way model).}
#'   \item{MSE}{Numeric. Residual mean square (two-way model only).}
#'   \item{MSW}{Numeric. Within-group mean square (one-way model only).}
#'   \item{df1}{Integer. Degrees of freedom for subjects (n-1).}
#'   \item{df2}{Integer. Residual degrees of freedom.}
#'   \item{df3}{Integer. Degrees of freedom for raters (k-1) (NULL for one-way model).}
#'   \item{n}{Integer. Number of subjects.}
#'   \item{k}{Integer. Number of raters.}
#' }
#'
#' @keywords internal
icc_calc_anova <- function(data_matrix, model_type, interaction = TRUE) {
  # Initialize output list
  result <- list(
    MSR = NULL,
    MSC = NULL,
    MSE = NULL,
    MSW = NULL,
    df1 = NULL,
    df2 = NULL,
    df3 = NULL,
    n = NULL,
    k = NULL
  )

  #----------------------------------------------------------------------------#
  # Step 1: Input validation
  #----------------------------------------------------------------------------#
  # Validate data matrix
  if (!is.matrix(data_matrix) || !is.numeric(data_matrix)) {
    stop("data_matrix must be a numeric matrix (output from icc_preprocess_data)")
  }
  n <- nrow(data_matrix)
  k <- ncol(data_matrix)
  if (n < 2 || k < 2) stop("Data requires at least 2 subjects and 2 raters")

  # Validate model type
  if (!model_type %in% c("oneway", "twoway")) {
    stop("model_type must be 'oneway' or 'twoway'")
  }

  # Store sample sizes
  result$n <- n
  result$k <- k

  #----------------------------------------------------------------------------#
  # Step 2: One-way random ANOVA model (ICC(1,1), ICC(1,k))
  #----------------------------------------------------------------------------#
  if (model_type == "oneway") {
    # Grand mean
    grand_mean <- mean(data_matrix, na.rm = TRUE)

    # Sum of squares
    ss_r <- sum(k * (rowMeans(data_matrix, na.rm = TRUE) - grand_mean)^2)
    ss_w <- sum((data_matrix - rowMeans(data_matrix, na.rm = TRUE))^2)

    # Degrees of freedom
    df1 <- n - 1  # Subjects
    df2 <- n * (k - 1)  # Within groups

    # Mean squares
    ms_r <- ss_r / df1
    ms_w <- ss_w / df2

    # Assign results (one-way: MSC, df3 = NULL)
    result$MSR <- ms_r
    result$MSW <- ms_w
    result$df1 <- df1
    result$df2 <- df2
  }

  #----------------------------------------------------------------------------#
  # Step 3: Two-way ANOVA model (all other ICC types)
  #----------------------------------------------------------------------------#
  if (model_type == "twoway") {
    # Grand mean & marginal means
    grand_mean <- mean(data_matrix, na.rm = TRUE)
    row_means <- rowMeans(data_matrix, na.rm = TRUE)
    col_means <- colMeans(data_matrix, na.rm = TRUE)

    # Sum of squares
    ss_r <- sum(k * (row_means - grand_mean)^2)  # Subjects
    ss_c <- sum(n * (col_means - grand_mean)^2)  # Raters
    ss_e <- sum((data_matrix - row_means - col_means + grand_mean)^2)  # Residual/Interaction

    # Degrees of freedom
    df1 <- n - 1    # Subjects
    df3 <- k - 1    # Raters

    # Handle interaction term
    if (interaction) {
      df2 <- (n - 1) * (k - 1)  # Residual df (with interaction)
    } else {
      # Merge rater effect into residual if interaction = FALSE
      # This is equivalent to assuming no rater effect variance
      ss_e <- ss_e + ss_c
      df2 <- (n - 1) * (k - 1) + (k - 1)
      ss_c <- 0
      df3 <- 0
    }

    # Mean squares
    ms_r <- ss_r / df1
    ms_c <- ifelse(df3 == 0, 0, ss_c / df3)
    ms_e <- ss_e / df2

    # Assign results (two-way: MSW = NULL)
    result$MSR <- ms_r
    result$MSC <- ms_c
    result$MSE <- ms_e
    result$df1 <- df1
    result$df2 <- df2
    result$df3 <- df3
  }

  return(result)
}


#==============================================================================#
# Function 2: icc_tool_point
# Unified point estimate for 10 ICC types (Table 4/5, McGraw & Wong 1996)
#==============================================================================#
#' Calculate ICC Point Estimate
#'
#' @description
#' Unified low-level function to compute point estimates for all 10 ICC types
#' based on the standard formulas from McGraw & Wong (1996) Table 4 and 5.
#' Serves as the single source of truth for ICC point calculations.
#'
#' @param anova_result List. Output from \code{icc_calc_anova} containing ANOVA statistics.
#' @param icc_type Character. ICC type code, must be one of:
#'   "1,1", "1,k", "2,1", "2,k", "3,1", "3,k",
#'   "2,1,consistency", "2,k,consistency", "3,1,absolute", "3,k,absolute"
#'
#' @return Numeric. Point estimate of the intraclass correlation coefficient.
#'
#' @keywords internal
#' @references
#' McGraw, K. O., & Wong, S. P. (1996). Forming inferences about some
#' intraclass correlation coefficients. Psychological Methods, 1(1), 30-46.
icc_tool_point <- function(anova_result, icc_type) {
  #----------------------------------------------------------------------------#
  # Step 1: Input validation
  #----------------------------------------------------------------------------#
  # Validate ANOVA result input
  if (!is.list(anova_result)) {
    stop("anova_result must be the output list from icc_calc_anova()")
  }
  required_elements <- c("MSR", "MSW", "MSC", "MSE", "n", "k")
  if (!all(required_elements %in% names(anova_result))) {
    stop("anova_result is missing required ANOVA components")
  }

  # Validate ICC type
  valid_icc_types <- c(
    "1,1", "1,k", "2,1", "2,k", "3,1", "3,k",
    "2,1,consistency", "2,k,consistency",
    "3,1,absolute", "3,k,absolute"
  )
  if (!icc_type %in% valid_icc_types) {
    stop(sprintf("icc_type must be one of: %s", paste(valid_icc_types, collapse = ", ")))
  }

  #----------------------------------------------------------------------------#
  # Step 2: Extract key ANOVA values
  #----------------------------------------------------------------------------#
  MSR <- anova_result$MSR
  MSW <- anova_result$MSW
  MSC <- anova_result$MSC
  MSE <- anova_result$MSE
  n   <- anova_result$n
  k   <- anova_result$k


  # Prevent potential errors when MSC is NULL (e.g., two-way model with interaction = FALSE)
  MSC <- ifelse(is.null(MSC), 0, MSC)

  # For Type C ICC (consistency), MSW may be NULL in two-way model; use MSE as fallback
  if (icc_type %in% c("3,1","3,k","2,1,consistency","2,k,consistency")) {
    if (is.null(MSW)) MSW <- MSE
  }

  #----------------------------------------------------------------------------#
  # Step 3: Calculate point estimate by ICC type (100% match Table 4/5 1996)
  #----------------------------------------------------------------------------#
  icc_point <- switch(
    EXPR = icc_type,
    # One-way models
    "1,1" = (MSR - MSW) / (MSR + (k - 1) * MSW),
    "1,k" = (MSR - MSW) / MSR,

    # Two-way random - absolute agreement
    "2,1" = (MSR - MSE) / (MSR + (k - 1) * MSE + k * (MSC - MSE) / n),
    "2,k" = (MSR - MSE) / (MSR + (MSC - MSE) / n),

    # Two-way mixed - consistency
    "3,1" = (MSR - MSE) / (MSR + (k - 1) * MSE),
    "3,k" = (MSR - MSE) / MSR,

    # Random + consistency (mathematically equivalent to 3,1 / 3,k)
    "2,1,consistency" = (MSR - MSE) / (MSR + (k - 1) * MSE),
    "2,k,consistency" = (MSR - MSE) / MSR,

    # Mixed + absolute agreement (mathematically equivalent to 2,1 / 2,k)
    "3,1,absolute" = (MSR - MSE) / (MSR + (k - 1) * MSE + k * (MSC - MSE) / n),
    "3,k,absolute" = (MSR - MSE) / (MSR + (MSC - MSE) / n)
  )

  #----------------------------------------------------------------------------#
  # Step 4: Return final point estimate
  #----------------------------------------------------------------------------#
  return(icc_point)
}

#==============================================================================#
# Function 3: icc_tool_ci
# Exact F-distribution confidence intervals for ICC (with Satterthwaite correction)
#==============================================================================#
#' Calculate ICC Confidence Intervals
#'
#' @description
#' Computes confidence intervals for all 10 ICC types using exact F-distribution
#' method from McGraw & Wong (1996) Table 7. Implements Satterthwaite degrees of
#' freedom correction for Type A ICC models (ICC(2,1), ICC(2,k) and their variants).
#'
#' @param anova_result List. Output from \code{icc_calc_anova}.
#' @param icc_type Character. ICC type code (matches icc_tool_point).
#' @param point_est Numeric. Point estimate from \code{icc_tool_point}.
#' @param alpha Numeric. Significance level, default 0.05 (95\% CI).
#'
#' @return A named list with CI results:
#' \describe{
#'   \item{ci_level}{Numeric. Confidence level (1 - alpha).}
#'   \item{ci_lower}{Numeric. Lower bound of CI (truncated to 0).}
#'   \item{ci_upper}{Numeric. Upper bound of CI (truncated to 1).}
#'   \item{df_corrected}{Numeric or NULL. Satterthwaite-corrected df (Type A only).}
#' }
#'
#' @keywords internal
#' @references
#' McGraw, K. O., & Wong, S. P. (1996). Forming inferences about some
#' intraclass correlation coefficients. Psychological Methods, 1(1), 30-46.
icc_tool_ci <- function(anova_result, icc_type, point_est, alpha = 0.05) {
  # Initialize output
  result <- list(
    ci_level = 1 - alpha,
    ci_lower = NULL,
    ci_upper = NULL,
    df_corrected = NULL
  )

  # Input validation
  valid_icc <- c("1,1","1,k","2,1","2,k","3,1","3,k","2,1,consistency","2,k,consistency","3,1,absolute","3,k,absolute")
  if (!icc_type %in% valid_icc) stop("Invalid icc_type")
  if (!is.list(anova_result)) stop("Invalid anova_result")

  # Extract parameters
  MSR <- anova_result$MSR
  MSW <- anova_result$MSW
  MSC <- anova_result$MSC
  MSE <- anova_result$MSE
  n <- anova_result$n
  k <- anova_result$k
  df1 <- anova_result$df1
  df2 <- anova_result$df2
  df3 <- anova_result$df3

  # Prevent potential errors when MSC is NULL (e.g., two-way model with interaction = FALSE)
  MSC <- ifelse(is.null(MSC), 0, MSC)

  # For Type C ICC (consistency), MSW may be NULL in two-way model; use MSE as fallback
  if (icc_type %in% c("3,1","3,k","2,1,consistency","2,k,consistency")) {
    if (is.null(MSW)) MSW <- MSE
  }

  # F quantiles (two-tailed)
  F_lower <- stats::qf(1 - alpha/2, df1, df2)  # 上界对应右尾
  F_upper <- stats::qf(alpha/2, df1, df2)

  # CI calculation by ICC type
  if (icc_type %in% c("1,1")) {
    F1 <- MSR / MSW
    lower <- (F1 / F_lower - 1) / (F1 / F_lower + (k-1))
    upper <- (F1 / F_upper - 1) / (F1 / F_upper + (k-1))
  } else if (icc_type %in% c("1,k")) {
    F1 <- MSR / MSW
    lower <- 1 - 1/F_lower
    upper <- 1 - 1/F_upper
  } else if (icc_type %in% c("3,1", "2,1,consistency")) {
    F1 <- MSR / MSE
    lower <- (F1 / F_lower - 1) / (F1 / F_lower + (k-1))
    upper <- (F1 / F_upper - 1) / (F1 / F_upper + (k-1))
  } else if (icc_type %in% c("3,k", "2,k,consistency")) {
    F1 <- MSR / MSE
    lower <- 1 - 1/F_lower
    upper <- 1 - 1/F_upper
  } else if (icc_type %in% c("2,1", "3,1,absolute", "2,k", "3,k,absolute")) {
    # Type A: Satterthwaite degrees of freedom correction (McGraw & Wong 1996, p.37)
    a <- k * MSC / n + (n - 1) * MSE
    b <- (n - 1) * k * MSE / n
    df_corrected <- (a + b)^2 / (a^2 / df1 + b^2 / df2)
    result$df_corrected <- df_corrected

    # Recalculate F quantiles with corrected degrees of freedom
    F_lower_corr <- stats::qf(1 - alpha/2, df1, df_corrected)  # 右尾
    F_upper_corr <- stats::qf(alpha/2, df1, df_corrected)

    if (icc_type %in% c("2,1", "3,1,absolute")) {
      F_val <- MSR / MSE
      # Modified Type A ICC CI formula 5.16
      lower <- (F_val / F_lower_corr - 1) / (F_val / F_lower_corr + (k-1) + k*(MSC - MSE)/(n*MSE))
      upper <- (F_val / F_upper_corr - 1) / (F_val / F_upper_corr + (k-1) + k*(MSC - MSE)/(n*MSE))
    } else {
      F_val <- MSR / MSE
      lower <- 1 - ( (MSR + (MSC-MSE)/n ) / (MSE * F_lower_corr) ) / (MSR/MSE)
      upper <- 1 - ( (MSR + (MSC-MSE)/n ) / (MSE * F_upper_corr) ) / (MSR/MSE)
    }
  }

  # Truncate CI to valid ICC range [0, 1]
  result$ci_lower <- max(lower, 0)
  result$ci_upper <- min(upper, 1)

  return(result)
}

#==============================================================================#
# Function 4: icc_calc_f_test
# Unified F-test for ICC (H0: ICC = 0 or non-zero rho0)
#==============================================================================#
#' Hypothesis Testing for ICC
#'
#' @description
#' Performs one-tailed F-test for ICC significance following McGraw & Wong (1996) Table 8.
#' Supports both null hypothesis of zero ICC and custom non-zero null value.
#'
#' @param anova_result List. Output from \code{icc_calc_anova}.
#' @param icc_type Character. ICC type code.
#' @param rho0 Numeric. Null hypothesis ICC value, default 0.
#' @param alpha Numeric. Significance level, default 0.05.
#'
#' @return Named list with test results:
#' \describe{
#'   \item{H0}{Character. Null hypothesis statement.}
#'   \item{F_stat}{Numeric. F test statistic.}
#'   \item{df1}{Integer. Numerator degrees of freedom.}
#'   \item{df2}{Integer. Denominator degrees of freedom.}
#'   \item{p_value}{Numeric. One-tailed p-value (H1: ICC > rho0).}
#' }
#'
#' @keywords internal
#' @references
#' McGraw, K. O., & Wong, S. P. (1996). Forming inferences about some
#' intraclass correlation coefficients. Psychological Methods, 1(1), 30-46.
icc_calc_f_test <- function(anova_result, icc_type, rho0 = 0, alpha = 0.05) {
  # Initialize output
  result <- list(
    H0 = paste0("ICC = ", rho0),
    F_stat = NULL,
    df1 = NULL,
    df2 = NULL,
    p_value = NULL
  )

  # Input validation
  if (!is.list(anova_result)) stop("Invalid anova_result")
  if (rho0 < 0 || rho0 > 1) stop("rho0 must be between 0 and 1")

  # Extract parameters
  MSR <- anova_result$MSR
  MSW <- anova_result$MSW
  MSC <- anova_result$MSC
  MSE <- anova_result$MSE
  n <- anova_result$n
  k <- anova_result$k
  df1 <- anova_result$df1
  df2 <- anova_result$df2

  # F statistic calculation
  if (rho0 == 0) {
    # Standard zero test (McGraw & Wong 1996, Table 8)
    if (icc_type %in% c("1,1", "1,k")) {
      result$F_stat <- MSR / MSW
      result$df1 <- df1
      result$df2 <- n*(k-1)
    } else if (icc_type %in% c("3,1","3,k","2,1,consistency","2,k,consistency")) {
      result$F_stat <- MSR / MSE
      result$df1 <- df1
      result$df2 <- df2
    } else if (icc_type %in% c("2,1","2,k","3,1,absolute","3,k,absolute")) {
      result$F_stat <- MSR / MSE
      result$df1 <- df1
      result$df2 <- df2
    }
  } else {
    # Non-zero test (McGraw & Wong 1996, Table 8)
    if (icc_type %in% c("1,1", "1,k")) {
      result$F_stat <- (MSR/MSW) * (1 - rho0) / (1 + (k-1)*rho0)
      result$df1 <- df1
      result$df2 <- n*(k-1)
    } else if (icc_type %in% c("3,1","3,k","2,1,consistency","2,k,consistency")) {
      result$F_stat <- (MSR/MSE) * (1 - rho0) / (1 + (k-1)*rho0)
      result$df1 <- df1
      result$df2 <- df2
    } else if (icc_type %in% c("2,1","2,k","3,1,absolute","3,k,absolute")) {
      # Type A models: include rater variance component
      numerator <- MSR * (1 - rho0)
      denominator <- MSE * (1 + (k-1)*rho0) + k * (MSC - MSE) * rho0 / n
      result$F_stat <- numerator / denominator
      result$df1 <- df1
      result$df2 <- df2
    }
  }

  # One-tailed p-value (standard for reliability: H1: ICC > rho0)
  result$p_value <- stats::pf(result$F_stat, result$df1, result$df2, lower.tail = FALSE)

  return(result)
}

# File: sample_size_calculate.R
# Description: Sample size and power analysis for ICC
# Author: [Ziyu Liu]
# Date: [20260421]

#' Sample Size for ICC based on Lower Confidence Limit
#'
#' @param rho Anticipated ICC value.
#' @param rho0 Desired lower bound.
#' @param k Number of observations per subject. Default 3.
#' @param same_raters Logical.
#' @param rater_effect "random" or "fixed".
#' @param rating_type "single" or "average".
#' @param agreement_type "absolute" or "consistency".
#' @param alpha Significance level. Default 0.05.
#' @param assurance Assurance probability. Default 0.8.
#' @param rating_target Shortcut for rho0.
#' @param verbose Print messages. Default TRUE.
#'
#' @return Required sample size.
#' @export
icc_sample_size_lower <- function(
    rho, rho0 = NULL, k = 3, same_raters, rater_effect = NULL,
    rating_type, agreement_type = NULL, alpha = 0.05, assurance = 0.8,
    rating_target = NULL, verbose = TRUE
) {
  if (!is.null(rating_target)) {
    rating_map <- c(poor=0.5, moderate=0.5, good=0.75, excellent=0.9)
    if (!rating_target %in% names(rating_map)) stop("rating_target error")
    rho0 <- rating_map[rating_target]
  }
  if (is.null(rho0)) stop("Provide rho0 or rating_target")
  
  design_check <- icc_check_design(same_raters, rater_effect, rating_type, agreement_type, k)
  if (!design_check$is_valid) stop(design_check$error_msg)
  mapping <- icc_map_design_to_icc(same_raters, rater_effect, rating_type, agreement_type)
  
  if (verbose) message("Mapped ICC type: ", mapping$icc_full_name)
  stopifnot(rho>=0&rho<=1, rho0>=0&rho0<=1, rho>rho0, k>=2)
  
  z_alpha <- stats::qnorm(1-alpha)
  z_beta <- stats::qnorm(assurance)
  F_rho <- (1+(k-1)*rho)/(1-rho)
  F_rho0 <- (1+(k-1)*rho0)/(1-rho0)
  N <- 1 + 2*(z_alpha+z_beta)^2*k / ((log(F_rho/F_rho0))^2*(k-1))
  ceiling(N)
}

#' Sample Size for ICC based on Confidence Interval Width
#'
#' @param rho Anticipated ICC value.
#' @param omega Desired half-width.
#' @param k Number of observations. Default 3.
#' @param same_raters Logical.
#' @param rater_effect "random" or "fixed".
#' @param rating_type "single" or "average".
#' @param agreement_type "absolute" or "consistency".
#' @param alpha Significance level. Default 0.05.
#' @param assurance Assurance probability. Default 0.8.
#' @param verbose Print messages. Default TRUE.
#'
#' @return Required sample size.
#' @export
icc_sample_size_width <- function(
    rho, omega, k=3, same_raters, rater_effect=NULL, rating_type,
    agreement_type=NULL, alpha=0.05, assurance=0.8, verbose=TRUE
) {
  design_check <- icc_check_design(same_raters, rater_effect, rating_type, agreement_type, k)
  if (!design_check$is_valid) stop(design_check$error_msg)
  if (verbose) message("Mapped ICC type.")
  stopifnot(rho>=0&rho<=1, omega>0, k>=2)
  
  z_alpha2 <- stats::qnorm(1-alpha/2)
  z_beta <- stats::qnorm(assurance)
  A <- (1-rho)*(1+(k-1)*rho)
  B <- k-2+2*rho-2*k*rho
  absB <- abs(B)
  
  term1 <- A*z_alpha2
  term2 <- sqrt(A^2*z_alpha2^2 + 4*omega*z_alpha2*z_beta*A*absB)
  sqrt_N_minus_1 <- (term1+term2)/(omega*sqrt(2*k*(k-1)))
  N <- 1 + sqrt_N_minus_1^2
  ceiling(N)
}

#' Power Calculation for ICC Study Design
#'
#' @param n Number of subjects.
#' @param rho Anticipated ICC value.
#' @param rho0 Lower bound for method="lower".
#' @param omega Half-width for method="width".
#' @param k Number of observations. Default 3.
#' @param same_raters Logical.
#' @param rater_effect "random" or "fixed".
#' @param rating_type "single" or "average".
#' @param agreement_type "absolute" or "consistency".
#' @param alpha Significance level. Default 0.05.
#' @param method "lower" or "width".
#' @param verbose Print messages. Default TRUE.
#'
#' @return Power.
#' @export
icc_power <- function(
    n, rho, rho0=NULL, omega=NULL, k=3, same_raters, rater_effect=NULL,
    rating_type, agreement_type=NULL, alpha=0.05, method=c("lower","width"), verbose=TRUE
) {
  method <- match.arg(method)
  stopifnot(n>=2, rho>=0&rho<=1)
  
  if (method=="lower") {
    if (is.null(rho0)) stop("Provide rho0")
    z_alpha <- stats::qnorm(1-alpha)
    F_rho <- (1+(k-1)*rho)/(1-rho)
    F_rho0 <- (1+(k-1)*rho0)/(1-rho0)
    z_beta <- log(F_rho/F_rho0)*sqrt((k-1)*(n-1)/(2*k)) - z_alpha
    return(stats::pnorm(z_beta))
  }
  
  if (method=="width") {
    if (is.null(omega)) stop("Provide omega")
    z_alpha2 <- stats::qnorm(1-alpha/2)
    A <- (1-rho)*(1+(k-1)*rho)
    B <- k-2+2*rho-2*k*rho
    absB <- abs(B)
    numerator <- omega*sqrt(2*k*(k-1))*sqrt(n-1) - A*z_alpha2
    denominator <- 2*omega*z_alpha2*A*absB
    z_beta <- sqrt(numerator^2/denominator)
    return(stats::pnorm(z_beta))
  }
}

#' Unified ICC Sample Size & Power Interface
#'
#' @param method "lower", "width", "power".
#' @param ... Arguments passed to underlying functions.
#'
#' @return Sample size or power.
#' @export
icc_sample_size <- function(method = c("lower", "width", "power"), ...) {
  method <- match.arg(method)
  switch(method,
         lower = icc_sample_size_lower(...),
         width = icc_sample_size_width(...),
         power = icc_power(...)
  )
}
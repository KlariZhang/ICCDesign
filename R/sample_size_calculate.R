#' Sample size for ICC based on lower confidence limit (Zou 2012, Eq 7)
#'
#' Computes the number of subjects required to ensure, with a given assurance
#' probability, that the lower limit of a one-sided confidence interval for the
#' intraclass correlation coefficient (ICC) is no less than a specified value.
#'
#' @importFrom stats qnorm
#' @param rho anticipated ICC (planning value)
#' @param rho0 desired lower bound of the confidence interval
#' @param k number of observations per subject (raters or replicates)
#' @param alpha significance level (default 0.05 for 95\% confidence)
#' @param assurance desired assurance probability (1 - beta, default 0.8)
#' @return required number of subjects (integer, rounded up)
#' @references Zou, G. Y. (2012). Sample size formulas for estimating intraclass
#'   correlation coefficients with precision and assurance. Statistics in Medicine,
#'   31(29), 3972-3984. doi:10.1002/sim.5466
#' @examples
#' # From paper Section 4: ρ=0.725, ρ0=0.7, k=3, α=0.05, assurance=0.8
#' icc_sample_size_lower(rho = 0.725, rho0 = 0.7, k = 3, assurance = 0.8)
#' @export
icc_sample_size_lower <- function(rho, rho0, k, alpha = 0.05, assurance = 0.8) {
  # Input validation
  stopifnot(
    "rho must be between -1 and 1" = rho >= -1 && rho <= 1,
    "rho0 must be between -1 and 1" = rho0 >= -1 && rho0 <= 1,
    "rho must be > rho0 for meaningful calculation" = rho > rho0,
    "k must be integer >= 2" = k >= 2 && round(k) == k,
    "alpha must be between 0 and 1" = alpha > 0 && alpha < 1,
    "assurance must be between 0 and 1" = assurance > 0 && assurance < 1
  )

  # Quantiles
  z_alpha <- qnorm(1 - alpha)  # one-sided upper quantile
  z_beta  <- qnorm(assurance)  # since assurance = 1 - beta

  # F(rho) = (1 + (k-1)*rho) / (1 - rho)
  F_rho  <- (1 + (k - 1) * rho)  / (1 - rho)
  F_rho0 <- (1 + (k - 1) * rho0) / (1 - rho0)

  # Equation (7)
  numerator   <- 2 * (z_alpha + z_beta)^2 * k
  denominator <- (log(F_rho / F_rho0))^2 * (k - 1)
  N <- 1 + numerator / denominator

  ceiling(N)
}


#' Sample size for ICC based on desired confidence interval width with assurance
#' (Zou 2012, Eq 5)
#'
#' Computes the number of subjects required to ensure, with a given assurance
#' probability, that the half-width of a two-sided confidence interval for the
#' ICC does not exceed a specified value.
#'
#' @param rho anticipated ICC (planning value)
#' @param omega desired half-width of the two-sided confidence interval
#' @param k number of observations per subject (raters or replicates)
#' @param alpha significance level (default 0.05 for 95\% confidence)
#' @param assurance desired assurance probability (1 - beta, default 0.8)
#' @return required number of subjects (integer, rounded up)
#' @references Zou, G. Y. (2012). Sample size formulas for estimating intraclass
#'   correlation coefficients with precision and assurance. Statistics in Medicine,
#'   31(29), 3972-3984. doi:10.1002/sim.5466
#' @examples
#' # From paper Table I: ρ=0.6, ω=0.1, k=3, α=0.05, assurance=0.5 -> ~101
#' icc_sample_size_width(rho = 0.6, omega = 0.1, k = 3, assurance = 0.5)
#' @export
icc_sample_size_width <- function(rho, omega, k, alpha = 0.05, assurance = 0.8) {
  # Input validation
  stopifnot(
    "rho must be between -1 and 1" = rho >= -1 && rho <= 1,
    "omega must be positive" = omega > 0,
    "k must be integer >= 2" = k >= 2 && round(k) == k,
    "alpha must be between 0 and 1" = alpha > 0 && alpha < 1,
    "assurance must be between 0 and 1" = assurance > 0 && assurance < 1
  )

  # Quantiles
  z_alpha2 <- qnorm(1 - alpha / 2)  # two-sided
  z_beta   <- qnorm(assurance)      # assurance = 1 - beta

  # Auxiliary quantities
  A <- (1 - rho) * (1 + (k - 1) * rho)
  B <- k - 2 + 2 * rho - 2 * k * rho
  absB <- abs(B)

  # Equation (5)
  term1 <- A * z_alpha2
  term2 <- sqrt(A^2 * z_alpha2^2 + 4 * omega * z_alpha2 * z_beta * A * absB)
  numerator <- term1 + term2
  denominator <- omega * sqrt(2 * k * (k - 1))

  sqrt_N_minus_1 <- numerator / denominator
  N <- 1 + sqrt_N_minus_1^2

  ceiling(N)
}


#' Unified interface for ICC sample size calculation
#'
#' @param method either "lower" (for lower confidence limit) or "width" (for interval width)
#' @param ... arguments passed to the specific function
#' @export
icc_sample_size <- function(method = c("lower", "width"), ...) {
  method <- match.arg(method)
  if (method == "lower") {
    icc_sample_size_lower(...)
  } else {
    icc_sample_size_width(...)
  }
}

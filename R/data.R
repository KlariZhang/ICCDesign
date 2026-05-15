#' Example ICC Dataset
#'
#' A small, carefully constructed example dataset containing ratings from
#' 4 raters on 5 subjects. Designed to demonstrate all package functionality
#' with fast execution and predictable results.
#'
#' This dataset can be used to calculate all 10 ICC types supported by the
#' package, simply by changing the design parameters in \code{\link{icc_calc}}.
#'
#' @name icc_data
#' @format A numeric matrix with 5 rows (subjects) and 4 columns (raters).
#'   Row names are "Subject1" to "Subject5", column names are "Rater1" to "Rater4".
#'
#' @details
#' The dataset was simulated to have an approximate ICC of 0.8, which falls
#' into the "Good" reliability category according to Koo & Li (2016).
#'
#' @source Simulated data for demonstration purposes.
#'
#' @examples
#' data(icc_data)
#' head(icc_data)
NULL

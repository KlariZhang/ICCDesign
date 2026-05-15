# ICCDesign

A comprehensive R package for intraclass correlation coefficient (ICC) analysis and sample size planning.

## Features

- Full support for all 10 ICC types from McGraw & Wong (1996)
- Intuitive 4-question decision framework (no need to memorize ICC codes)
- Automated reliability evaluation based on Koo & Li (2016)
- Publication-ready report generation
- Sample size and power analysis
- Interactive Shiny web application with visualization
- Comprehensive data validation and error handling

## Installation

```r
# Install devtools if not already installed
install.packages("devtools")

# Install ICCDesign from GitHub
devtools::install_github("KlariZhang/ICCDesign")

# Load the package
library(ICCDesign)
```

## Quick Start

1. Command Line Interface

```r
# Use the built-in example dataset
data(icc_data)

# Calculate ICC (two-way random, single rating, absolute agreement)
result <- icc_calc(
  data = icc_data,
  same_raters = TRUE,
  rater_effect = "random",
  rating_type = "single",
  agreement_type = "absolute"
)

# View the full report
print(result)

# Extract specific results
result$icc_result$point_est  # ICC point estimate
result$evaluation$rating_en  # Reliability rating
```

2. Interactive Shiny Interface

```r
### Launch the point-and-click web application
run_icc_app() 3. Sample Size Calculation
```

3. Sample Size Calculation

```r
### Calculate required sample size for 80% assurance that ICC >= 0.75

n <- icc_sample_size(
method = "lower",
rho = 0.85,
rating_target = "good",
k = 3,
same_raters = TRUE,
rater_effect = "random",
rating_type = "single",
agreement_type = "absolute"
)

cat("Required sample size:", n, "\n")
```

References

1. McGraw, K. O., & Wong, S. P. (1996). Forming inferences about some intraclass correlation coefficients. Psychological Methods, 1(1), 30-46.
2. Koo, T. K., & Li, M. Y. (2016). A guideline of selecting and reporting intraclass correlation coefficients for reliability research. Journal of Chiropractic Medicine, 15(2), 155-163.

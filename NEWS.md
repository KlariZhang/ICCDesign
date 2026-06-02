# ICCDesign 0.1.1

* Refactored residual sum of squares calculation in `icc_calc_anova()` (R/utils-calc.R) to strictly follow ANOVA decomposition theorem.
* Corrected confidence interval logic and Satterthwaite degrees of freedom in `icc_tool_ci()`.
* Improved non-zero null hypothesis tests in `icc_calc_f_test()` with full compliance to McGraw & Wong (1996).
* Aligned all ICC calculation results with `irr` and `psych` benchmark packages.

# ICCDesign 0.1.0

* Initial release on CRAN.
* Core functions: icc_calc(), icc_sample_size(), run_icc_app().

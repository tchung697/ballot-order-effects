# 0. Description --------------------------------------------------------------
#
# Author: Ting-Chih Hung
#
# Last Modified: 2025-09-18
#
# Goal: 
#   - Compute the difference in means within each stratum and then average them
#

# 1. Function -----------------------------------------------------------------

diff_in_means_sre <- function(data, outcome, treatment, strata) {
  require(dplyr)

  if (missing(data) || missing(outcome) || missing(treatment) || missing(strata)) {
    stop("All arguments (data, outcome, treatment, strata) must be provided")
  }

  data |>
    group_by({{ strata }}) |>
    summarise(
      n_k          = n(),
      n_treated    = sum({{ treatment }}),
      n_control    = n_k - n_treated,
      mean_treated = mean({{ outcome }}[{{ treatment }} == 1], na.rm = TRUE),
      mean_control = mean({{ outcome }}[{{ treatment }} == 0], na.rm = TRUE),
      diff_k       = mean_treated - mean_control,
      weight       = n_k / nrow(data),
      .groups      = 'drop'
    ) |>
    summarise(
      weighted_diff_in_means = sum(weight * diff_in_means)
    ) |>
    pull(weighted_diff_in_means)
}

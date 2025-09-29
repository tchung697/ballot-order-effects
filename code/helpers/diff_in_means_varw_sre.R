# 0. Description --------------------------------------------------------------
#
# Author: Ting-Chih Hung
#
# Last Modified: 2025-09-18
#
# Goal:
#   - Compute the within-stratum difference in means (treated - control)
#     and average them using weights proportional to (n_k - 1),
#     i.e., w_k = (n_k - 1) / sum_{k'} (n_{k'} - 1)
#

# 1. Function -----------------------------------------------------------------

diff_in_means_varw_sre <- function(data, outcome, treatment, strata) {
  require(dplyr)

  if (missing(data) || missing(outcome) || missing(treatment) || missing(strata)) {
    stop("All arguments (data, outcome, treatment, strata) must be provided")
  }

  # per-stratum means and diff (treated - control), plus stratum size n_k
  per_stratum <- data |>
    group_by({{ strata }}) |>
    summarise(
      n_k          = n(),
      n_treated    = sum({{ treatment }}),
      n_control    = n_k - n_treated,
      mean_treated = mean({{ outcome }}[{{ treatment }} == 1], na.rm = TRUE),
      mean_contrl  = mean({{ outcome }}[{{ treatment }} == 0], na.rm = TRUE),
      diff_k       = mean_treated - mean_contrl,
      .groups      = "drop"
    )

  # Under our design, each stratum must have exactly 1 treated unit
  if (any(per_stratum$n_treated != 1L)) {
    stop("Each stratum must have exactly 1 treated unit.")
  }

  if (any(per_stratum$n_control < 1L)) {
    stop("Each stratum must have at least 1 control unit (n_k >= 2).")
  }

  # Weights proportional to (n_k - 1)
  per_stratum <- per_stratum |>
    mutate(
      w_raw = pmax(n_k - 1, 0),
      w_k   = w_raw / sum(w_raw)
    )

  # Weighted average of within-stratum differences
  per_stratum |>
    summarise(weighted_diff_in_means = sum(w_k * diff_k)) |>
    pull(weighted_diff_in_means)
}

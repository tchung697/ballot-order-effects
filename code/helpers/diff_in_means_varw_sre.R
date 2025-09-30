# 0. Description --------------------------------------------------------------
#
# Author: Ting-Chih Hung
#
# Last Modified: 2025-09-30
#
# Goal:
#   - Compute the within-stratum difference in means (treated - control)
#     and average them using variance weights consistent with SRE p.200.
#     Under the design of exactly 1 treated per stratum, the normalized
#     weights are:
#       w_k = [(n_k - 1) / n_k] / sum_j [(n_j - 1) / n_j].
#

# 1. Function -----------------------------------------------------------------

diff_in_means_varw_sre <- function(data, outcome, treatment, strata) {
  require(dplyr)

  if (missing(data) || missing(outcome) || missing(treatment) || missing(strata)) {
    stop("All arguments (data, outcome, treatment, strata) must be provided")
  }

  per_stratum <- data |>
    group_by({{ strata }}) |>
    summarise(
      n_k          = n(),
      n_treated    = sum({{ treatment }} == 1, na.rm = TRUE),
      n_control    = sum({{ treatment }} == 0, na.rm = TRUE),
      mean_treated = mean({{ outcome }}[{{ treatment }} == 1], na.rm = TRUE),
      mean_contrl  = mean({{ outcome }}[{{ treatment }} == 0], na.rm = TRUE),
      diff_k       = mean_treated - mean_contrl,
      .groups      = "drop"
    )

  # Design checks: exactly 1 treated and at least 1 control in every stratum
  if (any(per_stratum$n_treated != 1L)) {
    stop("Each stratum must have exactly 1 treated unit.")
  }
  if (any(per_stratum$n_control < 1L)) {
    stop("Each stratum must have at least 1 control unit (n_k >= 2).")
  }

  # SRE-consistent variance weights: w_k \propto (n_k - 1)/n_k, then normalize
  per_stratum <- per_stratum |>
    mutate(
      w_raw = (n_k - 1) / n_k,
      w_k   = w_raw / sum(w_raw)
    )

  # Weighted average of within-stratum differences
  per_stratum |>
    summarise(weighted_diff_in_means = sum(w_k * diff_k)) |>
    pull(weighted_diff_in_means)
}


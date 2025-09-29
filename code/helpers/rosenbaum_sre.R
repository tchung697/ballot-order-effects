# 0. Description --------------------------------------------------------------
#
# Author: Ting-Chih Hung
#
# Last Modified: 2025-09-19
#
# Goal: Compute Rosenbaum (2007) test statistic for stratified randomized 
#       experiments with potential interference.
#       F = T - E[T*], where T is observed Mann-Whitney statistic and 
#       E[T*] is expected value under uniformity trial (no treatment effect).
#

# 1. Function -----------------------------------------------------------------

rosenbaum_sre <- function(data, outcome, treatment, strata) {
  require(dplyr)
  
  if (missing(data) || missing(outcome) || missing(treatment) || missing(strata)) {
    stop("All arguments (data, outcome, treatment, strata) must be provided")
  }
  
  # Calculate T: total number of times treated > control across all strata
  result <- data |>
    group_by({{ strata }}) |>
    summarise(
      # Within each stratum, count treated > control comparisons
      mann_whitney_count = {
        treated_outcomes <- {{ outcome }}[{{ treatment }} == 1]
        control_outcomes <- {{ outcome }}[{{ treatment }} == 0]
        
        # Handle edge cases: no treated or no control candidates
        if (length(treated_outcomes) == 0 || length(control_outcomes) == 0) {
          0
        } else {
          # Count how many times treated outcome > control outcome
          sum(outer(treated_outcomes, control_outcomes, ">"))
        }
      },
      n_k = n(),
      .groups = "drop"
    ) |>
    summarise(
      T_observed = sum(mann_whitney_count),
      # Expected value under uniformity trial: sum of (n_k - 1)/2 across strata
      T_expected_uniformity = sum((n_k - 1) / 2),
      .groups = "drop"
    )
  
  # F = T - E[T*] 
  # Positive values indicate treated candidates perform better than expected under no effect
  F_statistic <- result$T_observed - result$T_expected_uniformity
  
  return(F_statistic)
}

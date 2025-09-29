# 0. Description --------------------------------------------------------------
#
# Author: Ting-Chih Hung
#
# Last Modified: 2025-09-18
#
# Goal: Compute the rank-based test statistic within each stratum 
#       and then average them
#

# 1. Function -----------------------------------------------------------------

rank_sre <- function(data, outcome, treatment, strata) {
  require(dplyr)

  if (missing(data) || missing(outcome) || missing(treatment) || missing(strata)) {
    stop("All arguments (data, outcome, treatment, strata) must be provided")
  }

  df <- data |>
    group_by({{ strata }}) |>
    mutate(
      n_k        = n(),
      rank_raw   = rank({{ outcome }}, ties.method = "average"),
      # normalized within-stratum rank: subtract (n+1)/2
      R_strat    = rank_raw - (n_k + 1) / 2
    ) |>
    ungroup() |>
    select({{ treatment }}, R_strat)
  
  R_treated <- df |> 
    filter({{ treatment }} == 1) |>
    summarise(mean_rank = mean(R_strat)) |>
    pull(mean_rank)

  R_control <- df |> 
    filter({{ treatment }} == 0) |>
    summarise(mean_rank = mean(R_strat)) |>
    pull(mean_rank)
  
  abs(R_treated - R_control)
}

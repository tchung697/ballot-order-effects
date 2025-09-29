# 0. Description --------------------------------------------------------------
#
# Author: Ting-Chih Hung
#
# Last Modified: 2025-09-18
#
# Goal: Perform Fisher's randomization inference to test for ballot order effects.
#
# Outputs:
# - outputs/results/fisher_tests_by_year.rds
#

# 1. Load Libraries and Set Random Seed ---------------------------------------

library(tidyverse)
set.seed(123)

# 2. Source Helper Functions --------------------------------------------------

walk(
  list.files(
    file.path("code", "helpers"), 
    pattern = "\\.R$", 
    full.names = TRUE), 
  source
)

# 3. Read Data ----------------------------------------------------------------

elections <- read_csv(file.path("data", "candidates_2008_2020.csv"))

# 4. Perform Randomization Inference ------------------------------------------

fisher_tests <- list()

fisher_tests$diff_in_means_varw <- elections |>
  group_by(year) |>
  group_map(
    ~ fisherian_test_sre(
      .x, 
      outcome = percentage, 
      treatment = left, 
      strata = district, 
      test_statistic = "diff_in_means_varw",
      n_sim = 10000,
      alternative = "two.sided",
      progress = TRUE
    ), 
    .keep = TRUE
  )

names(fisher_tests$diff_in_means_varw) <- unique(elections$year)

fisher_tests_tidy <- fisher_tests |>
  map_dfr(\(x) map_dfr(x, tidy, .id = "year"))

fisher_tests_critical_values <- fisher_tests |>
  map_dfr(\(x) map_dfr(x, critical_values, .id = "year"), .id = "test_statistic")

fisher_tests_permuted_stats <- fisher_tests |>
  map(\(x) map(x, \(y) y$permuted_stats))

fisher_tests_permuted_stats_tidy <- fisher_tests_permuted_stats |>
  map_dfr(
    \(x) map_dfr(
      x, 
      \(y) tibble(permuted_stat = y), 
      .id = "year"
    ), 
    .id = "test_statistic"
  )

# 5. Save Output --------------------------------------------------------------

write_csv(
  fisher_tests_tidy, 
  file.path("outputs", "tables", "fisher_tests.csv")
)

write_csv(
  fisher_tests_permuted_stats_tidy, 
  file.path("outputs", "tables", "fisher_tests_permuted_stats.csv")
)



# 0. Description --------------------------------------------------------------
#
# Author: Ting-Chih Hung
#
# Last Modified: 2025-09-19
#
# Goal: Generate and save all visuals for the ballot order effect project.
#
# Inputs:
#   - data/candidates_2008_2020.csv
#   - outputs/tables/n_candidate_election_counts.csv
#   - outputs/tables/conventional_models.csv
# 
# Outputs:
#   - outputs/figures/election_count_plot.png
#   - outputs/figures/vote_share_density_plot.png
#   - outputs/figures/effect_plot.png
#

# 1. Load Libraries -----------------------------------------------------------

library(tidyverse)
DATA_PATH <- "data"
OUTPUTS_TABLES_PATH <- file.path("outputs", "tables")
OUTPUTS_FIGURES_PATH <- file.path("outputs", "figures")

# 2. Read Data & Analysis Results ---------------------------------------------

elections <- read_csv(file.path(DATA_PATH, "candidates_2008_2020.csv"))

n_candidate_election_counts <- read_csv(
  file.path(OUTPUTS_TABLES_PATH, "n_candidate_election_counts.csv")
)

conventional_models <- read_csv(
  file.path(OUTPUTS_TABLES_PATH, "conventional_models.csv")
)

fisher_tests <- read_csv(
  file.path(OUTPUTS_TABLES_PATH, "fisher_tests.csv")
)

fisher_tests_permutatd_stats <- read_csv(
  file.path(OUTPUTS_TABLES_PATH, "fisher_tests_permuted_stats.csv")
)

# 3. Generate Visuals ---------------------------------------------------------

## 3.1 Counts of Elections by Number of Candidates and Year -------------------

election_count_plot <- n_candidate_election_counts |>
  ggplot(aes(x = factor(n_candidates), y = election_count)) +
  geom_bar(stat = "identity") +
  labs(
    x = "Number of Candidates",
    y = "Number of Elections",
  ) +
  theme_bw() +
  theme(
    text = element_text(size = 15)
  ) +
  facet_wrap(~ year, nrow = 1)

## 3.2 Density Plot of Vote Share by Treatment Group --------------------------

vote_share_density_plot <- elections |>
  mutate(Group = ifelse(left == 1, "Listed First", "Not Listed First")) |>
  ggplot(aes(x = percentage, fill = Group)) +
  geom_density(alpha = 0.5, color = "white") +
  labs(
    x = "Vote Share (%)",
    y = "Density",
    fill = "Ballot Position"
  ) +
  theme_bw() +
  theme(
    text = element_text(size = 15),
    legend.position = "top"
  ) +
  facet_wrap(~ year, nrow = 1)

## 3.3 Coefficient Plot for Conventional Models with Error Bars ---------------

effect_plot <- conventional_models |>
  ggplot(aes(x = factor(year), y = est, color = method)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(
    aes(ymin = est - 1.96 * se, ymax = est + 1.96 * se), 
    position = position_dodge(width = 0.5), 
    width = 0.2
  ) +
  labs(
    x = "Election Year",
    y = "Estimated Effect of Being Listed First (%)",
    color = "Method"
  ) +
  theme_bw() +
  theme(
    text = element_text(size = 15),
    legend.position = "top"
  )

# 3.4 Randomization Distribution with Observed Statistic ----------------------

fisher_tests_permutatd_stats <- fisher_tests_permutatd_stats |>
  left_join(
    fisher_tests |> 
      select(year, test_statistic, statistic),
    by = c("year", "test_statistic")
  )

randomization_distribution_diff_in_means_varw_plot <- 
  fisher_tests_permutatd_stats |>
  filter(test_statistic == "diff_in_means_varw") |>
  ggplot(aes(x = permuted_stat)) +
  geom_histogram(bins = 30, color = "white", alpha = 0.6) +
  geom_vline(
    aes(xintercept = statistic),
    color = "red", 
    linetype = "dashed", 
    linewidth = 0.5
  ) +
  facet_wrap(~ year, scales = "free") +
  labs(
    x = "Permuted Test Statistic (Difference in Means)",
    y = "Frequency"
  ) +
  theme_bw() +
  theme(
    text = element_text(size = 15)
  )

# 4. Save Visuals -------------------------------------------------------------

ggsave(
  filename = file.path(OUTPUTS_FIGURES_PATH, "election_count_plot.png"), 
  plot = election_count_plot, 
  width = 10,
  height = 3
)

ggsave(
  filename = file.path(OUTPUTS_FIGURES_PATH, "vote_share_density_plot.png"),
  plot = vote_share_density_plot,
  width = 10,
  height = 3
)

ggsave(
  filename = file.path(OUTPUTS_FIGURES_PATH, "effect_plot.png"), 
  plot = effect_plot, 
  width = 8, 
  height = 5
)

ggsave(
  filename = file.path(
    OUTPUTS_FIGURES_PATH, 
    "randomization_distribution_diff_in_means_varw_plot.png"
  ), 
  plot = randomization_distribution_diff_in_means_varw_plot,
  width = 8, 
  height = 6
)

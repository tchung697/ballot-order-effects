# 0. Description --------------------------------------------------------------
#
# Author: Ting-Chih Hung
#
# Last Modified: 2025-09-19
#
# Goal: Generate descriptive statistics for the ballot order effect project.
#
# Outputs:
# - outputs/tables/summary_statistics.csv
# - outputs/tables/n_candidate_election_counts.csv
# - outputs/tables/desc_stats_overall.csv
# - outputs/tables/desc_stats_by_n_candidates.csv
# - outputs/tables/desc_stats_by_left.csv
# - outputs/tables/desc_stats_by_right.csv
# - outputs/tables/desc_stats_by_middle.csv
#

# 1. Load Libraries -----------------------------------------------------------

library(tidyverse)

# 2. Read Data ----------------------------------------------------------------

# Assumes that the cleaning script (00_clean_data.R) has been run
# and the data contains `left`, `right`, and `middle` indicator variables.
elections <- read_csv(file.path("data", "candidates_2008_2020.csv"))

# 3. Generate Summary Statistics ----------------------------------------------

summary_stats <- elections |>
  group_by(year) |>
  summarise(
    total_candidates = n(),
    sex_ratio = mean(male),
    birth_year = round(mean(birth_year)),
    incumbent_ratio = mean(incumbency),
    avg_candidates_per_district = n() / n_distinct(district)
  ) |>
  ungroup() |>
  rename(
    Year = year,
    `#Candidates` = total_candidates,
    `Sex Ratio` = sex_ratio,
    `Avg. Birth Year` = birth_year,
    `Incumbent Ratio` = incumbent_ratio,
    `Avg. #Candidates per District` = avg_candidates_per_district
  )

n_candidate_election_counts <- elections |>
  group_by(year, n_candidates) |>
  summarise(election_count = n_distinct(district)) |>
  ungroup()

# Overall Descriptive Statistics
vars_to_summarize <- c("percentage", "incumbency", "male", "birth_year")
desc_stats_overall <- elections |>
  select(all_of(vars_to_summarize)) |>
  pivot_longer(everything(), names_to = "Variable", values_to = "Value") |>
  group_by(Variable) |>
  summarise(
    N = n(),
    Mean = mean(Value, na.rm = TRUE),
    SD = sd(Value, na.rm = TRUE),
    Min = min(Value, na.rm = TRUE),
    Q1 = quantile(Value, 0.25, na.rm = TRUE),
    Median = median(Value, na.rm = TRUE),
    Q3 = quantile(Value, 0.75, na.rm = TRUE),
    Max = max(Value, na.rm = TRUE)
  ) |>
  ungroup()

desc_stats_overall_by_year <- elections |>
  select(year, all_of(vars_to_summarize)) |>
  pivot_longer(-year, names_to = "Variable", values_to = "Value") |>
  group_by(year, Variable) |>
  summarise(
    N = n(),
    Mean = mean(Value, na.rm = TRUE),
    SD = sd(Value, na.rm = TRUE),
    Min = min(Value, na.rm = TRUE),
    Q1 = quantile(Value, 0.25, na.rm = TRUE),
    Median = median(Value, na.rm = TRUE),
    Q3 = quantile(Value, 0.75, na.rm = TRUE),
    Max = max(Value, na.rm = TRUE)
  ) |>
  ungroup()

desc_stats_by_n_candidates <- elections |>
  add_count(year, district, name = "n_candidates") |>
  group_by(n_candidates) |>
  summarise(
    `Number of Districts` = n_distinct(paste(year, district)),
    `Total Candidates` = n(),
    `Mean Vote Share (First)` = mean(percentage[left == 1], na.rm = TRUE),
    `Mean Vote Share (Others)` = mean(percentage[left == 0], na.rm = TRUE)
  ) |>
  mutate(
    Difference = `Mean Vote Share (First)` - `Mean Vote Share (Others)`
  ) |>
  arrange(n_candidates)

# Comparison by Treatment Group (left, right, middle)
generate_treatment_summary <- function(data, treatment_var, group_labels) {
  data |>
    mutate(Group = ifelse({{ treatment_var }} == 1, group_labels[1], group_labels[2])) |>
    group_by(Group) |>
    summarise(
      `Number of Candidates` = n(),
      `Mean Vote Share (%)` = mean(percentage, na.rm = TRUE),
      `Win Rate` = mean(win, na.rm = TRUE),
      `Incumbency Rate` = mean(incumbency, na.rm = TRUE),
      `Proportion Male` = mean(male, na.rm = TRUE),
      `Mean Birth Year` = mean(birth_year, na.rm = TRUE)
    ) |>
    pivot_longer(-Group, names_to = "Metric", values_to = "Value") |>
    pivot_wider(names_from = Group, values_from = Value)
}

desc_stats_by_left <- generate_treatment_summary(
  elections, left, c("Treated (Listed Left)", "Control (Not First)")
)
desc_stats_by_right <- generate_treatment_summary(
  elections, right, c("Treated (Listed Right)", "Control (Not Right)")
)
desc_stats_by_middle <- generate_treatment_summary(
  elections, middle, c("Treated (Listed Middle)", "Control (Not Middle)")
)

# 4. Save Output --------------------------------------------------------------

write_csv(
  summary_stats, 
  file.path("outputs", "tables", "summary_statistics.csv")
)

write_csv(
  n_candidate_election_counts, 
  file.path("outputs", "tables", "n_candidate_election_counts.csv")
)

write_csv(
  desc_stats_overall,
  file.path("outputs", "tables", "desc_stats_overall.csv")
)

write_csv(
  desc_stats_by_n_candidates,
  file.path("outputs", "tables", "desc_stats_by_n_candidates.csv")
)

write_csv(
  desc_stats_by_left,
  file.path("outputs", "tables", "desc_stats_by_left.csv")
)

write_csv(
  desc_stats_by_right,
  file.path("outputs", "tables", "desc_stats_by_right.csv")
)

write_csv(
  desc_stats_by_middle,
  file.path("outputs", "tables", "desc_stats_by_middle.csv")
)


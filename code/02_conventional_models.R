# 0. Description --------------------------------------------------------------
#
# Author: Ting-Chih Hung
#
# Last Modified: 2025-09-18
#
# Goal: Estimate ballot order effects using conventional methods 
#       (e.g., naive OLS, fixed effects, IPW).
#
# Outputs:
# - outputs/tables/conventional_models.csv
#

# 1. Load Libraries -----------------------------------------------------------

library(tidyverse)
library(estimatr)

# 2. Read Data ----------------------------------------------------------------

elections <- read_csv(file.path("data", "candidates_2008_2020.csv"))

# 3. Conventional Models ------------------------------------------------------

reg_results <- elections |>
  group_by(year) |>
  group_modify(
    ~ {
      naive <- lm_robust(percentage ~ left, data = .x)
      fe <- lm_robust(percentage ~ left + as.factor(n_candidates), data = .x)
      tibble(
        naive_est = coef(naive)["left"],
        naive_se = naive$std.error["left"],
        fe_est = coef(fe)["left"],
        fe_se = fe$std.error["left"],
      )
    }
  ) |>
  ungroup() |>
  pivot_longer(
    cols = -year,
    names_to = c("method", ".value"),
    names_pattern = "(.*)_(.*)"
  )

ipw_results <- elections |>
  group_by(year) |>
  group_modify(
    ~ {
      ipw <- lm_robust(
        percentage ~ left, 
        data = .x, 
        weights = if_else(left == 1, n_candidates, n_candidates / (n_candidates - 1))
      )
      tibble(
        ipw_est = coef(ipw)["left"],
        ipw_se = ipw$std.error["left"]
      )
    }
  ) |>
  ungroup() |>
  pivot_longer(
    cols = -year,
    names_to = c("method", ".value"),
    names_pattern = "(.*)_(.*)"
  )

conventional_models_summary <- bind_rows(reg_results, ipw_results) |>
  mutate(
    method = case_when(
      method == "naive" ~ "Naive",
      method == "fe" ~ "Fixed Effects",
      method == "ipw" ~ "IPW"
    )
  )


# 4. Save Output --------------------------------------------------------------

write_csv(conventional_models_summary, file.path("outputs", "tables", "conventional_models.csv"))

# 0. Description --------------------------------------------------------------
#
# Author: Ting-Chih Hung
#
# Last Modified: 2025-09-18
#
# Goal: Given a test statistic (diff-in-means or rank-based), 
#       perform Fisherian randomization inference 
#       and calculate the p-value.
#       Provides S3 methods for printing, summarizing, and tidying results.
#

require(tidyverse)
require(broom)

# 1. Function -----------------------------------------------------------------

fisherian_test_sre <- function(
  data, 
  outcome, 
  treatment, 
  strata, 
  test_statistic = c(
    "diff_in_means", "diff_in_means_varw", "rank", "rosenbaum"
  ),
  n_sim = 1000, 
  alternative = c("two.sided", "greater", "less"),
  progress = TRUE,
  seed = NULL
) {

  require(dplyr)
  require(purrr)
  
  if (missing(data) || missing(outcome) || missing(treatment) || missing(strata)) {
    stop("All arguments (data, outcome, treatment, strata) must be provided")
  }
  
  test_statistic <- match.arg(test_statistic)
  alternative    <- match.arg(alternative)

  if (!is.null(seed)) set.seed(seed)

  # rank_sre returns an absolute difference by design, so enforce two-sided
  if (test_statistic == "rank" && alternative != "two.sided") {
    warning("`rank` statistic returns an absolute difference; forcing alternative = 'two.sided'.")
    alternative <- "two.sided"
  }
  
  # compute observed statistic
  observed_stat <- switch(
    test_statistic,
    "diff_in_means"      = diff_in_means_sre(data, {{ outcome }}, {{ treatment }}, {{ strata }}),
    "diff_in_means_varw" = diff_in_means_varw_sre(data, {{ outcome }}, {{ treatment }}, {{ strata }}),
    "rank"               = rank_sre(data, {{ outcome }}, {{ treatment }}, {{ strata }}),
    "rosenbaum"          = rosenbaum_sre(data, {{ outcome }}, {{ treatment }}, {{ strata }})
  )
  
  # one permutation: shuffle treatment labels within each stratum (preserving counts)
  single_permutation <- function(i) {
    permuted_data <- data |>
      group_by({{ strata }}) |>
      mutate(
        treatment_perm = sample({{ treatment }}, size = n(), replace = FALSE)
      ) |>
      ungroup()
    
    switch(
      test_statistic,
      "diff_in_means"      = diff_in_means_sre(permuted_data, {{ outcome }}, treatment_perm, {{ strata }}),
      "diff_in_means_varw" = diff_in_means_varw_sre(permuted_data, {{ outcome }}, treatment_perm, {{ strata }}),
      "rank"               = rank_sre(permuted_data, {{ outcome }}, treatment_perm, {{ strata }}),
      "rosenbaum"          = rosenbaum_sre(permuted_data, {{ outcome }}, treatment_perm, {{ strata }})
    )
  }

  # run permutations
  permuted_stats <- map_dbl(1:n_sim, single_permutation, .progress = progress)

  # drop any NA stats (just in case)
  valid_stats <- permuted_stats[!is.na(permuted_stats)]
  n_valid     <- length(valid_stats)
  if (n_valid < n_sim) warning(paste("Using", n_valid, "valid permutations out of", n_sim))

  # p-value
  p_value <- switch(
    alternative,
    "two.sided" = mean(abs(valid_stats) >= abs(observed_stat)),
    "greater"   = mean(valid_stats >= observed_stat),
    "less"      = mean(valid_stats <= observed_stat)
  )

  p_value_se <- sqrt(p_value * (1 - p_value) / n_valid)
  
  result <- list(
    observed_stat = observed_stat,
    p_value = p_value,
    p_value_se = p_value_se,
    test_statistic = test_statistic,
    alternative = alternative,
    n_sim = n_sim,
    n_valid_sim = n_valid,
    permuted_stats = valid_stats,
    seed = seed,
    call = match.call()
  )

  class(result) <- "fisherian_test"
  return(result)
}

# 2. S3 Methods ---------------------------------------------------------------

## 2.1 Print Method -----------------------------------------------------------

print.fisherian_test <- function(x, ...) {
  cat("Fisherian Randomization Test\n")
  cat("============================\n\n")
  cat("Test statistic:", x$test_statistic, "\n")
  cat("Alternative hypothesis:", x$alternative, "\n")
  cat("Number of permutations:", x$n_valid_sim, "\n\n")
  cat("Observed statistic:", round(x$observed_stat, 4), "\n")
  cat("P-value:", format.pval(x$p_value, digits = 4), "\n")
  cat("Standard error of P-value:", round(x$p_value_se, 4), "\n")
  
  # Significance stars
  if (x$p_value < 0.001) {
    cat("***\n")
  } else if (x$p_value < 0.01) {
    cat("**\n")
  } else if (x$p_value < 0.05) {
    cat("*\n")
  } else if (x$p_value < 0.1) {
    cat(".\n")
  }
  
  cat("\nSignif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n")
}

## 2.2 Critical Value Method --------------------------------------------------

critical_values <- function(x, alpha = c(0.1, 0.05, 0.01, 0.001)) {
  UseMethod("critical_values")
}

critical_values.fisherian_test <- function(x, alpha = c(0.1, 0.05, 0.01, 0.001)) {
  
  compute_critical <- function(a) {
    if (x$alternative == "two.sided") {
      # For two-sided: find t such that P(|T| >= |t|) = alpha
      abs_stats <- abs(x$permuted_stats)
      quantile(abs_stats, 1 - a, names = FALSE)
    } else if (x$alternative == "greater") {
      # For greater: find t such that P(T >= t) = alpha  
      quantile(x$permuted_stats, 1 - a, names = FALSE)
    } else { # less
      # For less: find t such that P(T <= t) = alpha
      quantile(x$permuted_stats, a, names = FALSE)
    }
  }
  
  critical_vals <- map_dbl(alpha, compute_critical)
  
  tibble(
    alpha = alpha,
    critical_value = critical_vals,
    alternative = x$alternative,
    reject_if = case_when(
      x$alternative == "two.sided" ~ paste("|statistic| >=", round(critical_vals, 4)),
      x$alternative == "greater" ~ paste("statistic >=", round(critical_vals, 4)),
      x$alternative == "less" ~ paste("statistic <=", round(critical_vals, 4))
    )
  )
}

## 2.3 Summary Method ---------------------------------------------------------

summary.fisherian_test <- function(object, ...) {
  cat("Fisherian Randomization Test Summary\n")
  cat("====================================\n\n")
  
  cat("Call:\n")
  print(object$call)
  cat("\n")
  
  cat("Test Details:\n")
  cat("  Test statistic:", object$test_statistic, "\n")
  cat("  Alternative hypothesis:", object$alternative, "\n")
  cat("  Permutations requested:", object$n_sim, "\n")
  cat("  Valid permutations:", object$n_valid_sim, "\n")
  if (!is.null(object$seed)) cat("  Seed:", object$seed, "\n")
  cat("\n")
  
  cat("Results:\n")
  cat("  Observed statistic:", round(object$observed_stat, 6), "\n")
  cat("  P-value:", format(object$p_value, scientific = FALSE, digits = 6), "\n") 
  cat("  Standard error of P-value:", round(object$p_value_se, 6), "\n")

  cat("\nPermutation Distribution Summary:\n")
  cat("  Min:", round(min(object$permuted_stats), 4), "\n")
  cat("  1st Qu.:", round(quantile(object$permuted_stats, 0.25), 4), "\n")
  cat("  Median:", round(median(object$permuted_stats), 4), "\n")
  cat("  Mean:", round(mean(object$permuted_stats), 4), "\n")
  cat("  3rd Qu.:", round(quantile(object$permuted_stats, 0.75), 4), "\n")
  cat("  Max:", round(max(object$permuted_stats), 4), "\n")

  cat("\nCritical Values:\n")
  crit_vals <- critical_values(object, alpha = c(0.1, 0.05, 0.01))
  for (i in 1:nrow(crit_vals)) {
    cat(sprintf("  α = %.3f: %s\n", 
                crit_vals$alpha[i], 
                crit_vals$reject_if[i]))
  }
}

## 2.4 Tidy Method ------------------------------------------------------------

tidy.fisherian_test <- function(x, ...) {
  require(broom)
  require(tibble)
  tibble::tibble(
    statistic = x$observed_stat,
    p.value = x$p_value,
    p.value.se = x$p_value_se,
    test_statistic = paste(x$test_statistic),
    alternative = x$alternative,
    n_permutations = x$n_valid_sim
  )
}

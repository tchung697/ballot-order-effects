# 0. Description --------------------------------------------------------------
#
# Author: Ting-Chih Hung
#
# Last Modified: 2025-09-18
#
# Goal: Run the entire analysis pipeline for the ballot order effect project.
#       This script ensures full reproducibility from data to the final report.
#

# 1. Set Working Directory ----------------------------------------------------

if (!require(here)) install.packages("here")
library(here)

# 2. Run Analysis Scripts in Order ------------------------------------------

run_script <- function(script_name) {
  tryCatch({
    print(paste("--- Running", script_name, "---"))
    source(here("code", script_name))
    print(paste("--- Finished", script_name, "---"))
  }, error = function(e) {
    stop(paste("Error in", script_name, ":", e$message), call. = FALSE)
  })
}

scripts_to_run <- c(
  "01_descriptive_stats.R",
  "02_conventional_models.R",
  "03_randomization_inference.R",
  "04_generate_visuals.R",
  "05_generate_latex_tables.R"
)

# Execute all scripts
for (script in scripts_to_run) {
  run_script(script)
}

# 3. Render the Quarto Report -------------------------------------------------

if (require(quarto)) {
  print("--- Rendering Quarto Report ---")
  quarto::quarto_render(here("index.qmd"))
  print("--- Report rendering complete ---")
} else {
  warning("Quarto package not found. Please install it to render the report.")
}

print("Full pipeline executed successfully.")

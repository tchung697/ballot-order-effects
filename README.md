# ballot-order-effects

This repository contains the data, code, and manuscript for the paper, "The Ballot Order Effect in Taiwan's Legislative Elections, 2008--2020."

## Data

The data used in this analysis comes from the official legislative election 
results published by the Central Election Commission (CEC) of Taiwan. 
The dataset, `data/candidates_2008_2020.csv`, 
contains detailed information for each candidate in every district 
from 2008 to 2020, 
including electoral district, ballot number, party affiliation, 
vote share, and incumbency status.

## How to Run

To reproduce the analysis, you can either run the `run_all.R` script in R, or use the `Makefile`:

```bash
make all
```

This will:
1.  Run the entire R analysis pipeline.
2.  Compile the LaTeX paper in the `paper/` directory.

## Dependencies

The R scripts require the following packages:

*   `here`
*   `tidyverse`
*   `modelsummary`
*   `knitr`
*   `kableExtra`
*   `ggplot2`


To compile the paper, you will need a LaTeX distribution (e.g., TeX Live, MiKTeX) and the `latexmk` utility.

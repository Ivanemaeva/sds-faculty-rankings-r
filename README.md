# Faculty Job Preference Rankings — Statistical Analysis in R

Statistics for Data Science project (Università di Pisa) — Prof. Salvatore Ruggieri.

## Overview

Re-implementation of statistical methods for analyzing ranked preference data, applied to a survey where faculty members rank six job-related attributes (contract, salary, health care, workload, chair support, travel budget) in order of importance.

## Methods

- **Descriptive statistics for ranking data**: rank aggregation and mean-rank statistics (`pmr` package)
- **Chi-square goodness-of-fit test** for non-random ranking behavior
- **Group comparison**: bachelor's vs. graduate degree holders (chi-square, Fisher's exact test, t-tests)
- **Unfolding Multidimensional Scaling (UMDS)**: joint configuration of items and respondents, Shepard diagram for model fit, stress/variance explained
- **Plackett-Luce Model (PLM)**: maximum-likelihood ranking model, item worth estimation, pairwise Z-test between attributes (e.g., salary vs. health care)
- **Quasi-variance computation** for stable confidence intervals on model coefficients
- **PLM with covariates (PLM-tree)**: rankings modeled as a function of respondent experience

## Repository structure


SDS_RankedData_R.R      # full analysis script
data/
└── pare-1331-finch.xlsx   # faculty survey ranking data
report/


## Tech stack

R — packages: `pmr`, `PlackettLuce`, `prefmod`, `qvcalc`, `smacof`, `readxl`

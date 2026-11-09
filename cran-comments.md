## Submission

This is an update from 1.0.0 to 1.1.0.

New: a one-call workflow for replicated field trials, ANOVA with SEm, CD,
CV and letter groups, objective indicator weights (entropy, CRITIC, PCA),
TOPSIS ranking, ranking robustness with Kendall's W, energy budgeting from
physical quantities, two datasets and ggplot2 plot methods.

Fixed: an extra factor of 0.1 in soil_carbon_stock(), reversed row labels
in nexus_heatmap_gg(), a fixed nutrient harvest index in
nutrient_use_efficiency(), area handling in water_footprint(), and an
incorrect reference DOI. Details are in NEWS.md. New arguments default to
the 1.0.0 behaviour except where a calculation was wrong.

## Test environments

* Local: Ubuntu 24.04, R 4.6.2 (R CMD check --as-cran)
* Local: Ubuntu 24.04, R 4.6.2 without the suggested package ggplot2
* (to run before submission) win-builder R-devel, and the GitHub Actions
  workflow for macOS, Windows and Ubuntu

## R CMD check results

0 errors | 0 warnings | 0 notes

## Reverse dependencies

There are no reverse dependencies.

# nullabor 0.3.15

* Reduced examples for lineup_residuals() and lineup_histogram() so passes CRAN automatic check

# nullabor 0.3.14

* Fix to qqplot for standardised residuals

# nullabor 0.3.13

- Updated null_lm to optionally compute leverages and standardized residuals.
- Added a new function, lineup_residuals, for creating four different types of lineup plots for residuals with a single line of code.
- Added two new functions, lineup_histograms and lineup_qq for creating lineup histogram and Q-Q plots to assess distributional assumptions.
- Added two new vignettes.
- Adds Måns Thulin as a co-author, due to the substantial additions.

# nullabor 0.3.12

- CITATION revised to satisfy CRAN check
- And URLs in vignettes change to DOI because the DOI URL is apparently not valid according to the automated checks

# nullabor 0.3.11

- URLs in DESCRIPTION, CITATION, vignettes were causing an error on CRAN checks!

# nullabor 0.3.10

- removed dependency on reshape2
- adjusting other dependencies
- new theme to remove context from plots
- updated roxygen dependency

# nullabor 0.3.9

- CRAN fixes, minor code changes

# nullabor 0.3.8

- CRAN fixes, minor code changes

# nullabor 0.3.7

- Bug fixes, minor

# nullabor 0.3.6

- Added a sample size calculator

# nullabor 0.3.5

- Added time series null generating mechanism
- Fixed a bug in the residual generating mechanism reported by Jan Vanhove
- Changed the encrypt/decrypt to have 2 digits
- Using dplyr and purrr for making lineups now thanks to Stuart Lee

# nullabor 0.3.6

- Fix from Jenny Bryan on tidyverse
- CITATION file added
- vignettes update

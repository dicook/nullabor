## Overview 

These are small changes, including removing some dependencies. Also fixed issues at https://cran.rstudio.com//web/checks/check_results_nullabor.html. And links flagged by CRAN check.

── R CMD check results ──────────── nullabor 0.3.12 ────
Duration: 1m 2.3s

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

## Test environment

* R version 4.4.1 (2024-06-14) -- "Race for Your Life"
* https://win-builder.r-project.org/ A Note is produced that says URLs might be invalid. They have all been checked to be correct, and the correct URL, even though several exist.
* Linux, using R-hub v2. Has an error on compiling vignettes because the build machine doesn't have Rmarkdown. The vignettes build everywhere else. 

## Reverse dependencies

Installing DEV version of nullabor
── CHECK ─────────────────────────────────────────── 3 packages ──
✔ agridat 1.23                           ── E: 0     | W: 0     | N: 0    
✔ metaviz 0.3.1                          ── E: 0     | W: 0     | N: 0    
✔ regressinator 0.1.3                    ── E: 0     | W: 0     | N: 0    
OK: 3                                                           
BROKEN: 0

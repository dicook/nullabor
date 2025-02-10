## Overview 

These are small changes that fix several bugs. 

Also fixed https://win-builder.r-project.org/incoming_pretest/nullabor_0.3.14_20250210_040443/Debian/00check.log where 
the package failed automatic checks on linux because lineup_histograms() and lineup_residuals() took 5.669s and 5.574s to 
complete on linux, by removing one example in each.

- Using devtools::check()

── R CMD check results ───────────────────────────────── nullabor 0.3.15 ────
Duration: 2m 29s

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

- Using R CMD CHECK ../nullabor_0.3.15.tar.gz

* DONE

Status: OK


## Test environment

* R version 4.4.2 (2024-10-31) -- "Pile of Leaves"

Checks made using R-CMD-check.yaml GitHub Actions on the repo for environments: 
linux, macos, windows. It fails on the vignettes due to rmarkdown 
not being available on GitHub, beyond my control, but all other checks pass. 

## Reverse dependencies

All are ok

> revdep_check()
── INIT ──────────────────────────────────────────────── Computing revdeps ──
── INSTALL ──────────────────────────────────────────────────── 2 versions ──
Installing DEV version of nullabor
── CHECK ────────────────────────────────────────────────────── 3 packages ──
✔ agridat 1.24                           ── E: 0     | W: 0     | N: 0       
✔ metaviz 0.3.1                          ── E: 0     | W: 0     | N: 0       
✔ regressinator 0.2.0                    ── E: 0     | W: 0     | N: 0       
OK: 3                                                                      
BROKEN: 0
Total time: 9 min

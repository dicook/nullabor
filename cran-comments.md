## Overview 

These are small changes that fix several bugs. 

Also fixed https://win-builder.r-project.org/incoming_pretest/nullabor_0.3.14_20250210_040443/Debian/00check.log where 
the package failed automatic checks on linux because lineup_histograms() and lineup_residuals() took 5.669s and 5.574s to 
complete on linux, by removing one example in each.

- Using devtools::check()

── R CMD check results ───────────────────────────────── nullabor 0.3.16 ────
Duration: 1m 8.7s

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

## Test environment

* R version 4.6.1 (2026-06-24) -- "Happy Hop"

Using `rhub::rhub_check()` 

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

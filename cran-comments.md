## Overview 

These are small changes that address changes in purrr.

## Checks

- Using devtools::check()

── R CMD check results ───────────────────────────────── nullabor 0.3.16 ────
Duration: 1m 8.7s

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

## Test environment

* R version 4.6.1 (2026-06-24) -- "Happy Hop"

Using `rhub::rhub_check()` at https://github.com/dicook/nullabor/actions/runs/36212952059, suggests all is good.

## Reverse dependencies

One reverse dependency `metaviz` is broken. The authors have been notified and a pull request to fix the issue has been made.

> revdepcheck::revdep_check()
── INIT ───────────────────────────────────────────────────────────────────── Computing revdeps ──
── INSTALL ───────────────────────────────────────────────────────────────────────── 2 versions ──
Installing CRAN version of nullabor
Installing DEV version of nullabor
Installing 3 packages: DEoptimR, flexmix, fpc
── CHECK ─────────────────────────────────────────────────────────────────────────── 3 packages ──
✔ agridat 1.26                           ── E: 0     | W: 0     | N: 0                            
✖ metaviz 0.3.1                          ── E: 0  +1 | W: 0     | N: 0                            
✔ regressinator 0.3.1                    ── E: 0     | W: 0     | N: 0                            
OK: 2
BROKEN: 1
Total time: 16 min
── REPORT ────────────────────────────────────────────────────────────────────────────────────────
Writing summary to 'revdep/README.md'
Writing problems to 'revdep/problems.md'
Writing failures to 'revdep/failures.md'
Writing CRAN report to 'revdep/cran.md'
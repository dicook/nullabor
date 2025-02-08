## Overview 

These are small changes that fix several bugs.

- Using devtools::check()

── R CMD check results ───────────────────────────────── nullabor 0.3.14 ────
Duration: 2m 29s

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

- Using R CMD CHECK ../nullabor_0.3.14.tar.gz

* DONE

Status: OK


## Test environment

* R version 4.4.2 (2024-10-31) -- "Pile of Leaves"

Checks made using rhub::rhub_check() for environments: linux, macos, windows

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

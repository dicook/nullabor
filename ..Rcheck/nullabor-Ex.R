pkgname <- "nullabor"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('nullabor')

assign(".oldSearch", search(), pos = 'CheckExEnv')
cleanEx()
nameEx("decrypt")
### * decrypt

flush(stderr()); flush(stdout())

### Name: decrypt
### Title: Use decrypt to reveal the position of the real data.
### Aliases: decrypt

### ** Examples

decrypt("0uXR2p rut L2O2")



cleanEx()
nameEx("lineup")
### * lineup

flush(stderr()); flush(stdout())

### Name: lineup
### Title: The line-up protocol.
### Aliases: lineup

### ** Examples

if (require("ggplot2")) {
qplot(mpg, wt, data = mtcars) %+%
  lineup(null_permute("mpg"), mtcars) +
  facet_wrap(~ .sample)
qplot(mpg, .sample, data = lineup(null_permute("cyl"), mtcars),
  colour = factor(cyl))
}



cleanEx()
nameEx("null_lm")
### * null_lm

flush(stderr()); flush(stdout())

### Name: null_lm
### Title: Generate null data with null residuals from a model.
### Aliases: null_lm

### ** Examples

if (require("ggplot2") && require("reshape2")) {

x <- lm(tip ~ total_bill, data = tips)
tips.reg <- data.frame(tips, .resid = residuals(x), .fitted = fitted(x))
qplot(total_bill, .resid, data = tips.reg) %+%
  lineup(null_lm(tip ~ total_bill, method = "rotate"), tips.reg) +
  facet_wrap(~ .sample)
}



### * <FOOTER>
###
cat("Time elapsed: ", proc.time() - get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')

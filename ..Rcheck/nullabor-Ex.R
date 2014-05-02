pkgname <- "nullabor"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('nullabor')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("bin_dist")
### * bin_dist

flush(stderr()); flush(stdout())

### Name: bin_dist
### Title: Binned Distance
### Aliases: bin_dist

### ** Examples

with(mtcars, bin_dist(data.frame(wt, mpg), data.frame(sample(wt), mpg)))



cleanEx()
nameEx("box_dist")
### * box_dist

flush(stderr()); flush(stdout())

### Name: box_dist
### Title: Distance based on side by side Boxplots for two levels
### Aliases: box_dist

### ** Examples

if(require('plyr')) {with(mtcars, box_dist(data.frame(as.factor(am), mpg),
data.frame(as.factor(sample(am)), mpg)))}



cleanEx()
nameEx("calc_diff")
### * calc_diff

flush(stderr()); flush(stdout())

### Name: calc_diff
### Title: Calculating the difference between true plot and the null plot
###   with the maximum distance.
### Aliases: calc_diff

### ** Examples

calc_diff(lineup(null_permute('mpg'), mtcars, pos = 10), var = c('mpg', 'wt'), X.bin = 5, Y.bin = 5, pos = 10)



cleanEx()
nameEx("decrypt")
### * decrypt

flush(stderr()); flush(stdout())

### Name: decrypt
### Title: Use decrypt to reveal the position of the real data.
### Aliases: decrypt

### ** Examples

decrypt('0uXR2p rut L2O2')



cleanEx()
nameEx("distmet")
### * distmet

flush(stderr()); flush(stdout())

### Name: distmet
### Title: Empirical distribution of the distance
### Aliases: distmet

### ** Examples

distmet(lineup(null_permute('mpg'), mtcars, pos = 10), var = c('mpg', 'wt'), 'reg_dist', null_permute('mpg'), pos = 10)
distmet(lineup(null_permute('mpg'), mtcars, pos = 10), var = c('mpg', 'wt'), 'bin_dist', null_permute('mpg'), pos = 10, dist.arg = list(X.bin = 5, Y.bin = 5))
lineup.dat <- lineup(null_permute('mpg'), mtcars)
qplot(mpg, wt, data = lineup.dat, geom = 'point') + facet_wrap(~ .sample)
#decrypt('...') # Copy and paste the output from lineup.dat to get the
#position of the true plot
#[1] 'True data in position 13' # Use pos = 13
distmet(lineup.dat, var = c('mpg', 'wt'), 'bin_dist', null_permute('mpg'), pos = 13, dist.arg = list(X.bin = 5, Y.bin = 5))
#Example using uni_dist
mod <- lm(wt ~ mpg, data = mtcars)
resid.dat <- data.frame(residual = mod$resid)
lineup.dat <- lineup(null_dist('residual', dist = 'normal'), resid.dat)
qplot(residual, data = lineup.dat, geom = 'histogram', binwidth = 0.25) + facet_wrap(~ .sample)
#decrypt('....') #Copy and paste to get the true position
distmet(lineup.dat, var = 'residual', 'uni_dist', null_dist('residual', dist = 'normal'), pos = 19)
# Assuming pos = 19; but put the true position for pos



cleanEx()
nameEx("distplot")
### * distplot

flush(stderr()); flush(stdout())

### Name: distplot
### Title: Plotting the distribution of the distance measure
### Aliases: distplot

### ** Examples

distplot(distmet(lineup(null_permute('mpg'), mtcars, pos = 10), var = c('mpg', 'wt'), 'reg_dist', null_permute('mpg'), pos = 10))



cleanEx()
nameEx("lineup")
### * lineup

flush(stderr()); flush(stdout())

### Name: lineup
### Title: The line-up protocol.
### Aliases: lineup

### ** Examples

if (require('ggplot2')) {
qplot(mpg, wt, data = mtcars) %+%
  lineup(null_permute('mpg'), mtcars) +
  facet_wrap(~ .sample)
qplot(mpg, .sample, data = lineup(null_permute('cyl'), mtcars),
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

x <- lm(tip ~ total_bill, data = tips)
tips.reg <- data.frame(tips, .resid = residuals(x), .fitted = fitted(x))
qplot(total_bill, .resid, data = tips.reg) %+%
  lineup(null_lm(tip ~ total_bill, method = 'rotate'), tips.reg) +
  facet_wrap(~ .sample)



cleanEx()
nameEx("opt_diff")
### * opt_diff

flush(stderr()); flush(stdout())

### Name: opt_diff
### Title: Finds the number of bins in x and y direction which gives the
###   maximum binned distance
### Aliases: opt_diff

### ** Examples

opt_diff(lineup(null_permute('mpg'), mtcars, pos = 10), var = c('mpg', 'wt'), 2, 10, 2, 10, 10, plot = TRUE)



cleanEx()
nameEx("reg_dist")
### * reg_dist

flush(stderr()); flush(stdout())

### Name: reg_dist
### Title: Distance based on the regression parameters
### Aliases: reg_dist

### ** Examples

with(mtcars, reg_dist(data.frame(wt, mpg), data.frame(sample(wt), mpg)))



cleanEx()
nameEx("sep_dist")
### * sep_dist

flush(stderr()); flush(stdout())

### Name: sep_dist
### Title: Distance based on separation of clusters
### Aliases: sep_dist

### ** Examples

if(require('fpc')) { with(mtcars, sep_dist(data.frame(wt, mpg,
as.numeric(as.factor(mtcars$cyl))), data.frame(sample(wt), mpg,
as.numeric(as.factor(mtcars$cyl))), clustering = TRUE))}



cleanEx()
nameEx("uni_dist")
### * uni_dist

flush(stderr()); flush(stdout())

### Name: uni_dist
### Title: Distance for univariate data
### Aliases: uni_dist

### ** Examples

if(require('moments')){uni_dist(rnorm(100), rpois(100, 2))}



### * <FOOTER>
###
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')

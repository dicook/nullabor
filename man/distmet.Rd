\name{distmet}
\alias{distmet}
\title{Empirical distribution of the distance}
\usage{
distmet(lineup.dat, var, met, method, pos, repl = 1000, dist.arg = NULL,
  m = 20, progress.bar = TRUE)
}
\arguments{
  \item{lineup.dat}{lineup data}

  \item{var}{a vector of names of the variables to be used}

  \item{met}{distance metric needed to calculate the
  distance as a character}

  \item{method}{method for generating null data sets}

  \item{pos}{position of the observed data in the lineup}

  \item{repl}{number of sets of null plots selected to
  obtain the distribution; 1000 by default}

  \item{dist.arg}{a list or vector of inputs for the
  distance metric met; NULL by default}

  \item{m}{the number of plots in the lineup; m = 20 by
  default}

  \item{progress.bar}{LOGICAL; shows progress of function,
  by default TRUE}
}
\description{
The empirical distribution of the distance measures is
calculated based on the mean distance of each of the null
plots from the other null plots in a lineup.
}
\examples{
if(require('reshape')) {
if(require('plyr')) { distmet(lineup(null_permute('mpg'), mtcars, pos =
10), var = c('mpg', 'wt'), 'reg_dist', null_permute('mpg'), pos = 10) }}
if(require('reshape')) {
if(require('plyr')) {
distmet(lineup(null_permute('mpg'), mtcars, pos = 10), var = c('mpg', 'wt'), 'bin_dist',
null_permute('mpg'), pos = 10, dist.arg = list(X.bin = 5, Y.bin =
5)) }}
if(require('ggplot2')) {
if(require('reshape')) {
if(require('plyr')) {
lineup.dat <- lineup(null_permute('mpg'), mtcars)
qplot(mpg, wt, data = lineup.dat, geom = 'point') + facet_wrap(~ .sample)
#decrypt('...') # Copy and paste the output from lineup.dat to get the
#position of the true plot
#[1] 'True data in position 13' # Use pos = 13
distmet(lineup.dat, var = c('mpg', 'wt'), 'bin_dist', null_permute('mpg'), pos = 13,
dist.arg = list(X.bin = 5, Y.bin = 5)) }}}
#Example using uni_dist
if(require('moments')) {
if(require('ggplot2')) {
if(require('reshape')) {
if(require('plyr')) {
mod <- lm(wt ~ mpg, data = mtcars)
resid.dat <- data.frame(residual = mod$resid)
lineup.dat <- lineup(null_dist('residual', dist = 'normal'), resid.dat)
qplot(residual, data = lineup.dat, geom = 'histogram', binwidth = 0.25) +
facet_wrap(~ .sample)
#decrypt('....') #Copy and paste to get the true position
distmet(lineup.dat, var = 'residual', 'uni_dist', null_dist('residual', dist = 'normal'),
pos = 19) }}}} # Assuming pos = 19; but put the true position for pos
}


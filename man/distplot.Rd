\name{distplot}
\alias{distplot}
\title{The distribution of the distance measure is plotted with the distances for
the null plots and true plot overlaid.}
\usage{
  distplot(dat, m = 20)
}
\arguments{
  \item{dat}{output from \code{\link{distmet}}}

  \item{m}{the number of plots in the lineup; m = 20 by
  default}
}
\description{
  The distribution of the distance measure is plotted with
  the distances for the null plots and true plot overlaid.
}
\examples{
if(require('ggplot2')) {
if(require('reshape')) {
if(require('plyr')) {distplot(distmet(lineup(null_permute('mpg'), mtcars, pos
= 10), var = c('mpg', 'wt'), 'reg_dist', null_permute('mpg'), pos = 10))}}}
}


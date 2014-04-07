\name{calc_diff}
\alias{calc_diff}
\title{Calculating the difference between true plot and the null plot with the maximum distance.}
\usage{
  calc_diff(lineup.dat, var, X.bin, Y.bin, pos, m = 20)
}
\arguments{
  \item{lineup.dat}{lineup data to get the lineup}

  \item{var}{a vector of names of the variables to be used
  to calculate the difference}

  \item{X.bin}{number of bins on the x-direction}

  \item{Y.bin}{number of bins on the y-direction}

  \item{pos}{position of the true plot in the lineup}

  \item{m}{number of plots in the lineup, by default m =
  20}
}
\value{
  difference between the mean distance of the true plot and
  the maximum mean distance of the null plots
}
\description{
  Binned distance is used to calculate the mean distance
  between the true plot and all the null plots in a lineup.
  The mean distances of each null plot to all the other
  null plots are calculated. The difference between the
  mean distance of the true plot and the maximum mean
  distance of the null plots is then calculated.
}
\examples{
if(require('plyr')) { if(require('reshape'))
{ calc_diff(lineup(null_permute('mpg'), mtcars, pos = 10), var = c('mpg', 'wt'),
X.bin = 5, Y.bin = 5, pos = 10)}}
}


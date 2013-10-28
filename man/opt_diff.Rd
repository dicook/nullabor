\name{opt_diff}
\alias{opt_diff}
\title{finds the difference using \code{calc_diff} for all combinations of
number of bins in x and y direction}
\usage{
  opt_diff(lineup.dat, xlow, xhigh, ylow, yhigh, pos,
    plot = FALSE, m = 20)
}
\arguments{
  \item{lineup.dat}{lineup data to get the lineup}

  \item{xlow}{the lowest value of number of bins on the
  x-direction}

  \item{xhigh}{the highest value of number of bins on the
  x-direction}

  \item{ylow}{the lowest value of number of bins on the
  y-direction}

  \item{yhigh}{the highest value of number of bins on the
  y-direction}

  \item{pos}{position of the true plot in the lineup}

  \item{plot}{LOGICAL; if true, returns a tile plot for the
  combinations of number of bins with the differences as
  weights}

  \item{m}{number of plots in the lineup, by default m =
  20}
}
\value{
  a dataframe with the number of bins and differences the
  maximum mean distance of the null plots
}
\description{
  finds the difference using \code{calc_diff} for all
  combinations of number of bins in x and y direction
}


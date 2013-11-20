\name{bin_dist}
\alias{bin_dist}
\title{Binned Distance}
\usage{
  bin_dist(X, PX, lineup.dat = NULL, X.bin = 5, Y.bin = 5)
}
\arguments{
  \item{X}{a data.frame with two variables, the first two
  columns are used}

  \item{PX}{another data.frame with two variables, the
  first two columns are used}

  \item{lineup.dat}{data.frame providing lineup data if
  used with a lineup, so that the binning is done based on
  the lineup data and not the individual plots, by default
  lineup.dat = NULL}

  \item{X.bin}{number of bins on the x-direction, by
  default X.bin = 5}

  \item{Y.bin}{number of bins on the y-direction, by
  default Y.bin = 5}
}
\value{
  distance between X and PX
}
\description{
  euclidean distance is calculated by binning the data and
  counting the number of points in each bin
}
\examples{
with(mtcars, bin_dist(data.frame(wt, mpg), data.frame(sample(wt), mpg)))
}


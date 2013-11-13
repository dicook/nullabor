\name{bin_dist}
\alias{bin_dist}
\title{Binned Distance}
\usage{
bin_dist(X, PX, X.bin = 5, Y.bin = 5)
}
\arguments{
  \item{X}{a data.frame with two variables, the first two
  columns are used}

  \item{PX}{another data.frame with two variables, the
  first two columns are used}

  \item{X.bin}{number of bins on the x-direction, by
  default nbin.X = 5}

  \item{Y.bin}{number of bins on the y-direction, by
  default nbin.Y = 5}
}
\value{
  distance between X and PX
}
\description{
  euclidean distance is calculated by binning the data and
  counting the number of points in each bin
}

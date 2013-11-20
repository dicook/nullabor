\name{reg_dist}
\alias{reg_dist}
\title{Distance based on the regression parameters}
\usage{
  reg_dist(X, PX, nbins = 1)
}
\arguments{
  \item{X}{a data.frame with two variables, the first
  column giving the explanatory variable and the second
  column giving the response variable}

  \item{PX}{another data.frame with two variables, the
  first column giving the explanatory variable and the
  second column giving the response variable}

  \item{nbins}{number of bins on the x-direction, by
  default nbins = 1}
}
\value{
  distance between X and PX
}
\description{
  Distance based on the regression parameters
}
\examples{
with(mtcars, reg_dist(data.frame(wt, mpg), data.frame(sample(wt), mpg)))
}


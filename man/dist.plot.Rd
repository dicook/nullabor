\name{dist.plot}
\alias{dist.plot}
\title{Plotting the distribution of the distances with the distances for
the null plots and true plot overlaid}
\usage{
  dist.plot(dat, pos, m = 20)
}
\arguments{
  \item{dat}{output from \code{\link{distmet}}}

  \item{pos}{position of the observed data in the lineup}

  \item{m}{the number of plots in the lineup; m = 20 by
  default}
}
\description{
  Plotting the distribution of the distances with the
  distances for the null plots and true plot overlaid
}
\examples{
dist.plot(dat, pos = 10) ## position of the true plot in \\code{\\link{distmet}}
example
}


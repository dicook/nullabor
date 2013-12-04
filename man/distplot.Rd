\name{distplot}
\alias{distplot}
\title{Plotting the distribution of the distances with the distances for
the null plots and true plot overlaid}
\usage{
  distplot(dat, pos, m = 20)
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
if(require("ggplot2")) {distplot(distmet(subset(lineup(null_permute('mpg'),
mtcars, pos = 10), select = c(mpg, wt, .sample)), reg_dist, null_permute('mpg'), pos = 10),
pos = 10)} ## position of the true plot in \\code{\\link{distmet}} example
}


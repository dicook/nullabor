\name{uni_dist}
\alias{uni_dist}
\title{Distance for univariate data}
\usage{
  uni_dist(X, PX)
}
\arguments{
  \item{X}{a data.frame where the first column is only
  used}

  \item{PX}{another data.frame where the first column is
  only used}
}
\value{
  distance between X and PX
}
\description{
  distance is calculated based on the first four moments
}
\examples{
if(require("moments")){uni_dist(rnorm(100), rpois(100, 2))}
}


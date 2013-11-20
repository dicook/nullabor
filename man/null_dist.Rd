\name{null_dist}
\alias{null_dist}
\title{Generate null data with a specific distribution.}
\usage{
  null_dist(var, dist, params = NULL)
}
\arguments{
  \item{var}{variable name}

  \item{dist}{distribution name. One of: beta, cauchy,
  chi-squared, exponential, f, gamma, geometric,
  log-normal, lognormal, logistic, negative binomial,
  normal, poisson, t, weibull}

  \item{params}{list of parameters of distribution. If
  \code{NULL}, will use \code{\link[MASS]{fitdistr}} to
  estimate them.}
}
\value{
  a function that given \code{data} generates a null data
  set.  For use with \code{\link{lineup}} or
  \code{\link{rorschach}}
}
\description{
  Null hypothesis: variable has specified distribution
}


\name{null_permute}
\alias{null_permute}
\title{Generate null data by permuting a variable.}
\usage{
  null_permute(var)
}
\arguments{
  \item{var}{name of variable to permute}
}
\value{
  a function that given \code{data} generates a null data
  set.  For use with \code{\link{lineup}} or
  \code{\link{rorschach}}
}
\description{
  Null hypothesis: variable is independent of others
}


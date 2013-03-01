\name{distmet}
\alias{distmet}
\title{Calculates the distance measures}
\usage{
  distmet(dat, method, pos, meas.distr = FALSE,
    plot = FALSE, repl = 10000, no.samp = 10)
}
\arguments{
  \item{dat}{lineup data}

  \item{method}{method for generating null data sets}

  \item{pos}{position of the observed data in the lineup}

  \item{meas.distr}{LOGICAL; if TRUE, returns the distn of
  the distance measures, by default FALSE}

  \item{plot}{LOGICAL; if TRUE and if meas.distr is TRUE,
  returns density plot showing the distn of the measures}

  \item{repl}{the number of replicates}

  \item{no.samp}{number of samples}
}
\description{
  Calculates the distance measures
}
\author{
  Niladri Roy Chowdhury \email{niladrir
}


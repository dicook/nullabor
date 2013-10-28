\name{distmet}
\alias{distmet}
\title{Calculates the distance measures}
\usage{
  distmet(lineup.dat, met, method, pos, m = 20,
    dist.arg = NULL, plot = TRUE)
}
\arguments{
  \item{lineup.dat}{lineup data}

  \item{met}{distance metric needed to calculate the
  distance}

  \item{method}{method for generating null data sets}

  \item{pos}{position of the observed data in the lineup}

  \item{m}{the number of plots in the lineup; m = 20 by
  default}

  \item{dist.arg}{a vector of inputs for the distance
  metric met; NULL by default}

  \item{plot}{LOGICAL; if TRUE, returns density plot
  showing the distn of the measures; TRUE by default}
}
\description{
  Calculates the distance measures
}
\author{
  Niladri Roy Chowdhury
}


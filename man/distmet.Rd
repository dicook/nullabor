\name{distmet}
\alias{distmet}
\title{Calculates the distance measures}
\usage{
  distmet(lineup.dat, met, method, pos, dist.arg = NULL,
    m = 20)
}
\arguments{
  \item{lineup.dat}{lineup data}

  \item{met}{distance metric needed to calculate the
  distance}

  \item{method}{method for generating null data sets}

  \item{pos}{position of the observed data in the lineup}

  \item{m}{the number of plots in the lineup; m = 20 by
  default}

  \item{dist.arg}{a list of inputs for the distance metric
  met; NULL by default}

  \item{plot}{LOGICAL; if TRUE, returns density plot
  showing the distn of the measures; TRUE by default}
}
\description{
  Calculates the distance measures
}
\examples{
if(require("reshape")) {
	if(require("plyr")) { distmet(lineup(null_permute('mpg'), mtcars, pos =
10), var = c("mpg", "wt"), reg_dist, null_permute('mpg'), pos = 10) }}
if(require("reshape")) {
if(require("plyr")) {
distmet(lineup(null_permute('mpg'), mtcars, pos = 10), var = c("mpg", "wt"), bin_dist,
null_permute('mpg'), pos = 10, dist.arg = list(dist.lineup = TRUE, X.bin = 5, Y.bin =
5)) }}
}


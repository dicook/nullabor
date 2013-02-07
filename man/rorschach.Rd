\name{rorschach}
\alias{rorschach}
\title{The Rorschach protocol.}
\usage{
  rorschach(method, true = NULL, n = 20, p = 0)
}
\arguments{
  \item{method}{method for generating null data sets}

  \item{true}{true data set. If \code{NULL},
  \code{\link{find_plot_data}} will attempt to extract it
  from the current ggplot2 plot.}

  \item{n}{total number of samples to generate (including
  true data)}

  \item{p}{probability of including true data with null
  data.}
}
\description{
  This protocol is used to calibrate the eyes for variation
  due to sampling. All plots are typically null data sets,
  data that is consistent with a null hypothesis. The
  protocol is described in Buja, Cook, Hofmann, Lawrence,
  Lee, Swayne, Wickham (2009) Statistical inference for
  exploratory data analysis and model diagnostics, Phil.
  Trans. R. Soc. A, 367, 4361-4383.
}


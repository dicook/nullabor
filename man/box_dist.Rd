\name{box_dist}
\alias{box_dist}
\title{Distance based on side by side Boxplots for two levels}
\usage{
  box_dist(X, PX)
}
\arguments{
  \item{X}{a data.frame with one factor variable and one
  continuous variable}

  \item{PX}{a data.frame with one factor variable and one
  continuous variable}
}
\value{
  distance between X and PX
}
\description{
  distance is calculated by looking at the difference
  between first quartile, median and third quartile
}
\examples{
with(mtcars, box_dist(data.frame(as.factor(am), mpg),
data.frame(as.factor(sample(am)), mpg)))
}


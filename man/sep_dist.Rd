\name{sep_dist}
\alias{sep_dist}
\title{Distance based on separation of clusters}
\usage{
  sep_dist(X, PX, clustering = FALSE, nclust = 3)
}
\arguments{
  \item{X}{a data.frame with two or three columns, the
  first two columns providing the dataset}

  \item{PX}{a data.frame with two or three columns, the
  first two columns providing the dataset}

  \item{clustering}{LOGICAL; if TRUE, the third column is
  used as the clustering variable, by default FALSE}

  \item{nclust}{the number of clusters to be obtained by
  hierarchial clustering, by default nclust = 3}
}
\value{
  distance between X and PX
}
\description{
  distance based on the separation between clusters
  separation is the minimum distances of a point in the
  cluster to a a point of another cluster
}
\examples{
with(mtcars, sep_dist(data.frame(wt, mpg, as.factor(cyl)),
data.frame(sample(wt), mpg, as.factor(cyl)), clustering = TRUE))
}


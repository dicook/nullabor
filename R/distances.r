## Distance Metrics -----------------------------------

#' Distance based on the regression parameters
#'
#' Dataset X is binned into 5 bins in x-direction. A regression line is fitted to the
#' data in each bin and the regression coefficients are noted. Same is done for
#' dataset PX. An euclidean distance is calculated between the two sets of regression
#' parameters. If the relationship between X and PX looks linear, number of bins should
#' be equal to 1.
#' @param X a data.frame with two variables, the first column giving
#' the explanatory variable and the second column giving the response
#' variable
#' @param PX another data.frame with two variables, the first column giving
#' the explanatory variable and the second column giving the response
#' variable
#' @param nbins number of bins on the x-direction, by default nbins = 1
#' @param intercept include the distances between intercepts?
#' @param scale logical value: should the variables be scaled before computing regression coefficients?
#' @return distance between X and PX
#' @importFrom stats dist
#' @importFrom magrittr %>%
#' @importFrom dplyr do
#' @export
#' @examples with(mtcars, reg_dist(data.frame(wt, mpg), data.frame(sample(wt), mpg)))
reg_dist <- function(X, PX, nbins = 1, intercept=TRUE, scale=TRUE) {
  .group <- NULL
  . <- NULL
  dc <- function(dX) {
    if (scale) dX <- data.frame(scale(dX))
    dX$.group <- 1
    if (nbins > 1) dX$.group <- cut(dX[,1], breaks=nbins)
    dX$.y <- dX[,2]
    dX$.x <- dX[,1]

    group_by(dX, .group) %>% do(data.frame(rbind(stats::coef(stats::lm(.y~.x, data=.)))))
  }

  beta.X <- dc(X)
  beta.PX <- dc(PX)
  if (intercept)
    return(sum((beta.X[,-1] - beta.PX[,-1])^2))
  else
    return(sum((beta.X[,3] - beta.PX[,3])^2))
}

#' Binned Distance
#'
#' Data X is binned into X.bin bins in x-direction and Y.bins in y-direction. The number
#' of points in each cell is then counted. Same is done for data PX. An euclidean
#' distance is calculated between the number of points in each cell between X and PX.
#'
#' @param X a data.frame with two variables, the first two columns
#' are used
#' @param PX another data.frame with two variables, the first two columns
#' are used
#' @param lineup.dat lineup data so that the binning is done based on the lineup data and not
#' the individual plots, by default lineup.dat = lineup.dat ; if one wishes to calculate the
#' binned distance between two plots, one should use lineup.dat = NULL
#' @param X.bin number of bins on the x-direction, by default X.bin = 5
#' @param Y.bin number of bins on the y-direction, by default Y.bin = 5
#' @return distance between X and PX
#' @export
#' @examples with(mtcars, bin_dist(data.frame(wt, mpg), data.frame(sample(wt), mpg),
#' lineup.dat = NULL))
bin_dist <- function (X, PX, lineup.dat = lineup.dat, X.bin = 5, Y.bin = 5)
{
  # determine cutoff points - if lineup.dat is provided, use overall cutoff points:
  bin2d <- function(dX) {
    if (is.null(range1)) range1 <- range(as.numeric(dX[, 1]))
    if (is.null(range2)) range2 <- range(as.numeric(dX[, 2]))

    breaks1 <- seq(range1[1], range1[2], length.out=X.bin+1)
    breaks2 <- seq(range2[1], range2[2], length.out=Y.bin+1)
    as.numeric(table(cut(as.numeric(dX[,1]), breaks=breaks1, include.lowest=TRUE),
                     cut(dX[,2], breaks=breaks2, include.lowest=TRUE)))
  }

  range1 <- range2 <- NULL
  if (!is.null(lineup.dat)) {
    range1 <- range(as.numeric(lineup.dat[, 1]))
    range2 <- range(as.numeric(lineup.dat[, 2]))
  }

  if (!is.numeric(X[,1])) X.bin <- length(unique(X[,1]))

  sqrt(sum( (bin2d(X) - bin2d(PX))^2 ))
}

#' Distance for univariate data
#'
#' The first four moments is calculated for data X and data PX. An euclidean distance
#' is calculated between these moments for X and PX.
#'
#' @param X a data.frame where the first column is only used
#' @param PX another data.frame where the first column is only used
#' @return distance between X and PX
#' @export
#' @import moments
#' @importFrom stats sd
#' @examples if(require('moments')){uni_dist(rnorm(100), rpois(100, 2))}
uni_dist <- function(X, PX) {
    if (is.data.frame(X) & is.data.frame(PX)) {
        xx <- X[, 1]
        yy <- PX[, 1]
    } else if (is.data.frame(X) && !is.data.frame(PX)) {
        xx <- X[, 1]
        yy <- PX
    } else if (!is.data.frame(X) && is.data.frame(PX)) {
        xx <- X
        yy <- PX[, 1]
    } else {
        xx <- X
        yy <- PX
    }
    stat.xx <- c(mean(xx), stats::sd(xx), skewness(xx), kurtosis(xx))
    stat.yy <- c(mean(yy), stats::sd(yy), skewness(yy), kurtosis(yy))
    sqrt(sum((stat.xx - stat.yy)^2))
}

#' Distance based on side by side Boxplots
#'
#' Assuming that data set X consists of a categorical group variable a numeric value,
#' a summary of the first quartile, median and third quartile of this value is calculated
#' for each group.
#' The extent (as absolute difference) of the minimum and maximum value across groups is computed for
#' first quartile, median and third quartile. Same is done for data PX.
#' Finally an euclidean distance is calculated between the absolute differences of
#' X and PX.
#'
#' @param X a data.frame with one factor variable and one continuous
#' variable
#' @param PX a data.frame with one factor variable and one continuous
#' variable
#' @return distance between X and PX
#' @importFrom dplyr summarise group_by
#' @export
#' @importFrom stats resid quantile
#' @examples
#' if(require('dplyr')) {
#'   with(mtcars,
#'     box_dist(data.frame(as.factor(am), mpg),
#'     data.frame(as.factor(sample(am)), mpg))
#'   )
#' }
box_dist <- function(X, PX) {
	find_factor <- function(dframe) {
	  isfactor <- c(is.factor(dframe[,1]), is.factor(dframe[,2]))
	  if (sum(isfactor) != 1) stop("data must have exactly one factor variable\n\n")
	  isfactor
	}

	dq <- function(dX) {
	  .group <- NULL
	  .val <- NULL

	  # compute absolute difference between min and max of each statistic
	  Xfactor <- find_factor(dX)
	  if (length(Xfactor) > 2) stop("Dataset cannot not have more than one categorical and one continuous data.\n\n")
	  dX$.group <- dX[, Xfactor]
	  dX$.val <- dX[, !Xfactor]
	  X.sum <- summarise(group_by(dX, .group), q1 = stats::quantile(.val, 0.25), q2 = stats::quantile(.val, 0.5), q3 = quantile(.val,0.75))
	  unlist(lapply(X.sum[,-1], function(x) abs(diff(range(x)))))
	}

	abs.diff.X <- dq(X)
	abs.diff.PX <- dq(PX)


  sqrt(sum((abs.diff.X - abs.diff.PX)^2))
}


#' Distance based on separation of clusters
#'
#' The separation between clusters is defined by the minimum distances of a point in
#' the cluster to a point in another cluster. The number of clusters are provided.
#' If not, the hierarchical clustering method is used to obtain the clusters. The
#' separation between the clusters for dataset X is calculated. Same is done for
#' dataset PX. An euclidean distance is then calculated between these separation for
#' X and PX.
#'
#' @param X a data.frame with two or three columns, the first two columns
#' providing the dataset
#' @param PX a data.frame with two or three columns, the first two columns
#' providing the dataset
#' @param clustering LOGICAL; if TRUE, the third column is used as the
#' clustering variable, by default FALSE
#' @param nclust the number of clusters to be obtained by hierarchical
#' clustering, by default nclust = 3
#' @param type character string to specify which measure to use for distance, see ?cluster.stats for details
#' @return distance between X and PX
#' @export
#' @import fpc
#' @importFrom stats cutree hclust
#' @examples
#' if(require('fpc')) {
#' with(mtcars, sep_dist(data.frame(wt, mpg, as.numeric(as.factor(mtcars$cyl))),
#'               data.frame(sample(wt), mpg, as.numeric(as.factor(mtcars$cyl))),
#'               clustering = TRUE))
#'}
#'
#'if (require('fpc')) {
#'with(mtcars, sep_dist(data.frame(wt, mpg, as.numeric(as.factor(mtcars$cyl))),
#'              data.frame(sample(wt), mpg, as.numeric(as.factor(mtcars$cyl))),
#'              nclust = 3))
#'}
sep_dist <- function(X, PX, clustering = FALSE, nclust = 3, type="separation") {
  cl_dist <- function(Y) {
    dY <- stats::dist(Y[, 1:2])
    if (clustering) {
      Y$cl <- Y[, 3]
    } else {
      Y$cl <- stats::cutree(stats::hclust(dY), nclust)
    }
    cluster.stats(dY, clustering = Y$cl)[[type]]
  }

  sqrt(sum((cl_dist(X) - cl_dist(PX))^2))
}

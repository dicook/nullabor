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
#' @return distance between X and PX
#' @export
#' @examples with(mtcars, reg_dist(data.frame(wt, mpg), data.frame(sample(wt), mpg)))
reg_dist <- function(X, PX, nbins = 1) {
    ss <- seq(min(X[, 1]), max(X[, 1]), length = nbins + 1)
    beta.X <- NULL
    beta.PX <- NULL
    for (k in 1:nbins) {
        X.sub <- subset(X, X[, 1] >= ss[k] & X[, 1] <= ss[k + 1])
        PX.sub <- subset(PX, X[, 1] >= ss[k] & X[, 1] <= ss[k + 1])
        b.X <- as.numeric(coef(lm(X.sub[, 2] ~ X.sub[, 1])))
        b.PX <- as.numeric(coef(lm(PX.sub[, 2] ~ PX.sub[, 1])))
        beta.X <- rbind(beta.X, b.X)
        beta.PX <- rbind(beta.PX, b.PX)
    }
    beta.X <- subset(beta.X, !is.na(beta.X[, 2]))
    beta.PX <- subset(beta.PX, !is.na(beta.PX[, 2]))
    sum((beta.X[, 1] - beta.PX[, 1])^2 + (beta.X[, 2] - beta.PX[, 2])^2)
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
bin_dist <- function(X, PX, lineup.dat = lineup.dat, X.bin = 5, Y.bin = 5) {
    if (!is.null(lineup.dat)) {
        if (!is.numeric(X[, 1])) {
            X[, 1] <- as.numeric(X[, 1])
            nij <- as.numeric(table(cut(X[, 1], breaks = seq(min(X[, 1]), max(X[, 1]), length.out = length(unique(X[,
                1])) + 1), include.lowest = TRUE), cut(X[, 2], breaks = seq(min(lineup.dat[, 2]), max(lineup.dat[,
                2]), length.out = Y.bin + 1), include.lowest = TRUE)))
        } else nij <- as.numeric(table(cut(X[, 1], breaks = seq(min(lineup.dat[, 1]), max(lineup.dat[,
            1]), length.out = X.bin + 1), include.lowest = TRUE), cut(X[, 2], breaks = seq(min(lineup.dat[,
            2]), max(lineup.dat[, 2]), length.out = Y.bin + 1), include.lowest = TRUE)))
        if (!is.numeric(PX[, 1])) {
            PX[, 1] <- as.numeric(PX[, 1])
            mij <- as.numeric(table(cut(PX[, 1], breaks = seq(min(X[, 1]), max(X[, 1]), length.out = length(unique(X[,
                1])) + 1), include.lowest = TRUE), cut(PX[, 2], breaks = seq(min(lineup.dat[, 2]), max(lineup.dat[,
                2]), length.out = Y.bin + 1), include.lowest = TRUE)))
        } else mij <- as.numeric(table(cut(PX[, 1], breaks = seq(min(lineup.dat[, 1]), max(lineup.dat[,
            1]), length.out = X.bin + 1), include.lowest = TRUE), cut(PX[, 2], breaks = seq(min(lineup.dat[,
            2]), max(lineup.dat[, 2]), length.out = Y.bin + 1), include.lowest = TRUE)))
    } else if (is.null(lineup.dat)) {
        if (!is.numeric(X[, 1])) {
            X[, 1] <- as.numeric(X[, 1])
            nij <- as.numeric(table(cut(X[, 1], breaks = seq(min(X[, 1]), max(X[, 1]), length.out = length(unique(X[,
                1])) + 1), include.lowest = TRUE), cut(X[, 2], breaks = seq(min(X[, 2]), max(X[, 2]),
                length.out = Y.bin + 1), include.lowest = TRUE)))
        } else nij <- as.numeric(table(cut(X[, 1], breaks = seq(min(X[, 1]), max(X[, 1]), length.out = X.bin +
            1), include.lowest = TRUE), cut(X[, 2], breaks = seq(min(X[, 2]), max(X[, 2]), length.out = Y.bin +
            1), include.lowest = TRUE)))
        if (!is.numeric(PX[, 1])) {
            PX[, 1] <- as.numeric(PX[, 1])
            mij <- as.numeric(table(cut(PX[, 1], breaks = seq(min(X[, 1]), max(X[, 1]), length.out = length(unique(X[,
                1])) + 1), include.lowest = TRUE), cut(PX[, 2], breaks = seq(min(lineup.dat[, 2]), max(lineup.dat[,
                2]), length.out = Y.bin + 1), include.lowest = TRUE)))
        } else mij <- as.numeric(table(cut(PX[, 1], breaks = seq(min(PX[, 1]), max(PX[, 1]), length.out = X.bin +
            1), include.lowest = TRUE), cut(PX[, 2], breaks = seq(min(PX[, 2]), max(PX[, 2]), length.out = Y.bin +
            1), include.lowest = TRUE)))
    }
    sqrt(sum((nij - mij)^2))
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
#' @examples if(require('moments')){uni_dist(rnorm(100), rpois(100, 2))}
uni_dist <- function(X, PX) {
    if (is.data.frame(X) & is.data.frame(PX)) {
        xx <- X[, 1]
        yy <- PX[, 1]
    } else if (is.data.frame(X) & !is.data.frame(PX)) {
        xx <- X[, 1]
        yy <- PX
    } else if (!is.data.frame(X) & is.data.frame(PX)) {
        xx <- X
        yy <- PX[, 1]
    } else {
        xx <- X
        yy <- PX
    }
    stat.xx <- c(mean(xx), sd(xx), skewness(xx), kurtosis(xx))
    stat.yy <- c(mean(yy), sd(yy), skewness(yy), kurtosis(yy))
    sqrt(sum((stat.xx - stat.yy)^2))
}

#' Distance based on side by side Boxplots for two levels
#'
#' Assuming there are only two groups, the first quartile, median and third quartile
#' is calculated for each group of data X. The absolute difference between these
#' statistics between the two groups are then calculated. Same is done for data PX.
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
#' @examples if(require('dplyr')) {with(mtcars, box_dist(data.frame(as.factor(am), mpg),
#' data.frame(as.factor(sample(am)), mpg)))}
box_dist <- function(X, PX) {
	val <- group <- NULL
    if (!is.factor(X[, 1]) & !is.factor(X[, 2])) {
        stop("X should have one factor variable \n \n")
    } else if (is.factor(X[, 1])) {
        X$group <- X[, 1]
        X$val <- X[, 2]
        X.sum <- summarise(group_by(X, group), q1 = quantile(val, 0.25), q2 = quantile(val, 0.5), q3 = quantile(val,0.75))
    } else if (is.factor(X[, 2])) {
        X$group <- X[, 2]
        X$val <- X[, 1]
        X.sum <- summarise(group_by(X, group), q1 = quantile(val, 0.25), q2 = quantile(val, 0.5), q3 = quantile(val,0.75))
    }
    if (!is.factor(PX[, 1]) & !is.factor(PX[, 2])) {
        stop("PX should have one factor variable \n \n")
    } else if (is.factor(PX[, 1])) {
        PX$group <- PX[, 1]
        PX$val <- PX[, 2]
        PX.sum <- summarise(group_by(PX, group), q1 = quantile(val, 0.25), q2 = quantile(val, 0.5), q3 = quantile(val,0.75))
    } else {
        PX$group <- PX[, 2]
        PX$val <- PX[, 1]
        PX.sum <- summarise(group_by(PX, group), q1 = quantile(val, 0.25), q2 = quantile(val, 0.5), q3 = quantile(val,0.75))    }
    abs.diff.X <- with(X.sum, abs(as.numeric(X.sum[group == levels(group)[1], ])[2:4] - as.numeric(X.sum[group == levels(group)[2], ])[2:4]))
    abs.diff.PX <- with(PX.sum, abs(as.numeric(PX.sum[group == levels(group)[1], ])[2:4] - as.numeric(PX.sum[group == levels(group)[2], ])[2:4]))
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
#' @param nclust the number of clusters to be obtained by hierarchial
#' clustering, by default nclust = 3
#' @return distance between X and PX
#' @export
#' @import fpc
#' @examples if(require('fpc')) { with(mtcars, sep_dist(data.frame(wt, mpg,
#' as.numeric(as.factor(mtcars$cyl))), data.frame(sample(wt), mpg,
#' as.numeric(as.factor(mtcars$cyl))), clustering = TRUE))}
#' @examples if(require('fpc')) { with(mtcars, sep_dist(data.frame(wt, mpg,
#' as.numeric(as.factor(mtcars$cyl))), data.frame(sample(wt), mpg,
#' as.numeric(as.factor(mtcars$cyl))), nclustering = 3))}
sep_dist <- function(X, PX, clustering = FALSE, nclust = 3) {
    dX <- dist(X[, 1:2])
    dPX <- dist(PX[, 1:2])
    if (clustering) {
        X$cl <- X[, 3]
        PX$cl <- PX[, 3]
        X.clus <- cluster.stats(dX, clustering = X$cl)$separation
        PX.clus <- cluster.stats(dPX, clustering = PX$cl)$separation
    } else {
        complete.X <- cutree(hclust(dX), nclust)
        complete.PX <- cutree(hclust(dPX), nclust)
        X.clus <- cluster.stats(dX, complete.X)$separation
        PX.clus <- cluster.stats(dPX, complete.PX)$separation
    }
    sqrt(sum((X.clus - PX.clus)^2))
}

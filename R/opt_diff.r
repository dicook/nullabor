# Calculating the difference between the mean distance of true plot and the maximum of the null plots.
#'
#' Uses binned distance to calculate the mean distance between the true plot
#' and the null plots in a lineup. also calculates the mean distance of the
#' null plots among themselves and finds the difference between the mean
#' distance of the true plot and the maximum mean distance of the null plots
#'
#'
#' @param lineup.dat lineup data to get the lineup
#' @param X.bin number of bins on the x-direction
#' @param Y.bin number of bins on the y-direction
#' @param pos position of the true plot in the lineup
#' @param m number of plots in the lineup, by default m = 20
#' @return difference between the mean distance of the true plot and
#' the maximum mean distance of the null plots
#' @export
#' @examples lineup.dat <- subset(lineup(null_permute('mpg'), mtcars, pos = 10), select = 
#' c(mpg, wt, .sample))
#' calc_diff(lineup.dat, X.bin = 5, Y.bin = 5, pos = 10)
calc_diff <- function(lineup.dat, X.bin, Y.bin, pos, m = 20) {
    d <- sapply(1:m, function(i) {
        X <- lineup.dat[lineup.dat$.sample == i, ]
        sapply(1:m, function(j) {
            PX <- lineup.dat[lineup.dat$.sample == j, ]
            dis <- bin_dist(X, PX, X.bin, Y.bin)
        })
    })
    d.m <- melt(d)
    names(d.m) <- c("pos.2", "plotno", "bin")
    dat.bin <- subset(d.m, plotno != pos.2 & pos.2 != pos)
    dat.bin.mean <- ddply(dat.bin, .(plotno), summarize, bin.m = mean(bin), len = length(bin))
    with(dat.bin.mean, bin.m[len == (m - 1)] - max(bin.m[len != (m - 1)]))
}

# Finds the number of bins on x and y axis which gives the maximum binned distance
#'
#' finds the difference using \code{calc_diff} for all combinations of
#' number of bins in x and y direction
#'
#' @param lineup.dat lineup data to get the lineup
#' @param xlow the lowest value of number of bins on the x-direction
#' @param xhigh the highest value of number of bins on the x-direction
#' @param ylow the lowest value of number of bins on the y-direction
#' @param yhigh the highest value of number of bins on the y-direction
#' @param pos position of the true plot in the lineup
#' @param plot LOGICAL; if true, returns a tile plot for the combinations
#' of number of bins with the differences as weights
#' @param m number of plots in the lineup, by default m = 20
#' @return a dataframe with the number of bins and differences
#' the maximum mean distance of the null plots
#' @export
#' @examples opt_diff(lineup.dat, 2, 10, 2, 10, 10) ## position of the observed data
opt_diff <- function(lineup.dat, xlow, xhigh, ylow, yhigh, pos, plot = FALSE, m = 20) {
    d <- sapply(xlow:xhigh, function(X.bin) {
        sapply(ylow:yhigh, function(Y.bin) {
            calc_diff(lineup.dat, X.bin, Y.bin, pos, m)
        })
    })
    d.m <- melt(d)
    names(d.m) <- c("q", "p", "Diff")
    d.m$p <- d.m$p + xlow - 1
    d.m$q <- d.m$q + ylow - 1
    d.m <- data.frame(p = d.m$p, q = d.m$q, Diff = d.m$Diff)
    if (plot) {
        p <- ggplot(d.m, aes(x = factor(p), y = factor(q))) + geom_tile(aes(fill = Diff)) + scale_fill_gradient(high = "blue", low = "white") + xlab("p") + 
            ylab("q")
        return(list(dat = d.m, p = p))
    } else {
        return(dat = d.m)
    }
} 

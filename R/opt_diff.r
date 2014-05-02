#' Calculating the difference between true plot and the null plot with the maximum distance.
#'
#' Binned distance is used to calculate the mean distance between the true plot
#' and all the null plots in a lineup. The mean distances of each null plot to all
#' the other null plots are calculated. The difference between the mean
#' distance of the true plot and the maximum mean distance of the null plots is then 
#' calculated.
#'
#' @param lineup.dat lineup data to get the lineup
#' @param var a vector of names of the variables to be used to calculate the difference
#' @param X.bin number of bins on the x-direction
#' @param Y.bin number of bins on the y-direction
#' @param pos position of the true plot in the lineup
#' @param m number of plots in the lineup, by default m = 20
#' @return difference between the mean distance of the true plot and
#' the maximum mean distance of the null plots
#' @export
#' @importFrom plyr ddply
#' @importFrom reshape melt
#' @examples 
#'calc_diff(lineup(null_permute('mpg'), mtcars, pos = 10), var = c('mpg', 'wt'), X.bin = 5, Y.bin = 5, pos = 10)
calc_diff <- function(lineup.dat, var, X.bin, Y.bin, pos, m = 20) {
	plotno <- pos.2 <- bin <- NULL
    lineup.dat <- lineup.dat[, c(var, ".sample")]
    d <- sapply(1:m, function(i) {
        X <- lineup.dat[lineup.dat$.sample == i, ]
        sapply(1:m, function(j) {
            PX <- lineup.dat[lineup.dat$.sample == j, ]
            dis <- bin_dist(X, PX, lineup.dat = lineup.dat, X.bin, Y.bin)
        })
    })
    d.m <- melt(d)
    names(d.m) <- c("pos.2", "plotno", "bin")
    dat.bin <- subset(d.m, plotno != pos.2 & pos.2 != pos)
    dat.bin.mean <- ddply(dat.bin, .(plotno), summarize, bin.m = mean(bin), len = length(bin))
    with(dat.bin.mean, bin.m[len == (m - 1)] - max(bin.m[len != (m - 1)]))
}


#' Finds the number of bins in x and y direction which gives the maximum binned distance
#'
#' This function finds the optimal number of bins in both x and y direction which should
#' be used to calculate the binned distance. The binned distance is calculated for each
#' combination of provided choices of number of bins in x and y direction and finds the
#' difference using \code{calc_diff} for each combination. The combination for which the
#' difference is maximum should be used.
#'
#' @param lineup.dat lineup data to get the lineup
#' @param var a list of names of the variables to be used to calculate the difference
#' @param xlow the lowest value of number of bins on the x-direction
#' @param xhigh the highest value of number of bins on the x-direction
#' @param ylow the lowest value of number of bins on the y-direction
#' @param yhigh the highest value of number of bins on the y-direction
#' @param pos position of the true plot in the lineup
#' @param plot LOGICAL; if true, returns a tile plot for the combinations
#' of number of bins with the differences as weights
#' @param m number of plots in the lineup, by default m = 20
#' @param progress.bar LOGICAL; shows progress of function, by default TRUE
#' @return a dataframe with the number of bins and differences
#' the maximum mean distance of the null plots
#' @export
#' @importFrom plyr ldply
#' @examples 
#' opt_diff(lineup(null_permute('mpg'), mtcars, pos = 10), var = c('mpg', 'wt'), 2, 10, 2, 10, 10, plot = TRUE)
opt_diff <- function(lineup.dat, var, xlow, xhigh, ylow, yhigh, pos, plot = FALSE, m = 20, progress.bar = TRUE) {
	Diff <- NULL
	if(progress.bar){
    d.m <- ldply(xlow:xhigh, function(X.bin) {
        ldply(ylow:yhigh, function(Y.bin) {
            data.frame(X.bin, Y.bin, calc_diff(lineup.dat, var, X.bin, Y.bin, pos, m))
        })
    }, .progress = progress_text(char = "="))
    }else{
    	d.m <- ldply(xlow:xhigh, function(X.bin) {
        ldply(ylow:yhigh, function(Y.bin) {
            data.frame(X.bin, Y.bin, calc_diff(lineup.dat, var, X.bin, Y.bin, pos, m))
        })
    })
    	}
    names(d.m) <- c("p", "q", "Diff")
    if (plot) {
        p <- ggplot(d.m, aes(x = factor(p), y = factor(q))) + geom_tile(aes(fill = Diff)) + scale_fill_gradient(high = "blue", 
            low = "white") + xlab("p") + ylab("q")
        return(list(dat = d.m, p = p))
    } else {
        return(dat = d.m)
    }
} 

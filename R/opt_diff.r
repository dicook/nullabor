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
#' @importFrom dplyr summarise group_by
#' @export
#' @examples 
#' if(require('dplyr')){
#' calc_diff(lineup(null_permute('mpg'), mtcars, pos = 10), var = c('mpg', 'wt'), X.bin
#' = 5, Y.bin = 5, pos = 1, m = 8)}
calc_diff <- function(lineup.dat, var, X.bin, Y.bin, pos, m = 20){
	pos.1 <- pos.2 <- b <- NULL
	lineup.dat <- lineup.dat[, c(var, ".sample")]
	pos.dat <- expand.grid(pos.1 = 1:20, pos.2 = 1:20)
	pos.dat <- filter(pos.dat, pos.1 != pos.2 & pos.2 != pos)
	bin.dist <- summarise(group_by(pos.dat, pos.1, pos.2), b = bin_dist(X = filter(lineup.dat,.sample == pos.1), PX = filter(lineup.dat, .sample == pos.2), lineup.dat = lineup.dat, X.bin, Y.bin))
	bin.mean <- summarise(group_by(bin.dist, pos.1), bin = mean(b))
	with(bin.mean, bin[pos.1 == pos] - max(bin[pos.1 != pos]))
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
#' @return a dataframe with the number of bins and differences
#' the maximum mean distance of the null plots
#' @importFrom dplyr summarise group_by
#' @export
#' @examples 
#' if(require('dplyr')){
#' opt_diff(lineup(null_permute('mpg'), mtcars, pos = 1), var = c('mpg', 'wt'), 2, 4,
#' 4, 6, pos = 1, plot = TRUE, m = 8)}
opt_diff <- function(lineup.dat, var, xlow, xhigh, ylow, yhigh, pos, plot = FALSE, m = 20) {
	Diff <- xbins <- ybins <- NULL
	bins <- expand.grid(xbins = xlow:xhigh, ybins = ylow:yhigh)
	diff.bins <- summarise(group_by(bins, xbins, ybins), Diff = calc_diff(lineup.dat, var, xbins, ybins, pos, m))	
    if (plot) {
        p <- ggplot(diff.bins, aes(x = factor(xbins), y = factor(ybins))) + geom_tile(aes(fill = Diff)) + scale_fill_gradient(high = "blue", 
            low = "white") + xlab("xbins") + ylab("ybins")
        return(list(dat = diff.bins, p = p))
    } else {
        return(dat = diff.bins)
    }
} 

#' Calculating the mean distances of each plot in the lineup.
#'
#' Distance metric is used to calculate the mean distance between the true plot
#' and all the null plots in a lineup. The mean distances of each null plot to all
#' the other null plots are calculated. The mean distances are returned for all the plots
#' in the lineup.
#'
#' @param lineup.dat lineup data of the lineup
#' @param var a vector of names of the variables to be used to calculate the mean distances
#' @param met distance metric needed to calculate the distance as a character
#' @param pos position of the true plot in the lineup
#' @param dist.arg a list or vector of inputs for the distance metric met; NULL by default
#' @param m number of plots in the lineup, by default m = 20
#' @return the mean distances of each plot in the lineup
#' @importFrom dplyr summarise group_by filter
#' @export
#' @examples
#' if(require('dplyr')){
#' calc_mean_dist(lineup(null_permute('mpg'), mtcars, pos = 1), var = c('mpg', 'wt'),
#' met = 'reg_dist', pos = 1, m = 10)}
calc_mean_dist <- function(lineup.dat, var, met, pos, dist.arg = NULL, m = 20){
	plotno <- pos.2 <- b <- NULL
	dat.pos <- expand.grid(plotno = 1:m, pos.2 = 1:m)
	dat.pos <- dplyr::filter(dat.pos, plotno != pos.2 & pos.2 != pos)
    lineup.dat <- lineup.dat[, c(var, ".sample")]
    if (!is.character(met)) {
        stop("function met should be a character")
    }
    func <- match.fun(met)
    d <- summarise(group_by(dat.pos, plotno, pos.2), b = with(lineup.dat, ifelse(is.null(dist.arg),
    			do.call(func, list(dplyr::filter(lineup.dat, .sample == plotno),
    			                   dplyr::filter(lineup.dat, .sample == pos.2))),
    			do.call(func, append(list(dplyr::filter(lineup.dat, .sample == plotno),
    			                          dplyr::filter(lineup.dat, .sample == pos.2)), unname(dist.arg))))))
    summarise(group_by(d, plotno), mean.dist = mean(b))
}
#' Calculating the difference between true plot and the null plot with the maximum distance.
#'
#' Distance metric is used to calculate the mean distance between the true plot
#' and all the null plots in a lineup. The difference between the mean
#' distance of the true plot and the maximum mean distance of the null plots is
#' calculated.
#'
#' @param lineup.dat lineup data to get the lineup
#' @param var a vector of names of the variables to be used to calculate the difference
#' @param met distance metric needed to calculate the distance as a character
#' @param pos position of the true plot in the lineup
#' @param dist.arg a list or vector of inputs for the distance metric met; NULL by default
#' @param m number of plots in the lineup, by default m = 20
#' @return difference between the mean distance of the true plot and
#' the maximum mean distance of the null plots
#' @importFrom dplyr summarise group_by
#' @export
#' @examples
#' if(require('dplyr')){
#' lineup.dat <- lineup(null_permute('mpg'), mtcars, pos = 1)
#' calc_diff(lineup.dat, var = c('mpg', 'wt'), met = 'bin_dist',
#' dist.arg = list(lineup.dat = lineup.dat, X.bin = 5, Y.bin = 5), pos = 1, m = 8)}
#'
#' if(require('dplyr')){
#' calc_diff(lineup(null_permute('mpg'), mtcars, pos = 1), var = c('mpg', 'wt'), met = 'reg_dist',
#' dist.arg = NULL, pos = 1, m = 8)}
calc_diff <- function(lineup.dat, var, met, pos, dist.arg = NULL, m = 20){
	dist.mean <- calc_mean_dist(lineup.dat, var, met, pos, dist.arg, m)
	with(dist.mean, mean.dist[plotno == pos] - max(mean.dist[plotno != pos]))
}
#' Finds the number of bins in x and y direction which gives the maximum binned distance.
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
#' @importFrom ggplot2 ggplot aes geom_tile scale_fill_gradient xlab ylab
#' @export
#' @examples
#' if(require('dplyr')){
#' opt_bin_diff(lineup(null_permute('mpg'), mtcars, pos = 1), var = c('mpg', 'wt'),
#' 2, 5, 4, 8, pos = 1, plot = TRUE, m = 8)
#' }
opt_bin_diff <- function(lineup.dat, var, xlow, xhigh, ylow, yhigh, pos, plot = FALSE, m = 20) {
	Diff <- xbins <- ybins <- NULL
	bins <- expand.grid(xbins = xlow:xhigh, ybins = ylow:yhigh)
	diff.bins <- summarise(group_by(bins, xbins, ybins), Diff = calc_diff(lineup.dat, var, met = 'bin_dist', pos, dist.arg = list(lineup.dat = lineup.dat, X.bin = xbins, Y.bin = ybins), m))
    if (plot) {
        p <- ggplot(diff.bins, aes(x = factor(xbins), y = factor(ybins))) +
          geom_tile(aes(fill = Diff)) +
          scale_fill_gradient(high = "blue", low = "white") +
          xlab("xbins") + ylab("ybins")
        return(list(dat = diff.bins, p = p))
    } else {
        return(dat = diff.bins)
    }
}

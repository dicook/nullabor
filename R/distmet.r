#' Empirical distribution of the distance
#'
#' The empirical distribution of the distance measures is calculated based on the mean
#' distance of each of the null plots from the other null plots in a lineup. At this moment
#' this method works only for \code{\link{null_permute}} method. This function helps get some
#' assessment of whether the actual data plot is very different from the null plots.
#'
#' @export
#' @param lineup.dat lineup data
#' @param var a vector of names of the variables to be used
#' @param met distance metric needed to calculate the distance as a character
#' @param method method for generating null data sets
#' @param pos position of the observed data in the lineup
#' @param repl number of sets of null plots selected to obtain the distribution; 1000 by
#' default
#' @param dist.arg a list or vector of inputs for the distance metric met; NULL by default
#' @param m the number of plots in the lineup; m = 20 by default
#' @return lineup has the data used for the calculations
#' @return null_values contains new null samples from which to compare nulls in lineup
#' @return diff difference in distance between nulls and actual data and that of the null
#' that is most different from other nulls. A negative value means that the actual data
#' plot is similar to the null plots.
#' @return closest list of the five closest nulls to the actual data plot
#' @return pos position of the actual data plot in the lineup
#' @importFrom dplyr summarise group_by
#' @examples
#' # Each of these examples uses a small number of nulls (m=8), and a small number of
#' # repeated sampling from the null distribution (repl=100), to make it faster to run.
#' # In your own examples you should think about increasing each of these, at least to the defaults.
#' \dontrun{
#' if (require('dplyr')) {
#'   d <- lineup(null_permute('mpg'), mtcars, pos = 1)
#'   dd <- distmet(d, var = c('mpg', 'wt'),
#'     'reg_dist', null_permute('mpg'), pos = 1, repl = 100, m = 8)
#'   distplot(dd, m=8)
#' }
#' }
#'
#' \dontrun{
#' d <- lineup(null_permute('mpg'), mtcars, pos=4, n=8)
#' library(ggplot2)
#' ggplot(d, aes(mpg, wt)) + geom_point() + facet_wrap(~ .sample, ncol=4)
#' if (require('dplyr')) {
#'   dd <- distmet(d, var = c('mpg', 'wt'), 'bin_dist', null_permute('mpg'),
#'     pos = 4, repl = 100, dist.arg = list(lineup.dat = d, X.bin = 5,
#'     Y.bin = 5), m = 8)
#'   distplot(dd, m=8)
#' }
#' }
#'
#' # Example using bin_dist
#' \dontrun{
#' if (require('dplyr')) {
#'   d <- lineup(null_permute('mpg'), mtcars, pos = 1)
#'   library(ggplot2)
#'   ggplot(d, aes(mpg, wt)) + geom_point() + facet_wrap(~ .sample, ncol=5)
#'   dd <- distmet(d, var = c('mpg', 'wt'),
#'     'bin_dist', null_permute('mpg'), pos = 1, repl = 500,
#'     dist.arg = list(lineup.dat = d, X.bin = 5, Y.bin = 5))
#'   distplot(dd)
#' }
#' }
#'
#' # Example using uni_dist
#' \dontrun{
#' mod <- lm(wt ~ mpg, data = mtcars)
#' resid.dat <- data.frame(residual = mod$resid)
#' d <- lineup(null_dist('residual', dist = 'normal'), resid.dat, pos=19)
#' ggplot(d, aes(residual)) + geom_histogram(binwidth = 0.25) + facet_wrap(~ .sample, ncol=5)
#' if (require('dplyr')) {
#'   dd <- distmet(d, var = 'residual', 'uni_dist', null_dist('residual',
#'     dist = 'normal'), pos = 19, repl = 500)
#'   distplot(dd)
#' }
#' }
distmet <- function(lineup.dat, var, met, method, pos, repl = 1000, dist.arg = NULL, m = 20) {
    dist.mean <- calc_mean_dist(lineup.dat, var, met, pos, dist.arg, m)
    diff <- with(dist.mean, mean.dist[plotno == pos] - max(mean.dist[plotno != pos]))
    closest <- dist.mean[order(dist.mean$mean.dist, decreasing = TRUE), ]$plotno[2:6]
    obs.dat <- lineup.dat[lineup.dat$.sample == pos, c(var, ".sample")]
    all.samp <- replicate(repl, {
    	null <- method(obs.dat)
    	null_gen(lineup.dat, null, met, method, m, dist.arg)
    	})
   return(list(lineup = dist.mean[, c(pos.1 = "plotno", dist = "mean.dist")], null_values = all.samp, diff = diff,
        closest = closest, pos = pos))
}

#' Computing th distance for the null plots
#'
#' @keywords internal
null_gen <- function(lineup.dat, null, met, method, m, dist.arg){
	func <- match.fun(met)
	Dist <- replicate(m - 2, {
		null.dat <- method(null)
		 ifelse(is.null(dist.arg), do.call(func, list(null, null.dat)),
		do.call(func, append(list(null, null.dat), unname(dist.arg))))
	})
	mean(Dist)
}

#' Plotting the distribution of the distance measure
#'
#' The permutation distribution of the distance measure is plotted with the distances for
#' the null plots. Distance measure values for the null plots and the true plot are overlaid.
#'
#' @param dat output from \code{\link{distmet}}
#' @param m the number of plots in the lineup; m = 20 by default
#' @export
#' @importFrom stats density
#' @examples
#' \dontrun{
#' if (require('dplyr')) {
#'   d <- lineup(null_permute('mpg'), mtcars, pos = 1)
#'   library(ggplot2)
#'   ggplot(d, aes(mpg, wt)) + geom_point() + facet_wrap(~.sample)
#'   distplot(distmet(d, var = c('mpg', 'wt'), 'reg_dist', null_permute('mpg'),
#'     pos = 1, repl = 100, m = 8), m = 8)
#' }
#' }
distplot <- function(dat, m = 20) {
  null_values <- NULL
  mean.dist <- NULL
  y <- yend <- NULL
  true <- NULL
  plotno <- NULL

    null <- data.frame(null_values=dat$null_values)
    lineupvals <- dat$lineup
    lineupvals$true <- ifelse(lineupvals$plotno == dat$pos, "true", "null")
    lineupvals$y <- 0.01 * min(density(null$null_values)$y)
    lineupvals$yend <- 0.05 * max(density(null$null_values)$y)

    p <- ggplot2::ggplot(null, ggplot2::aes(x=null_values)) +
         ggplot2::geom_density(fill = I("grey80"), colour = I("grey80")) +
              xlab("Permutation distribution") + ylab("")
    p <- p + ggplot2::geom_segment(data=lineupvals, ggplot2::aes(x=mean.dist, xend=mean.dist,
                            y=y, yend=yend, colour=true), alpha=0.8)
    p <- p + ggplot2::geom_text(data=lineupvals,
                              ggplot2::aes(x = mean.dist, y=0, label = plotno, colour=true), vjust="top")
    p <- p + ggplot2::scale_colour_manual(values=c("true"="darkorange","null"="grey50"))
    p <- p + ggplot2::theme(legend.position="none")

    return(p)
}

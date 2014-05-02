#' Empirical distribution of the distance
#'
#' The empirical distribution of the distance measures is calculated based on the mean
#' distance of each of the null plots from the other null plots in a lineup.  
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
#' @param progress.bar LOGICAL; shows progress of function, by default TRUE
#' @importFrom reshape melt
#' @examples
#' distmet(lineup(null_permute('mpg'), mtcars, pos = 10), var = c('mpg', 'wt'), 'reg_dist', null_permute('mpg'), pos = 10) 
#' distmet(lineup(null_permute('mpg'), mtcars, pos = 10), var = c('mpg', 'wt'), 'bin_dist', null_permute('mpg'), pos = 10, dist.arg = list(X.bin = 5, Y.bin = 5)) 
#' lineup.dat <- lineup(null_permute('mpg'), mtcars)
#' qplot(mpg, wt, data = lineup.dat, geom = 'point') + facet_wrap(~ .sample)
#' #decrypt('...') # Copy and paste the output from lineup.dat to get the
#' #position of the true plot
#' #[1] 'True data in position 13' # Use pos = 13
#' distmet(lineup.dat, var = c('mpg', 'wt'), 'bin_dist', null_permute('mpg'), pos = 13, dist.arg = list(X.bin = 5, Y.bin = 5))
#' #Example using uni_dist
#' mod <- lm(wt ~ mpg, data = mtcars)
#' resid.dat <- data.frame(residual = mod$resid)
#' lineup.dat <- lineup(null_dist('residual', dist = 'normal'), resid.dat)
#' qplot(residual, data = lineup.dat, geom = 'histogram', binwidth = 0.25) + facet_wrap(~ .sample)
#' #decrypt('....') #Copy and paste to get the true position
#' distmet(lineup.dat, var = 'residual', 'uni_dist', null_dist('residual', dist = 'normal'), pos = 19) 
#' # Assuming pos = 19; but put the true position for pos
distmet <- function(lineup.dat, var, met, method, pos, repl = 1000, dist.arg = NULL, m = 20, progress.bar = TRUE) {
	plotno <- pos.2 <- b <- NULL
    lineup.dat <- lineup.dat[, c(var, ".sample")]
    if (!is.character(met)) {
        stop("function met should be a character")
    }
    func <- match.fun(met)
    if (as.character(met) == "bin_dist") {
        dist.arg <- list(lineup.dat, dist.arg[[1]], dist.arg[[2]])
    }
    d <- sapply(1:m, function(x) {
        sapply(1:m, function(y) {
            if (is.null(dist.arg)) {
                dis <- do.call(func, list(lineup.dat[lineup.dat$.sample == x, ], lineup.dat[lineup.dat$.sample == 
                  y, ]))
            } else {
                dis <- do.call(func, append(list(lineup.dat[lineup.dat$.sample == x, ], lineup.dat[lineup.dat$.sample == 
                  y, ]), unname(dist.arg)))
            }
        })
    })
    d.m <- melt(d)
    names(d.m) <- c("pos.2", "plotno", "b")
    d <- subset(d.m, plotno != pos.2 & pos.2 != pos)
    dist.mean <- ddply(d, .(plotno), summarize, mean.dist = mean(b), len = length(b))
    diff <- with(dist.mean, mean.dist[len == (m - 1)] - max(mean.dist[len == (m - 2)]))
    closest <- dist.mean[order(dist.mean$mean.dist, decreasing = TRUE), ]$plotno[2:6]
    obs.dat <- lineup.dat[lineup.dat$.sample == pos, ]
    if(progress.bar){
    all.samp <- ldply(1:repl, function(k) {
        null <- method(obs.dat)  # method
        Dist <- ldply(1:(m - 2), function(l) {
            null.dat <- method(null)  # method
            if (is.null(dist.arg)) {
                do.call(func, list(null, null.dat))
            } else {
                do.call(func, append(list(null, null.dat), unname(dist.arg)))  # dist.met
            }
        })
        mean(Dist$V1)
    }, .progress = progress_text(char = "="))
    }else{
    	 all.samp <- ldply(1:repl, function(k) {
        null <- method(obs.dat)  # method
        Dist <- ldply(1:(m - 2), function(l) {
            null.dat <- method(null)  # method
            if (is.null(dist.arg)) {
                do.call(func, list(null, null.dat))
            } else {
                do.call(func, append(list(null, null.dat), unname(dist.arg)))  # dist.met
            }
        })
        mean(Dist$V1)
    })
    	}
    return(list(lineup = dist.mean[, c("plotno", dist = "mean.dist")], null_values = all.samp, diff = diff, 
        closest = closest, pos = pos))
}
#' Plotting the distribution of the distance measure
#'
#' The distribution of the distance measure is plotted with the distances for 
#' the null plots and true plot overlaid.  
#'
#' @param dat output from \code{\link{distmet}}
#' @param m the number of plots in the lineup; m = 20 by default
#' @export
#' @examples 
#' distplot(distmet(lineup(null_permute('mpg'), mtcars, pos = 10), var = c('mpg', 'wt'), 'reg_dist', null_permute('mpg'), pos = 10)) 
distplot <- function(dat, m = 20) {
    p <- with(dat, qplot(null_values$V1, geom = "density", fill = I("grey80"), colour = I("grey80"), 
        xlab = "Permutation distribution", ylab = "") + geom_segment(aes(x = lineup$mean.dist[lineup$plotno != 
        pos], xend = lineup$mean.dist[lineup$plotno != pos], y = rep(0.01 * min(density(null_values$V1)$y), 
        (m - 1)), yend = rep(0.05 * max(density(null_values$V1)$y), (m - 1))), size = 1, alpha = I(0.7)) + 
        geom_segment(aes(x = lineup$mean.dist[lineup$plotno == pos], xend = lineup$mean.dist[lineup$plotno == 
            pos], y = 0.01 * min(density(null_values$V1)$y), yend = 0.1 * max(density(null_values$V1)$y)), 
            colour = "darkorange", size = 1) + geom_text(data = lineup, y = -0.03 * max(density(null_values$V1)$y), 
        size = 2.5, aes(x = mean.dist, label = plotno)) + ylim(c(-0.04 * max(density(null_values$V1)$y), 
        max(density(null_values$V1)$y) + 0.1)))
    return(p)
} 

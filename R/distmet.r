#' Calculates the distance measures
#'
#' @export
#' @param lineup.dat lineup data
#' @param met distance metric needed to calculate the distance
#' @param method method for generating null data sets
#' @param pos position of the observed data in the lineup
#' @param m the number of plots in the lineup; m = 20 by default
#' @param dist.arg a list of inputs for the distance metric met; NULL by default
#' @param plot LOGICAL; if TRUE, returns density plot showing the distn of 
#' the measures; TRUE by default
#' @examples if(require("reshape")) {
#'	if(require("plyr")) { distmet(lineup(null_permute('mpg'), mtcars, pos =
#' 10), var = c("mpg", "wt"), reg_dist, null_permute('mpg'), pos = 10) }}
#' if(require("reshape")) {
#' if(require("plyr")) {
#' distmet(lineup(null_permute('mpg'), mtcars, pos = 10), var = c("mpg", "wt"), bin_dist, 
#' null_permute('mpg'), pos = 10, dist.arg = list(dist.lineup = TRUE, X.bin = 5, Y.bin =
#' 5)) }}
distmet <- function(lineup.dat, met, method, pos, dist.arg = NULL, m = 20) {
    func <- match.fun(met)
    d <- sapply(1:m, function(x) {
        sapply(1:m, function(y) {
            if (is.null(dist.arg)) {
                dis <- do.call(func, list(lineup.dat[lineup.dat$.sample == x, ], lineup.dat[lineup.dat$.sample == y, ]))
            } else {
                dis <- do.call(func, append(list(lineup.dat[lineup.dat$.sample == x, ], lineup.dat[lineup.dat$.sample == y, ]), unname(dist.arg)))
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
    all.samp <- ldply(1:1000, function(k) {
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
    return(list(dist.mean = dist.mean[, c("plotno", dist = "mean.dist")], all.val = all.samp, diff = diff, closest = closest))
}

#' Plotting the distribution of the distances with the distances for 
#' the null plots and true plot overlaid  
#'
#' @param dat output from \code{\link{distmet}}
#' @param pos position of the observed data in the lineup
#' @param m the number of plots in the lineup; m = 20 by default
#' @export
#' @examples if(require("ggplot2")) {distplot(distmet(subset(lineup(null_permute('mpg'),
#' mtcars, pos = 10), select = c(mpg, wt, .sample)), reg_dist, null_permute('mpg'), pos = 10),
#' pos = 10)} ## position of the true plot in \code{\link{distmet}} example
distplot <- function(dat, pos, m = 20) {
    p <- with(dat, qplot(all.val$V1, geom = "density", fill = I("grey80"), colour = I("grey80"), xlab = "Permutation distribution", ylab = "") + geom_segment(aes(x = dist.mean$mean.dist[dist.mean$plotno != 
        pos], xend = dist.mean$mean.dist[dist.mean$plotno != pos], y = rep(0.01 * min(density(all.val$V1)$y), (m - 1)), yend = rep(0.05 * max(density(all.val$V1)$y), 
        (m - 1))), size = 1, alpha = I(0.7)) + geom_segment(aes(x = dist.mean$mean.dist[dist.mean$plotno == pos], xend = dist.mean$mean.dist[dist.mean$plotno == 
        pos], y = 0.01 * min(density(all.val$V1)$y), yend = 0.1 * max(density(all.val$V1)$y)), colour = "darkorange", size = 1) + geom_text(data = dist.mean, 
        y = -0.03 * max(density(all.val$V1)$y), size = 2.5, aes(x = mean.dist, label = plotno)) + ylim(c(-0.04 * max(density(all.val$V1)$y), max(density(all.val$V1)$y) + 
        0.1)))
    return(p)
    
} 

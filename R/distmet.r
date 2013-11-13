#' Calculates the distance measures
#'
#' @export
#' @param lineup.dat lineup data
#' @param met distance metric needed to calculate the distance
#' @param method method for generating null data sets
#' @param pos position of the observed data in the lineup
#' @param m the number of plots in the lineup; m = 20 by default
#' @param dist.arg a vector of inputs for the distance metric met; NULL by default
#' @param plot LOGICAL; if TRUE, returns density plot showing the distn of 
#' the measures; TRUE by default
#'
#' @author Niladri Roy Chowdhury 
#'
distmet <- function(lineup.dat, met, method, pos, m = 20, dist.arg = NULL, plot = TRUE){
  func <- match.fun(met)
  d <- sapply(1:m, function(x) {
    sapply(1:m, function(y) {
      if (is.null(dist.arg)) {
        dis <- do.call(func, list(lineup.dat[lineup.dat$.sample == x, ], lineup.dat[lineup.dat$.sample == y, ]))
      }
      else {
        dis <- do.call(func, append(list(lineup.dat[lineup.dat$.sample == x, ],
           lineup.dat[lineup.dat$.sample == y, ]), unname(dist.arg)))
       }
    })
  })
  require(reshape)
  d.m <- melt(d)
  names(d.m) <- c("pos.2", "plotno", "b")
  d <- subset(d.m, plotno != pos.2 & pos.2 != pos)
  require(plyr)
  dist.mean <- ddply(d, .(plotno), summarize, mean.dist = mean(b), len =   
  length(b))
  diff <- with(dist.mean, mean.dist[len == (m - 1)] - max(mean.dist[len == (m - 2)]))
  closest <- dist.mean[order(dist.mean$mean.dist, decreasing = TRUE), ]$plotno[2:6]
  obs.dat <- lineup.dat[lineup.dat$.sample == pos, ]
  all.samp <- ldply(1:1000, function(k){
    null <- method(obs.dat) # method
    Dist <- ldply(1:(m - 2), function(l){
      null.dat <- method(null) # method
      if(is.null(dist.arg)){
        do.call(func, list(null, null.dat))
      }else{
        do.call(func, append(list(null, null.dat), unname(dist.arg)))  # dist.met
        }
    })
    mean(Dist$V1)
  })
  if (plot) {
   dev.new()
   require(ggplot2)
   p <- qplot(all.samp$V1, geom="density", fill=I("grey80"), colour=I("grey80"), 
            xlab="Permutation distribution", ylab="") +
                geom_segment(aes(x=dist.mean$mean.dist[dist.mean$len == (m - 2)],
                  xend = dist.mean$mean.dist[dist.mean$len == (m - 2)],
                  y=rep(0.01*min(density(all.samp$V1)$y), (m - 1)),
                  yend=rep(0.05*max(density(all.samp$V1)$y), (m - 1))), size=1, alpha = I(0.7)) +
                geom_segment(aes(x = dist.mean$mean.dist[dist.mean$len == (m - 1)],
                   xend = dist.mean$mean.dist[dist.mean$len == (m - 1)], y = 0.01*min(density(all.samp$V1)$y),
                   yend = 0.1*max(density(all.samp$V1)$y)), colour="darkorange", size=1) + 
          geom_text(data = dist.mean, y = - 0.03*max(density(all.samp$V1)$y),
                   size = 2.5, aes(x = mean.dist, label = plotno)) +
                ylim(c(- 0.04*max(density(all.samp$V1)$y), max(density(all.samp$V1)$y) + 0.1))
      return(list(dist.mean = dist.mean[,c("plotno", dist = "mean.dist")], diff = diff, closest = closest, p))
  } else {
          return(list(dist.mean = dist.mean[,c("plotno", dist = "mean.dist")], all.val = all.samp,
            diff = diff, closest = closest))
  }
}   

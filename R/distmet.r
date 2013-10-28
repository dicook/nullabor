
#' Calculates the distance between densities
#'

wbdist_fun = function(z1, z2) {
	sqrt(sum((z1 - z2)^2)/(sum(z1^2) * sum(z2^2)))
}

# This function will get the densities given the data
dens_z = function(dat, nbins = 10) {
res = MASS::kde2d(dat[, 1], dat[, 2], n = nbins, lims = c(range(dat[, 1]), range(dat[, 2])))
res$z
}

#' Calculates the mean distance between the null datasets 
#'
#' @export
#' @param dat observed data set using
#' @param no.samp number of samples
#' @param method null generating mechanism
#'

mean_met <- function(dat, no.samp, method){
	z1 = dens_z(dat)
	dat1 <- data.frame(1:no.samp, true_wbdist = replicate(no.samp, {
		r = method(dat)
		z2 = dens_z(r)
	  wbdist_fun(z1, z2)
	}))
  mean(dat1[,2])
}

#' Calculates the mean distance for all replicates
#'
#' @export
#' @param dat observed data set using
#' @param repl the number of replicates
#' @param no.samp number of samples
#' @param method null generating mechanism
#'

all_sample = function(dat, repl, no.samp, method) {
	mean.WBdist_all <- NULL	
	for(i in 1:repl){
		samp.dat <- method(dat)
		z3 <- dens_z(samp.dat)
		dat2 <- data.frame(k = 1:no.samp, null_wbdist = replicate(no.samp, {
			r = method(samp.dat)
			z4 = dens_z(r)
			wbdist_fun(z3, z4)
			}))
		mean.WBdist_all <- c(mean.WBdist_all,  mean(dat2[,2]) )
		}
return(mean.WBdist_all)
}



#' Calculates the mean distance for each plot in the lineup.
#'
#' @export
#' @param dat lineup data
#' @param no.samp number of samples
#' @param method null generating mechanism
#'


mean.samp <- function(dat, no.samp, method){
	dat1 <- NULL
for (i in 1:length(unique(dat$.sample))){
	dat1 <- rbind(dat1, data.frame(PlotNo = i, means = mean_met(dat[dat$.sample == i,], no.samp, method)))
}
return(dat1)
}


#' Calculates the distance measures
#'
#' @export
#' @param dat lineup data
#' @param method method for generating null data sets
#' @param pos position of the observed data in the lineup
#' @param meas.distr LOGICAL; if TRUE, returns the distn of the distance measures, by default FALSE
#' @param plot LOGICAL; if TRUE and if meas.distr is TRUE, returns density plot showing the distn of 
#' the measures
#' @param repl the number of replicates
#' @param no.samp number of samples
#'
#' @author Niladri Roy Chowdhury 
#'


distmet <- function(dat, method, pos, meas.distr = FALSE, plot = FALSE, repl = 10000, no.samp = 10 ){
	if(missing(method)){
		cat("Need the null generating mechanism")
	}else{
	if(missing(pos)){
		 cat("Need the position of the true dataset")
		 }else{
	true.dat <- data.frame(dat[,1][dat$.sample == pos], dat[,2][dat$.sample == pos])
	names(true.dat) <- names(dat[,1:2])
	WBdist <- NULL
  	 for(i in 1:length(unique(dat$.sample))){ 
  	 	wb_dist = wbdist_fun(dens_z(true.dat), dens_z(data.frame(dat[,1][dat$.sample == i], dat[,2][dat$.sample == i])))
  	 	WBdist<-rbind(WBdist, data.frame(PlotNo = i, wb_dist))
  	 	}
  	 plots <- WBdist[order(WBdist$wb_dist), ]$PlotNo[2:6]
  	   	if(meas.distr){
  		all <- all_sample(true.dat, repl, no.samp, method)
  		    cal.mean <- mean.samp(dat, no.samp, method)
  		 	perc.val <- sum(all > cal.mean$means[cal.mean$PlotNo == pos])/repl
  		 	ratio <- perc.val*repl/sum(all > max(cal.mean$means[cal.mean$PlotNo != pos])) 
  		 if(plot){
  		 	dev.new()
  		 	p <- qplot(all, geom="density", fill=I("grey80"), colour=I("grey80"), xlab="Permutation distribution", ylab="") + geom_segment(aes(x=cal.mean$means[cal.mean$PlotNo != pos], xend = cal.mean$means[cal.mean$PlotNo != pos], y=rep(- 0.02, 19), yend=rep(-0.05*max(density(all)$y), 19)), size=1, alpha = I(0.7)) + geom_segment(aes(x= cal.mean$means[cal.mean$PlotNo == pos], xend = cal.mean$means[cal.mean$PlotNo == pos], y=- 0.02, yend = -0.1*max(density(all)$y)), colour="darkorange", size=1)
  		 	return(list(all.wb = all, wbdist = WBdist, closest = plots, perc.val = perc.val, ratio = ratio,cal.mean = cal.mean, p))
  		 }
  		 else
  		 return(list(all.wb = all, wbdist = WBdist, closest = plots, cal.mean = cal.mean))
  	}
  	else
  return(list( wbdist = WBdist, closest = plots)) 
   }
   }
  }


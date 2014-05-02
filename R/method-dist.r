dists <- c(beta = "beta", cauchy = "cauchy", `chi-squared` = "chisq", exponential = "exp", f = "f", gamma = "gamma", 
    geometric = "geom", `log-normal` = "lnorm", lognormal = "lnorm", logistic = "logis", `negative binomial` = "nbinom", 
    normal = "norm", poisson = "pois", t = "t", weibull = "weibull")

# Specific distribution ------------------------------------------------------

#' Generate null data with a specific distribution.
#'
#' Null hypothesis: variable has specified distribution
#'
#' @param var variable name
#' @param dist distribution name. One of: beta, cauchy, chi-squared,
#'   exponential, f, gamma, geometric, log-normal, lognormal, logistic,
#'   negative binomial, normal, poisson, t, weibull
#' @param params list of parameters of distribution. If \code{NULL}, will
#'   use \code{\link[MASS]{fitdistr}} to estimate them.
#' @return a function that given \code{data} generates a null data set.
#'   For use with \code{\link{lineup}} or \code{\link{rorschach}}
#' @export
#' @importFrom MASS fitdistr
null_dist <- function(var, dist, params = NULL) {
    dist <- match.arg(dist, names(dists))
    generator <- match.fun(paste("r", dists[dist], sep = ""))
    
    function(df) {
        # If parameters not specified, use fitdistr from MASS to find them
        if (is.null(params)) {
            params <- as.list(coef(fitdistr(df[[var]], dist)))
        }
        params$n <- nrow(df)
        df[[var]] <- do.call(generator, params)
        df
    }
} 

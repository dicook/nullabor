dists <- c(beta = "beta", cauchy = "cauchy", `chi-squared` = "chisq", exponential = "exp", f = "f", gamma = "gamma",
    geometric = "geom", `log-normal` = "lnorm", lognormal = "lnorm", logistic = "logis", `negative binomial` = "nbinom", binomial = "binom",
    normal = "norm", poisson = "pois", t = "t", uniform = 'unif', weibull = "weibull")

# Specific distribution ------------------------------------------------------

#' Generate null data with a specific distribution.
#'
#' Null hypothesis: variable has specified distribution
#'
#' @param var variable name
#' @param dist distribution name. One of: beta, cauchy, chisq,
#'   exp, f, gamma, geom, lnorm, logis,
#'   nbinom, binom, norm, pois, t, unif, weibull
#' @param params list of parameters of distribution. If \code{NULL}, will
#'   use \code{\link[MASS]{fitdistr}} to estimate them.
#' @return a function that given \code{data} generates a null data set.
#'   For use with \code{\link{lineup}} or \code{\link{rorschach}}
#' @export
#' @seealso null_permute, null_lm
#' @importFrom MASS fitdistr
#' @importFrom stats coef
#' @examples
#' dframe <- data.frame(x = rnorm(150))
#' library(ggplot2)
#' # three histograms of normally distributed values
#' ggplot(
#'   data=rorschach(method=null_dist("x", "norm"), n = 3, true=dframe)
#'   ) +
#'   geom_histogram(aes(x=x, y=..density..), binwidth=0.25) +
#'   facet_grid(.~.sample) +
#'   geom_density(aes(x=x), colour="steelblue", size=1)
#'
#' # uniform distributions are not as easy to recognize as such
#' dframe$x = runif(150)
#' ggplot(
#'   data=rorschach(method=null_dist("x", "uniform",
#'                  params=list(min=0, max=1)),
#'   n = 3, true=dframe)) +
#'   geom_histogram(aes(x=x, y=..density..), binwidth=0.1) +
#'   facet_grid(.~.sample) +
#'   geom_density(aes(x=x), colour="steelblue", size=1)
null_dist <- function(var, dist, params = NULL) {
    dist <- match.arg(dist, names(dists))
    generator <- match.fun(paste("r", dists[dist], sep = ""))

    function(df) {
        # If parameters not specified, use fitdistr from MASS to find them
        if (is.null(params)) {
          if (dist == "uniform") stop("specify minimum and maximum of the uniform distribution in the function call, use the form: params = list(min = ., max = .)")
            params <- as.list(stats::coef(fitdistr(df[[var]], dist)))
        }
        params$n <- nrow(df)
        df[[var]] <- do.call(generator, params)
        df
    }
}

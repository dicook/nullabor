# Convenience functions: residual plots for lm objects -------------------------------------------------------

#' Compare residual plots of a fitted model to plots of null residuals.
#'
#' @description This function is used to quickly create lineup version of the residual
#' plots created by \code{plot.lm} and \code{ggfortify::autoplot.lm}; see Details for
#' descriptions of these plots.
#' In the lineup protocol the plot of the real data is embedded amongst a field of
#' plots of data generated to be consistent with some null hypothesis.
#' If the observer can pick the real data as different from the others, this
#' lends weight to the statistical significance of the structure in the plot.
#' The protocol is described in Buja et al. (2009).
#'
#' @details Four types of plots are available:
#'
#' 1. Residual vs fitted. Null hypothesis: variable is linear combination
#'    of predictors.
#'
#' 2. Normal Q-Q plot. Null hypothesis: errors are normal. Always uses
#'    \code{method = "pboot"} to generate residuals under the null hypothesis.
#'
#' 3. Scale-location. Null hypothesis: errors are homoscedastic.
#'
#' 4. Residuals vs leverage. Used to identify points with high residuals
#'    and high leverage, which are likely to have a strong influence on
#'    the model fit.
#'
#' 19 null datasets are plotted together the the true data (randomly
#' positioned). If you pick the real data as being noticeably different, then
#' you have formally established that it is different to with p-value 0.05.
#' Run the \code{decrypt} message printed in the R Console to see which
#' plot represents the true data.
#'
#' If the null hypothesis in the type 1 plot is violated, consider using
#' a different model. If the null hypotheses in the type 2 or 3 plots
#' are violated, consider using bootstrap p-values; see
#' \href{https://www.modernstatisticswithr.com/regression.html#bootreg1}{Section 8.1.5}
#' of Thulin (2024) for details and recommendations.
#'
#' @param model a model object fitted using \code{\link{lm}}.
#' @param type type of plot: 1 = residuals vs fitted, 2 = normal Q-Q,
#' 3 = scale-location, 4 = residuals vs leverage.
#' @param method method for generating null residuals.  Built in methods
#'   'rotate', 'perm', 'pboot' and 'boot' are defined by \code{\link{resid_rotate}},
#'   \code{\link{resid_perm}}, \code{\link{resid_pboot}} and \code{\link{resid_boot}}
#'   respectively. 'pboot' is always used for plots of type 2.
#' @param color_points the color used for points in the plot. Can be a name
#'   or a color HEX code.
#' @param color_trends the color used for trend curves in the plot.
#' @param color_lines the color used for reference lines in the plot.
#' @param alpha_points the alpha (opacity) used for points in the plot (between
#'   0 and 1, where 1 is opaque).
#' @param ... other arguments passed onto \code{method}.
#' @return a \code{ggplot}
#' @references Buja, Cook, Hofmann, Lawrence, Lee, Swayne, Wickham. (2009).
#' Statistical inference for exploratory data analysis and model diagnostics,
#' \emph{Phil. Trans. R. Soc. A}, 367, 4361-4383.
#' @references Thulin, M. (2024) \emph{Modern Statistics with R}. Boca Raton: CRC Press.
#'    ISBN 9781032512440. \url{https://www.modernstatisticswithr.com/}
#' @export
#' @importFrom stats lm predict deviance df.residual fitted formula lm.influence residuals
#' @importFrom ggplot2 ggplot geom_point geom_smooth geom_qq geom_qq_line geom_abline geom_line facet_wrap labs .data
#' @seealso null_lm
#' @examples
#' data(tips)
#' x <- lm(tip ~ total_bill, data = tips)
#' lineup_residuals(x, type = 1) # Residuals vs Fitted
#' lineup_residuals(x, type = 2, method = "pboot") # Normal Q-Q plot
#' lineup_residuals(x, type = 4) # Residuals vs Leverage
#'
#' # Style the plot using color settings and ggplot2 functions:
#' lineup_residuals(x, type = 3,
#'                 color_points = "skyblue",
#'                 color_trends = "darkorange") +
#'     ggplot2::theme_minimal()
lineup_residuals <- function(model, type = 1, method = "rotate", color_points = "black", color_trends = "blue", color_lines = "brown3", alpha_points = 0.5, ...)
{
  # Get residuals, standardized residuals and leverage for the
  # original model:
  model.reg <- data.frame(model$model,
                          .resid = residuals(model),
                          .fitted = fitted(model)
  )
  s <- sqrt(deviance(model)/df.residual(model))
  hii <- lm.influence(model, do.coef = FALSE)$hat
  model.reg$.stdresid <- dropInf(model.reg$.resid/(s * sqrt(1 - hii)), hii)
  model.reg$.leverage <- dropInf(hii, hii)

  if(type == 1) {
    p <- ggplot(lineup(null_lm(formula(model), method = method, additional = TRUE), model.reg)) +
          geom_point(aes(x = .data$.fitted, y = .data$.resid), alpha = alpha_points, color = color_points) +
          geom_abline(aes(intercept = 0, slope = 0), colour = color_lines, linetype = "dashed") +
          geom_smooth(aes(x = .data$.fitted, y = .data$.resid), se = FALSE, color = color_trends, method = "loess", formula = y ~ x) +
          facet_wrap(~ .data$.sample) +
          labs(x = "Fitted values", y = "Residuals", title = "Residuals vs Fitted")
  }
  if(type == 2) {
    if(method != "pboot") { warning(paste0("Method \"", method, "\" does not generate normal residuals. Using method = \"pboot\" instead."))}
    p <- ggplot(lineup(null_lm(formula(model), method = "pboot", additional = TRUE), model.reg)) +
          geom_qq_line(aes(sample = .data$.stdresid), colour = color_lines) +
          geom_qq(aes(sample = .data$.stdresid), alpha = alpha_points, color = color_points) +
          facet_wrap(~ .data$.sample) +
          labs(x = "Theoretical quantiles", y = "Standardized residuals", title = "Normal Q-Q plot")
  }
  if(type == 3) {
    p <- ggplot(lineup(null_lm(formula(model), method = method, additional = TRUE), model.reg)) +
          geom_point(aes(x = .data$.fitted, y = sqrt(abs(.data$.stdresid))), alpha = alpha_points, color = color_points) +
          geom_smooth(aes(x = .data$.fitted, y = sqrt(abs(.data$.stdresid))), se = FALSE, color = color_trends, method = "loess", formula = y ~ x) +
          facet_wrap(~ .data$.sample) +
          labs(x = "Fitted values", y = "Square-root of absolute standardized residuals", title = "Scale-Location")
  }
  if(type == 4) {
    p <- ggplot(lineup(null_lm(formula(model), method = method, additional = TRUE), model.reg)) +
          geom_point(aes(x = .data$.leverage, y = .data$.stdresid), alpha = alpha_points, color = color_points) +
          geom_smooth(aes(x = .data$.leverage, y = .data$.stdresid), se = FALSE, color = color_trends, method = "loess", formula = y ~ x) +
          facet_wrap(~ .data$.sample) +
          labs(x = "Leverage", y = "Square-root of absolute standardized residuals", title = "Residuals vs Leverage")
  }
  p
}


#' Check distributional assumptions using histograms and the lineup protocol.
#'
#' @description This function is used to quickly create lineup plots to check
#' distributional assumptions using histograms with kernel density estimates.
#' The null hypothesis is that the data follows the distribution specified by the
#' \code{dist} argument.
#' In the lineup protocol the plot of the real data is embedded amongst a field of
#' plots of data generated to be consistent with some null hypothesis.
#' If the observer can pick the real data as different from the others, this
#' lends weight to the statistical significance of the structure in the plot.
#' The protocol is described in Buja et al. (2009).
#'
#' @details 19 null datasets are plotted together the the true data (randomly
#' positioned)  If you pick the real data as being noticeably different, then
#' you have formally established that it is different to with p-value 0.05.
#'
#' Run the \code{decrypt} message printed in the R Console to see which
#' plot represents the true data.
#'
#' @param data a data frame.
#' @param variable the name of the variable that should be plotted.
#' @param dist the null distribution name. One of: "beta", "cauchy",
#'  "chi-squared", "exponential", "f", "gamma", "geometric", "log-normal",
#'   "lognormal", "logistic", "negative binomial", "binomial", "normal",
#'   "poisson", "t", "uniform", "weibull"
#' @param params list of parameters of distribution. If \code{NULL}, will
#'   use \code{\link[MASS]{fitdistr}} to estimate them if possible. For
#'   uniform, beta, and binomial distributions, the parameters must be specified.
#'   See \code{?dunif}, \code{?dbeta}, and \code{?dbinom} for parameter names.
#' @param color_bars the color used for the borders of the bars. Can be a name
#'   or a color HEX code.
#' @param fill_bars the color used to fill the bars.
#' @param color_lines the color used for the density curves.
#' @return a \code{ggplot}
#' @references Buja, Cook, Hofmann, Lawrence, Lee, Swayne, Wickham. (2009).
#' Statistical inference for exploratory data analysis and model diagnostics,
#' \emph{Phil. Trans. R. Soc. A}, 367, 4361-4383.
#' @export
#' @importFrom ggplot2 ggplot geom_histogram geom_density facet_wrap labs after_stat .data
#' @seealso null_dist
#' @examples
#' data(tips)
#' lineup_histograms(tips, "total_bill", dist = "normal") # Normal distribution
#'
#' # Some distributions require that the parameters be specified:
#' lineup_histograms(tips, "size", dist = "binomial", params = list(size = 6, p = 0.3))
#'
#' # Style the plot using color settings and ggplot2 functions:
#' lineup_histograms(tips, "total_bill",
#'                   dist = "gamma",
#'                   color_bars = "steelblue",
#'                   color_lines = "magenta") +
#'     ggplot2::theme_minimal()
lineup_histograms <- function(data, variable, dist = NULL, params = NULL, color_bars = "black", fill_bars = "grey", color_lines = "brown3")
{
  if(is.null(dist)) { stop("\"dist\" must be specified. See ?lineup_distribution for details.")}

  if(dist %in% c("unif", "uniform")) {
    if(is.null(params)) { stop("\"params\" must be specified for the uniform distribution. See ?lineup_distribution for details.") }
    n <- nrow(data)
    p <-  ggplot(data = lineup(method = null_dist(variable, dist, params), true = data)) +
        geom_histogram(aes(x = .data[[variable]], y = after_stat(density)), breaks = seq(as.numeric(params[1]), as.numeric(params[2]), length.out = round(n/10)), fill = fill_bars, color = color_bars) +
        facet_wrap(.~.sample) +
        geom_density(aes(x = .data[[variable]]), colour = color_lines, size=1) +
        labs(y = "Density", title = "Lineup: Histograms with kernel density estimates")
  } else if(dist == "beta") {
    if(is.null(params)) { stop("\"params\" must be specified for the beta distribution. See ?lineup_distribution for details.") }
      n <- nrow(data)
      p <-  ggplot(data = lineup(method = null_dist(variable, dist, params), true = data)) +
              geom_histogram(aes(x = .data[[variable]], y = after_stat(density)), breaks = seq(0, 1, length.out = round(n/10)), fill = fill_bars, color = color_bars) +
              facet_wrap(.~.sample) +
              geom_density(aes(x = .data[[variable]]), colour = color_lines, size=1) +
              labs(y = "Density", title = "Lineup: Histograms with kernel density estimates")
  } else {
    if(dist %in% c("binom", "binomial") & is.null(params)) { stop("\"params\" must be specified for the binomial distribution. See ?lineup_distribution for details.") }
      p <-  ggplot(data = lineup(method = null_dist(variable, dist, params), true = data)) +
        geom_histogram(aes(x = .data[[variable]], y = after_stat(density)), binwidth = 0.25, fill = fill_bars, color = color_bars) +
        facet_wrap(.~.sample) +
        geom_density(aes(x = .data[[variable]]), colour = color_lines, size=1) +
        labs(y = "Density", title = "Lineup: Histograms with kernel density estimates")
  }
  p
}


#' Check distributional assumptions using Q-Q plots and the lineup protocol.
#'
#' @description This function is used to quickly create lineup plots to check
#' distributional assumptions using Q-Q plots. The null hypothesis is that the
#' data follows the distribution specified by the \code{dist} argument.
#' In the lineup protocol the plot of the real data is embedded amongst a field of
#' plots of data generated to be consistent with some null hypothesis.
#' If the observer can pick the real data as different from the others, this
#' lends weight to the statistical significance of the structure in the plot.
#' The protocol is described in Buja et al. (2009).
#'
#' @details 19 null datasets are plotted together the the true data (randomly
#' positioned)  If you pick the real data as being noticeably different, then
#' you have formally established that it is different to with p-value 0.05.
#'
#' Run the \code{decrypt} message printed in the R Console to see which
#' plot represents the true data.
#'
#' @param data a data frame.
#' @param variable the name of the variable that should be plotted.
#' @param dist the null distribution name. One of: "beta", "cauchy",
#'  "chi-squared", "exponential", "f", "gamma", "geometric", "log-normal",
#'   "lognormal", "logistic", "negative binomial", "normal",
#'   "poisson", "t", "uniform", "weibull"
#' @param params list of parameters of distribution. If \code{NULL}, will
#'   use \code{\link[MASS]{fitdistr}} to estimate them if possible. For
#'   uniform and beta distributions, the parameters must be specified.
#'   See \code{?dunif} and \code{?dbeta} for parameter names.
#' @param color_points the color used for points. Can be a name
#'   or a color HEX code.
#' @param color_lines the color used for reference lines.
#' @param alpha_points the alpha (opacity) used for points (between
#'   0 and 1, where 1 is opaque).
#' @return a \code{ggplot}
#' @references Buja, Cook, Hofmann, Lawrence, Lee, Swayne, Wickham. (2009).
#' Statistical inference for exploratory data analysis and model diagnostics,
#' \emph{Phil. Trans. R. Soc. A}, 367, 4361-4383.
#' @export
#' @importFrom ggplot2 ggplot geom_qq geom_qq_line facet_wrap labs .data
#' @seealso null_dist
#' @examples
#' data(tips)
#' lineup_qq(tips, "total_bill", dist = "normal") # Normal distribution
#' lineup_qq(tips, "total_bill", dist = "gamma") # Gamma distribution
#'
#' # Some distributions require that the parameters be specified:
#' tips$proportion_tips <- tips$tip/(tips$total_bill+tips$tip)
#' lineup_qq(tips, "size", dist = "beta", params = list(shape1 = 0.1, shape2 = 0.2))
#'
#' # Style the plot using color settings and ggplot2 functions:
#' lineup_qq(tips, "total_bill",
#'           dist = "gamma",
#'           color_points = "chocolate",
#'           color_lines = "cyan",
#'           alpha_points = 0.25) +
#'     ggplot2::theme_minimal()
lineup_qq <- function(data, variable, dist = NULL, params = NULL, color_points = "black", color_lines = "brown3", alpha_points = 0.5)
{
    if(is.null(dist)) { stop("\"dist\" must be specified. See ?lineup_distribution for details.")}
    dist <- match.arg(dist, names(dists))
    quantile_function <- match.fun(paste("q", dists[dist], sep = ""))
    if(is.null(params)) { params <- as.list(stats::coef(fitdistr(data[[variable]], dist))) }

    p <- ggplot(data = lineup(method = null_dist(variable, dist, params), true = data)) +
          geom_qq_line(aes(sample = .data[[variable]]), colour = color_lines, linewidth = 1, distribution = quantile_function, dparams = params) +
          geom_qq(aes(sample = .data[[variable]]), alpha = alpha_points, color = color_points, distribution = quantile_function, dparams = params) +
          facet_wrap(~ .data$.sample) +
          labs(x = "Theoretical quantiles", y = "Standardized residuals", title = "Lineup: Q-Q plots")

    p
}

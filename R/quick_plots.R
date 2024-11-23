# Convenience functions: residual plots for lm objects -------------------------------------------------------

#' Compare residual plots of a fitted model to plots of null residuals.
#'
#' @description This function is used to quickly create lineup version of the residual
#' plots created by \code{plot.lm} and \code{ggfortify::autoplot}; see Details for
#' descriptions of these plots.
#' In the lineup protocol the plot of the real data is embedded amongst a field of
#' plots of data generated to be consistent with some null hypothesis.
#' If the observe can pick the real data as different from the others, this
#' lends weight to the statistical significance of the structure in the plot.
#' The protocol is described in Buja et al. (2009).
#'
#' @details Four types of plots are available:
#'
#' 1. Residual vs fitted. Null hypothesis: variable is linear combination
#'    of predictors.
#'
#' 2. Normal Q-Q plot. Null hypothesis: errors are normal.
#'
#' 3. Scale-location. Null hypothesis: errors are homoscedastic.
#'
#' 4. Residuals vs leverage. Used to identify points with high residuals
#'    and high leverage, which are likely to have a strong influence on
#'    the model fit.
#'
#' Generate n - 1 null datasets and randomly position the true data.  If you
#' pick the real data as being noticeably different, then you have formally
#' established that it is different to with p-value 1/n.
#'
#' If the null hypothesis in the type 1 plot is violated, consider using
#' a different model. If the null hypotheses in the type 2 or 3 plots
#' are violated, consider using bootstrap p-values; see \href{https://www.modernstatisticswithr.com/regression.html#bootreg1}{Section 8.1.5} of
#' Thulin (2024) for details and recommendations.
#'
#' @param model a model object fitted using \code{\link{lm}}.
#' @param type type of plot: 1 = residuals vs fitted, 2 = normal Q-Q,
#' 3 = scale-location, 4 = residuals vs leverage.
#' @param method method for generating null residuals.  Built in methods
#'   'rotate', 'perm', 'pboot' and 'boot' are defined by \code{\link{resid_rotate}},
#'   \code{\link{resid_perm}}, \code{\link{resid_pboot}} and \code{\link{resid_boot}}
#'   respectively. 'pboot' is recommended for plots of type 2.
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
#' residual_lineup(x, type = 1) # Residuals vs Fitted
#' residual_lineup(x, type = 2, method = "pboot") # Normal Q-Q plot
#' residual_lineup(x, type = 3) # Scale-Location
#' residual_lineup(x, type = 4) # Residuals vs Leverage
residual_lineup <- function(model, type = 1, method = "rotate", ...)
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
    p <- ggplot(lineup(null_lm(formula(model), method = method), model.reg)) +
          geom_point(aes(x = .data$.fitted, y = .data$.resid), alpha = 0.5) +
          geom_abline(aes(intercept = 0, slope = 0), colour = "red", linetype = "dashed") +
          geom_smooth(aes(x = .data$.fitted, y = .data$.resid), se = FALSE) +
          facet_wrap(~ .data$.sample) +
          labs(x = "Fitted values", y = "Residuals", title = "Residuals vs Fitted")
  }
  if(type == 2) {
    p <- ggplot(lineup(null_lm(formula(model), method = method), model.reg)) +
          geom_qq(aes(sample = .data$.stdresid), colour = "blue", alpha = 0.5) + geom_qq_line(aes(sample = .data$.resid)) +
          facet_wrap(~ .data$.sample) +
          labs(x = "Theoretical quantiles", y = "Standardized residuals", title = "Normal Q-Q plot")
  }
  if(type == 3) {
    p <- ggplot(lineup(null_lm(formula(model), method = method), model.reg)) +
          geom_point(aes(x = .data$.fitted, y = sqrt(.data$.stdresid)), alpha = 0.5) +
          geom_smooth(aes(x = .data$.fitted, y = sqrt(.data$.stdresid)), se = FALSE) +
          facet_wrap(~ .data$.sample) +
          labs(x = "Fitted values", y = "Square-root of standardized residuals", title = "Scale-Location")
  }
  if(type == 4) {
    p <- ggplot(lineup(null_lm(formula(model), method = method), model.reg)) +
          geom_point(aes(x = .data$.leverage, y = .data$.stdresid), alpha = 0.5) +
          geom_smooth(aes(x = .data$.leverage, y = .data$.stdresid), se = FALSE) +
          facet_wrap(~ .data$.sample) +
          labs(x = "Leverage", y = "Square-root of standardized residuals", title = "Residuals vs Leverage")
  }
  p
}

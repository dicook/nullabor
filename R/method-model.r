# Linear model as null -------------------------------------------------------

#' Generate null data with null residuals from a model.
#'
#' Null hypothesis: variable is linear combination of predictors
#'
#' @param f model specification formula, as defined by \code{\link{lm}}
#' @param method method for generating null residuals.  Built in methods
#'   'rotate', 'perm', 'pboot' and 'boot' are defined by \code{\link{resid_rotate}},
#'   \code{\link{resid_perm}}, \code{\link{resid_pboot}} and \code{\link{resid_boot}}
#'   respectively
#' @param additional whether to compute additional measures: standardized
#'   residuals and leverage
#' @param ... other arguments passed onto \code{method}.
#' @return a function that given \code{data} generates a null data set.
#'   For use with \code{\link{lineup}} or \code{\link{rorschach}}
#' @export
#' @importFrom stats lm predict deviance df.residual lm.influence
#' @seealso null_permute, null_dist
#' @examples
#' data(tips)
#' x <- lm(tip ~ total_bill, data = tips)
#' tips.reg <- data.frame(tips, .resid = residuals(x), .fitted = fitted(x))
#' library(ggplot2)
#' ggplot(lineup(null_lm(tip ~ total_bill, method = 'rotate'), tips.reg)) +
#'   geom_point(aes(x = total_bill, y = .resid)) +
#'   facet_wrap(~ .sample)
null_lm <- function(f, method = "rotate", additional = FALSE, ...) {
  n <- NULL
    if (is.character(method)) {
        method <- match.fun(paste("resid", method, sep = "_"))
    }
    function(df) {
        model <- eval(substitute(lm(formula, data = df), list(formula = f)))
        resp_var <- all.vars(f[[2]])

        resid <- method(model, df, ...)
        fitted <- predict(model, df)
        df[".resid"] <- resid
        df[".fitted"] <- fitted
        if(additional){
          s <- sqrt(deviance(model)/df.residual(model))
          hii <- lm.influence(model, do.coef = FALSE)$hat
          df[".leverage"] <- dropInf(hii, hii)
          df[".stdresid"] <- dropInf(resid/(s * sqrt(1 - hii)), hii)
        }
        df[[resp_var]] <- fitted + resid
        df
    }
}

# Extractor methods
rss <- function(model) sum(stats::resid(model)^2)
sigma <- function(model) summary(model)$sigma
n <- function(model) length(stats::resid(model))

#' Rotation residuals.
#'
#' For use with \code{\link{null_lm}}
#'
#' @param model to extract residuals from
#' @param data used to fit model
#' @importFrom stats update
#' @export
resid_rotate <- function(model, data) {
    data[names(model$model)[1]] <- stats::rnorm(nrow(data))

    rmodel <- stats::update(model, data = data)
    stats::resid(rmodel) * sqrt(rss(model)/rss(rmodel))
}

#' Parametric bootstrap residuals.
#'
#' For use with \code{\link{null_lm}}
#'
#' @param model to extract residuals from
#' @param data used to fit model
#' @export
resid_pboot <- function(model, data) {
  stats::rnorm(n = length(model$residuals), sd = sigma(model))
}

#' Residuals simulated by a normal model, with specified sigma
#'
#' For use with \code{\link{null_lm}}
#'
#' @param model to extract residuals from
#' @param data used to fit model
#' @param sigma, a specific sigma to model
#' @importFrom stats rnorm
#' @export
resid_sigma <- function(model, data, sigma = 1) {
  stats::rnorm(n = n(model), sd = sigma)
}

#' Bootstrap residuals.
#'
#' For use with \code{\link{null_lm}}
#'
#' @param model to extract residuals from
#' @param data used to fit model
#' @importFrom stats resid
#' @export
resid_boot <- function(model, data) {
    sample(stats::resid(model), replace = TRUE)
}

#' Permutation residuals.
#'
#' For use with \code{\link{null_lm}}
#'
#' @param model to extract residuals from
#' @importFrom stats resid
#' @param data used to fit model
#' @export
resid_perm <- function(model, data) {
    sample(stats::resid(model))
}


# Helper function for leverages, adapted from plot.lm
dropInf <- function(x, h) {
  if (any(isInf <- h >= 1)) {
    warning(gettextf("not plotting observations with leverage greater than one:\n  %s",
                     paste(which(isInf), collapse = ", ")), call. = FALSE,
            domain = NA)
    x[isInf] <- NaN
  }
  x
}

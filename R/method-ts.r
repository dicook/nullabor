# Temporal dependence --------------------------

#' Generate null data by simulating from a time series model.
#'
#' Null hypothesis: data follows a time series model using auto.arima from the forecast package
#'
#' @param var variable to model as a time series
#' @param modelfn method for simulating from ts model.
#' @return a function that given \code{data} generates a null data set.
#'   For use with \code{\link{lineup}} or \code{\link{rorschach}}
#' @export
#' @seealso null_model
#' @importFrom stats as.ts simulate
#' @importFrom tibble as_data_frame
#' @examples
#' require(forecast)
#' require(ggplot2)
#' require(dplyr)
#' data(aud)
#' l <- lineup(null_ts("rate", auto.arima), aud)
#' ggplot(l, aes(x=date, y=rate)) + geom_line() +
#'   facet_wrap(~.sample, scales="free_y") +
#'   theme(axis.text = element_blank()) +
#'   xlab("") + ylab("")
#' l_dif <- l %>%
#'   group_by(.sample) %>%
#'   mutate(d=c(NA,diff(rate))) %>%
#'   ggplot(aes(x=d)) + geom_density() +
#'   facet_wrap(~.sample)
null_ts <- function(var, modelfn) {
  function(df) {
    ts <- as.ts(df[[var]])

    model_fit <- ts %>%
      modelfn
    x <- simulate(model_fit, future=FALSE)
    df[[var]] <- as.vector(x)
    df
  }
}

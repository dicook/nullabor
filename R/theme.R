

#' A theme to minimally strip away the context
#'
#' Note this is not a complete theme hence why there are no arguments.
#'
#' @examples
#' library(ggplot2)
#' ggplot(cars, aes(dist, speed)) + theme_strip()
#'
#' @export
theme_strip <- function() {
  ggplot2::theme(axis.title = ggplot2::element_blank(),
                 axis.text = ggplot2::element_blank(),
                 axis.ticks.length = grid::unit(0, "mm"))

}

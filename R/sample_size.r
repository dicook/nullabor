#' Sample size calculator
#'
#' This function calculates a table of sample sizes for
#' with an experiment, given a lineup size, and
#' estimates of the detection rate.
#' @param n range of sample sizes to check, default is 53:64
#' @param m linup size, default 20
#' @param pA range of estimated detection rates to consider,
#'    default is seq(1/20, 1/3, 0.01)
#' @param conf confidence level to use to simulate from binomial
#' @importFrom stats qbinom
#' @examples
#' pow <- sample_size()
#' pow
#' library(ggplot2)
#' library(viridis)
#' ggplot(pow, aes(x=n, y=pA, fill=prob, group=pA)) +
#'   geom_tile() +
#'   scale_fill_viridis_c("power") +
#'   ylab("detect rate (pA)") + xlab("sample size (n)") +
#'   theme_bw()
#' @export
sample_size <- function(n=53:64, m=20, pA=seq(1/20, 1/3, 0.01), conf=0.95) {
  g <- expand.grid(n, pA)
  k <- qbinom(1-conf, g[,1], 1/m, lower.tail=FALSE)
  pow <- tibble(n=g[,1], k=k, pA=g[,2])
  pow <- pow %>%
    mutate(prob = pbinom(k-1, n, pA, lower.tail=FALSE))
  pow
}

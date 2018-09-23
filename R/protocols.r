#' The Rorschach protocol.
#'
#' This protocol is used to calibrate the eyes for variation due to sampling.
#' All plots are typically null data sets, data that is consistent with a null
#' hypothesis. The protocol is described in Buja, Cook, Hofmann, Lawrence,
#' Lee, Swayne, Wickham (2009) Statistical inference for exploratory data
#' analysis and model diagnostics, Phil. Trans. R. Soc. A, 367, 4361-4383.
#'
#' @export
#' @param method method for generating null data sets
#' @param true true data set. If \code{NULL}, \code{\link{find_plot_data}}
#'   will attempt to extract it from the current ggplot2 plot.
#' @param n total number of samples to generate (including true data)
#' @param p probability of including true data with null data.
#' @importFrom purrr rerun
#' @importFrom tidyr unnest
#' @importFrom tibble tibble
#' @importFrom stats rbinom
rorschach <- function(method, true = NULL, n = 20, p = 0) {
    data <- NULL
    true <- find_plot_data(true)
    show_true <- stats::rbinom(1, 1, p) == 1

    if (show_true) {
        n <- n - 1
    }
    samples <- tibble(
      .n = seq_len(n),
      data = purrr::rerun(n, method(true)))
    samples <- data.frame(tidyr::unnest(samples, data))
#        samples <- plyr::rdply(n, method(true))

    if (show_true) {
        pos <- sample(n + 1, 1)
        message(encrypt("True data in position ", pos+10))
        samples <- add_true(samples, true, pos)
    } else {
      samples$.sample <- samples$.n
      samples$.n <- NULL
    }

    samples
}

#' The line-up protocol.
#'
#' In this protocol the plot of the real data is embedded amongst a field of
#' plots of data generated to be consistent with some null hypothesis.
#' If the observe can pick the real data as different from the others, this
#' lends weight to the statistical significance of the structure in the plot.
#' The protocol is described in Buja, Cook, Hofmann, Lawrence,
#' Lee, Swayne, Wickham (2009) Statistical inference for exploratory data
#' analysis and model diagnostics, Phil. Trans. R. Soc. A, 367, 4361-4383.
#'
#' Generate n - 1 null datasets and randomly position the true data.  If you
#' pick the real data as being noticeably different, then you have formally
#' established that it is different to with p-value 1/n.
#'
#' @param method method for generating null data sets
#' @param true true data set. If \code{NULL}, \code{\link{find_plot_data}}
#'   will attempt to extract it from the current ggplot2 plot.
#' @param n total number of samples to generate (including true data)
#' @param pos position of true data.  Leave missing to pick position at
#'   random.  Encryped position will be printed on the command line,
#'   \code{\link{decrypt}} to understand.
#' @param samples samples generated under the null hypothesis. Only specify
#'   this if you don't want lineup to generate the data for you.
#' @importFrom tibble tibble
#' @export
#' @examples
#' library(ggplot2)
#' ggplot(lineup(null_permute('mpg'), mtcars), aes(mpg, wt)) +
#'   geom_point() +
#'   facet_wrap(~ .sample)
#' ggplot(lineup(null_permute('cyl'), mtcars),
#'        aes(mpg, .sample, colour = factor(cyl))) +
#'        geom_point()
lineup <- function(method, true = NULL, n = 20, pos = sample(n, 1), samples = NULL) {
    data <- NULL
    true <- find_plot_data(true)

    if (is.null(samples)) {
      samples <- tibble(
        .n = seq_len(n-1),
        data = purrr::rerun(n-1, method(true)))
      samples <- data.frame(tidyr::unnest(samples, data))
#      samples <- plyr::rdply(n - 1, method(true))
    }
    if (missing(pos)) {
        message("decrypt(\"", encrypt("True data in position ", pos+10), "\")")
    }
    add_true(samples, true, pos)
}

#' Add true data into data frame containing null data sets.
#' @keywords internal
add_true <- function(samples, true, pos) {
    samples$.sample <- with(samples, ifelse(.n >= pos, .n + 1, .n))
    samples$.n <- NULL
    true$.sample <- pos

    all <- dplyr::bind_rows(samples, true)
    attr(all, "pos") <- pos
    all[order(all$.sample), ]
}

#' Find plot data.
#' If data is not specified, this function will attempt to find the data
#' corresponding to the last ggplot2 created or displayed. This will work
#' in most situations where you are creating the plot and immediately
#' displaying it, but may not work in other situations.  In those cases,
#' please specify the data explicitly.
#'
#' @keywords internal
#' @importFrom ggplot2 last_plot
find_plot_data <- function(data) {
    if (!is.null(data))
        return(data)

    if (exists("last_plot") && !is.null(last_plot())) {
        last_plot()$data
    } else {
        stop("Missing true dataset")
    }
}

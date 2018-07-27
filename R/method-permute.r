# Multivariate independence --------------------------------------------------


#' Generate null data by permuting a variable.
#'
#' Null hypothesis: variable is independent of others
#'
#' @param var name of variable to permute
#' @return a function that given \code{data} generates a null data set.
#'   For use with \code{\link{lineup}} or \code{\link{rorschach}}
#' @export
#' @seealso null_lm, null_dist
#' @examples
#' data(mtcars)
#' library(ggplot2)
#' ggplot(data=rorschach(method=null_permute("mpg"), n = 3, true=mtcars)) +
#' geom_boxplot(aes(x=factor(cyl), y=mpg, fill=factor(cyl))) +facet_grid(.~.sample) +
#' theme(legend.position="none", aspect.ratio=1)
null_permute <- function(var) {
    function(df) {
        df[[var]] <- sample(df[[var]])
        df
    }
}

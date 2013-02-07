# Multivariate independence --------------------------------------------------


#' Generate null data by permuting a variable.
#'
#' Null hypothesis: variable is independent of others
#' 
#' @param var name of variable to permute
#' @return a function that given \code{data} generates a null data set.  
#'   For use with \code{\link{lineup}} or \code{\link{rorschach}}
#' @export
null_permute <- function(var) {
  function(df) {
    df[[var]] <- sample(df[[var]])
    df
  }
}
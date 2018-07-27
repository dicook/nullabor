#' Power calculations.
#'
#' This function simply counts the proportion of people who selected the data plot,
#' in a set of lineups. It adjusts for multiple picks by the same individual, by weighting
#' by the total number of choices.
#' @param data summary of the results, containing columns id, pic_id, response, detected
#' @param m size of the lineup
#' @return vector of powers for each pic_id
#' @export
#' @importFrom dplyr mutate
#' @examples
#' data(turk_results)
#' visual_power(turk_results)

visual_power <- function(data, m=20) {
  pic_id <- NULL
  detected <- NULL
  nchoices_wgt <- NULL

  data <- data %>% mutate(
    nchoices_wgt = (m-lengths(strsplit(as.character(data$response), ",")))/19)
  visual_p <- data %>% group_by(pic_id) %>%
    summarise(power = sum(detected*nchoices_wgt)/length(detected), n=length(detected))
  return(visual_p)
}

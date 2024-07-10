#' Conversion rate of 1 Australian Doller (AUD) to 1 US Dollar
#'
#' The dataset consists of the daily exchange rates of 1 Australian Dollar to 1 US Dollar between Jan 9 2018 and Feb 21 2018.
#'
#' @docType data
#' @name aud
NULL

#' Los Angeles Lakers play-by-play data.
#'
#' Play by play data from all games played by the Los Angeles lakers in the
#' 2008/2009 season.
#'
#' @docType data
#' @name lal
NULL

#' Wasp gene expression data.
#'
#' Data from Toth et al (2010) used in Niladri Roy et al (2015)
#'
#' @docType data
#' @name wasps
NULL

#' Sample turk results
#'
#' Subset of data from a Turk experiment, used to show how to compute power of a lineup
#'
#' @docType data
#' @name turk_results
NULL

#' Polls and election results from the 2012 US Election
#'
#'
#' @format A list with two data frames:
#' polls is a data frame of 51 rows and 4 variables
#' \describe{
#' \item{State}{State name}
#' \item{Electoral.vote}{Number of electoral votes in the 2012 election}
#' \item{Margin}{Margin between the parties with the highest number of votes and second highest number of votes.
#' These margins are based on polls.}
#' \item{Democrat}{logical vector True, if the democratic party is  the majority party in this state. }
#' }
#' `election` is a data frame of 51 rows and 5 variables
#' \describe{
#' \item{State}{State name}
#' \item{Candidate}{character string of the winner: Romney or Obama}
#' \item{Electoral.vote}{Number of electoral votes in the 2012 election}
#' \item{Margin}{Margin between the parties with the highest number of votes and second highest number of votes.
#' These margins are based on the actual election outcome}
#' \item{Democrat}{logical vector True, if the democratic party is  the majority party in this state. }
#' }
#' @docType data
#' @name electoral
NULL

#' Tipping data
#'
#'
#' One waiter recorded information about each tip he received over a
#' period of a few months working in one restaurant. He collected several
#' variables:
#'
#' \itemize{
#'  \item tip in dollars,
#'  \item bill in dollars,
#'  \item sex of the bill payer,
#'  \item whether there were smokers in the party,
#'  \item day of the week,
#'  \item time of day,
#'  \item size of the party.
#' }
#'
#' In all he recorded 244 tips. The data was reported in a collection of
#' case studies for business statistics (Bryant & Smith 1995).
#'
#' @references Bryant, P. G. and Smith, M (1995) \emph{Practical Data
#'   Analysis: Case Studies in Business Statistics}.  Homewood, IL: Richard D.
#'   Irwin Publishing:
#' @format A data frame with 244 rows and 7 variables
#' @keywords datasets
"tips"

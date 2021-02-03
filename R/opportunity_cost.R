#' Compute the opportunity cost
#'
#' @param choices A list of available choices
#' @param chosen A character of chosen option name
#'
#' @return Populate chosen choice with its best forgone choice and its opportunity cost
#' @export
#'
#' @examples
#' choices <- list()
#' choices$A <- list(benefit=300, accounting_cost=210)
#' choices$B <- list(benefit=132, accounting_cost=100)
#' choices$C <- list(benefit=256, accounting_cost=231)
#' choices$D <- list(benefit=168, accounting_cost=262)
#'
#' choices$A
#' choices$B
#' choices$C
#' choices$D
#'
#' compute_opportunityCost(choices, "A")
#' compute_opportunityCost(choices, "B")
#' compute_opportunityCost(choices, "C")
#'
#' computeEconomicGain(choices,"A")
compute_opportunityCost <- function(choices, chosen){
  require(ggplot2)
  chosenIndex <- getChosenIndex(choices, chosen)

  choicesForgone <- {
    choices[-chosenIndex]
  }

  netGainsOfForgone <-
    purrr::map(choicesForgone,
               ~{
                 .x$benefit - .x$accounting_cost
               })

  orderOfNetGains <- order(unlist(netGainsOfForgone),
                           decreasing=T)

  .GlobalEnv$choices[[chosen]]$theBestOfForgones <- choicesForgone[orderOfNetGains[[1]]]

  .GlobalEnv$choices[[chosen]]$opportunityCost <-
    netGainsOfForgone[[orderOfNetGains[[1]]]] +
    choices[[chosen]]$accounting_cost
}

#' Compute economic (net) gain of an option
#'
#' @param choices A list of available choices
#' @param chosen A character of chosen option name
#'
#' @return Populate chosen choice with its economic net gain
#' @export
#'
#' @seealso compute_opportunityCost
computeEconomicGain <- function(choices, chosen){
  if(is.null(choices[[chosen]]$opportunityCost))
  {stop("Please compute_opportunityCost first.")}

  .GlobalEnv$choices[[chosen]]$economicGain <-
    choices[[chosen]]$benefit - choices[[chosen]]$opportunityCost
}

# helpers -----------------------------------------------------------------


getChosenIndex <- function(choices, chosen){
  which(names(choices)==chosen)
}





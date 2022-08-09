#' Estimate the intensity of effects using odds ratio and probabilities
#'
#' Estimates the intensity of the effects using odds ratio and probabilities
#'
#' This function processes the output of an Exponential Random Graph or a logistic models model and estimates
#' the intensity of the effects using either Odds ratios or Probabilities
#' The function returns a data frame with four colums:
#' - Estimates
#' - Either Odds ratios or Probabilities
#' - Standard Errors
#' - P-values
#'
#'
#' @param m A model object
#' @param type Prints either Odds ratios or Probabilities ("odds", "prob")
#'
#' @return a \code{data.frame}
#'
#' @family statistics functions
#' @examples
#' \dontrun{
#' data(florentine, package = "snafun")
#' fflom <- florentine$flomarriage
#' flom <- to_network(fflom)
#' m <- ergm::ergm(flom ~ edges + nodecov("Wealth"))
#'
#' stat_ef_int(m, "prob")
#' }
#' @export
stat_ef_int <- function(m, type = "odds"){

  if (class(m) != "ergm") {
    stop(paste("ERROR: The argument provided is not class ergm. Please, provide an ergm class object")
    )
  }

  coefM <- summary(m)
  tab <- data.frame(Estimate = round(coefM$coefficients[,1], 3))
  rownames(tab) <- rownames(coefM$coefficients)


  if(type == "odds") {

    or <- NULL

    for (i in 1:nrow(tab)) {

      temp <- exp(tab[i, 1])
      or <- append(or, temp)
    }

    tab <- cbind(tab, Odds = round(or, 3))

  } else if (type == "prob") {

    P <- NULL

    for (i in 1:nrow(tab)) {

      temp <- exp(tab[i, 1])/ (1 + exp(tab[i, 1]))
      P <- append(P, temp)}

    tab <- cbind(tab, Prob = round(P, 3))

  }

  tab <- cbind(tab, Std.Error = round(coefM$coefficients[,2],3), Pval = round(coefM$coefficients[,4],3))

  return(tab)

}

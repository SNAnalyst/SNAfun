#' gof and plot (btergm style)
#' 
#' Determine the gof and plot the gof, using the btergm approach
#' 
#' For a fitted \code{ergm} or \code{btergm}, this function calculates 
#' the goodness of fit (\code{gof}) and plots it. 
#' This is done using the \code{btergm} engine, which also works for 
#' fitted \code{ergm} models.
#' 
#' For calculating the gof, btergm provides the option to check the fit of the 
#' model against a range of statistics. 
#' See \link[btergm]{gof-statistics} for an overview of the available statistics 
#' to check against. Note that these partly overlap with statistics available 
#' in the \code{ergm} package, but not exactly. 
#' If a statistic is required that is not inluded in the \code{btergm} package 
#' for use in the \code{gof} function (such as "nodecov"), you need to use the 
#' \code{ergm} package for this and can not use this function.
#' 
#' The \code{...} argument allows one to pass arguments to the 
#' \link[btergm]{gof} function, such as \code{target}, \code{formula}, 
#' \code{parallel}, \code{ncpus}. 
#' See the help of \link[btergm]{gof} for the options.
#' 
#' Many messages that are returned during the whole process can be suppressed 
#' by setting \code{silent = TRUE} and \code{verbose = FALSE}.
#'
#' NOTE: the goodness of fit is silently returned. The plot is 
#' always plotted.
#' 
#' NOTE: \code{ergm} and \code{btergm} are packages made by separate research 
#' groups. As a result, the objects and functions are not 100 percent 
#' compatible. If applying this function to an \code{ergm}-model returns 
#' weird errors, it is better to calculate the gof using the 
#' \code{ergm} package and then plot that (using \link{GOF_plot} or 
#' using the \code{ergm} package functions).
#'
#' @param m a fitted \code{ergm} or {btergm} model
#' @param ... optional arguments, see description
#' @param btergm_statistics vector of statistics to be used in the gof
#' @param silent logical, if \code{TRUE} fewer messages will be printed during the 
#' calculation of the gof (but the function will often still print some output)
#' @param verbose logical, if \code{FALSE}, the printing of most details 
#' by the gof function is suppressed (but some warnings may still appear and 
#' errors are always shown)
#'
#' @return the goodness of fit is silently returned
#' @export
#'
#' @examples
#' \dontrun{
#' ### ergm model
#' flo <-SNA4DSData::florentine
#' flom <- to_network(flo$flomarriage)
#' m_ergm <- ergm::ergm(flom ~ edges + nodecov("Wealth"))
#' gof_ergm <- GOF_and_plot_as_btergm(m_ergm)
#' gof_ergm <- GOF_and_plot_as_btergm(m_ergm, btergm_statistics = c(btergm::esp,
#'      btergm::dsp, btergm::deg))
#' gof_ergm
#' 
#' ### btergm model
#' data(alliances, package = "SNA4DSData")
#' m_btergm <- btergm::btergm(allianceNets ~ edges + 
#'   gwesp(0, fixed = TRUE) +
#'   edgecov(lastYearsSharedPartners) + 
#'   edgecov(militaryDisputes) + 
#'   nodecov("polity") +
#'   nodecov("Composite_Index_National_Capability") + 
#'   absdiff("polity") + 
#'   absdiff("Composite_Index_National_Capability") +
#'   edgecov(sharedBorder), 
#'   R = 100, # number of bootstraps
#'   parallel = "snow", ncpus = 16  # optional line
#' )
#' 
#' gof_btergm <- GOF_and_plot_as_btergm(m_btergm)
#' gof_btergm <- GOF_and_plot_as_btergm(m_btergm, 
#'   parallel = "snow", ncpus = 12)
#' gof_btergm
#' GOF_and_plot_as_btergm(m_btergm, 
#'   parallel = "snow", ncpus = 12, silent = TRUE)
#' GOF_and_plot_as_btergm(m_btergm, 
#'   parallel = "snow", ncpus = 12, verbose = FALSE, silent = TRUE)
#' }
GOF_and_plot_as_btergm <- function(m, 
                                   ...,
                                   btergm_statistics = c(btergm::esp, 
                                                         btergm::geodesic, 
                                                         btergm::deg,
                                                         btergm::rocpr),
                                   silent = FALSE,
                                   verbose = TRUE) {
  
  if (!inherits(m, "ergm") & !inherits(m, "btergm")) {
    klasse <- class(m)[1]
    stop("'m' should be a fitted 'ergm' or 'btergm' model, but you inputted a ", klasse, " object")
  }
  
  if (silent) {
    g <- suppressMessages(gof_btergm(m, statistics = btergm_statistics, ...,
                                              verbose = verbose),
                          classes = c("message", "warning"))
  }
  g <- suppressWarnings(gof_btergm(m, statistics = btergm_statistics, ...,
                                            verbose = verbose))
  plot_gof_btergm(g)
  invisible(g)
}




#' GOF plot
#'
#' Plots the Goodness of fit for the output of a model of class \code{ergm} or \code{btergm}
#'
#' This function plots the goodness-of-fit of unipartite Exponential Random Graphs Models 
#' or BTERGM models.
#'
#' It does not process bipartite ERGMs model objects nor other types of 
#' Exponential random graph models object.
#'
#' @param gof A gof model object for a fitted \code{ergm} or \code{btergm} model
#'
#' @return Plots displaying the Goodness of Fit
#' @export GOF_plot
#'
#' @examples
#' \dontrun{
#' ### ergm model
#' flo <-SNA4DSData::florentine
#' flom <- to_network(flo$flomarriage)
#' m_ergm <- ergm::ergm(flom ~ edges + nodecov("Wealth"))
#' gof_ergm <- ergm::gof(m_ergm)
#' GOF_plot(gof_ergm)
#' 
#' ### btergm model
#' data(alliances, package = "SNA4DSData")
#' m_btergm <- btergm::btergm(allianceNets ~ edges + 
#'   gwesp(0, fixed = TRUE) +
#'   edgecov(lastYearsSharedPartners) + 
#'   edgecov(militaryDisputes) + 
#'   nodecov("polity") +
#'   nodecov("Composite_Index_National_Capability") + 
#'   absdiff("polity") + 
#'   absdiff("Composite_Index_National_Capability") +
#'   edgecov(sharedBorder), 
#'   R = 100, # number of bootstraps
#'   parallel = "snow", ncpus = 16  # optional line
#' )
#' 
#' gof_btergm <- gof_btergm(m_btergm)
#' GOF_plot(gof_btergm)
#' }
GOF_plot <- function(gof) {
  if (inherits(gof, "gof.ergm")) {
    plot_gof_ergm(gof)
  } else if (inherits(gof, "gof")) {
    plot_gof_btergm(gof)
  } else {
    stop("The input for 'm' has to be a gof object from a 'ergm' or a 'btergm' model")
  }
}

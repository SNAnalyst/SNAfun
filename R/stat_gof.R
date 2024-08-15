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
#' \code{ergm} package and then plot that (using \link{stat_plot_gof} or 
#' using the \code{ergm} package functions).
#' 
#' NOTE2: because the gof statistics for the models are determined inside the 
#' plotting functions (so they can be used for the plotting), they can take some
#' time to run. This is why it is useful that the gof results themselves are not 
#' only calculated by this function, but also returned for further inspection.
#'
#' @param m a fitted \code{ergm} or \code{btergm} model
#' @param ... optional arguments, see description
#' @param btergm_statistics vector of statistics to be used in the gof
#' @param silent logical, if \code{TRUE} fewer messages will be printed during the 
#' calculation of the gof (but the function will often still print some output)
#' @param verbose logical, if \code{FALSE}, the printing of most details 
#' by the gof function is suppressed (but some warnings may still appear and 
#' errors are always shown)
#' @family statistics functions
#' @return the goodness of fit is silently returned
#' @export
#'
#' @examples
#' \dontrun{
#' ### ergm model
#' data(florentine, package = "snafun")
#' flom <- to_network(florentine$flomarriage)
#' m_ergm <- ergm::ergm(flom ~ edges + nodecov("Wealth"))
#' gof_ergm <- stat_plot_gof_as_btergm(m_ergm)
#' gof_ergm <- stat_plot_gof_as_btergm(m_ergm, 
#'         btergm_statistics = c(btergm::esp, btergm::dsp, btergm::deg))
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
#'   R = 100, # number of bootstraps, perhaps set lower to just try the example
#'   parallel = "snow", ncpus = 16  # optional line
#' )
#' 
#' # gof with only 10 simulations to keep time for the example low
#' gof_btergm <- stat_plot_gof_as_btergm(m_btergm, nsim = 10)
#' gof_btergm <- stat_plot_gof_as_btergm(m_btergm, 
#'   parallel = "snow", ncpus = 12)
#' gof_btergm
#' gof_btergm <- stat_plot_gof_as_btergm(m_btergm, 
#'   parallel = "snow", ncpus = 12, silent = TRUE)
#' gof_btergm <- stat_plot_gof_as_btergm(m_btergm, 
#'   parallel = "snow", ncpus = 12, verbose = FALSE, silent = TRUE)
#' }
stat_plot_gof_as_btergm <- function(m, 
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
  
  gofbtergm <- utils::getFromNamespace("gof.btergm", "btergm")
  if (silent) {
    g <- suppressMessages(gofbtergm(m, statistics = btergm_statistics, ...,
                                              verbose = verbose),
                          classes = c("message", "warning"))
  }
  g <- suppressWarnings(gofbtergm(m, statistics = btergm_statistics, ...,
                                            verbose = verbose))
  plotgof <- utils::getFromNamespace("plot.gof", "btergm")
  plotgof(g)
  invisible(g)
}




#' Plot the Goodness of fit of a (bt)ergm result
#'
#' Plots the Goodness of fit for the output of a model of class \code{ergm} or \code{btergm}
#'
#' This function plots the goodness-of-fit of unipartite Exponential Random Graphs Models 
#' or BTERGM models.
#'
#' It does not process bipartite ERGMs model objects nor other types of 
#' Exponential random graph models object.
#' 
#' The graphical arguments for this function are only used when a \code{btergm} 
#' gof object is passed, since the ergm gof object does not support the setting 
#' of graphical arguments (with the exception of the \code{...} argument).
#'
#' @param gof A gof model object for a fitted \code{ergm} or \code{btergm} model
#' @param median_include logical, should the median be included? 
#' Default is \code{TRUE}
#' @param median_col character, name of the color to plot the median in.
#' @param median_lwd number, width of the line for the median.
#' @param median_lty what line type should be used to plot the 
#' median. Line types can either be specified as an integer 
#' (0 = blank, 1 = solid (default), 2 = dashed, 3 = dotted, 4 = dotdash, 
#' 5 = longdash, 6 = twodash) or as one of the character strings "blank", 
#' "solid", "dashed", "dotted", "dotdash", "longdash", or "twodash", 
#' where "blank" uses ‘invisible lines’ (i.e., does not draw them).
#' @param mean_include logical, should the mean be included? 
#' Default is \code{TRUE}. Ignored if \code{gof} is the gof of an ergm.
#' @param mean_col character, name of the color to plot the mean in.
#' @param mean_lwd number, width of the line for the mean.
#' @param mean_lty what line type should be used to plot the 
#' medn. Default is "dashed".
#' @param ... additional arguments. When, \code{gof} is an \code{ergm} gof 
#' object, it will be passed to the \link{boxplot} function (really only 
#' somewhat potentially useful to set color in the \code{border} argument). 
#' For a \code{btergm} gof, it is passed to the \code{plot.gof} function of 
#' the \code{btergm} package.
#'
#' @return Plots displaying the Goodness of Fit
#' @export 
#' @family statistics functions
#' @examples
#' \dontrun{
#' ### ergm model
#' data(florentine, package = "snafun")
#' flom <- to_network(florentine$flomarriage)
#' m_ergm <- ergm::ergm(flom ~ edges + nodecov("Wealth"))
#' gof_ergm <- ergm::gof(m_ergm)
#' stat_plot_gof(gof_ergm)
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
#'   R = 10, # number of bootstraps
#'   parallel = "snow", ncpus = 16  # optional line
#' )
#' 
#' # gof with on;y 10 simulations to keep time for the example low
#' gof_btergm <- btergm:::gof.btergm(m_btergm, nsim = 10)
#' stat_plot_gof(gof_btergm)
#' stat_plot_gof(gof_btergm, median_include = FALSE)
#' stat_plot_gof(gof_btergm, mean_col = "green", mean_lwd = 4, mean_lty = "dotted")
#' stat_plot_gof(gof_btergm, mean_include = FALSE, median_col = "orange")
#' stat_plot_gof(gof_btergm, mean_include = FALSE, median_include = FALSE)
#' }
stat_plot_gof <- function(gof, 
                          median_include = TRUE, median_col = "black",
                          median_lwd = 2, median_lty = "solid", 
                          mean_include = TRUE, mean_col = "black",
                          mean_lwd = 1, mean_lty = "dashed", 
                          ...) {
  if (inherits(gof, "gof.ergm")) {
    plotgof <- utils::getFromNamespace("plot.gof", "ergm")
    plotgof(gof, ...)
  } else if (inherits(gof, "gof")) {
    plotgof <- utils::getFromNamespace("plot.gof", "btergm")
    plotgof(gof, 
                      mean = mean_include, mean.lwd = mean_lwd, 
                      mean.lty = mean_lty, mean.col = mean_col,
                      median = median_include, median.lwd = median_lwd, 
                      median.lty = median_lty, median.col = median_col,
                      ...)
  } else {
    stop("The input for 'm' has to be a gof object from a 'ergm' or a 'btergm' model")
  }
}

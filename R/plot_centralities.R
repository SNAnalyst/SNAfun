
#' Plot one or more centrality scores
#'
#' Plot one or more centrality scores of all vertices in the network
#' 
#' This function creates of grid of plots containing the values a set of 
#' centrality scores, selected by the user. The function will calculate the 
#' requested scores and then plot them. When a large network is used and 
#' many or complex measures are requested, the calculations can take some time. 
#' The plotting itself will be fast.
#' 
#' Since the function calculates the centrality scores itself, the choice 
#' is currently limited to the centralities implemented in this package.
#' 
#' At the moment, these are "betweenness", "closeness", "degree", "eccentricity", 
#' "eigenvector", "geokpath", "harmonic", "shapley", and "stress". The default is to 
#' plot "betweenness", "closeness", "degree", and "eccentricity".
#' 
#' The various arguments relate to these measures. The \code{mode}, 
#' \code{directed}, and \code{rescaled} arguments will be appled to all chosen 
#' measures
#'
#' The function takes an \code{igraph} object or a \code{network} object as input.
#'
#' @param net a network of class \code{igraph} or \code{network}
#' @param measures character vector of one or more centrality measures
#' @param directed logical, should direction be taken into account? This argument
#' is optional, if it is not chosen the directionality of the network will be used.
#' @param mode Character. The default is "all".
#' @param rescaled Logical scalar, whether to rescale the centrality scores to 
#' they add up to 1.
#' @param k The k parameter for the "geokpath" measure. The default is 3.
#' @param ... other arguments passed to \code{\link{plot}}.
#'
#' @return a plot
#' @export
#'
#' @examples
#' \dontrun{
#' g <- igraph::erdos.renyi.game(20, 1/20)
#' plot_centralities(g, measures = c("degree", "betweenness"))
#' # note that closeness is not well-defined for disconnected graphs
#' plot_centralities(g)
#' 
#' data(florentine, package = "snafun")
#' business <- florentine$flobusiness
#' plot_centralities(business)
#' plot_centralities(business, rescaled = TRUE)
#' plot_centralities(business, measures = c("betweenness", "closeness", "degree", 
#'    "eccentricity", "eigenvector", "geokpath", "harmonic", "stress"))
#' bus_n <- snafun::to_network(business)
#' plot_centralities(bus_n, c("degree", "betweenness"), mode = "out", use_vertexnames = TRUE)
#' }
plot_centralities <- function(net,
                              measures = c("betweenness", "closeness", "degree", "eccentricity"),
                              directed = TRUE,
                              mode = c("all", "out", "in"),
                              k = 3,
                              rescaled = FALSE,
                              ...) {
  if (!inherits(net, "igraph") & !inherits(net, "network")) {
    stop("Please provide a graph of class 'igraph' or 'network'.")
  }
  measures <- unique(measures)  # in case the same measure is accidentally asked for twice
  implemented <- c("betweenness", "closeness", "degree", "eccentricity", 
  "eigenvector", "geokpath", "harmonic", "shapley", "stress")
  if (any(!measures %in% implemented)) {
    stop('Only ', paste(implemented, collapse = ", "), ' are currently implemented.')
  }

  mode = snafun.match.arg(mode)

  out <- matrix(ncol = 3)
  colnames(out) <- c("node", "measure", "value")
  out <- as.data.frame(out[-1, ])
  
  namen <- 1:snafun::count_vertices(net)

  num_plots <- length(measures)
  opar <- graphics::par("mfrow")
  on.exit(par(mfrow = opar), add = TRUE)
  if (num_plots <= 3) {
    par(mfrow = c(1, num_plots))
  } else if (num_plots == 4) {
    par(mfrow = c(2, 2))
  } else {  # 5 or more plots
    n_rows <- ceiling(num_plots/3)   # 3 cols per row
    par(mfrow = c(n_rows, 3))
  }
  

  if ("betweenness" %in% measures) {
    score <- v_betweenness(net, directed = directed, rescaled = rescaled)
    out <- rbind(out, data.frame(node = namen, measure = "betweenness", value = score))
  }
  if ("closeness" %in% measures) {
    score <- v_closeness(net, mode = mode, rescaled = rescaled)
    out <- rbind(out, data.frame(node = namen, measure = "closeness", value = score))
  }
  if ("degree" %in% measures) {
    score <- v_degree(net, mode = mode, rescaled = rescaled)
    out <- rbind(out, data.frame(node = namen, measure = "degree", value = score))
  }
  if ("eccentricity" %in% measures) {
    score <- v_eccentricity(net, mode = mode, rescaled = rescaled)
    out <- rbind(out, data.frame(node = namen, measure = "eccentricity", value = score))
  }
  if ("eigenvector" %in% measures) {
    score <- v_eigenvector(net, directed = directed, rescaled = rescaled)
    out <- rbind(out, data.frame(node = namen, measure = "eigenvector", value = score))
  }
  if ("geokpath" %in% measures) {
    score <- v_geokpath(net, mode = mode, rescaled = rescaled, k = k)
    out <- rbind(out, data.frame(node = namen, measure = "geokpath", value = score))
  }
  if ("harmonic" %in% measures) {
    score <- v_harmonic(net, mode = mode, rescaled = rescaled)
    out <- rbind(out, data.frame(node = namen, measure = "harmonic", value = score))
  }
  if ("shapley" %in% measures) {
    score <- v_shapley(net, rescaled = rescaled)
    out <- rbind(out, data.frame(node = namen, measure = "shapley", value = score))
  }
  if ("stress" %in% measures) {
    score <- v_stress(net, directed = directed, rescaled = rescaled)
    out <- rbind(out, data.frame(node = namen, measure = "stress", value = score))
  }
  
  for (measure in measures) {
    plot_measure(data = out, measure = measure)
  }
  
  suppressWarnings(graphics::par(opar))
  invisible()
}


plot_measure <- function(data, measure, ...) {
  d <- data[data$measure == measure, ]
  plot(x = d$value, y = d$node, pch = 16, main = as.character(measure), 
       type = "b",
       xlab = "", ylab = "", yaxt = "n", ...)
  graphics::axis(side = 2, las = 2)
}

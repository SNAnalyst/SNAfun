


#' Plot the graph object
#' 
#' Plot the graph object
#' 
#' Plots a graph object of class \code{network} or \code{igraph} exactly as it 
#' would be plotted by the \code{network} or \code{igraph} packages, 
#' respectively. The function wraps \link[network]{plot.network} and 
#' \link[igraph]{plot.igraph}.  
#' 
#' Whatever is set in \code{...} is passed onto these original functions. 
#' See the respective help pages (\link[network]{plot.network}, 
#' \link[igraph]{plot.igraph}, and \link[igraph]{igraph.plotting}) 
#' if you want to use settings that deviate from 
#' the default settings in these packages (which is almost always better than 
#' using the defaults).
#' 
#' This function plots the objects just as they would be plotted if the
#' packages were attached, but now they do not need to be attached.
#'
#' The roxygen export tag on this block exports the name \code{plot} without
#' creating an object called \code{plot} inside the \code{snafun} namespace, so
#' \code{snafun::plot(x)} resolves to \code{base::plot} through the namespace's
#' parent chain. Both details matter. Drop the tag and \code{snafun::plot()}
#' stops existing; add an actual \code{plot} object to the namespace (say,
#' \code{plot <- base::plot} or a local \code{UseMethod("plot")} generic) and
#' \code{loadNamespace()} treats \code{plot} as a local generic, registering the
#' methods below in snafun's own S3 table instead of in the one belonging to
#' \code{base}. A bare \code{plot(x)} would then quietly fall through to
#' \code{plot.default} for classes such as \code{stat_cug}. roxygen2 warns that
#' \code{plot} is "listed as an export, but not present in namespace"; that
#' warning is a false alarm here.
#'
#' @param x graph object of class \code{network} or \code{igraph}
#' @param ... Additional arguments.
#'
#' @return nothing, the object is merely plotted
#' @name plot
#' @export
#' @examples
#' g_i <- snafun::create_random_graph(10, "gnm", m = 20, graph = "igraph")
#' g_n <- snafun::create_random_graph(10, "gnm", m = 20, graph = "network")
#' plot(g_i)
#' plot(g_i, vertex.size = 12, vertex.color = "green", edge.width = 5, edge.curved = TRUE)
#' plot(g_n)
#' plot(g_n, vertex.cex = 3, vertex.col = "green", edge.lwd = 10,
#'    edge.col = "darkgrey", usecurve = TRUE, edge.curve = .05,
#'    arrowhead.cex = 3, displaylabels = TRUE, label.pos = 5)
#' snafun::plot(g_i)
NULL

#' @rdname plot
#' @export
plot.igraph <- function(x, ...) {
  igraph::plot.igraph(x = x, ...)
}


#' @rdname plot
#' @export
plot.network <- function(x, ...) {
  network::plot.network(x = x, ...)
}


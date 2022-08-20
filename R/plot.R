


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



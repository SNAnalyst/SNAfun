


#' Print the graph object
#' 
#' Print the graph object
#' 
#' Prints a graph object of class \code{network} or \code{igraph} exactly as it 
#' would be printed by the \code{network} or \code{igraph} packages, 
#' respectively. The function wraps \link[network]{print.network} and 
#' \link[igraph]{print.igraph}.  
#' Whatever is set in \code{...} is passed onto these original functions. 
#' See the respective help pages if you want to use settings that deviate from 
#' the default settings in these packages.
#' 
#' This function prints the objects just as they would be printed if the 
#' packages were attached, but now they do not need to be attached.
#'
#' @param x graph object of class \code{network} or \code{igraph}
#' @param na.omit logical; omit summarization of missing attributes in a 
#' \code{network} object?
#' @param ... Additional agruments.
#'
#' @return nothing, the object is merely printed
#' @name print
#' @export
#' @examples 
#' g_i <- snafun::create_random_graph(10, "gnm", m = 20, graph = "igraph")
#' g_n <- snafun::create_random_graph(10, "gnm", m = 20, graph = "network")
#' print(g_i)
#' print(g_n)
NULL

#' @rdname print
#' @export
print.igraph <- function(x, ...) {
  igraph::print.igraph(x = x, ...)
}


#' @rdname print
#' @export
print.network <- function(x, na.omit = FALSE, ...) {
  network::print.network(x = x, na.omit = na.omit, ...)
}



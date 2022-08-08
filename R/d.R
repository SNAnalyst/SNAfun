

#' Dyad level indices
#' 
#' Dyad level indices
#' 
#' Calculate several dyad level indices. 
#' 
#' @param x graph object
#' @param mode Character constant, gives whether the shortest paths to or from 
#' the given vertices should be calculated for directed graphs. If \code{out} 
#' then the shortest paths from the vertex, if \code{in} then to it will be 
#' considered. If \code{all}, the default, then the corresponding undirected 
#' graph will be used, ie. not directed paths are searched. 
#' This argument is ignored for undirected graphs.
#' @name dli
NULL


####----------------------------------------------------------------------------
#' @describeIn dli Distance between each pair of vertices in the graph. 
#' Weights are discarded. The output of this function is a matrix.
#' Use \code{\link[igraph]{distances}} or \code{\link[sna]{geodist}} if edge 
#' weights are to be used or specific options are needed. 
#' @export
#' @examples 
#' # 
#' # distance
#' g <- igraph::make_ring(10)
#' igraph::distances(g)
#' d_distance(g)
#' d_distance(snafun::to_network(g))
#' igraph::V(g)$name <- LETTERS[1:10]
#' igraph::distances(g)
#' d_distance(g)
#' d_distance(snafun::to_network(g))
d_distance <- function(x, mode = c("all", "out", "in")) {
  UseMethod("d_distance")
}


#' @export
d_distance.default <- function(x, mode = c("all", "out", "in")) {
  txt <- methods_error_message("x", "d_distance")
  stop(txt)
}


#' @export
d_distance.igraph <- function(x, mode = c("all", "out", "in")) {
  mode <- snafun.match.arg(mode)
  igraph::distances(
    graph = x,
    mode = mode,
    weights = NA,
    algorithm = "automatic"
  )
}


#' @export
d_distance.network <- function(x, mode = c("all", "out", "in")) {
  mode <- snafun.match.arg(mode)
  g <- to_igraph(x)
  d_distance.igraph(g, mode = mode)
}


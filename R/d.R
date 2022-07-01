

#' Dyad level indices
#' 
#' Dyad level indices
#' 
#' Calculate several dyad level indices. 
#' 
#' @param x graph object
#' @param directed Logical, whether directed or undirected paths are to be 
#' considered. This is ignored for undirected graphs.
#' @param unconnected Logical, what to do if the graph is unconnected. If \code{FALSE}, 
#' the function will return a number that is one larger the largest possible 
#' diameter, which is always the number of vertices. If \code{TRUE}, the 
#' diameters of the connected components will be calculated and the largest 
#' one will be returned.
#' @param type Character constant, gives whether the shortest paths to or from 
#' the given vertices should be calculated for directed graphs. If \code{out} 
#' then the shortest paths from the vertex, if \code{in} then to it will be 
#' considered. If \code{all}, the default, then the corresponding undirected 
#' graph will be used, ie. not directed paths are searched. 
#' This argument is ignored for undirected graphs.
#' @name dli
NULL



#' @describeIn dli Diameter of a graph. Weights are discarded. 
#' Use \code{\link[igraph]{diameter}} if edge weights are to be used or specific 
#' options are needed. 
#' @export
#' @examples 
#' #
#' # diameter
#' g <- igraph::make_ring(10)
#' g2 <- igraph::delete_edges(g, c(1,2,1,10))
#' igraph::diameter(g2, unconnected=TRUE)
#' igraph::diameter(g2, unconnected=FALSE)
#' d_diameter(g2)  # 7
#' d_diameter(g2, unconnected = FALSE)  # Inf
#' 
#' d_diameter(to_network(g2)) # 7
#' d_diameter(to_network(g2), unconnected = FALSE) # Inf
d_diameter <- function(x, 
                       directed = is_directed(x),
                       unconnected = TRUE) {
  UseMethod("d_diameter")
}


#' @export
d_diameter.default <- function(x, 
                               directed = is_directed(x),
                               unconnected = TRUE) {
  txt <- methods_error_message("x", "d_diameter")
  stop(txt)
}


#' @export
d_diameter.igraph <- function(x, 
                              directed = is_directed(x),
                              unconnected = TRUE) {
  igraph::diameter(x, directed = directed,
                   unconnected = unconnected,
                   weights = NA)
}


#' @export
d_diameter.network <- function(x, 
                               directed = is_directed(x),
                               unconnected = TRUE) {
  g <- to_igraph(x)
  d_diameter.igraph(x = g, directed = directed,
             unconnected = unconnected)
}








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
d_distance <- function(x, type = c("all", "out", "in")) {
  UseMethod("d_distance")
}


#' @export
d_distance.default <- function(x, type = c("all", "out", "in")) {
  txt <- methods_error_message("x", "d_distance")
  stop(txt)
}


#' @export
d_distance.igraph <- function(x, type = c("all", "out", "in")) {
  type <- snafun.match.arg(type)
  igraph::distances(
    graph = x,
    mode = type,
    weights = NA,
    algorithm = "automatic"
  )
}


#' @export
d_distance.network <- function(x, type = c("all", "out", "in")) {
  type <- snafun.match.arg(type)
  g <- to_igraph(x)
  d_distance.igraph(g, type = type)
}


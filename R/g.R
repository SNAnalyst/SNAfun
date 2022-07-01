
#' Graph level indices
#' 
#' Graph level indices
#' 
#' Calculate several graph level indices. 
#' 
#' @param x graph object
#' @param loops logical, should loops/diagonal be included? Default is \code{FALSE}.
#' This is almost always what you want.
#' @examples 
#' m <- matrix(rbinom(25, 1, 0.5), 5, 5)
#' diag(m) <- 0
#' g_density(snafun::to_network(m))
#' g_density(snafun::to_igraph(m))
#' # when loops matter
#' g <- igraph::graph( c(1,2, 2,2, 2,3) )
#' g_density(g, loops = FALSE)   # this is wrong, if loops matter
#' g_density(g, loops = TRUE)   # this is correct, if loops matter
#' 
#' g <- sna::rgraph(10, mode = "digraph")
#' g_n <- snafun::to_network(g)
#' g_mean_distance(g_n)
#' g_i <- snafun::to_igraph(g)
#' g_mean_distance(g_i)
#' 
#' g <- sna::rgraph(10, mode = "graph")
#' g_n <- snafun::to_network(g)
#' g_mean_distance(g_n)
#' g_i <- snafun::to_igraph(g)
#' g_mean_distance(g_i)
#' @name gli
NULL

#' @describeIn gli Density of a graph. Weights are discarded. 
#' Use \code{\link[sna]{gden}} if edge weights are to be used and \code{code} is 
#' a graph of class \code{network}.
#' @export
g_density <- function(x, loops = FALSE) {
  UseMethod("g_density")
}


#' @export
g_density.default <- function(x, loops = FALSE) {
  txt <- methods_error_message("x", "g_density")
  stop(txt)
}


#' @export
g_density.igraph <- function(x, loops = FALSE) {
  igraph::edge_density(x, loops = loops)
}


#' @export
g_density.network <- function(x, loops = FALSE) {
  sna::gden(dat = x, diag = loops, 
            mode = ifelse(snafun::is_directed(x), "digraph", "graph"),
            ignore.eval = TRUE)
}




#' @describeIn gli Mean path distance
#' @export
g_mean_distance <- function(x) {
  UseMethod("g_mean_distance")
}


#' @export
g_mean_distance.default <- function(x) {
  txt <- methods_error_message("x", "g_mean_distance")
  stop(txt)
}


#' @export
g_mean_distance.igraph <- function(x) {
  igraph::mean_distance(x, weights = NULL, directed = TRUE,
                        unconnected = TRUE,
                        details = FALSE)
}


#' @export
g_mean_distance.network <- function(x) {
  dists <- sna::geodist(x, count.paths = FALSE,
                        ignore.eval = TRUE)$gdist
  n <- network::network.size(x)
  sum(dists)/(n * (n-1))
}




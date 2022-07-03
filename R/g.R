
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









#' @describeIn gli Product-moment correlation between two networks. 
#' Missing values are permitted. Takes into account whether the 
#' graph is (un)directed (judged from \code{g1}). Both input graphs should 
#' be of the same type.
#' @export
#' @param g1 an input graph of class \code{igraph}, \code{network}, \code{matrix}
#' @param g2 an input graph of class \code{igraph}, \code{network}, \code{matrix}
#' @param diag logical, indicating whether or not the diagonal should be treated 
#' as valid data. Set this \code{TRUE} if and only if the data can contain 
#' loops. Is \code{FALSE} by default.
#' @examples
#' # 
#' # correlation
#' # matrices
#' g1 <- sna::rgraph(10,1,tprob=c(0.2,0.2,0.5,0.5,0.8,0.8))
#' g2 <- sna::rgraph(10,1,tprob=c(0.2,0.2,0.5,0.5,0.8,0.8))
#' g_correlation(g1, g2)
#' 
#' g1 <- to_network(g1); g2 <- to_network(g2)
#' g_correlation(g1, g2)
#' 
#' g1 <- to_igraph(g1); g2 <- to_igraph(g2)
#' g_correlation(g1, g2)
g_correlation <- function(g1, g2, diag = FALSE) {
  UseMethod("g_correlation")
}


#' @export
g_correlation.default <- function(g1, g2, diag = FALSE) {
  txt <- methods_error_message("g1", "g_correlation")
  stop(txt)
}


#' @export
g_correlation.igraph <- function(g1, g2, diag = FALSE) {
  if (!inherits(g2, "igraph")) stop("Graph 'g2' should be of class 'igraph' too")
  x1 <- to_matrix(g1)
  x2 <- to_matrix(g2)
  directed <- is_directed(g1)
  sna::gcor(dat = x1, dat2 = x2, diag = diag, 
            mode = ifelse(directed, "digraph", "graph"))
}


#' @export
g_correlation.network <- function(g1, g2, diag = FALSE) {
  if (!inherits(g2, "network")) stop("Graph 'g2' should be of class 'network' too")
  directed <- is_directed(g1)
  sna::gcor(dat = g1, dat2 = g2, diag = diag, 
            mode = ifelse(directed, "digraph", "graph"))
}

#' @export
g_correlation.matrix <- function(g1, g2, diag = FALSE) {
  if (!inherits(g2, "matrix")) stop("Graph 'g2' should be of class 'matrix' too")
  directed <- is_directed(g1)
  sna::gcor(dat = g1, dat2 = g2, diag = diag, 
            mode = ifelse(directed, "digraph", "graph"))
}








#' @describeIn gli Reciprocity
#' @export
#' @examples
#' # 
#' # reciprocity
#' g <- igraph::erdos.renyi.game(10, .3, type = "gnp", directed = TRUE)
#' g_reciprocity(g)
#' g_reciprocity(snafun::to_network(g))
#' 
#' g <- snafun::create_random_graph(10, strategy = "gnm", m = 2, directed = FALSE, graph = "igraph")
#' g_reciprocity(g)
#' g_reciprocity(snafun::to_network(g))
g_reciprocity <- function(x) {
  UseMethod("g_reciprocity")
}


#' @export
g_reciprocity.default <- function(x) {
  txt <- methods_error_message("x", "g_reciprocity")
  stop(txt)
}


#' @export
g_reciprocity.igraph <- function(x) {
  igraph::reciprocity(x, ignore.loops = TRUE, mode = "default")
}


#' @export
g_reciprocity.network <- function(x) {
  sna::grecip(dat = x, measure = "edgewise") |> unname()
}



#' @describeIn gli Transitivity Transitivity is a triadic, algebraic structural 
#' constraint. In its weak form (which is the common form), the transitive constraint 
#' corresponds to a -> b -> c implying a -> c. This measure returns  the fraction 
#' of potentially intransitive triads obeying the weak condition.
#' Weights are discarded. Specific functions that can alternatively be used (and are 
#' called by this function) include \code{\link[sna]{gtrans}} (for objects of 
#' class \code{network}) and \code{\link[igraph]{transitivity}} (for objects of 
#' class \code{igraph}).
#' @export
#' @examples
#' #
#' # transitivity
#' data("emon", package = "network")
#' g <- emon$Cheyenne
#' is_directed(g)  # TRUE
#' g_transitivity(g)
#' g_transitivity(to_network(g))
#' 
#' data(florentine, package = "snafun")
#' g <- florentine$flobusiness
#' is_directed(g)  # FALSE
#' g_transitivity(g)
#' g_transitivity(to_network(g))
g_transitivity <- function(x) {
  UseMethod("g_transitivity")
}


#' @export
g_transitivity.default <- function(x) {
  txt <- methods_error_message("x", "g_transitivity")
  stop(txt)
}


#' @export
g_transitivity.igraph <- function(x) {
  igraph::transitivity(x, type = "global")
}


#' @export
g_transitivity.network <- function(x) {
  sna::gtrans(x, 
              mode = ifelse(is_directed(x), "digraph", "graph"),
              measure = "weak",
              use.adjacency = TRUE)
}





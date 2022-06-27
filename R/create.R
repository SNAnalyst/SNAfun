
#' Create a random bipartite network
#' 
#' Generate bipartite graphs using the Erdos-Renyi model
#' 
#' Generate a random bipartite network, either as a \code{igraph} or 
#' \code{network} object.
#' 
#' When using strategy 'gnp', a bipartite network is generated where each edge
#' has probability 'p' of occuring. This means that consecutive runs of the 
#' algorithm will usually result in graphs with different numbers of edges. 
#' 
#' When using strategy 'gnm', a bipartite network is generated with exactly 'm' 
#' edges. This means that consecutive runs of the 
#' algorithm will result in graphs with the same number of edges (but will occur 
#' between different vertices). 
#' 
#' The generation is done through the \code{\link[igraph]{sample_bipartite}} 
#' function.
#'
#' @param n_type1 integer, number of vertices of the first type
#' @param n_type2 integer, number of vertices of the first type
#' @param strategy Character scalar, the type of the graph, ‘gnp’ creates a 
#' $G(n,p)$ graph, ‘gnm’ creates a $G(n,m)$ graph. See details below.
#' @param p probability for $G(n,p)$ graphs. Should not be given for $G(n,m)$ graphs.
#' @param m integer, the number of edges for $G(n,p)$ graphs. Should not be given for $G(n,p)$ graphs.
#' @param directed logical, whether to create a directed graph
#' @param mode Character scalar, specifies how to direct the edges in directed 
#' graphs. If it is ‘out’, then directed edges point from bottom vertices to top 
#' vertices. If it is ‘in’, edges point from top vertices to bottom vertices. 
#' ‘out’ and ‘in’ do not generate mutual edges. 
#' If this argument is ‘all’, then each edge direction is considered independently 
#' and mutual edges might be generated. This argument is ignored for undirected graphs.
#' @param graph character, the type of graph to be generated: \code{igraph} or 
#' \code{network}
#'
#' @return a graph of class \code{network} or \code{igraph}
#' @export
create_bipartite <- function(n_type1, 
                             n_type2,
                             strategy = c("gnp", "gnm"),
                             p, 
                             m,
                             directed = FALSE,
                             mode = c("out", "in", "all"),
                             graph = c("igraph", "network")) {
  strategy <- snafun.match.arg(strategy)
  graph <- snafun.match.arg(graph)
  mode <- snafun.match.arg(mode)
  
  if (!missing(p)) {
    p <- as.numeric(p)
    if (p < 0 | p > 1) {
      stop("p should be between 0 and 1 (inclusive)")
    }
  }
  if (!missing(m)) {
    m <- as.integer(m)
  }
  # generate the random igraph bipartite network
  uit <- igraph::sample_bipartite(
    n1 = as.integer(n_type1),
    n2 = as.integer(n_type2),
    type = strategy,
    p,
    m,
    directed = as.logical(directed),
    mode = mode
  )
  
  if (graph == "network") {
    inc <- igraph::as_incidence_matrix(uit, sparse = FALSE)
    uit <- to_network(inc, bipartite = TRUE)
  }
  
  uit
}
  



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
  






#' Create a random bipartite network
#' 
#' Generate a random graph
#' 
#' Generate a random network, either as a \code{igraph} or 
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
#' @param n_vertices integer, number of vertices to include in the graph
#' @param strategy Character scalar, the type of the graph, ‘gnp’ creates a 
#' $G(n,p)$ graph, ‘gnm’ creates a $G(n,m)$ graph. See details below.
#' @param p probability for $G(n,p)$ graphs. Should not be given for $G(n,m)$ graphs.
#' @param m integer, the number of edges for $G(n,p)$ graphs. Should not be given for $G(n,p)$ graphs.
#' @param directed logical, whether to create a directed graph
#' @param graph character, the type of graph to be generated: \code{igraph} or 
#' \code{network}
#'
#' @return a graph of class \code{network} or \code{igraph}
#' @export
#' @examples
#' create_random_graph(n_vertices = 10, "gnp", p = .25, graph = "igraph")
#' create_random_graph(n_vertices = 10, "gnp", p = .25, graph = "network")
#' create_random_graph(n_vertices = 10, "gnm", m = 15, graph = "igraph")
#' create_random_graph(n_vertices = 10, "gnm", m = 15, graph = "network")
create_random_graph <- function(n_vertices, 
                             strategy = c("gnp", "gnm"),
                             p, 
                             m,
                             directed = TRUE,
                             graph = c("igraph", "network")) {
  strategy <- snafun.match.arg(strategy)
  graph <- snafun.match.arg(graph)
  
  if (!missing(p)) {
    p <- as.numeric(p)
    if (p < 0 | p > 1) {
      stop("p should be between 0 and 1 (inclusive)")
    }
  }
  if (!missing(m)) {
    m <- as.integer(m)
  }
  # generate the random network
  if (strategy == "gnp" & graph == "igraph") {
    uit <- igraph::sample_gnp(n = n_vertices, p = p, directed = directed, loops = FALSE)
  }
  if (strategy == "gnm" & graph == "igraph") {
    uit <- igraph::sample_gnm(n = n_vertices, m = m, directed = directed, loops = FALSE)
  }
  if (strategy == "gnp" & graph == "network") {
    uit <- sna::rgraph(n = n_vertices, m = 1, tprob = p,
                       mode = ifelse(directed, "digraph", "graph"), 
                       diag = FALSE, replace = FALSE) |> 
      network::as.network.matrix(directed = directed)
  }
  if (strategy == "gnm" & graph == "network") {
    uit <- sna::rgnm(n = 1, nv = n_vertices, m = m, 
                       mode = ifelse(directed, "digraph", "graph"), 
                       diag = FALSE) |> 
      network::as.network.matrix(directed = directed)
  }
  
  uit
}









#' Create an empty network
#' 
#' Generate a graph with no edges
#' 
#' Generate a graph with no edges, either as a \code{igraph} or 
#' \code{network} object.
#'
#' @param n_vertices numeric, number of vertices 
#' @param directed logical, whether to create a directed graph
#' @param graph character, the type of graph to be generated: \code{igraph} or 
#' \code{network}
#'
#' @return a graph of class \code{network} or \code{igraph}
#' @export
create_empty_graph <- function(n_vertices, 
                             directed = TRUE,
                             graph = c("igraph", "network")) {
  graph <- snafun.match.arg(graph)
  if (!is.numeric(n_vertices)) {
    stop("'n_vertices' has to be a number")
  }
  if (graph == "igraph") {
    igraph::make_empty_graph(n = n_vertices, directed = directed)
  } else if (graph == "network") {
    network::network.initialize(n = n_vertices, directed = directed)
  } else {
    stop("'graph' should either be 'igraph' or 'network'")
  }
}








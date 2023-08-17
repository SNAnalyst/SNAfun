
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
#' @param m integer, the number of edges for $G(n,p)$ graphs. Should not be given 
#' for $G(n,p)$ graphs.
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









#' Create a manual (literal) graph
#' 
#' Create a (small) graph quickly
#' 
#' This function is useful if you want to create a small (named) graph quickly, 
#' it works for both directed and undirected graphs.
#' You need to supply one or more R expressions giving the structure of the graph. 
#' The expressions consist of vertex names and edge operators. An edge operator 
#' is a sequence of ‘-’ and ‘+’ characters, the former is for the edges and 
#' the latter is used for arrow heads. The edges can be arbitrarily long, 
#' i.e. you may use as many ‘-’ characters to “draw” them as you like.
#' 
#' This function mainly acts as a wrapper for \code{\link[igraph]{graph_from_literal}} 
#' and almost all of the examples come from that function as well.
#'
#' @param ... the formulae giving the structure of the graph, see details below.
#' @param simplify Logical scalar. By default the graph is simplified, 
#' loops and multiple edges are removed.
#' @param graph character, the type of graph to be generated: \code{igraph} (default) 
#' or \code{network}
#'
#' @return a graph of class \code{network} or \code{igraph}
#' @export
#'
#' @examples
#' # undirected graph
#' create_manual_graph(1-2)
#' ##  the length of the edges does not matter, so the following this creates the same graph
#' create_manual_graph( 1-----2 )
#' create_manual_graph(1-2, 1-3, 2-3, 2-4, 3-5, 4-5, 4-6, 4-7, 5-6, 6-7, 8, 
#'   graph ="network")
#' # directed graph
#' create_manual_graph(Sam-+Mary, Sam-+Tom, Mary++Tom)
#' 
#' # bipartite graph
#' g_bipartite <- create_manual_graph(# listing organizations
#'       organization1:organization2:organization3,  
#'       # listing projects
#'       project1:project2,
#'       # specifying connections between organizations and projects 
#'       organization1:organization2 - project1, 
#'       organization2:organization3 - project2)
#' # note that the graph is bipartite mathematically, but the graph object 
#' # 'does not know' that yet.
#' snafun::extract_vertex_names(g_bipartite)
#' g_bipartite <- snafun::add_vertex_attributes(g_bipartite, "type", 
#'   value = grepl(pattern = "^project", x = snafun::extract_vertex_names(g_bipartite)))
#' snafun::is_bipartite(g_bipartite)
#' 
#' 
#' # If you have many disconnected components in the graph, separate them with commas. 
#' # You can also give isolate vertices.
#' create_manual_graph( A--B, C--D, E--F, G--H, I, J, K )
#' 
#' # The ‘:’ operator can be used to define vertex sets. 
#' # If an edge operator connects two vertex sets then every vertex from the first 
#' # set will be connected to every vertex in the second set. 
#' # The following form creates a full graph, including loop edges:
#' create_manual_graph( A:B:C:D -- A:B:C:D )
#' 
#' # In directed graphs, edges will be created only if the edge operator includes 
#' # a arrow head (‘+’) at the end of the edge:
#' create_manual_graph( A -+ B -+ C )
#' create_manual_graph( A +- B -+ C )
#' create_manual_graph( A +- B -- C )
#' # Thus in the third example no edge is created between vertices B and C.
#' 
#' 
#' # Mutual edges can be also created with a simple edge operator:
#' create_manual_graph( A +-+ B +---+ C ++ D + E)
#' # Note again that the length of the edge operators is arbitrary.
#' 
#' # If the vertex names include spaces or other special characters then you 
#' # need to quote them:
#' create_manual_graph( "this is" +- "a silly" -+ "graph here" )
#' # You can include any character in the vertex names this way, 
#' # even ‘+’ and ‘-’ characters.
#' 
#' 
#' # More examples
#' # A simple undirected graph
#' g <- create_manual_graph(
#'   Alice - Bob - Cecil - Alice,
#'   Daniel - Cecil - Eugene,
#'   Cecil - Gordon
#' )
#' g
#' 
#' # Another undirected graph, ":" notation
#' g2 <- create_manual_graph(Alice - Bob:Cecil:Daniel, Cecil:Daniel - Eugene:Gordon)
#' g2
#' 
#' # A directed graph
#' g3 <- create_manual_graph(
#'   Alice +-+ Bob --+ Cecil +-- Daniel,
#'   Eugene --+ Gordon:Helen
#' )
#' g3
#' 
#' # A graph with isolate vertices
#' g4 <- create_manual_graph(Alice -- Bob -- Daniel, Cecil:Gordon, Helen)
#' g4
#' snafun::extract_vertex_names(g4)
#' 
#' # "Arrows" can be arbitrarily long
#' g5 <- create_manual_graph(Alice +---------+ Bob)
#' g5
#' 
#' # Special vertex names
#' g6 <- create_manual_graph("+" -- "-", "*" -- "/", "%%" -- "%/%")
#' g6
create_manual_graph <- function(..., simplify = TRUE, graph = c("igraph", "network")) {
  graph <- snafun.match.arg(graph)
  mf <- as.list(match.call())[-1]
  
  if ("graph" %in% names(mf)) {
    w <- which(names(mf) == "graph")
    mf <- mf[-w]
  }
  
  make_manual_graph <- utils::getFromNamespace("graph_from_literal_i", "igraph")
  g <- make_manual_graph(mf)
  if (graph == "network") {
    g <- snafun::to_network(g)
  }
  g
}





#' Create a Dyad Census-Conditioned Random Graph
#' 
#' Generate a network with pre-specified dyad census
#' 
#' Two approaches are offered, the \code{exact} method and the \code{probability} 
#' method.
#' 
#' When \code{method == "probability"}, the graph is generated based on 
#' independent draws from a multinomial distribution. This will rarely yield a 
#' graph with these exact probabilities, but large graphs will get quite close.
#' When values are passed to this function that do not add up to 1, they will be 
#' rescaled to do so (and a message is printed to the console).
#' 
#' When \code{method == "exact"}, the graph is generated with this exact MAN 
#' count, as long as they add up to the actualnumber of edges in the graph.
#' 
#' This function largely builds on the \code{\link[sna]{rguman}} function. 
#' The main difference is that we allow multiple classes of output and generate 
#' only a single conditioned graph. When a large number of MAN-conditioned 
#' graphs are to be generated, \code{\link[sna]{rguman}} is potentially more 
#' efficient.
#'
#' @param n_vertices number of vertices in the graph.
#' @param mut if \code{method == "probability"}, the probability of obtaining a 
#' mutual dyad; otherwise, the number of mutual dyads.
#' @param asym if \code{method == "probability"}, the probability of obtaining an 
#' asymmetric dyad; otherwise, the number of asymmmetric dyads.
#' @param null if \code{method == "probability"}, the probability of obtaining a 
#' null dyad; otherwise, the number of null dyads.
#' @param method either \code{"probability"} or \code{"exact"}, see details
#' @param graph output class, either \code{"igraph"}, \code{"network"}, 
#' \code{"matrix"}, or \code{"edgelist"}
#'
#' @return a graph with the desired MAN property
#' @export
#'
#' @examples
#' create_census_graph(10, mut = 45, asym = 0, null = 0, method = "exact")
#' # empty graph
#' create_census_graph(10, mut = 0, asym = 0, null = 45, method = "exact")
#' create_census_graph(10, mut = 0, asym = 0, null = 45, method = "probability")
#' create_census_graph(10, mut = 0, asym = 0, null = 1, method = "probability")
#' 
#' create_census_graph(10, mut = 0, asym = 45, null = 0, method = "exact") |> 
#'   snafun::count_dyads()
#' 
#' # will be rescaled
#' create_census_graph(10, mut = 0, asym = 45, null = 0, method = "probability") |> 
#'   snafun::count_dyads()
#' 
#' create_census_graph(10, mut = 0, asym = 1, null = 0, method = "probability") |> 
#'   snafun::count_dyads()
#' 
#' create_census_graph(10, mut = .25, asym = .30, null = .45, method = "probability") |> 
#'   snafun::count_dyads()
#' 
#' create_census_graph(10, method = "probability", graph = "igraph")
#' create_census_graph(10, method = "probability", graph = "network")
#' create_census_graph(10, method = "probability", graph = "edgelist")
#' create_census_graph(10, method = "probability", graph = "matrix")
create_census_graph <- function (n_vertices, mut = 0.25, asym = 0.5, null = 0.25, 
                                 method = c("probability", "exact"),
                                 graph = c("igraph", "network", "matrix", "edgelist")) {
  
  method = snafun.match.arg(method)
  if (!method %in% c("probability", "exact")) {
    stop("Please specify a correct 'method'")
  }
  
  graph = snafun.match.arg(graph)
  if (!graph %in% c("igraph", "network", "matrix", "edgelist")) {
    stop("Please specify a correct output 'graph' class")
  }
  
  
  g <- matrix(0, nrow = n_vertices, ncol = n_vertices)
  
  dl <- matrix(1:(n_vertices^2), n_vertices, n_vertices)
  upper_mat <- dl[upper.tri(dl)]
  lower_mat <- t(dl)[upper.tri(dl)]
  ndl <- length(upper_mat)
  
  
  if ((match.arg(method) == "exact") && (mut + asym + null != ndl)) {
    stop("Sum of dyad counts must equal number of dyads for method==exact.\n")
  } else if ((match.arg(method) == "probability") && (mut + asym + null != 1)) {
    s <- mut + asym + null
    mut <- mut/s
    asym <- asym/s
    null <- null/s
    message("Probabilities did not add to 1, they have been rescaled")
  }
  
  if (method == "probability") {
    mc <- stats::rbinom(1, ndl, mut)
    ac <- stats::rbinom(1, ndl - mc, asym/(asym + null))
    nc <- ndl - mc - ac
  } else {
    mc <- mut
    ac <- asym
    nc <- null
  }
  ds <- sample(rep(1:3, times = c(mc, ac, nc)))
  if (mc > 0) {
    g[upper_mat[ds == 1]] <- 1
    g[lower_mat[ds == 1]] <- 1
  }
  if (ac > 0) {
    g[upper_mat[ds == 2]] <- stats::rbinom(ac, 1, 0.5)
    g[lower_mat[ds == 2]] <- 1 - g[upper_mat[ds == 2]]
  }
  
  if (graph == "igraph") {
    snafun::to_igraph(g)
  } else if (graph == "network") {
    snafun::to_network(g)
  } else if (graph == "edgelist") {
    snafun::to_edgelist(g)
  } else {
    g
  }
}







#' Create a graph with a given community structure
#' 
#' Generates a random network with a probabilistic community structure
#' 
#' The researcher specifies the size of each community and the probabilities of 
#' vertices within each community. In addition, there is an overall probability 
#' of edges between vertices of different communities and a specified fraction of
#' edges is removed from the graph.
#' 
#' The internal code is largely based upon \code{\link[wfg]{network.simu}}, 
#' in the \pkg{wfg} package, which
#' implements simulation of networks under the framework by Girvan and Newman.
#' 
#' @param communitySizes a vector, the number of communities equals the number 
#' of vertices in this vector.
#' @param p_intra a vector of probability of a vertex to be randomly linked to 
#' other vertices in the same community.
#' @param p_inter the probability of a vertex to be randomly linked to vertices 
#' in other communities.
#' @param p_del the proportion of links that are randomly deleted.
#' @param graph output class, either "igraph" (default), "network", or "matrix"
#'
#' @return a graph with the given community structure
#' @export
#'
#' @examples
#' create_community_graph(graph = "matrix")
#' create_community_graph(c(10, 10, 15), p_intra = c(.25, .35, .45),
#'     p_inter = .2, p_del = .2, graph = "network")
#' create_community_graph(c(30, 30, 30), p_intra = c(.4, .2, .15),
#'     p_inter = .4, p_del = .4)
#' \dontrun{
#' g <- create_community_graph(c(5, 5, 5), p_intra = c(.75, .75, .70),
#'     p_inter = .15, p_del = .2)
#' g$graph <- snafun::add_vertex_attributes(g$graph, "color", g$group)
#' snafun::plot(g$graph)
#' }
create_community_graph <- function(communitySizes = c(10, 20, 30),
                                   p_intra = c(.3, .2, .3),
                                   p_inter = .2,
                                   p_del = 0,
                                   graph = c("igraph", "network", "matrix")) {
  
  graph <- snafun.match.arg(graph)
  if (!graph %in% c("igraph", "network", "matrix")) {
    stop("'graph' should be of class igraph, matrix, or network")
  }
  
  if (length(p_intra) != length(communitySizes)) {
    stop("Vector 'p_intra' must have the same length as the 'communitySizes' vector")
  }
  if (length(p_inter) != 1) stop("'p_inter' should have length 1")
  if (p_inter < 0 | p_inter > 1) stop("'p_inter' is a probability and should be between 0-1")
  if (length(p_del) != 1) stop("'p_del' should have length 1")
  if (p_del < 0 | p_del > 1) stop("'p_del' is a probability and should be between 0-1")
  if (any(p_intra < 0) | any(p_intra > 1)) stop("'p_intra' are probabilities and should all be between 0-1")
  
  n <- sum(communitySizes)
  n.edge <- n * (n - 1)/2
  A <- matrix(0, nrow = n, ncol = n)
  l <- length(communitySizes)
  group <- c()
  for (k in 1:l) {
    group <- c(group, rep(k, communitySizes[k]))
  }
  from.mat <- replicate(n, group)
  to.mat <- t(from.mat)
  fromv <- from.mat[upper.tri(from.mat)]
  tov <- to.mat[upper.tri(to.mat)]
  index <- list()
  adj.vector <- rep(0, n.edge)
  for (i in 1:l) {
    index[[i]] <- which(tov == i & fromv == i)
    n.come <- length(index[[i]])
    unif.vector <- stats::runif(n.come)
    com.adj.vector <- rep(0, n.come)
    com.adj.vector[unif.vector < p_intra[i]] <- 1
    adj.vector[index[[i]]] <- com.adj.vector
  }
  index.btw <- which(tov != fromv)
  n.btw <- length(index.btw)
  unif.vector <- stats::runif(n.btw)
  btw.adj.vector <- rep(0, n.btw)
  btw.adj.vector[unif.vector < p_inter] <- 1
  adj.vector[index.btw] <- btw.adj.vector
  ne <- sum(adj.vector)
  nd <- round(ne * p_del)
  index.del <- sample(1:ne, nd)
  index.edge <- which(adj.vector == 1)
  adj.vector[index.edge[index.del]] <- 0
  A[upper.tri(A)] <- adj.vector
  A <- A + t(A)
  
  if (graph == "igraph") {
    g <- snafun::to_igraph(A)
  } else if (graph == "network") {
    g <- snafun::to_network(A)
  } else {
    g <- A
  }
  
  out <- list()
  out$graph <- g
  out$group <- group
  
  return(out)
}






#' Create a graph with strict components
#' 
#' Create a graph consisting of separate components
#' 
#' Creates a graph where only vertices (and all) vertices are connected that 
#' are a member of the ame component.
#' 
#' If \code{n_vertices} has length 1, a one-mode graph is created. 
#' Each component has internal density 1 and there are no connections between 
#' the components.
#' 
#' If \code{n_vertices} has length 1, a two-mode graph is created. 
#' An edge between two vertices only occurs for between vertices that are 
#' both of dissimilar type and and both in thesame component. 
#' Regardless of the choice for \code{directed}, the ensuing graph will be 
#' undirected by definition.
#' 
#' The vertices in the final graph will be numerically named, from 1 to 
#' \code{n_vertices} for one-mode graphs or from 1 to \code{sum(n_vertices)} 
#' for two-mode graphs.
#'
#' @param n_vertices a single number for a one-mode graph, a vector with two 
#' numbers for a two-mode graph
#' @param directed logical, should the connections be directed or not (default)
#' @param membership numeric vector, with the group membership for each vertex
#' @param graph output class, either "igraph" (the default), "network", or "matrix"
#'
#' @return a graph of the specified class
#' @export
#' @note Much of the code comes from \code{\link[manynet]{create_components}} 
#' in the \pkg{manynet} package.
#'
#' @examples
#' create_components_graph(10, membership = c(1,1,1,2,2,2,3,3,3,3))
#' create_components_graph(10, membership = c(1,1,1,2,2,2,3,3,3,3), directed = TRUE) 
#' 
#' # bipartite
#' \dontrun{
#' create_components_graph(c(4, 6), membership = c(1,1,1,2,2,2,3,3,3,3))  |> 
#'   snafun::plot()
#' }
#' # the row vertices are numbered 1-4, the column vertices are 5-10
#' create_components_graph(c(4, 6), membership = c(1,1,2,3,2,2,3,3,3,1), graph = "matrix")
create_components_graph <- function(n_vertices, directed = FALSE, 
                                    membership = NULL,
                                    graph = c("igraph", "network", "matrix")) {
  
  if (length(membership) != sum(n_vertices)) {
    stop("The 'membership' vector should be of the same length as the number of vertices")
  }
  
  graph <- snafun.match.arg(graph)
  if (!graph %in% c("igraph", "network", "matrix")) {
    stop("'graph' should be of class igraph, network, or matrix")
  }
  
  if (!length(n_vertices) %in% c(1, 2)) {
    stop("'n_vertices' should have length 1 or 2")
  }
  
  bipartite <- ifelse(length(n_vertices) == 1, FALSE, TRUE)
  
  if (!is.logical(directed)) stop("'directed' should be TRUE or FALSE")
  
  if (is.null(membership)) {
    if (length(n_vertices) > 1) {
      membership <- c(sort(abs(seq_len(n_vertices[1]) %% 2 -2)), 
                      sort(abs(seq_len(n_vertices[2]) %% 2 -2)))
    } else membership <- sort(abs(seq_len(n_vertices) %% 2 -2))
  }
  
  if (length(n_vertices) == 1) {
    out <- matrix(0, n_vertices, n_vertices)
    for (x in unique(membership)) out[membership == x, membership == x] <- 1
    diag(out) <- 0
    if(directed) out[lower.tri(out)] <- 0
  } else if (length(n_vertices) == 2) {
    directed = FALSE
    out <- matrix(0, n_vertices[1], n_vertices[2])
    for (x in unique(membership)) {
      out[membership[1:n_vertices[1]] == x, membership[(n_vertices[1] + 1):length(membership)] == x] <- 1
    }
  }
  
  if (!bipartite) {
    rownames(out) <- colnames(out) <- 1:n_vertices
  } else {
    rownames(out) <- 1:n_vertices[1]
    colnames(out) <- (n_vertices[1] + 1):sum(n_vertices)
  }
  
  if (graph == "igraph") {
    snafun::to_igraph(out, bipartite = bipartite)
  } else if (graph == "network") {
    snafun::to_network(out, bipartite = bipartite)
  } else {
    out
  }
}








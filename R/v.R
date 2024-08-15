

#' Vertex level indices
#' 
#' Vertex level indices
#' 
#' Calculate several vertex level indices. 
#' 
#' @param x graph object
#' @param vids The vertices for the measure is to be calculated. By default, all 
#' vertices are included.
#' @param mode Character constant, gives whether the shortest paths to or from 
#' the given vertices should be calculated for directed graphs. 
#' If \code{out} then the shortest paths from the vertex, if \code{in} then to 
#' it will be considered. If \code{all}, the default, then the corresponding 
#' undirected graph will be used, edge directions will be ignored. 
#' This argument is ignored for undirected graphs.
#' @param directed Logical, whether the graph should be considered directed (if 
#' it is directed to begin with)
#' @param loops Logical; whether the loop edges are also counted. 
#' This rarely makes sense. Default is \code{FALSE}.
#' @param rescaled if \code{TRUE}, the scores are rescaled so they sum to 1.
#' @param damping The damping factor (‘d’ in the original paper).
#' @param k The k parameter. The default is 3.
#' @param add.vertex.names logical, should the output contain vertex names. 
#' This requires a vertex attribute \code{name} to be present in the graph. 
#' It is ignored if the attributed is missing.
#' @references
#' Michalak, T.P., Aadithya, K.V., Szczepanski, P.L., Ravindran, B. and 
#' Jennings, N.R., 2013. Efficient computation of the Shapley value for 
#' game-theoretic network centrality. Journal of Artificial Intelligence 
#' Research, 46, pp.607-650.
#' 
#' The code for the Shapley centrality is adapted from \code{CINNA::group_centrality} 
#' and gives the same result (but our version is slightly more robust).
#' @examples 
#' g <- igraph::make_star(10, mode = "undirected")
#' v_eccentricity(g)
#' v_eccentricity(g, vids = c(1,3,5))
#' g_n <- snafun::to_network(g)
#' v_eccentricity(g_n)
#' 
#' i_bus <- florentine$flobusiness
#' v_eccentricity(i_bus, vids = c(1, 5, 9))
#' v_eccentricity(i_bus, vids = c("Medici", "Peruzzi"))
#' n_bus <- to_network(i_bus)
#' v_eccentricity(n_bus, vids = c(1, 5, 9))
#' v_eccentricity(n_bus, vids = c("Medici", "Peruzzi"))
#' @name vli
#' @seealso For \code{igraph} objects: \code{\link[igraph]{eccentricity}}, 
#' \code{\link[igraph]{degree}}, \code{\link[igraph]{betweenness}}, 
#' \code{\link[igraph]{eigen_centrality}}.
#' 
#' For \code{network} objects: \code{\link[sna]{degree}}, .
#' \code{\link[sna]{betweenness}}, \code{\link[sna]{stresscent}}, 
#' \code{\link[sna]{evcent}}.
#' 
NULL





####----------------------------------------------------------------------------

#' @describeIn vli Degree of a vertex. Weights are discarded. 
#' 
#' Degree of a vertex is defined as the number of edges adjacent to it.
#' @examples
#' # 
#' # v_degree
#' g_i <- snafun::create_random_graph(10, strategy = "gnm", m = 12, 
#'                                   directed = TRUE, graph = "igraph")
#' g2_i <- snafun::add_edge_attributes(g_i, attr_name = "weight", value = 1:12)
#' v_degree(g_i)
#' v_degree(g_i, rescaled = TRUE)
#' v_degree(g_i, mode = "in")
#' v_degree(g_i, mode = "in", rescaled = TRUE)
#' v_degree(g_i, mode = "out")
#' v_degree(g_i, mode = "out", rescaled = TRUE)
#' v_degree(g2_i)   # weight is ignored
#' 
#' g_n <- snafun::create_random_graph(10, strategy = "gnm", m = 12, 
#'                                    directed = TRUE, graph = "network")
#' g2_n <- snafun::add_edge_attributes(g_n, attr_name = "weight", value = 1:12)
#' v_degree(g_n)
#' v_degree(g_n, rescaled = TRUE)
#' v_degree(g_n, mode = "in")
#' v_degree(g_n, mode = "in", rescaled = TRUE)
#' v_degree(g_n, mode = "out")
#' v_degree(g_n, mode = "out", rescaled = TRUE)
#' v_degree(g2_n)   # weight is ignored
#' @export
v_degree <- function(x, vids = NULL, 
                     mode = c("all", "out", "in"),
                     loops = FALSE,
                     rescaled = FALSE) {
  UseMethod("v_degree")
}


#' @export
v_degree.default <- function(x, vids = NULL, 
                             mode = c("all", "out", "in"),
                             loops = FALSE,
                             rescaled = FALSE) {
  txt <- methods_error_message("x", "v_degree")
  stop(txt)
}


#' @export
v_degree.igraph <- function(x, vids = NULL, 
                            mode = c("all", "out", "in"),
                            loops = FALSE,
                            rescaled = FALSE) {
  mode = snafun.match.arg(mode)
  if (is.null(vids)) {
    vids = igraph::V(x)
  }
  ret <- igraph::degree(x, v = vids, mode = mode, loops = loops, normalized = FALSE)
  rescale_sum_to_one(ret, rescaled)
}


#' @export
v_degree.network <- function(x, vids = NULL, 
                             mode = c("all", "out", "in"),
                             loops = FALSE,
                             rescaled = FALSE) {
  mode <- snafun.match.arg(mode)
  gmode <- ifelse(snafun::is_directed(x), "digraph", "graph")
  cmode <- switch(mode, 
                  "all" = "freeman",
                  "out" = "outdegree",
                  "in" = "indegree")
  ret <- sna::degree(dat = x, g = 1, nodes = vids, gmode = gmode, diag = loops, 
                     cmode = cmode, rescale = FALSE, ignore.eval = TRUE)
  rescale_sum_to_one(ret, rescaled)
}



####----------------------------------------------------------------------------

#' @describeIn vli Eccentricity of a vertex. Weights are discarded. 
#' The heavy lifting is done using \code{\link[igraph]{eccentricity}}.
#' 
#' The eccentricity of a vertex is its shortest path distance from the farthest 
#' other node in the graph. It is calculated by measuring the shortest distance 
#' from (or to) the vertex, to (or from) all vertices in the graph, and taking 
#' the maximum.
#' 
#' This implementation ignores vertex pairs that are in different components. 
#' Isolate vertices have eccentricity zero.
#' @export
v_eccentricity <- function(x, vids = NULL, mode = c("all", "out", "in"), rescaled = FALSE) {
  UseMethod("v_eccentricity")
}


#' @export
v_eccentricity.default <- function(x, vids = NULL, mode = c("all", "out", "in"),
                                   rescaled = FALSE) {
  txt <- methods_error_message("x", "v_eccentricity")
  stop(txt)
}


#' @export
v_eccentricity.igraph <- function(x, vids = NULL, mode = c("all", "out", "in"), 
                                  rescaled = FALSE) {
  mode = snafun.match.arg(mode)
  if (is.null(vids)) {
    vids = igraph::V(x)
  }
  ret <- igraph::eccentricity(x, vids = vids, mode = mode)
  rescale_sum_to_one(ret, rescaled)
}


#' @export
v_eccentricity.network <- function(x, vids = NULL, mode = c("all", "out", "in"), 
                                   rescaled = FALSE) {
  x <- to_igraph(x)
  mode <- snafun.match.arg(mode)
  if (is.null(vids)) {
    vids = igraph::V(x)
  }
  ret <- v_eccentricity.igraph(x = x, vids = vids, mode = mode)
  rescale_sum_to_one(ret, rescaled)
}







####----------------------------------------------------------------------------
#' @describeIn vli Betweenness of a vertex. Weights are discarded. The 
#' corresponding dedicated functions inside the \code{igraph} and \code{sna} 
#' have some additional functions (including diverging ways of taking weight 
#' into account). The settings of \code{v_betweenness} are what you want in 
#' most cases and works similarly for both \code{igraph} and \code{network} 
#' graph objects.
#' 
#' The betweenness of vertex \eqn{v} considers the number of shortest paths 
#' between all pairs of vertices (except paths to or from \eqn{v}. For each 
#' vertex pair, we calculate the proportion of shortest paths between vertices 
#' \eqn{i} and \eqn{j} that pass through \eqn{v}. The sum of these proportions 
#' is then equal to \eqn{v}'s betweenness. Mathematically, the betweenness of 
#' vertex \eqn{v} is given by 
#' \deqn{C_B(v) = \sum_{i,j : i \neq j, i \neq v, j \neq v} \frac{g_{ivj}}{g_{ij}}}
#'  where \eqn{g_{ijk}}{g_ijk} is the number of geodesics from \eqn{i} to \eqn{k} through \eqn{j}. 
#'  
#' In simple words, the betweenness of vertex \eqn{v} is the answer to: for each 
#' dyad that does not include \eqn{v}, add up the proportions of shortest paths 
#' that run through \eqn{v}.
#'  
#' Conceptually, high-betweenness vertices lie on a large proportion of shortest 
#' paths between other vertices; they can thus be thought of as ``bridges'' or 
#' ``boundary spanners'' and have been argued to be in an informationally-favorable 
#' position having disproportionately fast access to information and rumors, 
#' under the assumption that these will be more likely to flow through the 
#' shortest paths in the graph (or flow randomly).
#' 
#' It is important to consider whether it is assumed that flow through the network 
#' occurs along the directions of the edges (\code{directed == TRUE}) or whether 
#' edge direction does not matter (\code{directed == FALSE})--the latter is 
#' always the case when the graph is undirected itself.
#' @examples
#' # 
#' # v_betweenness
#' g_i <- snafun::create_random_graph(10, strategy = "gnm", m = 12, 
#'                                   directed = TRUE, graph = "igraph")
#' g2_i <- snafun::add_edge_attributes(g_i, attr_name = "weight", value = 1:12)
#' v_betweenness(g_i)
#' v_betweenness(g_i, rescaled = TRUE)
#' v_betweenness(g_i, vids = c(1, 2, 3, 5), rescaled = TRUE)
#' v_betweenness(g2_i)   # attribute "weight" is not used
#' 
#' g_n <- snafun::to_network(g_i)
#' v_betweenness(g_n)
#' v_betweenness(g_n, rescaled = TRUE)
#' v_betweenness(g_n, vids = c(1, 2, 3, 5), rescaled = TRUE)
#' 
#' # star network
#' g <- igraph::make_star(10, "in")
#' plot(g)
#' v_betweenness(g) # there are no shortest paths with length >= 3
#' v_betweenness(g, directed = FALSE) # all 36 shortest paths that do not include "1" go through "1"
#' @export
v_betweenness <- function(x, vids = NULL, 
                          directed = TRUE,
                          rescaled = FALSE) {
  UseMethod("v_betweenness")
}


#' @export
v_betweenness.default <- function(x, vids = NULL, 
                                  directed = TRUE,
                                  rescaled = FALSE) {
  txt <- methods_error_message("x", "v_betweenness")
  stop(txt)
}


#' @export
v_betweenness.igraph <- function(x, vids = NULL, 
                            directed = TRUE,
                            rescaled = FALSE) {
  if (is.null(vids)) {
    vids = igraph::V(x)
  }

  if (snafun::is_weighted(x)) {
      x <- snafun::remove_edge_attribute(x, attr_name = "weight")
  }

  ret <- igraph::betweenness(x, v = vids, directed = directed)
  rescale_sum_to_one(ret, rescaled)
}





#' @export
v_betweenness.network <- function(x, vids = NULL, 
                                  directed = TRUE,
                                  rescaled = FALSE) {
  gmode <- ifelse(snafun::is_directed(x), "digraph", "graph")
  cmode = ifelse(directed, "directed", "undirected")

  ret <- sna::betweenness(dat = x, g = 1, nodes = vids, gmode = gmode, diag = FALSE, 
              cmode = cmode, rescale = FALSE, ignore.eval = TRUE)
  # rescale not as part of `sna::betweenness` because that rescales the entire 
  # set of scores and then only returns the ones in vids, so this will not add 
  # to 1 if selected vids are requested
  rescale_sum_to_one(ret, rescaled)
}


####----------------------------------------------------------------------------
#' @describeIn vli Stress centrality of a vertex. Weights are discarded. 
#' 
#' The stress centrality of vertex \eqn{v} is the number of shortest paths 
#' between all pairs of vertices (except paths to or from \eqn{v} that pass 
#' through \eqn{v}. Mathematically, the stress centrality of 
#' vertex \eqn{v} is given by 
#' \deqn{C_S(v) = \sum_{i,j : i \neq j, i \neq v, j \neq v} g_{ivj}}
#'  where \eqn{g_{ivk}} is the number of geodesics from \eqn{i} to \eqn{k} 
#'  through \eqn{v}. 
#'  
#' Conceptually, high-stress vertices lie on a large number of shortest 
#' paths between other vertices; they can thus be thought of as ``bridges'' or 
#' ``boundary spanners'' and may experience high cognitive stress (in case of 
#' information networks) or physical stress (in case of physical flow networks).
#' 
#' It is important to consider whether it is assumed that flow through the network 
#' occurs along the directions of the edges (\code{directed == TRUE}) or whether 
#' edge direction does not matter (\code{directed == FALSE})--the latter is 
#' always the case when the graph is undirected itself.
#' @examples
#' # 
#' # v_stress
#' g_i <- snafun::create_random_graph(10, strategy = "gnm", m = 12, 
#'                                   directed = TRUE, graph = "igraph")
#' v_stress(g_i)
#' v_stress(g_i, rescaled = TRUE)
#' v_stress(g_i, vids = c(1, 2, 3, 5), rescaled = TRUE)
#' 
#' g_n <- snafun::to_network(g_i)
#' v_stress(g_n)
#' v_stress(g_n, rescaled = TRUE)
#' v_stress(g_n, vids = c(1, 2, 3, 5), rescaled = TRUE)
#' 
#' # star network
#' g <- igraph::make_star(10, "in")
#' plot(g)
#' v_stress(g) # there are no shortest paths with length >= 3
#' v_stress(g, directed = FALSE) # all 36 shortest paths that do not include "1" go through "1"
#' @export
v_stress <- function(x, vids = NULL, 
                          directed = TRUE,
                          rescaled = FALSE) {
  UseMethod("v_stress")
}


#' @export
v_stress.default <- function(x, vids = NULL, 
                                  directed = TRUE,
                                  rescaled = FALSE) {
  txt <- methods_error_message("x", "v_stress")
  stop(txt)
}


#' @export
v_stress.igraph <- function(x, vids = NULL, 
                                 directed = TRUE,
                                 rescaled = FALSE) {
  g <- snafun::to_network(x)
  v_stress.network(g, vids = vids, directed = directed,
                   rescaled = rescaled)
}





#' @export
v_stress.network <- function(x, vids = NULL, 
                                  directed = TRUE,
                                  rescaled = FALSE) {
  gmode <- ifelse(snafun::is_directed(x), "digraph", "graph")
  cmode = ifelse(directed, "directed", "undirected")
  
  ret <- sna::stresscent(dat = x, g = 1, nodes = vids, gmode = gmode, diag = FALSE, 
                          cmode = cmode, rescale = FALSE, ignore.eval = TRUE)
  rescale_sum_to_one(ret, rescaled)
}





####----------------------------------------------------------------------------
#' @describeIn vli Eigenvector centrality of a vertex. Weights are discarded. 
#' 
#' Eigenvector centrality scores correspond to the values of the first eigenvector 
#' of the graph adjacency matrix; these scores may, in turn, be interpreted as 
#' arising from a reciprocal process in which the centrality of each actor is 
#' proportional to the sum of the centralities of those actors to whom he or she i
#' s connected. In general, vertices with high eigenvector centralities are 
#' those which are connected to many other vertices which are, in turn, connected 
#' to many others (and so on).
#' 
#' Eigenvector centrality is generalized by the Bonacich power centrality measure; 
#' see \code{\link[igraph]{power_centrality}} and \code{\link[igraph]{bonpow}}
#' for more details on this generalization.
#' @examples
#' # 
#' # v_eigenvector
#' g_i <- snafun::create_random_graph(10, strategy = "gnm", m = 12, 
#'                                   directed = TRUE, graph = "igraph")
#' v_eigenvector(g_i)
#' v_eigenvector(g_i, rescaled = TRUE)
#' v_eigenvector(g_i, directed = FALSE)
#' 
#' g_n <- snafun::to_network(g_i)
#' v_eigenvector(g_n)
#' v_eigenvector(g_n, rescaled = TRUE)
#' v_eigenvector(g_n, directed = FALSE)
#' 
#' # star network
#' g <- igraph::make_star(10, "in")
#' \dontrun{
#' v_eigenvector(g) # all 0 + a warning is issued
#' }
#' v_eigenvector(g, directed = FALSE) 
#' @export
v_eigenvector <- function(x, 
                          directed = TRUE,
                          rescaled = FALSE) {
  UseMethod("v_eigenvector")
}


#' @export
v_eigenvector.default <- function(x, 
                                  directed = TRUE,
                                  rescaled = FALSE) {
  txt <- methods_error_message("x", "v_eigenvector")
  stop(txt)
}



#' @export
v_eigenvector.igraph <- function(x, 
                                 directed = TRUE,
                                 rescaled = FALSE) {
  ret <- igraph::eigen_centrality(x,
                                  directed = directed,
                                  scale = FALSE)$vector
  rescale_sum_to_one(ret, rescaled)
}





#' @export
v_eigenvector.network <- function(x,
                                  directed = TRUE,
                                  rescaled = FALSE) {
  g <- snafun::to_igraph(x)
  
  v_eigenvector.igraph(x = g, directed = directed, rescaled = rescaled)
}







####----------------------------------------------------------------------------
#' @describeIn vli Closeness of a vertex. Weights are discarded. 
#' 
#' The closeness centrality of a vertex \eqn{v} is calculated as the inverse of 
#' the sum of distances to all the other vertices in the graph. In other words, 
#' for a vertex \eqn{v}, determine the lengths of the shortest paths from \code{v} 
#' to all other vertices (in caae of "out"), add those. This measures the 
#' "farness" of \eqn{v}. The closeness of \eqn{v} is the inverse of this sum.
#' The higher the number, the shorter the number of steps to reach all other 
#' vertices.
#' 
#' Closeness centrality is meaningful only for connected graphs. In disconnected 
#' graphs, consider using the harmonic centrality with v_harmonic.
#' 
#' This function's work is performed by \code{\link[igraph]{closeness}}. An 
#' alternative implementation is \code{\link[sna]{closeness}}. The latter yields 
#' values that are g-1 times larger (where g is the number of vertices) and takes 
#' some alternative decisions in special cases (e.g., for unconnected vertices).
#' @examples
#' # 
#' # v_closeness
#' g_i <- snafun::create_random_graph(10, strategy = "gnm", m = 12, 
#'                                   directed = TRUE, graph = "igraph")
#' g2_i <- snafun::add_edge_attributes(g_i, attr_name = "weight", value = 1:12)
#' v_closeness(g_i)
#' 1/rowSums(igraph::distances(g_i))  # same thing
#' v_closeness(g_i, rescaled = TRUE)
#' v_closeness(g_i, vids = c(1, 2, 3, 5), rescaled = TRUE)
#' v_closeness(g2_i)   # attribute "weight" is not used
#' 
#' g_n <- snafun::to_network(g_i)
#' v_closeness(g_n)
#' v_closeness(g_n, rescaled = TRUE)
#' v_closeness(g_n, vids = c(1, 2, 3, 5), rescaled = TRUE)
#' 
#' # star network
#' g <- igraph::make_star(10, "in")
#' v_closeness(g)  # "1" has the highest closeness, the rest has the same value
#' v_closeness(g, mode= "in")  # only "1"
#' v_closeness(g, mode= "out") # all except "1"
#' @export
v_closeness <- function(x, vids = NULL, 
                        mode = c("all", "out", "in"),
                        rescaled = FALSE) {
  UseMethod("v_closeness")
}


#' @export
v_closeness.default <- function(x, vids = NULL, 
                                mode = c("all", "out", "in"),
                                rescaled = FALSE) {
  txt <- methods_error_message("x", "v_closeness")
  stop(txt)
}


#' @export
v_closeness.igraph <- function(x, vids = NULL, 
                               mode = c("all", "out", "in"),
                               rescaled = FALSE) {
  mode = snafun.match.arg(mode)
  if (is.null(vids)) {
    vids = igraph::V(x)
  }
  
  if (snafun::is_weighted(x)) {
    x <- snafun::remove_edge_attribute(x, attr_name = "weight")
  }
  
  ret <- igraph::closeness(x, vids = vids, mode = mode,
                           normalized = FALSE, cutoff = -1)
  rescale_sum_to_one(ret, rescaled)
}





#' @export
v_closeness.network <- function(x, vids = NULL, 
                                mode = c("all", "out", "in"),
                                rescaled = FALSE) {
  g <- snafun::to_igraph(x)
  mode = snafun.match.arg(mode)
  if (is.null(vids)) {
    vids = igraph::V(g)
  }
  
  v_closeness.igraph(x = g, vids = vids, mode = mode, rescaled = rescaled)
}




####----------------------------------------------------------------------------
#' @describeIn vli Harmonic centrality of a vertex. Weights are discarded. 
#' 
#' The harmonic centrality of a vertex is the mean inverse distance to all other 
#' vertices. The inverse distance to an unreachable vertex is considered to be zero.
#' 
#' The measure is closely related to \code{closeness}. While closeness 
#' centrality is meaningful only for connected graphs, harmonic centrality can 
#' be calculated also for disconnected graphs and provides a useful alternative 
#' in those cases.
#' @examples
#' # 
#' # v_harmonic
#' g_i <- snafun::create_random_graph(10, strategy = "gnm", m = 12, 
#'                                   directed = TRUE, graph = "igraph")
#' v_harmonic(g_i)
#' v_closeness(g_i)   # harmonic works for disconnected graphs, closeness does not
#' cor(v_harmonic(g_i), v_closeness(g_i), use = "complete.obs") # usually very high
#' v_harmonic(g_i, rescaled = TRUE)
#' v_harmonic(g_i, vids = c(1, 2, 3, 5), rescaled = TRUE)
#' 
#' g_n <- snafun::to_network(g_i)
#' v_harmonic(g_n)
#' v_harmonic(g_n, rescaled = TRUE)
#' v_harmonic(g_n, vids = c(1, 2, 3, 5), rescaled = TRUE)
#' 
#' # star network
#' g <- igraph::make_star(10, "in")
#' v_harmonic(g)  # "1" has the highest harmonic, the rest has the same value
#' v_harmonic(g, mode= "in")  # only "1"
#' v_harmonic(g, mode= "out") # all except "1"
#' @export
v_harmonic <- function(x, vids = NULL, 
                        mode = c("all", "out", "in"),
                        rescaled = FALSE) {
  UseMethod("v_harmonic")
}


#' @export
v_harmonic.default <- function(x, vids = NULL, 
                                mode = c("all", "out", "in"),
                                rescaled = FALSE) {
  txt <- methods_error_message("x", "v_harmonic")
  stop(txt)
}



#' @export
v_harmonic.igraph <- function(x, vids = NULL, 
                               mode = c("all", "out", "in"),
                               rescaled = FALSE) {
  mode = snafun.match.arg(mode)
  if (is.null(vids)) {
    vids = igraph::V(x)
  }
  
  if (snafun::is_weighted(x)) {
    x <- snafun::remove_edge_attribute(x, attr_name = "weight")
  }
  
  ret <- igraph::harmonic_centrality(x, vids = vids, mode = mode,
                           normalized = FALSE, cutoff = -1)
  rescale_sum_to_one(ret, rescaled)
}





#' @export
v_harmonic.network <- function(x, vids = NULL, 
                                mode = c("all", "out", "in"),
                                rescaled = FALSE) {
  g <- snafun::to_igraph(x)
  mode = snafun.match.arg(mode)
  if (is.null(vids)) {
    vids = igraph::V(g)
  }
  
  v_harmonic.igraph(x = g, vids = vids, mode = mode, rescaled = rescaled)
}






####----------------------------------------------------------------------------
#' @describeIn vli Google Pagerank centrality of a vertex. Weights are discarded. 
#' 
#' For the explanation of the PageRank algorithm, see the following webpage: 
#' \href{http://infolab.stanford.edu/~backrub/google.html}{The Anatomy of a Large-Scale Hypertextual Web Search Engine}, 
#' or the following reference: 
#' 
#' Sergey Brin and Larry Page: The Anatomy of a Large-Scale Hypertextual Web 
#' Search Engine. Proceedings of the 7th World-Wide Web Conference, 
#' Brisbane, Australia, April 1998.
#' 
#' The PageRank of a given vertex depends on the PageRank of all other vertices, 
#' so even if you want to calculate the PageRank for only some of the vertices, 
#' all of them must be calculated first. Requesting the PageRank for only some 
#' of the vertices therefore does not result in any performance increase.
#' 
#' For all vertices together, page rank always adds up to 1, so \code{rescaled} 
#' does not have an effect. The \code{rescaled} argument is potentially useful 
#' if \code{vids} is specified to consider only the values of a selected set 
#' of vertices (whose pagerank then usually does not add up to 1).
#' @examples
#' # 
#' # v_pagerank
#' g_i <- snafun::create_random_graph(10, strategy = "gnm", m = 12, 
#'                                   directed = TRUE, graph = "igraph")
#' v_pagerank(g_i)
#' v_pagerank(g_i, rescaled = TRUE)
#' v_pagerank(g_i, vids = c(1, 2, 3, 5), rescaled = TRUE)
#' v_pagerank(g_i, damping = 0)
#' v_pagerank(g_i, damping = .99)  # using 1 exactly may not be entirely stable
#' 
#' 
#' 
#' g_n <- snafun::to_network(g_i)
#' v_pagerank(g_n)
#' v_pagerank(g_n, rescaled = TRUE)
#' v_pagerank(g_n, vids = c(1, 2, 3, 5), rescaled = TRUE)
#' 
#' # star network
#' g <- igraph::make_star(10, "in")
#' v_pagerank(g)  # "1" has the highest pagerank, the rest has the same value
#' @export
v_pagerank <- function(x, vids = NULL, 
                       damping = 0.85,
                       directed = TRUE,
                       rescaled = FALSE) {
  UseMethod("v_pagerank")
}


#' @export
v_pagerank.default <- function(x, vids = NULL, 
                               damping = 0.85,
                               directed = TRUE,
                               rescaled = FALSE) {
  txt <- methods_error_message("x", "v_pagerank")
  stop(txt)
}



#' @export
v_pagerank.igraph <- function(x, vids = NULL, 
                              damping = 0.85,
                              directed = TRUE,
                              rescaled = FALSE) {
  if (!is.numeric(damping) | damping < 0 | damping > 1) {
    stop("Please provide a numeric value for 'damping' between 0 and 1")
  }
  
  if (is.null(vids)) {
    vids = igraph::V(x)
  }
  
  if (snafun::is_weighted(x)) {
    x <- snafun::remove_edge_attribute(x, attr_name = "weight")
  }
  
  ret <- igraph::page_rank(x, vids = vids,
                                     damping = damping,
                                     directed = directed)
  if (ret$value != 1) {
    warning("There was a numeric error, please try again with different settings")
  }
  rescale_sum_to_one(ret$vector, rescaled)
}




#' @export
v_pagerank.network <- function(x, vids = NULL, 
                               damping = 0.85,
                               directed = TRUE,
                               rescaled = FALSE) {
  g <- snafun::to_igraph(x)
  if (is.null(vids)) {
    vids = igraph::V(g)
  }
  
  v_pagerank.igraph(x = g, vids = vids, damping = damping,
                    directed = directed, rescaled = rescaled)
}





####----------------------------------------------------------------------------
#' @describeIn vli Geodesic k-path centrality. Weights are discarded. 
#' 
#' Geodesic K-path centrality for vertex \eqn{v} counts the number of vertices 
#' that can be reached by vertex \eqn{v} through a geodesic path of length less than "k". 
#' 
#' If weights are potentially required, use our alternative implementation 
#' at \code{\link{v_geokpath_w}}.
#' 
#' When \code{vids} is specified, the measure is calculated on the induced 
#' subgraph consisting of only these vertices (and their corresponding) edges. 
#' 
#' @examples
#' # 
#' # v_geokpath
#' g_i <- snafun::create_random_graph(10, strategy = "gnm", m = 12, 
#'                                   directed = TRUE, graph = "igraph")
#' g2_i <- snafun::add_edge_attributes(g_i, attr_name = "weight", value = 1:12)
#' v_geokpath(g_i)
#' v_geokpath(g_i, rescaled = TRUE)
#' v_geokpath(g_i, vids = c(1, 2, 3, 5), rescaled = TRUE)
#' v_geokpath(g_i, k = 2)
#' v_geokpath(g_i, k = 1)
#' v_degree(g_i) # same
#' 
#' g_n <- snafun::to_network(g_i)
#' v_geokpath(g_n)
#' v_geokpath(g_n, rescaled = TRUE)
#' v_geokpath(g_n, vids = c(1, 2, 3, 5), rescaled = TRUE)
#' @export
v_geokpath <- function(x, vids = NULL,
                       mode = c("all", "out", "in"),
                       k = 3,
                       rescaled = FALSE) {
  UseMethod("v_geokpath")
}


#' @export
v_geokpath.default <- function(x, vids = NULL,
                               mode = c("all", "out", "in"),
                               k = 3,
                               rescaled = FALSE) {
  txt <- methods_error_message("x", "v_geokpath")
  stop(txt)
}


#' @export
v_geokpath.network <- function(x, vids = NULL,
                               mode = c("all", "out", "in"),
                               k = 3,
                               rescaled = FALSE) {
  mode = snafun.match.arg(mode)
  g <- to_igraph(x)
  if (is.null(vids)) {
    vids = igraph::V(g)
  }
  v_geokpath.igraph(x = g, vids = vids,
                    mode = mode,
                    k = k,
                    rescaled = rescaled)
}



#' @export
v_geokpath.igraph <- function(x, vids = NULL,
                              mode = c("all", "out", "in"),
                              k = 3,
                              rescaled = FALSE){
  mode = snafun.match.arg(mode)
  if (is.null(vids)) {
    vids = igraph::V(x)
  }
  
  if (snafun::is_weighted(x)) {
    x <- snafun::remove_edge_attribute(x, attr_name = "weight")
  }
  
  induced <- FALSE
  if (!is.numeric(vids)) stop("'vids' must be a numeric vector")
  if (length(unique(vids)) < length(igraph::V(x))) {
    x <- igraph::induced_subgraph(x, vids = vids)
    # igraph hernummert nu de vertices, daarvoor moeten we dus corrigeren
    vids_orig <- sort(vids) # voor de zekerheid, omdat igraph vids in numerieke volgorde toepast
    vids <- igraph::V(x)
    induced <- TRUE
  } else if (any(!vids %in% igraph::V(x))) {
    stop("You asked for vertices that are not present in the graph")
  }
  
  # check vertex names
  v <- vids
  if (is.character(v) && "name" %in% igraph::vertex_attr_names(x)) {
    v <- as.numeric(match(v, igraph::V(x)$name))
    if (any(is.na(v))) {
      stop("Invalid vertex names: there are NA's in the names")
    }
    vids <- v
  } else {
    if (is.logical(v)) {
      res <- as.vector(igraph::V(x))[v]
    }
    else if (is.numeric(v) && any(v < 0)) {
      res <- as.vector(igraph::V(x))[v]
    }
    else {
      res <- as.numeric(v)
    }
    if (any(is.na(res))) {
      stop("Invalid vertex name(s): there are NA's in the names")
    }
    vids = res
  }
  k <- as.integer(k)
  if (k <= 0) stop("The k parameter must be greater than 0.", call. = FALSE)
  res <- integer()
  sp <- igraph::shortest.paths(x, v = igraph::V(x), mode = mode)
  for (v in igraph::V(x)[vids]) {
    res <- append(res, length(sp[v, sp[v,] <= k]) - 1);
  }
  if (igraph::is_named(x)) {
    names(res) <- igraph::V(x)$name[vids]
  }  
  rescale_sum_to_one(res, rescaled)
}



####----------------------------------------------------------------------------
#' @describeIn vli Shapley Centrality
#' 
#' This function computes the centrality of vertices in a graph
#' based on their Shapley value, following the approach from the 
#' Michalak et al. (2013) paper.
#' 
#' @examples
#' #
#' # Shapley centrality
#' # Figure 1 network from Michalak et al.
#' g1 <- igraph::graph(c(4,1,5,1,1,6,1,7,1,8,8,11,11,12,11,13,6,2,7,2,8,2,
#' 2,9,2,10,9,3,10,3), directed = FALSE)
#' igraph::V(g1)$name <- LETTERS[1:13]
#' v_shapley(g1)
#' v_shapley(g1, add.vertex.names = TRUE)
#' @export
v_shapley <- function(x,
                      add.vertex.names = FALSE,
                      vids = NULL, 
                      rescaled = FALSE) {
  UseMethod("v_shapley")
}


#' @export
v_shapley.default <- function(x,
                              add.vertex.names = FALSE,
                              vids = NULL, 
                              rescaled = FALSE) {
  txt <- methods_error_message("x", "v_shapley")
  stop(txt)
}


#' @export
v_shapley.igraph <- function(x,
                             add.vertex.names = FALSE,
                             vids = NULL, 
                             rescaled = FALSE) {
  if (is.null(vids)) {
    vids = igraph::V(x)
  }
  
  f <- function(v) {
    1/(1 + igraph::degree(graph = x, v = v)) + 
      sum(1/(1 + igraph::degree(x, igraph::neighbors(x, v, mode = 'all'))))
  }
  
  results <- sapply(igraph::V(x), f)
  results <- results[vids]
  
  if (add.vertex.names && igraph::is_named(x)) {
    names(results) <- igraph::V(x)$name[vids]
  } else 
    results <- as.vector(results)
  
  rescale_sum_to_one(results, rescaled)
}



#' @export
v_shapley.network <- function(x,
                              add.vertex.names = FALSE,
                              vids = NULL, 
                              rescaled = FALSE) {
  g <- to_igraph(x)
  v_shapley.igraph(x = g, add.vertex.names = add.vertex.names,
                   vids = vids, rescaled = rescaled)
}










####----------------------------------------------------------------------------
# helpers
rescale_sum_to_one <- function(x, rescaled) {
  if (rescaled) {
    if (any(is.nan(x))) {  # rescale, discarding NaN's
      return(x/sum(x, na.rm = TRUE))
    } else {
      return(x/sum(x))
    }
  } else {   # not rescaled
    return(x)
  }
}

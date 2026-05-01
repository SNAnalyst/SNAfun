

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
#' @param T Integer indicating the maximum number of diffusion steps. In the
#' first step the adjacency or probability matrix itself is used, in the second
#' step its square, and so on. If \code{NULL}, \code{T} defaults to the network
#' size, matching \code{keyplayer::diffusion()}.
#' @param M Maximum geodistance between two nodes, above which they are treated
#' as disconnected for fragmentation centrality. The default is \code{Inf}.
#' @param binary Logical scalar. If \code{TRUE}, edge values are ignored and the
#' graph is treated as binary for fragmentation centrality.
#' @param large Logical scalar. If \code{TRUE}, the baseline geodistance matrix
#' for fragmentation centrality is computed with \code{igraph}; otherwise the
#' \code{sna} implementation is used. This mirrors the interface of
#' \code{keyplayer::fragment()}.
#' @param geodist.precomp Optional precomputed geodistance matrix for the
#' original graph, used by \code{v_fragment()}. This can save work if the same
#' graph is scored repeatedly.
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
#' g <- snafun::create_manual_graph(1 -- 2:3:4:5:6:7:8:9:10)
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
#' g <- snafun::create_manual_graph(2:3:4:5:6:7:8:9:10 -+ 1)
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
#' g <- snafun::create_manual_graph(2:3:4:5:6:7:8:9:10 -+ 1)
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
#' g <- snafun::create_manual_graph(2:3:4:5:6:7:8:9:10 -+ 1)
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
#' 1 / rowSums(snafun::d_distance(g_i))  # same thing
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
#' g <- snafun::create_manual_graph(2:3:4:5:6:7:8:9:10 -+ 1)
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
#' g <- snafun::create_manual_graph(2:3:4:5:6:7:8:9:10 -+ 1)
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
#' @describeIn vli Local transitivity of a vertex.
#'
#' The local transitivity, also called the local clustering coefficient,
#' captures how densely the neighbors of a vertex are tied to one another.
#'
#' This implementation follows the standard undirected local clustering
#' coefficient. If the input graph is directed, it is first weakly symmetrized:
#' any edge in either direction is treated as an undirected tie. Edge weights
#' are discarded, because the aim here is a simple student-oriented local
#' clustering measure.
#'
#' Vertices with fewer than two neighbors have undefined local transitivity.
#' The \code{isolates} argument controls whether these undefined values are
#' returned as \code{NaN} or coerced to zero.
#'
#' Local transitivity is defined for one-mode graphs only. Bipartite inputs are
#' rejected.
#'
#' @param isolates character scalar. Either \code{"nan"} to keep undefined
#' values as \code{NaN}, or \code{"zero"} to replace them by zero.
#' @examples
#' #
#' # v_transitivity
#' g <- snafun::create_manual_graph(1 -- 2 -- 3 -- 1, 3 -- 4)
#' v_transitivity(g)
#' v_transitivity(g, isolates = "zero")
#' @export
v_transitivity <- function(x, vids = NULL,
                           isolates = c("nan", "zero")) {
  UseMethod("v_transitivity")
}


#' @export
v_transitivity.default <- function(x, vids = NULL,
                                   isolates = c("nan", "zero")) {
  txt <- methods_error_message("x", "v_transitivity")
  stop(txt)
}


#' @export
v_transitivity.igraph <- function(x, vids = NULL,
                                  isolates = c("nan", "zero")) {
  isolates <- snafun.match.arg(isolates)
  graph <- v_transitivity_prepare_graph(x)
  if (is.null(vids)) {
    vids <- igraph::V(graph)
  }
  igraph::transitivity(
    graph = graph,
    type = "localundirected",
    vids = vids,
    isolates = if (identical(isolates, "zero")) "zero" else "NaN"
  )
}


#' @export
v_transitivity.network <- function(x, vids = NULL,
                                   isolates = c("nan", "zero")) {
  isolates <- snafun.match.arg(isolates)
  graph <- v_transitivity_prepare_graph(x)
  v_transitivity.igraph(graph, vids = vids, isolates = isolates)
}


#' @export
v_transitivity.matrix <- function(x, vids = NULL,
                                  isolates = c("nan", "zero")) {
  isolates <- snafun.match.arg(isolates)
  graph <- v_transitivity_prepare_graph(x)
  v_transitivity.igraph(graph, vids = vids, isolates = isolates)
}


#' @export
v_transitivity.data.frame <- function(x, vids = NULL,
                                      isolates = c("nan", "zero")) {
  isolates <- snafun.match.arg(isolates)
  graph <- v_transitivity_prepare_graph(x)
  v_transitivity.igraph(graph, vids = vids, isolates = isolates)
}


v_transitivity_prepare_graph <- function(x) {
  if (inherits(x, "igraph")) {
    graph <- x
  } else if (is.matrix(x)) {
    graph <- snafun::to_igraph(x, bipartite = nrow(x) != ncol(x))
  } else {
    graph <- snafun::to_igraph(x)
  }
  
  if (snafun::is_bipartite(graph)) {
    stop(
      "Local transitivity is only defined for one-mode networks.",
      call. = FALSE
    )
  }
  
  adjacency <- snafun::to_matrix(graph)
  adjacency[adjacency != 0] <- 1
  diag(adjacency) <- 0
  
  if (!isSymmetric(adjacency)) {
    adjacency <- snafun::to_symmetric_matrix(adjacency, rule = "weak")
  }
  
  snafun::to_igraph(adjacency)
}



####----------------------------------------------------------------------------
#' @describeIn vli Diffusion centrality of a vertex.
#'
#' Diffusion centrality measures the expected number of information receivers
#' that can be reached from a vertex within \code{T} diffusion steps. The
#' implementation follows the definition used by
#' \code{keyplayer::diffusion()}: it sums the matrix powers
#' \eqn{A + A^2 + \dots + A^T} and then sums each row of that total matrix.
#'
#' The input matrix is interpreted directly as an adjacency or probability
#' matrix. For weighted graphs, the edge weights are therefore treated as the
#' probabilities or strengths with which information is passed along an edge.
#' No additional transformation is applied inside this function.
#'
#' Compared with \code{keyplayer::diffusion()}, this \code{snafun} version adds
#' S3 methods for \code{igraph}, \code{network}, adjacency matrices, and edge
#' lists, so the same measure can be requested through one consistent API.
#'
#' Diffusion centrality is defined for one-mode graphs only. Bipartite inputs,
#' and other inputs that convert to a non-square matrix, are rejected.
#' @references
#' An, W. and Liu, Y.-H. (2016). keyplayer: An R Package for Locating Key
#' Players in Social Networks. \emph{The R Journal}, 8(1), 257-268.
#'
#' Banerjee, A., Chandrasekhar, A., Duflo, E. and Jackson, M. (2013). Diffusion
#' of Microfinance. \emph{Science}, 341(6144).
#' @examples
#' #
#' # v_diffusion
#' P <- matrix(
#'   c(0, 0.2, 0.6, 0,   0,
#'     0, 0,   0,   0.8, 0,
#'     0.2, 0.2, 0, 0.4, 0,
#'     0, 0,   0,   0,   0.6,
#'     0, 0.4, 0,   0,   0),
#'   nrow = 5,
#'   byrow = TRUE
#' )
#' v_diffusion(P, T = 2)
#' v_diffusion(P, vids = c(1, 3), T = 2)
#'
#' g_i <- snafun::to_igraph(P)
#' v_diffusion(g_i, T = 2)
#'
#' g_n <- snafun::to_network(P)
#' v_diffusion(g_n, T = 2)
#' @export
v_diffusion <- function(x, vids = NULL, T = NULL, rescaled = FALSE) {
  UseMethod("v_diffusion")
}


#' @export
v_diffusion.default <- function(x, vids = NULL, T = NULL, rescaled = FALSE) {
  txt <- methods_error_message("x", "v_diffusion")
  stop(txt)
}


#' @export
v_diffusion.matrix <- function(x, vids = NULL, T = NULL, rescaled = FALSE) {
  if (nrow(x) != ncol(x)) {
    stop(
      "Diffusion centrality requires a square adjacency or probability matrix.",
      call. = FALSE
    )
  }
  if (!(is.numeric(x) || is.logical(x))) {
    stop(
      "'x' should be a numeric adjacency or probability matrix.",
      call. = FALSE
    )
  }
  if (any(is.na(x))) {
    stop(
      "'x' should not contain missing values.",
      call. = FALSE
    )
  }
  if (is.null(T)) {
    T <- ncol(x)
  }
  if (!is.numeric(T) || length(T) != 1 || is.na(T) || T < 1 || T != as.integer(T)) {
    stop(
      "Please provide a positive integer for 'T'.",
      call. = FALSE
    )
  }
  T <- as.integer(T)
  
  x_numeric <- as.matrix(x)
  storage.mode(x_numeric) <- "double"
  
  # We intentionally build the matrix powers iteratively instead of using an
  # extra dependency. This mirrors the logic of keyplayer::diffusion() while
  # keeping the implementation fully self-contained inside snafun.
  power_matrix <- x_numeric
  sum_of_powers <- x_numeric * 0
  for (step in seq_len(T)) {
    sum_of_powers <- sum_of_powers + power_matrix
    if (step < T) {
      power_matrix <- power_matrix %*% x_numeric
    }
  }
  
  ret <- rowSums(sum_of_powers)
  if (!is.null(vids)) {
    if (is.character(vids)) {
      if (is.null(names(ret))) {
        stop(
          "Character 'vids' can only be used when vertex names are available.",
          call. = FALSE
        )
      }
      if (any(!vids %in% names(ret))) {
        stop("You asked for vertices that are not present in the graph")
      }
    }
    ret <- ret[vids]
    if (any(is.na(ret))) {
      stop("You asked for vertices that are not present in the graph")
    }
  }
  
  rescale_sum_to_one(ret, rescaled)
}


#' @export
v_diffusion.igraph <- function(x, vids = NULL, T = NULL, rescaled = FALSE) {
  if (snafun::is_bipartite(x)) {
    stop(
      "Diffusion centrality is only defined for one-mode networks.",
      call. = FALSE
    )
  }
  mat <- snafun::to_matrix(x)
  v_diffusion.matrix(x = mat, vids = vids, T = T, rescaled = rescaled)
}


#' @export
v_diffusion.network <- function(x, vids = NULL, T = NULL, rescaled = FALSE) {
  if (snafun::is_bipartite(x)) {
    stop(
      "Diffusion centrality is only defined for one-mode networks.",
      call. = FALSE
    )
  }
  mat <- snafun::to_matrix(x)
  v_diffusion.matrix(x = mat, vids = vids, T = T, rescaled = rescaled)
}


#' @export
v_diffusion.data.frame <- function(x, vids = NULL, T = NULL, rescaled = FALSE) {
  # An edge list with fully non-overlapping sender and receiver sets is a
  # bipartite incidence description, not a one-mode adjacency structure.
  if (length(intersect(as.character(x[, 1]), as.character(x[, 2]))) == 0) {
    stop(
      "Diffusion centrality is only defined for one-mode networks.",
      call. = FALSE
    )
  }
  mat <- snafun::to_matrix(x)
  v_diffusion.matrix(x = mat, vids = vids, T = T, rescaled = rescaled)
}



####----------------------------------------------------------------------------
#' @describeIn vli Fragmentation centrality of a vertex.
#'
#' Fragmentation centrality measures how fragmented the residual network becomes
#' after removing a vertex or set of vertices. The more fragmented the residual
#' network is, the more central the removed vertex is under this criterion.
#'
#' This implementation mirrors the logic of \code{keyplayer::fragment()} while
#' fitting into the \code{snafun} S3 interface. In particular, the baseline
#' normalization factor is computed from the original graph, and residual
#' networks are evaluated from their geodistances after node removal.
#'
#' Edge values are interpreted as distances when \code{binary = FALSE}. If your
#' data encode tie strengths or probabilities instead, transform them to
#' distances before calling \code{v_fragment()}.
#'
#' Fragmentation centrality is defined for one-mode graphs only. Bipartite
#' inputs, and other inputs that convert to a non-square matrix, are rejected.
#'
#' Although the score is aggregated at the network level, the geodistances are
#' computed on the directed graph whenever the input is directed, matching the
#' behavior of \code{keyplayer::fragment()}.
#' @references
#' An, W. and Liu, Y.-H. (2016). keyplayer: An R Package for Locating Key
#' Players in Social Networks. \emph{The R Journal}, 8(1), 257-268.
#'
#' Borgatti, S. P. (2006). Identifying Sets of Key Players in a Network.
#' \emph{Computational and Mathematical Organization Theory}, 12(1), 21-34.
#' @examples
#' #
#' # v_fragment
#' W <- matrix(
#'   c(0, 1, 3, 0, 0,
#'     0, 0, 0, 4, 0,
#'     1, 1, 0, 2, 0,
#'     0, 0, 0, 0, 3,
#'     0, 2, 0, 0, 0),
#'   nrow = 5,
#'   byrow = TRUE
#' )
#' A <- W
#' A[W != 0] <- 1 / W[W != 0]
#'
#' v_fragment(A)
#' v_fragment(A, vids = c(1, 3))
#' v_fragment(A, M = 1)
#' v_fragment(A, binary = TRUE)
#'
#' g_i <- snafun::to_igraph(A)
#' v_fragment(g_i)
#'
#' g_n <- snafun::to_network(A)
#' v_fragment(g_n)
#' @export
v_fragment <- function(x, vids = NULL, M = Inf, binary = FALSE,
                       large = TRUE, geodist.precomp = NULL,
                       rescaled = FALSE) {
  UseMethod("v_fragment")
}


#' @export
v_fragment.default <- function(x, vids = NULL, M = Inf, binary = FALSE,
                               large = TRUE, geodist.precomp = NULL,
                               rescaled = FALSE) {
  txt <- methods_error_message("x", "v_fragment")
  stop(txt)
}


#' @export
v_fragment.matrix <- function(x, vids = NULL, M = Inf, binary = FALSE,
                              large = TRUE, geodist.precomp = NULL,
                              rescaled = FALSE) {
  if (nrow(x) != ncol(x)) {
    stop(
      "Fragmentation centrality requires a square adjacency or distance matrix.",
      call. = FALSE
    )
  }
  if (!(is.numeric(x) || is.logical(x))) {
    stop(
      "'x' should be a numeric adjacency or distance matrix.",
      call. = FALSE
    )
  }
  if (any(is.na(x))) {
    stop(
      "'x' should not contain missing values.",
      call. = FALSE
    )
  }
  if (!is.numeric(M) || length(M) != 1 || is.na(M) || M <= 0) {
    stop(
      "Please provide a positive numeric value for 'M'.",
      call. = FALSE
    )
  }
  if (!is.logical(binary) || length(binary) != 1 || is.na(binary)) {
    stop(
      "Please provide a single logical value for 'binary'.",
      call. = FALSE
    )
  }
  if (!is.logical(large) || length(large) != 1 || is.na(large)) {
    stop(
      "Please provide a single logical value for 'large'.",
      call. = FALSE
    )
  }
  
  x_numeric <- as.matrix(x)
  storage.mode(x_numeric) <- "double"
  vertex_ids <- v_fragment_resolve_vids(vids = vids, x = x_numeric)
  
  original_distances <- v_fragment_get_original_distances(
    x = x_numeric,
    binary = binary,
    large = large,
    geodist.precomp = geodist.precomp
  )
  normalization_factor <- v_fragment_get_normalization_factor(original_distances)
  
  if (is.null(vertex_ids)) {
    scores <- vapply(
      seq_len(ncol(x_numeric)),
      function(k) {
        residual_matrix <- x_numeric[-k, -k, drop = FALSE]
        v_fragment_score_removed_nodes(
          residual_matrix = residual_matrix,
          M = M,
          binary = binary,
          normalization_factor = normalization_factor
        )
      },
      numeric(1)
    )
    if (!is.null(rownames(x_numeric))) {
      names(scores) <- rownames(x_numeric)
    }
  } else {
    residual_matrix <- x_numeric[-vertex_ids, -vertex_ids, drop = FALSE]
    scores <- v_fragment_score_removed_nodes(
      residual_matrix = residual_matrix,
      M = M,
      binary = binary,
      normalization_factor = normalization_factor
    )
  }
  
  rescale_sum_to_one(scores, rescaled)
}


#' @export
v_fragment.igraph <- function(x, vids = NULL, M = Inf, binary = FALSE,
                              large = TRUE, geodist.precomp = NULL,
                              rescaled = FALSE) {
  if (snafun::is_bipartite(x)) {
    stop(
      "Fragmentation centrality is only defined for one-mode networks.",
      call. = FALSE
    )
  }
  mat <- snafun::to_matrix(x)
  v_fragment.matrix(
    x = mat,
    vids = vids,
    M = M,
    binary = binary,
    large = large,
    geodist.precomp = geodist.precomp,
    rescaled = rescaled
  )
}


#' @export
v_fragment.network <- function(x, vids = NULL, M = Inf, binary = FALSE,
                               large = TRUE, geodist.precomp = NULL,
                               rescaled = FALSE) {
  if (snafun::is_bipartite(x)) {
    stop(
      "Fragmentation centrality is only defined for one-mode networks.",
      call. = FALSE
    )
  }
  mat <- snafun::to_matrix(x)
  v_fragment.matrix(
    x = mat,
    vids = vids,
    M = M,
    binary = binary,
    large = large,
    geodist.precomp = geodist.precomp,
    rescaled = rescaled
  )
}


#' @export
v_fragment.data.frame <- function(x, vids = NULL, M = Inf, binary = FALSE,
                                  large = TRUE, geodist.precomp = NULL,
                                  rescaled = FALSE) {
  if (length(intersect(as.character(x[, 1]), as.character(x[, 2]))) == 0) {
    stop(
      "Fragmentation centrality is only defined for one-mode networks.",
      call. = FALSE
    )
  }
  mat <- snafun::to_matrix(x)
  v_fragment.matrix(
    x = mat,
    vids = vids,
    M = M,
    binary = binary,
    large = large,
    geodist.precomp = geodist.precomp,
    rescaled = rescaled
  )
}


# Normalize possible geodistance inputs to a plain matrix so callers can pass
# either a matrix directly or the full return value from sna::geodist().
v_fragment_normalize_geodist <- function(geodist.precomp) {
  if (is.list(geodist.precomp) && "gdist" %in% names(geodist.precomp)) {
    geodist.precomp <- geodist.precomp$gdist
  }
  if (!is.matrix(geodist.precomp)) {
    stop(
      "'geodist.precomp' should be a square matrix or a list containing 'gdist'.",
      call. = FALSE
    )
  }
  geodist.precomp
}


v_fragment_get_original_distances <- function(x, binary, large, geodist.precomp) {
  if (!is.null(geodist.precomp)) {
    distances <- v_fragment_normalize_geodist(geodist.precomp)
  } else if (isTRUE(large)) {
    graph <- igraph::graph_from_adjacency_matrix(
      x,
      mode = "directed",
      weighted = if (isTRUE(binary)) NULL else "weight",
      diag = TRUE
    )
    distances <- igraph::distances(graph, mode = "out")
  } else {
    distances <- sna::geodist(x, ignore.eval = binary)$gdist
  }
  if (!all(dim(distances) == dim(x))) {
    stop(
      "'geodist.precomp' should have the same dimensions as 'x'.",
      call. = FALSE
    )
  }
  distances
}


v_fragment_get_normalization_factor <- function(distances) {
  distances <- as.matrix(distances)
  diag(distances) <- Inf
  max(1 / distances)
}


v_fragment_apply_reachability_limit <- function(distances, M) {
  distances <- as.matrix(distances)
  distances[distances > M] <- Inf
  distances
}


v_fragment_score_removed_nodes <- function(residual_matrix, M, binary,
                                           normalization_factor) {
  residual_distances <- sna::geodist(residual_matrix, ignore.eval = binary)$gdist
  residual_distances <- v_fragment_apply_reachability_limit(
    distances = residual_distances,
    M = M
  )
  diag(residual_distances) <- Inf
  residual_weights <- 1 / residual_distances
  residual_sum <- sum(residual_weights)
  node_count <- ncol(residual_distances)
  1 - residual_sum / (node_count * (node_count - 1) * normalization_factor)
}


v_fragment_resolve_vids <- function(vids, x) {
  if (is.null(vids)) {
    return(NULL)
  }
  if (is.character(vids)) {
    if (is.null(rownames(x))) {
      stop(
        "Character 'vids' can only be used when vertex names are available.",
        call. = FALSE
      )
    }
    vids_numeric <- match(vids, rownames(x))
    if (any(is.na(vids_numeric))) {
      stop("You asked for vertices that are not present in the graph")
    }
    return(vids_numeric)
  }
  if (!is.numeric(vids)) {
    stop("'vids' should be numeric or character.", call. = FALSE)
  }
  vids_numeric <- as.integer(vids)
  if (any(is.na(vids_numeric)) || any(vids_numeric < 1) || any(vids_numeric > nrow(x))) {
    stop("You asked for vertices that are not present in the graph")
  }
  vids_numeric
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
#' g <- snafun::create_manual_graph(2:3:4:5:6:7:8:9:10 -+ 1)
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
#' g1 <- snafun::to_igraph(
#'   data.frame(
#'     from = c(4, 5, 1, 1, 1, 8, 11, 11, 6, 7, 8, 2, 2, 9, 10),
#'     to = c(1, 1, 6, 7, 8, 11, 12, 13, 2, 2, 2, 9, 10, 3, 3)
#'   ),
#'   directed = FALSE
#' )
#' g1 <- snafun::add_vertex_names(g1, LETTERS[1:13])
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



#' Bridge strength centrality
#'
#' Bridge centralities quantify how strongly a vertex connects different
#' communities. They require both a graph and a community assignment.
#'
#' These functions are intended for one-mode social networks. The
#' \code{communities} argument can be supplied in three ways:
#'
#' \itemize{
#' \item as an \code{igraph} \code{communities} object, such as the output of
#' \code{\link{extract_comm_walktrap}} or \code{\link{extract_comm_fastgreedy}};
#' \item as a vector of community labels with one value per vertex;
#' \item as a named list in which each element contains the vertices that belong
#' to one community.
#' }
#'
#' Named vectors and named lists are matched to vertex names when possible.
#' Unnamed vectors are aligned by vertex order.
#'
#' \code{v_bridge_strength()} sums the magnitudes of ties from a vertex to
#' vertices in other communities. By default, weights are treated in absolute
#' value, so a strong negative tie counts as a strong bridge as well.
#'
#' \code{v_bridge_expected_influence()} keeps the edge signs and sums the signed
#' weights from a vertex to vertices in other communities.
#'
#' \code{v_bridge_expected_influence2()} extends this idea by adding one extra
#' indirect step: a vertex receives credit not only for its direct cross-
#' community ties, but also for how strongly its neighbors connect onward into
#' other communities.
#'
#' Community assignments can be supplied either as an \code{igraph}
#' \code{communities} object (for example the output of
#' \code{\link{extract_comm_walktrap}}) or as a vector of community labels with
#' one value per vertex. Named vectors are matched to vertex names; unnamed
#' vectors are aligned by vertex order.
#'
#' Use \code{use_communities} to restrict the calculation to a subset of
#' communities. Vertices outside that subset receive \code{NA}.
#'
#' \code{v_bridge_closeness()} and \code{v_bridge_betweenness()} are shortest-
#' path based bridge measures. For these functions, strictly positive edge
#' weights are interpreted as tie strengths and converted to distances via
#' \code{1 / weight}. Zero and negative weights are ignored in the path search.
#' Use \code{weights = NA} if you want purely topological shortest paths.
#'
#' @param communities community assignment, either a \code{communities} object
#' , a vector with one community label per vertex, or a named list of
#' communities
#' @param use_communities optional subset of communities to keep in the bridge
#' calculation. Defaults to all communities.
#' @param type one of \code{"all"}, \code{"out"}, or \code{"in"}. For
#' undirected graphs this is treated as \code{"all"}.
#' @param absolute logical; should bridge strength use absolute tie magnitudes?
#' Defaults to \code{TRUE}.
#' @return Named numeric vector with one bridge-centrality score per vertex. If
#' the graph has vertex names, those names are used; otherwise the vector is
#' returned in vertex order. Vertices excluded via \code{use_communities}
#' receive \code{NA}.
#' @examples
#' bridge_graph <- snafun::create_manual_graph(
#'   A -+ B, A -+ C, B -+ D, C -+ A, D -+ A, E -+ A
#' )
#' bridge_graph <- snafun::add_edge_attributes(
#'   bridge_graph,
#'   attr_name = "weight",
#'   value = c(2, -1, 4, 3, 1, -2)
#' )
#' comms <- c(A = 1, B = 1, C = 2, D = 2, E = 3)
#'
#' v_bridge_strength(bridge_graph, communities = comms)
#' v_bridge_strength(bridge_graph, communities = comms, type = "out")
#' v_bridge_expected_influence(bridge_graph, communities = comms)
#' v_bridge_expected_influence2(bridge_graph, communities = comms)
#'
#' # limit the calculation to a subset of communities
#' v_bridge_strength(
#'   bridge_graph,
#'   communities = comms,
#'   use_communities = c(1, 2)
#' )
#'
#' # shortest-path bridge measures
#' v_bridge_closeness(bridge_graph, communities = comms)
#' v_bridge_betweenness(bridge_graph, communities = comms)
#'
#' # ignore weights and use purely topological shortest paths
#' v_bridge_closeness(bridge_graph, communities = comms, weights = NA)
#' v_bridge_betweenness(bridge_graph, communities = comms, weights = NA)
#'
#' # the communities argument can also be a named list
#' comm_list <- list(
#'   group_1 = c("A", "B"),
#'   group_2 = c("C", "D"),
#'   group_3 = "E"
#' )
#' v_bridge_strength(bridge_graph, communities = comm_list)
#'
#' # or the result of a community-detection function
#' g_u <- snafun::create_manual_graph(
#'   A - B, A - C, B - C,
#'   C - D, D - E, D - F, E - F
#' )
#' walk <- snafun::extract_comm_walktrap(g_u)
#' v_bridge_strength(g_u, communities = walk)
#'
#' # matrix and edgelist input work too
#' mat <- snafun::to_matrix(bridge_graph)
#' snafun::v_bridge_expected_influence(mat, communities = comms)
#'
#' el <- snafun::to_edgelist(bridge_graph)
#' snafun::v_bridge_expected_influence2(el, communities = comms)
#' @name v_bridge
NULL


#' @describeIn v_bridge Bridge strength centrality
#' @export
v_bridge_strength <- function(x,
                              communities,
                              use_communities = NULL,
                              type = c("all", "out", "in"),
                              absolute = TRUE,
                              rescaled = FALSE) {
  UseMethod("v_bridge_strength")
}


#' @export
v_bridge_strength.default <- function(x,
                                      communities,
                                      use_communities = NULL,
                                      type = c("all", "out", "in"),
                                      absolute = TRUE,
                                      rescaled = FALSE) {
  txt <- methods_error_message("x", "v_bridge_strength")
  stop(txt)
}


#' @export
v_bridge_strength.igraph <- function(x,
                                     communities,
                                     use_communities = NULL,
                                     type = c("all", "out", "in"),
                                     absolute = TRUE,
                                     rescaled = FALSE) {
  prepared <- prepare_bridge_inputs(
    graph = x,
    communities = communities,
    use_communities = use_communities,
    type = type
  )
  mat <- prepared$matrix
  if (isTRUE(absolute)) {
    mat <- abs(mat)
  }
  scores <- bridge_direct_score(
    matrix = mat,
    membership = prepared$membership,
    included = prepared$included
  )
  bridge_finalize_scores(scores, labels = prepared$labels, rescaled = rescaled)
}


#' @export
v_bridge_strength.network <- function(x,
                                      communities,
                                      use_communities = NULL,
                                      type = c("all", "out", "in"),
                                      absolute = TRUE,
                                      rescaled = FALSE) {
  v_bridge_strength(
    snafun::to_igraph(x),
    communities = communities,
    use_communities = use_communities,
    type = type,
    absolute = absolute,
    rescaled = rescaled
  )
}


#' @export
v_bridge_strength.matrix <- function(x,
                                     communities,
                                     use_communities = NULL,
                                     type = c("all", "out", "in"),
                                     absolute = TRUE,
                                     rescaled = FALSE) {
  v_bridge_strength(
    snafun::to_igraph(x, bipartite = nrow(x) != ncol(x)),
    communities = communities,
    use_communities = use_communities,
    type = type,
    absolute = absolute,
    rescaled = rescaled
  )
}


#' @export
v_bridge_strength.data.frame <- function(x,
                                         communities,
                                         use_communities = NULL,
                                         type = c("all", "out", "in"),
                                         absolute = TRUE,
                                         rescaled = FALSE) {
  v_bridge_strength(
    snafun::to_igraph(x),
    communities = communities,
    use_communities = use_communities,
    type = type,
    absolute = absolute,
    rescaled = rescaled
  )
}


#' @describeIn v_bridge Bridge expected influence (one-step)
#' @export
v_bridge_expected_influence <- function(x,
                                        communities,
                                        use_communities = NULL,
                                        type = c("all", "out", "in"),
                                        rescaled = FALSE) {
  UseMethod("v_bridge_expected_influence")
}


#' @export
v_bridge_expected_influence.default <- function(x,
                                                communities,
                                                use_communities = NULL,
                                                type = c("all", "out", "in"),
                                                rescaled = FALSE) {
  txt <- methods_error_message("x", "v_bridge_expected_influence")
  stop(txt)
}


#' @export
v_bridge_expected_influence.igraph <- function(x,
                                               communities,
                                               use_communities = NULL,
                                               type = c("all", "out", "in"),
                                               rescaled = FALSE) {
  prepared <- prepare_bridge_inputs(
    graph = x,
    communities = communities,
    use_communities = use_communities,
    type = type
  )
  scores <- bridge_direct_score(
    matrix = prepared$matrix,
    membership = prepared$membership,
    included = prepared$included
  )
  bridge_finalize_scores(scores, labels = prepared$labels, rescaled = rescaled)
}


#' @export
v_bridge_expected_influence.network <- function(x,
                                                communities,
                                                use_communities = NULL,
                                                type = c("all", "out", "in"),
                                                rescaled = FALSE) {
  v_bridge_expected_influence(
    snafun::to_igraph(x),
    communities = communities,
    use_communities = use_communities,
    type = type,
    rescaled = rescaled
  )
}


#' @export
v_bridge_expected_influence.matrix <- function(x,
                                               communities,
                                               use_communities = NULL,
                                               type = c("all", "out", "in"),
                                               rescaled = FALSE) {
  v_bridge_expected_influence(
    snafun::to_igraph(x, bipartite = nrow(x) != ncol(x)),
    communities = communities,
    use_communities = use_communities,
    type = type,
    rescaled = rescaled
  )
}


#' @export
v_bridge_expected_influence.data.frame <- function(x,
                                                   communities,
                                                   use_communities = NULL,
                                                   type = c("all", "out", "in"),
                                                   rescaled = FALSE) {
  v_bridge_expected_influence(
    snafun::to_igraph(x),
    communities = communities,
    use_communities = use_communities,
    type = type,
    rescaled = rescaled
  )
}


#' @describeIn v_bridge Bridge expected influence (two-step)
#' @export
v_bridge_expected_influence2 <- function(x,
                                         communities,
                                         use_communities = NULL,
                                         type = c("all", "out", "in"),
                                         rescaled = FALSE) {
  UseMethod("v_bridge_expected_influence2")
}


#' @export
v_bridge_expected_influence2.default <- function(x,
                                                 communities,
                                                 use_communities = NULL,
                                                 type = c("all", "out", "in"),
                                                 rescaled = FALSE) {
  txt <- methods_error_message("x", "v_bridge_expected_influence2")
  stop(txt)
}


#' @export
v_bridge_expected_influence2.igraph <- function(x,
                                                communities,
                                                use_communities = NULL,
                                                type = c("all", "out", "in"),
                                                rescaled = FALSE) {
  prepared <- prepare_bridge_inputs(
    graph = x,
    communities = communities,
    use_communities = use_communities,
    type = type
  )
  direct_score <- bridge_direct_score(
    matrix = prepared$matrix,
    membership = prepared$membership,
    included = prepared$included
  )
  indirect_score <- bridge_indirect_score(
    matrix = prepared$matrix,
    membership = prepared$membership,
    included = prepared$included
  )
  scores <- direct_score + indirect_score
  bridge_finalize_scores(scores, labels = prepared$labels, rescaled = rescaled)
}


#' @export
v_bridge_expected_influence2.network <- function(x,
                                                 communities,
                                                 use_communities = NULL,
                                                 type = c("all", "out", "in"),
                                                 rescaled = FALSE) {
  v_bridge_expected_influence2(
    snafun::to_igraph(x),
    communities = communities,
    use_communities = use_communities,
    type = type,
    rescaled = rescaled
  )
}


#' @export
v_bridge_expected_influence2.matrix <- function(x,
                                                communities,
                                                use_communities = NULL,
                                                type = c("all", "out", "in"),
                                                rescaled = FALSE) {
  v_bridge_expected_influence2(
    snafun::to_igraph(x, bipartite = nrow(x) != ncol(x)),
    communities = communities,
    use_communities = use_communities,
    type = type,
    rescaled = rescaled
  )
}


#' @export
v_bridge_expected_influence2.data.frame <- function(x,
                                                    communities,
                                                    use_communities = NULL,
                                                    type = c("all", "out", "in"),
                                                    rescaled = FALSE) {
  v_bridge_expected_influence2(
    snafun::to_igraph(x),
    communities = communities,
    use_communities = use_communities,
    type = type,
    rescaled = rescaled
  )
}


#' @describeIn v_bridge Bridge closeness centrality
#'
#' Bridge closeness is the inverse of the average shortest-path distance from a
#' vertex to vertices outside its own community.
#'
#' In directed graphs, \code{mode = "out"} asks how easily a vertex can reach
#' other communities, whereas \code{mode = "in"} asks how easily other
#' communities can reach that vertex.
#'
#' For these shortest-path bridge measures, strictly positive edge weights are
#' interpreted as tie strengths and converted to distances via \code{1/weight}.
#' If negative or zero-weight edges are present, they are removed before the
#' calculation. Use \code{weights = NA} to ignore weights completely and compute
#' purely topological shortest paths.
#'
#' @param mode one of \code{"out"}, \code{"in"}, or \code{"all"}.
#' @param weights optional edge weights. Leave \code{NULL} to use the graph's
#' stored \code{weight} attribute when present, or set \code{NA} to ignore edge
#' weights.
#' @export
v_bridge_closeness <- function(x,
                               communities,
                               use_communities = NULL,
                               mode = c("out", "in", "all"),
                               weights = NULL,
                               rescaled = FALSE) {
  UseMethod("v_bridge_closeness")
}


#' @export
v_bridge_closeness.default <- function(x,
                                       communities,
                                       use_communities = NULL,
                                       mode = c("out", "in", "all"),
                                       weights = NULL,
                                       rescaled = FALSE) {
  txt <- methods_error_message("x", "v_bridge_closeness")
  stop(txt)
}


#' @export
v_bridge_closeness.igraph <- function(x,
                                      communities,
                                      use_communities = NULL,
                                      mode = c("out", "in", "all"),
                                      weights = NULL,
                                      rescaled = FALSE) {
  prepared <- prepare_bridge_shortest_path_inputs(
    graph = x,
    communities = communities,
    use_communities = use_communities,
    weights = weights,
    mode = mode
  )
  scores <- bridge_closeness_score(
    graph = prepared$graph,
    membership = prepared$membership,
    included = prepared$included,
    mode = prepared$mode
  )
  bridge_finalize_scores(scores, labels = prepared$labels, rescaled = rescaled)
}


#' @export
v_bridge_closeness.network <- function(x,
                                       communities,
                                       use_communities = NULL,
                                       mode = c("out", "in", "all"),
                                       weights = NULL,
                                       rescaled = FALSE) {
  v_bridge_closeness(
    snafun::to_igraph(x),
    communities = communities,
    use_communities = use_communities,
    mode = mode,
    weights = weights,
    rescaled = rescaled
  )
}


#' @export
v_bridge_closeness.matrix <- function(x,
                                      communities,
                                      use_communities = NULL,
                                      mode = c("out", "in", "all"),
                                      weights = NULL,
                                      rescaled = FALSE) {
  v_bridge_closeness(
    snafun::to_igraph(x, bipartite = nrow(x) != ncol(x)),
    communities = communities,
    use_communities = use_communities,
    mode = mode,
    weights = weights,
    rescaled = rescaled
  )
}


#' @export
v_bridge_closeness.data.frame <- function(x,
                                          communities,
                                          use_communities = NULL,
                                          mode = c("out", "in", "all"),
                                          weights = NULL,
                                          rescaled = FALSE) {
  v_bridge_closeness(
    snafun::to_igraph(x),
    communities = communities,
    use_communities = use_communities,
    mode = mode,
    weights = weights,
    rescaled = rescaled
  )
}


#' @describeIn v_bridge Bridge betweenness centrality
#'
#' Bridge betweenness counts how often a vertex lies on shortest paths between
#' source and target vertices from different communities.
#'
#' In directed graphs, the calculation respects the graph's edge directions by
#' default. Set \code{directed = FALSE} if you want to ignore directions and
#' treat the graph as undirected for this measure.
#'
#' @param directed optional logical. Leave \code{NULL} to use the graph's own
#' directedness; set \code{FALSE} to ignore edge directions.
#' @export
v_bridge_betweenness <- function(x,
                                 communities,
                                 use_communities = NULL,
                                 directed = NULL,
                                 weights = NULL,
                                 rescaled = FALSE) {
  UseMethod("v_bridge_betweenness")
}


#' @export
v_bridge_betweenness.default <- function(x,
                                         communities,
                                         use_communities = NULL,
                                         directed = NULL,
                                         weights = NULL,
                                         rescaled = FALSE) {
  txt <- methods_error_message("x", "v_bridge_betweenness")
  stop(txt)
}


#' @export
v_bridge_betweenness.igraph <- function(x,
                                        communities,
                                        use_communities = NULL,
                                        directed = NULL,
                                        weights = NULL,
                                        rescaled = FALSE) {
  prepared <- prepare_bridge_shortest_path_inputs(
    graph = x,
    communities = communities,
    use_communities = use_communities,
    weights = weights,
    directed = directed
  )
  scores <- bridge_betweenness_score(
    graph = prepared$graph,
    membership = prepared$membership,
    included = prepared$included,
    directed = prepared$directed
  )
  bridge_finalize_scores(scores, labels = prepared$labels, rescaled = rescaled)
}


#' @export
v_bridge_betweenness.network <- function(x,
                                         communities,
                                         use_communities = NULL,
                                         directed = NULL,
                                         weights = NULL,
                                         rescaled = FALSE) {
  v_bridge_betweenness(
    snafun::to_igraph(x),
    communities = communities,
    use_communities = use_communities,
    directed = directed,
    weights = weights,
    rescaled = rescaled
  )
}


#' @export
v_bridge_betweenness.matrix <- function(x,
                                        communities,
                                        use_communities = NULL,
                                        directed = NULL,
                                        weights = NULL,
                                        rescaled = FALSE) {
  v_bridge_betweenness(
    snafun::to_igraph(x, bipartite = nrow(x) != ncol(x)),
    communities = communities,
    use_communities = use_communities,
    directed = directed,
    weights = weights,
    rescaled = rescaled
  )
}


#' @export
v_bridge_betweenness.data.frame <- function(x,
                                            communities,
                                            use_communities = NULL,
                                            directed = NULL,
                                            weights = NULL,
                                            rescaled = FALSE) {
  v_bridge_betweenness(
    snafun::to_igraph(x),
    communities = communities,
    use_communities = use_communities,
    directed = directed,
    weights = weights,
    rescaled = rescaled
  )
}


prepare_bridge_inputs <- function(graph,
                                  communities,
                                  use_communities,
                                  type) {
  type <- snafun.match.arg(type, c("all", "out", "in"))
  if (snafun::is_bipartite(graph)) {
    stop("Bridge centralities are only defined for one-mode networks.")
  }

  labels <- if (snafun::has_vertexnames(graph)) {
    as.character(snafun::extract_vertex_names(graph))
  } else {
    as.character(seq_len(snafun::count_vertices(graph)))
  }

  membership <- normalize_bridge_communities(communities = communities, graph = graph)
  membership <- membership[labels]
  membership_labels <- unique(as.character(unname(membership)))

  if (is.null(use_communities)) {
    keep_levels <- membership_labels
  } else {
    keep_levels <- as.character(use_communities)
    if (!all(keep_levels %in% membership_labels)) {
      stop("Unknown value in 'use_communities'.")
    }
  }
  if (length(unique(keep_levels)) < 2L) {
    stop("Please include at least two communities in 'use_communities'.")
  }

  included <- as.character(unname(membership)) %in% keep_levels

  mat <- snafun::to_matrix(graph)
  storage.mode(mat) <- "double"
  if (!snafun::is_directed(graph)) {
    type <- "all"
  }
  mat <- bridge_effective_matrix(mat = mat, type = type, directed = snafun::is_directed(graph))
  diag(mat) <- 0

  list(
    matrix = mat,
    membership = membership,
    included = included,
    labels = labels,
    type = type
  )
}


normalize_bridge_communities <- function(communities, graph) {
  if (inherits(communities, "communities")) {
    membership <- align_community_membership_to_graph(x = communities, graph = graph)
    names(membership) <- as.character(community_graph_vertex_keys(graph))
    return(membership)
  }

  if (is.list(communities) && !is.data.frame(communities)) {
    membership <- bridge_membership_from_list(communities = communities, graph = graph)
    return(membership)
  }

  if (is.atomic(communities)) {
    keys <- as.character(community_graph_vertex_keys(graph))
    membership <- communities
    if (is.null(names(membership))) {
      if (length(membership) != length(keys)) {
        stop(
          "The community vector and the supplied graph do not refer to the same ",
          "number of vertices."
        )
      }
      names(membership) <- keys
      return(membership)
    }

    index <- match(keys, as.character(names(membership)))
    if (anyNA(index)) {
      stop("The community vector and the supplied graph do not refer to the same vertices.")
    }
    out <- membership[index]
    names(out) <- keys
    return(out)
  }

  stop("'communities' should be a community object, an atomic vector, or a named list.")
}


bridge_membership_from_list <- function(communities, graph) {
  keys <- as.character(community_graph_vertex_keys(graph))
  out <- rep(NA_character_, length(keys))
  names(out) <- keys

  for (i in seq_along(communities)) {
    members <- communities[[i]]
    if (length(members) == 0L) {
      next
    }
    if (is.numeric(members)) {
      if (any(members < 1) || any(members > length(keys))) {
        stop("The community list contains vertex ids outside the graph.")
      }
      out[as.integer(members)] <- as.character(i)
    } else {
      idx <- match(as.character(members), keys)
      if (anyNA(idx)) {
        stop("The community list and the supplied graph do not refer to the same vertices.")
      }
      out[idx] <- as.character(i)
    }
  }

  if (anyNA(out)) {
    stop("The community list does not assign every vertex to exactly one community.")
  }

  out
}


bridge_effective_matrix <- function(mat, type, directed) {
  if (!directed || identical(type, "all")) {
    if (directed) {
      return(mat + t(mat))
    }
    return(mat)
  }
  if (identical(type, "out")) {
    return(mat)
  }
  t(mat)
}


bridge_direct_score <- function(matrix, membership, included) {
  membership_chr <- as.character(unname(membership))
  out <- rep(NA_real_, nrow(matrix))
  for (i in seq_len(nrow(matrix))) {
    if (!included[[i]]) {
      next
    }
    target_mask <- included & membership_chr != membership_chr[[i]]
    out[[i]] <- sum(matrix[i, target_mask, drop = TRUE], na.rm = TRUE)
  }
  out
}


bridge_indirect_score <- function(matrix, membership, included) {
  membership_chr <- as.character(unname(membership))
  community_levels <- sort(unique(membership_chr[included]))
  out <- rep(NA_real_, nrow(matrix))

  for (i in seq_len(nrow(matrix))) {
    if (!included[[i]]) {
      next
    }

    own <- membership_chr[[i]]
    other_levels <- setdiff(community_levels, own)
    subtotal <- 0

    for (one_level in other_levels) {
      one_step_to_level <- rep(0, nrow(matrix))
      target_mask <- included & membership_chr == one_level
      if (any(target_mask)) {
        one_step_to_level <- rowSums(matrix[, target_mask, drop = FALSE], na.rm = TRUE)
      }
      subtotal <- subtotal + sum(matrix[i, , drop = TRUE] * one_step_to_level, na.rm = TRUE)
    }

    out[[i]] <- subtotal
  }

  out
}


bridge_finalize_scores <- function(scores, labels, rescaled) {
  names(scores) <- labels
  if (isTRUE(rescaled)) {
    total <- sum(scores, na.rm = TRUE)
    if (!isTRUE(all.equal(total, 0))) {
      scores <- scores / total
    }
  }
  scores
}


prepare_bridge_shortest_path_inputs <- function(graph,
                                                communities,
                                                use_communities,
                                                weights,
                                                mode = NULL,
                                                directed = NULL) {
  if (snafun::is_bipartite(graph)) {
    stop("Bridge centralities are only defined for one-mode networks.")
  }

  labels <- if (snafun::has_vertexnames(graph)) {
    as.character(snafun::extract_vertex_names(graph))
  } else {
    as.character(seq_len(snafun::count_vertices(graph)))
  }

  membership <- normalize_bridge_communities(communities = communities, graph = graph)
  membership <- membership[labels]
  membership_labels <- unique(as.character(unname(membership)))

  if (is.null(use_communities)) {
    keep_levels <- membership_labels
  } else {
    keep_levels <- as.character(use_communities)
    if (!all(keep_levels %in% membership_labels)) {
      stop("Unknown value in 'use_communities'.")
    }
  }
  if (length(unique(keep_levels)) < 2L) {
    stop("Please include at least two communities in 'use_communities'.")
  }
  included <- as.character(unname(membership)) %in% keep_levels

  graph_sp <- bridge_shortest_path_graph(
    graph = graph,
    weights = weights,
    directed = directed
  )

  if (is.null(mode)) {
    mode_out <- if (isTRUE(igraph::is_directed(graph_sp))) "out" else "all"
  } else {
    mode_out <- snafun.match.arg(mode, c("out", "in", "all"))
    if (!isTRUE(igraph::is_directed(graph_sp))) {
      mode_out <- "all"
    }
  }

  list(
    graph = graph_sp,
    membership = membership,
    included = included,
    labels = labels,
    mode = mode_out,
    directed = isTRUE(igraph::is_directed(graph_sp))
  )
}


bridge_shortest_path_graph <- function(graph,
                                       weights,
                                       directed) {
  use_directed <- if (is.null(directed)) {
    snafun::is_directed(graph)
  } else {
    isTRUE(directed)
  }

  mat <- snafun::to_matrix(graph)
  storage.mode(mat) <- "double"

  if (!is.null(weights) && length(weights) == 1L && is.na(weights)) {
    mat[mat != 0] <- 1
  } else {
    positive_mask <- mat > 0
    mat[!positive_mask] <- 0
    mat[positive_mask] <- 1 / mat[positive_mask]
  }
  diag(mat) <- 0

  if (isTRUE(use_directed)) {
    return(igraph::graph_from_adjacency_matrix(
      mat,
      mode = "directed",
      weighted = TRUE,
      diag = FALSE
    ))
  }

  mat_sym <- pmin(mat, t(mat))
  only_one_side <- (mat_sym == 0) & ((mat > 0) | (t(mat) > 0))
  mat_sym[only_one_side] <- pmax(mat, t(mat))[only_one_side]
  diag(mat_sym) <- 0
  igraph::graph_from_adjacency_matrix(
    mat_sym,
    mode = "undirected",
    weighted = TRUE,
    diag = FALSE
  )
}


bridge_closeness_score <- function(graph,
                                   membership,
                                   included,
                                   mode) {
  labels <- if (snafun::has_vertexnames(graph)) {
    as.character(snafun::extract_vertex_names(graph))
  } else {
    as.character(seq_len(snafun::count_vertices(graph)))
  }
  out <- rep(NA_real_, length(labels))
  membership_chr <- as.character(unname(membership))

  for (i in seq_along(labels)) {
    if (!included[[i]]) {
      next
    }
    target_mask <- included & membership_chr != membership_chr[[i]]
    if (!any(target_mask)) {
      out[[i]] <- NA_real_
      next
    }
    dists <- igraph::distances(
      graph = graph,
      v = i,
      to = which(target_mask),
      mode = mode,
      weights = NULL
    )
    finite_dists <- dists[is.finite(dists)]
    if (length(finite_dists) == 0L) {
      out[[i]] <- NaN
    } else {
      out[[i]] <- 1 / mean(finite_dists)
    }
  }

  out
}


bridge_betweenness_score <- function(graph,
                                     membership,
                                     included,
                                     directed) {
  labels <- if (snafun::has_vertexnames(graph)) {
    as.character(snafun::extract_vertex_names(graph))
  } else {
    as.character(seq_len(snafun::count_vertices(graph)))
  }
  membership_chr <- as.character(unname(membership))
  counts <- stats::setNames(rep(0, length(labels)), labels)

  for (i in seq_along(labels)) {
    if (!included[[i]]) {
      next
    }
    target_ids <- which(included & membership_chr != membership_chr[[i]])
    if (length(target_ids) == 0L) {
      next
    }
    shortest <- igraph::all_shortest_paths(
      graph = graph,
      from = i,
      to = target_ids,
      mode = if (isTRUE(directed)) "out" else "all",
      weights = NULL
    )
    vpaths <- shortest$vpaths
    if (is.null(vpaths)) {
      vpaths <- shortest$res
    }
    if (length(vpaths) == 0L) {
      next
    }

    mids <- unlist(lapply(vpaths, function(one_path) {
      ids <- as.integer(one_path)
      if (length(ids) <= 2L) {
        return(integer(0))
      }
      ids[2:(length(ids) - 1L)]
    }))
    if (length(mids) == 0L) {
      next
    }
    tab <- table(factor(labels[mids], levels = labels))
    counts <- counts + as.numeric(tab)
  }

  if (!isTRUE(directed)) {
    counts <- counts / 2
  }
  counts[!included] <- NA_real_
  as.numeric(counts)
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

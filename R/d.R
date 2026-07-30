

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
#' g <- snafun::create_manual_graph(1 -- 2 -- 3 -- 4 -- 5 -- 6 -- 7 -- 8 -- 9 -- 10 -- 1)
#' d_distance(g)
#' d_distance(snafun::to_network(g))
#' g <- snafun::add_vertex_names(g, LETTERS[1:10])
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


####----------------------------------------------------------------------------
#' @describeIn dli Edge betweenness of each edge in the graph.
#'
#' The edge betweenness of an edge is the number of shortest paths (geodesics)
#' between all pairs of vertices that run through that edge. Edges with a high
#' edge betweenness bridge otherwise-distant parts of the graph and are often the
#' ties that, when removed, break the network apart; this is the basis of
#' Girvan-Newman community detection.
#'
#' Unlike the other \code{d_*} functions, the output is NOT a full vertex-by-
#' vertex matrix (edge betweenness is only defined for edges that exist).
#' Instead it is a \code{data.frame} with one row per edge and the columns
#' \code{from}, \code{to} and \code{betweenness}, in the same order as
#' \code{\link[igraph]{as_edgelist}}. The endpoints are the vertex names when the
#' graph has them, and the internal vertex ids otherwise. The heavy lifting is
#' done by \code{\link[igraph]{edge_betweenness}}.
#'
#' @param directed logical, should edge direction be taken into account? Default
#' \code{TRUE}. Ignored for undirected graphs.
#' @param weights edge weights. \code{NA} (the default) discards weights and
#' computes purely topological betweenness, consistent with the other
#' \code{d_*}/\code{v_*} functions. \code{NULL} uses the graph's \code{weight}
#' edge attribute if it has one; a numeric vector uses those weights (interpreted
#' as edge distances).
#' @export
#' @examples
#' #
#' # edge betweenness
#' g <- snafun::create_manual_graph(A -+ B, B -+ C, C -+ A, C -+ D)
#' d_betweenness(g)
#' d_betweenness(snafun::to_network(g))
d_betweenness <- function(x, directed = TRUE, weights = NA) {
  UseMethod("d_betweenness")
}


#' @export
d_betweenness.default <- function(x, directed = TRUE, weights = NA) {
  txt <- methods_error_message("x", "d_betweenness")
  stop(txt)
}


#' @export
d_betweenness.igraph <- function(x, directed = TRUE, weights = NA) {
  bt <- igraph::edge_betweenness(x, directed = directed, weights = weights)
  # Endpoints in the SAME order as edge_betweenness() returns its values (i.e.
  # the E(x) edge order), so the columns line up. names = TRUE yields the vertex
  # names when present, and the internal vertex ids otherwise.
  el <- igraph::as_edgelist(x, names = TRUE)
  data.frame(
    from = el[, 1],
    to = el[, 2],
    betweenness = bt,
    stringsAsFactors = FALSE
  )
}


#' @export
d_betweenness.network <- function(x, directed = TRUE, weights = NA) {
  g <- to_igraph(x)
  d_betweenness.igraph(g, directed = directed, weights = weights)
}

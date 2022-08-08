#' Contract multiple vertices into 1 new vertex
#'
#' \code{contract_vertices} combines a set of vertices into one pseudo-vertex 
#' and returns the reduced network.
#' 
#' This function contracts a set of vertices into a pseudo-vertex and returns 
#' the network with that new vertex included (and the contracted vertices 
#' excluded). 
#' 
#' Weighted ties can be used. 
#' This requires to either provide a weighted adjacency matrix OR 
#' an \code{igraph} object with an attribute that is used for weights. 
#' Make sure that this attribute is numeric.
#' By default, if the \code{igraph} object contains an edge attribute called 
#' \code{weight}, this attribute will be used as a weight by default, unless 
#' a different edge attribute is explicitly specified as \code{attr}.
#' 
#' The \code{method} specifies how the (possibly weighted) edges between the 
#' selected \code{vertices} and the other vertices are combined into edges between 
#' the new pseudovertex and the other vertices. The default is to use the 
#' \code{min} procedure.
#' 
#' NOTE: if \code{g} is an \code{igraph} object without vertex names, the 
#' names will be added automatically (as integeres, numbered from 1). 
#' This is useful, so it is clear who-is-who after the vertex contraction.
#'
#' @param g network as an \code{igraph} object or an adjacency matrix.
#'
#' @param vertices Numeric vertor indicating the indeces of the vertices are 
#' to be contracted.
#'
#' @param attr Either NULL or a character string giving an edge attribute name to 
#' be used as weights.  
#' If \code{NULL}, the unweighted network is used to calculate the values of the 
#' resulting contracted network. 
#' If not \code{NULL} then the values of the given edge attribute are included 
#' in the adjacency matrix. If the graph has multiple edges, the edge attribute 
#' of an arbitrarily chosen edge (for the multiple edges) is included. 
#' 
#' @param method Indication of which grouping criterion should be used.\cr
#' \code{method="min"} indicates the "minimum" criterion (edge values as distances).\cr
#' \code{method="max"} indicates the "maximum" criterion (edge values as non-cumulative strengths).\cr
#' \code{method="add"} indicates the "addition" criterion (edge values as cumulative strengths).\cr
#' \code{method="union"} indicates the "union" criterion (edge values as probability).\cr
#' The default is "min". See details for examples.
#'
#' @details Minimum Criterion: the edge value between a group and an outside vertex
#' is measured as the minimal value among all the (nonzero) edge values between
#' any vertex in the group and the outside vertex. Suggested if edge values are
#' interpreted as distances.\cr
#' \emph{Example: suppose vertex A to C has distance 2 and B to C has distance 1,
#' then according to the minimum criterion, the distance between C and
#' the merged set AB is 1. Note that if B and C are not connected,
#' the algorithm takes the distance between A and C to describe
#' the distance between AB and C.}
#'
#' Maximum Criterion: the edge value between a group and an outside vertex
#' is measured as the maximal value among all the (nonzero) edge values between
#' any vertex in the group and the outside vertex. Suggested if edge values are
#' interpreted as non-cumulative strengths. \cr
#' \emph{Example: we keep using the above example, but the figure now indicates
#' the strength of tie. According to the maximum criterion, the strength of tie
#' between AB and C is 2.}
#'
#' Addition Criterion: the edge value between a group and an outside vertex
#' is measured as the sum of all the edge values between any vertex in the group
#' and the outside vertex. Suggested if edge values are as cummulative strengths. \cr
#' \emph{Example: according to the addition criterion, the strength of tie between
#' AB and C is 3}
#'
#' Union Criterion: the edge value between a group and an outside vertex is measured
#' as the probability that there is at least one path connecting the group with
#' the outside vertex. Suggested if edge values are as probability. \cr
#' \emph{Example: suppose A has probability 0.2 to reach C and B has probability
#' 0.5 to reach C, then C can be reached from merged AB with probability
#' 1 - (1 - 0.2) * (1 - 0.5) = 0.6 according to the union criterion.}
#' 
#' @param new_name character, name of the new pseudovertex. If not specified, 
#' it will be called "set".
#' 
#' @param out_as_igraph logical. If \code{TRUE} the resulting network is 
#' returned as an \code{igraph} object. If \code{FALSE}, the output is 
#' returned as an adjacency matrix.
#'
#' @return An \code{igraph} object or an adjacency matrix of the contracted network.
#' 
#' @note Based on the \code{contract} function from the \code{keyplayer} package. 
#' The implementation in our package is more general.
#'
#' @examples
#' # Create a 5x5 weighted and directed adjacency matrix, where edge values
#' # represent the strength of tie
#' W <- matrix(
#'   c(0, 1, 3, 0, 0,
#'     0, 0, 0, 4, 0,
#'     1, 1, 0, 2, 0,
#'     0, 0, 0, 0, 3,
#'     0, 2, 0, 0, 0),
#'     nrow = 5, ncol = 5, byrow = TRUE)
#'
#' # If the strength is believed to be non-accumulative for a group of vertices,
#' # it is proper to use the "maximum" criterion to contract vertex 2 and 3
#' contract_vertices(W, c(2, 3), "max")
#'
#' # Transform the edge value to probability interpretaion
#' P <- W * 0.2
#'
#' # Contract vertex 2 and 3 using the "union" criterion as it is proper for
#' # probability matrix input
#' contract_vertices(P, c(2, 3), "union")
#'
#' @export
contract_vertices <- function (g, vertices, 
                               method = c("min", "max", 
                                        "union", "add"), 
                               attr = NULL,
                               new_name = "set",
                               out_as_igraph = TRUE) {
  is_directed <- FALSE
  
  if (inherits(g, "igraph")) {
    if (is.null(igraph::V(g)$name)) {
      igraph::V(g)$name <- 1:(igraph::vcount(g))
    }
    d <- igraph::as_adjacency_matrix(g, attr = attr, sparse = FALSE)
    is_directed <- igraph::is.directed(g)
    is_weighted <- !is.null(attr)
  } else {   # adjacency matrix
    if (isSymmetric(g)) is_directed <- TRUE
    d <- data.matrix(g, rownames.force = NA)
    colnames(d) <- 1:ncol(d)
    rownames(d) <- 1:nrow(d)
  }

  if (missing(method)) {
    if (length(vertices) == 1) {
      set <- d[, vertices]
      set.r <- d[vertices, ]
    }
    else {
      max <- max(d) + 1
      d <- replace(d, d == 0, max)
      set <- d[, ncol(d)]
      for (i in 1:nrow(d)) {
        set[i] <- min(d[i, vertices])
      }
      set.r <- d[ncol(d), ]
      for (j in 1:ncol(d)) {
        set.r[j] <- min(d[vertices, j])
      }
      d <- replace(d, d == max, 0)
      set <- replace(set, set == max, 0)
      set.r <- replace(set.r, set.r == max, 0)
    }
  }
  else if (method == "add") {
    if (length(vertices) == 1) {
      set <- d[, vertices]
      set.r <- d[vertices, ]
    }
    else {
      set <- rowSums(d[, vertices])
      set.r <- colSums(d[vertices, ])
    }
  }
  else if (method == "union") {
    if (length(vertices) == 1) {
      set <- d[, vertices]
      set.r <- d[vertices, ]
    }
    else {
      set <- rowSums(d[, vertices])
      for (i in 1:nrow(d)) {
        set[i] <- 1 - prod(1 - d[i, vertices])
      }
      set.r <- colSums(d[vertices, ])
      for (j in 1:ncol(d)) {
        set.r[j] <- 1 - prod(1 - d[vertices, j])
      }
    }
  }
  else if (method == "max") {
    if (length(vertices) == 1) {
      set <- d[, vertices]
      set.r <- d[vertices, ]
    }
    else {
      set <- d[, ncol(d)]
      for (i in 1:nrow(d)) {
        set[i] <- max(d[i, vertices])
      }
      set.r <- d[ncol(d), ]
      for (j in 1:ncol(d)) {
        set.r[j] <- max(d[vertices, j])
      }
    }
  }
  else {
    if (length(vertices) == 1) {
      set <- d[, vertices]
      set.r <- d[vertices, ]
    }
    else {
      max <- max(d) + 1
      d <- replace(d, d == 0, max)
      set <- d[, ncol(d)]
      for (i in 1:nrow(d)) {
        set[i] <- min(d[i, vertices])
      }
      set.r <- d[ncol(d), ]
      for (j in 1:ncol(d)) {
        set.r[j] <- min(d[vertices, j])
      }
      d <- replace(d, d == max, 0)
      set <- replace(set, set == max, 0)
      set.r <- replace(set.r, set.r == max, 0)
    }
  }
  set.r <- t(set.r)
  set.r <- cbind(set.r, 0)
  d <- cbind(d, set)
  d <- rbind(d, set.r)
  if (new_name != "set") {
    colnames(d)[which(colnames(d) == "set")] <- new_name
  }
  rownames(d) <- colnames(d)
  ColToDelete <- vertices
  d <- d[, -ColToDelete]
  RowToDelete <- t(vertices)
  d <- d[-RowToDelete, ]
  
  if (out_as_igraph) {
    actorNames <- colnames(d)
    if (is_directed) {
      d <- igraph::graph_from_adjacency_matrix(d, mode = "directed")
    } else {
      d <- igraph::graph_from_adjacency_matrix(d, mode = "undirected")
    }
    igraph::V(d)$name = actorNames
  }
  return(d)
}

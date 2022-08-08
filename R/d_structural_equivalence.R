
#' Structural equivalence
#' 
#' Calculate structural equivalence
#' 
#' Calculate structural equivalence based on correlations. 
#' The function, of course, disregards the diagonal.
#' 
#' Vertices are also perfectly structurally equivalent with 
#' themselves.
#' 
#' If requested, an edge attribute can be taken into account 
#' through the \code{weights} argument. By default, no weights 
#' are used.
#' 
#' The value varies between \[-1, 1\], with -1 denoting maximally 
#' possible distance between two vertices and +1 denoting 
#' exact structural equivalence.
#' 
#' An implementation for a \code{network} graph, is \link[sna]{sedist} (with 
#' \code{method == "correlation"}). This yields the same result, but does not 
#' allow the use of a weight.
#'
#' @param x graph of class \code{igraph}, \code{network}, \code{matrix}
#' @param weights Either \code{NA} (the default), \code{NULL}, or a character 
#' string giving an edge attribute name. If \code{NULL}, the edge attribute 
#' 'weight' will be used if it exists in the graph. If \code{NA}, no weight 
#' will be applied. If a character, the edge attribute with that name will be 
#' used (an error is thrown if that attribute does not occur in \code{x}.)
#' If the graph has multiple edges, the edge attribute of an arbitrarily chosen 
#' edge (for the multiple edges) is included. 
#' Note that the function requires the attribute to be either logical or numeric.
#' 
#' This parameter is ignored if \code{x} is a \code{matrix}. In this case, the 
#' weights are assumed to already be inside the adjacency matrix.
#' @param digits number of decimals to use in the result
#' @param suppressWarnings logical, whether or not any warnings should be returned 
#' (if issued by \link[stats]{cor}).
#'
#' @return matrix of size n*n, with n equal to the number of vertices in the graph
#' @export
#' @importFrom stats cor
#'
#' @examples
#' \dontrun{
#' data("judge_net", package = "snafun")
#' d_structural_equivalence(judge_net)
#' 
#' g1 <- create_random_graph(10, "gnm", m = 15, directed = TRUE, graph = "igraph")
#' g2 <- create_random_graph(10, "gnm", m = 15, directed = FALSE, graph = "igraph")
#' d_structural_equivalence(g1)
#' d_structural_equivalence(g2)
#' d_structural_equivalence(to_network(g1))
#' d_structural_equivalence(to_matrix(g1))
#' 
#' g3 <- add_edge_attributes(g1, "weight", value = 1:count_edges(g1))
#' d_structural_equivalence(g3, weights = NA)  # no weight used
#' d_structural_equivalence(g3, weights = NULL)
#' d_structural_equivalence(g3, weights = "weight") # same as NULL
#' }
d_structural_equivalence <- function(x, weights = NA, digits = 3,
                                     suppressWarnings = TRUE) {
  UseMethod("d_structural_equivalence")
}


#' @export
d_structural_equivalence.default <- function(x, weights = NA, digits = 3,
                                             suppressWarnings = TRUE) {
  txt <- methods_error_message("x", "d_structural_equivalence")
  stop(txt)
}



d_structural_equivalence.igraph <- function(x, weights = NA, digits = 3,
                                            suppressWarnings = TRUE) {
  
  attr_weight <- NULL    # placeholder
  if (is.null(weights)) {  # use weight if it exists, nothing if it doesn't
    if (is_weighted(x)) attr_weight <- "weight"
  } else if (is.character(weights)) {  # an edge attribute is requested to use as a weight
    if (has_edge_attribute(x, as.character(weights))) {
      attr_weight <- as.character(weights)
    } else {
      stop("The edge weights attribute you specified does not occur in 'x'")
    }
  }
  
  adj <- igraph::as_adjacency_matrix(x, type = "both",
                                       attr = attr_weight,
                                       sparse = FALSE)
  diag(adj) <- NA
  d <- rbind(adj, t(adj))
  if (suppressWarnings) {
    ret <- suppressWarnings(round(cor(d, use = "pairwise"), digits = digits))
  } else {
    ret <- round(stats::cor(d, use = "pairwise"), digits = digits)
  }
  
  ret[is.na(ret)] <- 0
  ret
}



d_structural_equivalence.network <- function(x, weights = NA, digits = 3,
                                            suppressWarnings = TRUE) {

  attr_weight <- NULL    # placeholder
  if (is.null(weights)) {  # use weight if it exists, nothing if it doesn't
    if (is_weighted(x)) attr_weight <- "weight"
  } else if (is.character(weights)) {  # an edge attribute is requested to use as a weight
    if (has_edge_attribute(x, as.character(weights))) {
      attr_weight <- as.character(weights)
    } else {
      stop("The edge weights attribute you specified does not occur in 'x'")
    }
  }
  
  adj <- network::as.matrix.network(x, matrix.type = "adjacency", attrname = attr_weight)

  diag(adj) <- NA
  d <- rbind(adj, t(adj))
  if (suppressWarnings) {
    ret <- suppressWarnings(round(cor(d, use = "pairwise"), digits = digits))
  } else {
    ret <- round(stats::cor(d, use = "pairwise"), digits = digits)
  }
  ret[is.na(ret)] <- 0
  ret
}




d_structural_equivalence.matrix <- function(x, weights = NA, digits = 3,
                                             suppressWarnings = TRUE) {
  
  if (nrow(x) != ncol(x)) {
    stop("Please input a square (weighted) adjacency matrix for 'x'")
  }
  
  adj <- x
  
  diag(adj) <- NA
  d <- rbind(adj, t(adj))
  if (suppressWarnings) {
    ret <- suppressWarnings(round(cor(d, use = "pairwise"), digits = digits))
  } else {
    ret <- round(stats::cor(d, use = "pairwise"), digits = digits)
  }
  ret
}

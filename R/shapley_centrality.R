
#' @title Shapley Centrality
#'
#' @description This function computes the centrality of vertices 
#' based on their Shapley value, following the approach from the 
#' Michalak et al. (2013) paper.
#'
#' @param x An object of class \code{igraph} or \code{network}
#' @param add.vertex.names logical, should the output contain vertex names. 
#' This requires a vertex attribute \code{name} to be present in the graph. 
#' It is ignored if the attributed is missing.
#' @param vids Vertices to be considered in the calculation (numeric vector)
#'
#' @return
#' Vector with the Shapley centrality values for each vertex
#' 
#' @references
#' Michalak, T.P., Aadithya, K.V., Szczepanski, P.L., Ravindran, B. and 
#' Jennings, N.R., 2013. Efficient computation of the Shapley value for 
#' game-theoretic network centrality. Journal of Artificial Intelligence 
#' Research, 46, pp.607-650.
#' 
#' The code is adapted from \code{CINNA::group_centrality} and gives the 
#' same result (but our version is slightly more robust).
#'
#' @examples
#' # Figure 1 network from Michalak et al.
#' g1 <- igraph::graph(c(4,1,5,1,1,6,1,7,1,8,8,11,11,12,11,13,6,2,7,2,8,2,
#' 2,9,2,10,9,3,10,3), directed = FALSE)
#' igraph::V(g1)$name <- paste0("v", 1:13)
#' shapley_centrality(g1)
#' shapley_centrality(g1, add.vertex.names = TRUE)
#' @export
shapley_centrality <- function (x, 
                                add.vertex.names = FALSE,
                                vids = igraph::V(x)) {
  
  if (!inherits(x, "igraph") & !inherits(x, "network")) {
    stop("The input should be an igraph object or a network object")
  }

  f <- function(v) {
    1/(1 + igraph::degree(graph = x, v = v)) + 
      sum(1/(1 + igraph::degree(x, igraph::neighbors(x, v, mode = 'all'))))
  }
  
  if (igraph::is_igraph(x)){
    results <- sapply(igraph::V(x), f)
    if (add.vertex.names && igraph::is_named(x)) {
      names(results) <- igraph::V(x)$name[vids]
    }
  }
  
  else{
    x <- intergraph::asIgraph(x)
    results <- sapply(igraph::V(x), f)
    if (add.vertex.names && igraph::is_named(x)) {
      names(results) <- igraph::V(x)$name[vids]
    }
  }
  return(results)
}


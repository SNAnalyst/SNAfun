
#' Compute Graph Efficiency
#' 
#' Computation of the efficiency of a network
#' 
#' This is a function to be able to calculate the 
#' efficiency of a network of class \code{igraph}.
#' 
#' The function that does the calculation is 
#' \code{\link[sna]{efficiency}}, which would require the conversion 
#' of the \code{igraph} object into a \code{network} object 
#' or a non-sparse adjacency matrix. 
#' 
#' This current function does the conversion to a non-sparse 
#' adjacency matrix under the hood and then feeds that to 
#'  \code{\link[sna]{efficiency}} for the actual calculation.
#'  
#'  Edge weights are not included in the algorithm, the graph is 
#'  dichotomized by default. 
#'  Make sure to not feed multiplex graphs to the algorithm, as it is not 
#'  obvious how to define graph efficiency in this case.
#'  
#'  From the \code{sna} help:
#'  
#'  Let G= G_1 U ... U G_n be a digraph with weak components 
#'  G_1,G_2,...,G_n. 
#'  For convenience, we denote the cardinalities of these 
#'  components' vertex sets by |V(G)|=N and |V(G_i)|=N_i, for 
#'  i in 1,...,n. 
#'  Then the Krackhardt efficiency of G is given by
#'  1 - ( |E(G)| - Sum(N_i-1,i=1,..,n) )/( Sum(N_i(N_i-1) - (N_i-1),i=1,..,n) )
#'  which can be interpreted as 1 minus the proportion of 
#'  possible 'extra' edges (above those needed to weakly connect the
#'  existing components) actually present in the graph. 
#'  A graph which an efficiency of 1 has precisely as many edges 
#'  as are needed to connect its components; as additional 
#'  edges are added, efficiency gradually falls towards 0.
#'  
#' @param g graph, as an object of class \code{igraph}, \code{network}, 
#' \code{matrix}, or \code{data.frame} (edgelist)
#' @param diag \code{TRUE} if the diagonal contains valid data; 
#' by default, \code{diag==FALSE}.
#'
#' @return
#' A single numeric value between 0 (completely inefficient graph) 
#' and 1 (maximally efficient graph).
#' The value 0 can occur when the network is disconnected.
#' 
#' @export
g_efficiency <- function(g, diag = FALSE) {
  if (inherits(g, "igraph")) {
    x <-  snafun::to_matrix(g)
  } else if (inherits(g, "network")) {
    x <-  snafun::to_matrix(g)
  } else if (inherits(g, "data.frame")) {
    x <-  snafun::to_matrix(g)
  } else if (inherits(g, "matrix")) {
    x <-  g
  } else {
    stop("'g' should be an igraph object, network object, edgelist, or an adjacency matrix")
  }

  if (nrow(x) != ncol(x)) {
    stop("'g' should be square")
  }

  # dichotomize the network, in case there are weights
  x <- snafun::to_binary_matrix(x, min = .001)
  
  sna::efficiency(x, diag = diag)
}

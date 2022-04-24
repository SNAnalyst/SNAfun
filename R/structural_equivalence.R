
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
#' through the \code{weight} argument. By default, no weights 
#' are used.
#' 
#' The value varies between \[-1, 1\], with -1 denoting maximally 
#' possible distance between two vertices and +1 denoting 
#' exact structural equivalence.
#'
#' @param g graph of class \code{igraph}
#' @param weight Either \code{NULL} (the default) or a character string giving 
#' an edge attribute name. 
#' If the graph has multiple edges, the edge attribute of an arbitrarily chosen 
#' edge (for the multiple edges) is included. 
#' Note that the function requires the attribute to be either logical or numeric.
#' @param digits number of decimals to use
#' @param suppressWarnings logical, whether or not any warnings should be returned
#'
#' @return matrix of size n*n, with n equal to the number of vertices in the graph
#' @export
#' @importFrom stats cor
#'
#' @examples
#' \dontrun{
#' #' data("judge_net", package = "SNA4DSData")
#' structural_equivalence(judge_net)
#' }
structural_equivalence <- function(g, weight = NULL, digits = 3,
                                   suppressWarnings = TRUE) {
  if (!inherits(g, "igraph")) {
    stop("'g' should be a graph of class 'igraph'.")
  }
  if (!is.null(weight)) {
    if (!is.character(weight)) {
      stop("If 'weight' is not 'NULL', it should be the name of an edge attribute.")
    } else if (!weight %in% igraph::edge_attr_names(g)) {
      stop("There is no edge attribute with the name you supplied in the 'weight' argument.")
    }
  }
  
  adj <- igraph::as_adjacency_matrix(g, type = "both",
                                       attr = weight,
                                       sparse = FALSE)
  diag(adj) <- NA
  d <- rbind(adj, t(adj))
  if (suppressWarnings) {
    suppressWarnings(round(cor(d, use = "pairwise"), digits = digits))
  } else {
    round(stats::cor(d, use = "pairwise"), digits = digits)
  }
}



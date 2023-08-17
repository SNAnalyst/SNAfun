
#' Intersection of graphs
#' 
#' The intersection of two or more graphs are created. 
#' The graphs may have identical or overlapping vertex sets.
#' 
#' \code{extract_intersection()} extracts the intersection of two or more graphs. 
#' The graphs to be 
#' intersected can be of class \code{igraph}, \code{network}, or \code{matrix}. 
#' Only edges present in all graphs will be included. 
#' 
#' If the \code{byname} argument is \code{TRUE} (or "auto" and all graphs are named), 
#' the operation is performed on symbolic vertex names instead of the internal 
#' numeric vertex ids.
#' 
#' \code{extract_intersection()} keeps the attributes of all graphs. All graph, 
#' vertex and edge attributes are copied to the result. 
#' If an attribute is present in multiple graphs and would result a name clash, 
#' then this attribute is renamed by adding suffixes: _1, _2, etc.
#' 
#' The \code{name} vertex attribute is treated specially if the operation is 
#' performed based on symbolic vertex names. 
#' In this case \code{name} must be present in all graphs, and it is not renamed 
#' in the result graph.
#' 
#' An error is generated if some input graphs are directed and others are undirected.
#' 
#' The actual intersection is performed through the \code{\link[igraph]{intersection.igraph}} 
#' function, which also is the basis of the documentation of this function.
#' 
#' What \code{extract_intersection()} mainly adds is the opportunity to intersect 
#' graphs of different classes, not just of class \code{igraph}. 
#' In fact, the intrepid can even intersect graphs of different types together 
#' (although that will rarely be useful). 
#' The output will be a graph with the same class as that of the first graph 
#' passed to this function.
#'
#' @param ... Graph objects or lists of graph objects. Allowed graph classes are 
#' \code{igraph}, \code{network}, \code{matrix}.
#' @param byname 	A logical scalar, or the character scalar \code{"auto"}. 
#' Whether to perform the operation based on symbolic vertex names. 
#' If it is "auto", that means \code{TRUE} if all graphs are named and 
#' \code{FALSE} otherwise. A warning is generated if "auto" and some 
#' (but not all) graphs are named.
#' @param keep.all.vertices	Logical, whether to keep vertices that only appear in a subset 
#' of the input graphs.
#'
#' @return a graph object of class \code{igraph}, \code{network}, or \code{matrix}.
#' @export
#'
#' @examples
#' net1 <- snafun::create_manual_graph(
#' D - A:B:F:G, A - C - F - A, B - E - G - B, A - B, F - G,
#' H - F:G, H - I - J
#' )
#' net2 <- snafun::create_manual_graph(D - A:F:Y, B - A - X - F - H - Z, F - Y)
#' extract_intersection(net1, net2)
#' 
#' g1 <- snafun::create_random_graph(10, "gnm", m = 15)
#' g2 <- snafun::create_random_graph(10, "gnm", m = 15)
#' extract_intersection(g1, g2)
#' 
#' # intersection of three graphs
#' g3 <- snafun::create_random_graph(15, "gnm", m = 20)
#' extract_intersection(g1, g2, g3)
extract_intersection <- function(..., byname = "auto", keep.all.vertices = TRUE) {
  kall <- as.list(match.call())[-1]
  n_graphs <- length(kall)
  if (n_graphs < 2) {
    stop("Please provide at least two graphs")
  }
  lijst <- vector(mode = "list", length = n_graphs)
  lijst[[1]] <- eval(kall[[1]])

  # get class of the first one before turning it into an igraph object
  if (inherits(lijst[[1]], "igraph")) {
    klasse <- "igraph"
  } else if (inherits(lijst[[1]], "network")) {
    klasse <- "network"
    lijst[[1]] <- snafun::to_igraph(lijst[[1]])
  } else if (inherits(lijst[[1]], "matrix")) {
    klasse <- "matrix"
    lijst[[1]] <- snafun::to_igraph(lijst[[1]])
  } else {
    stop("Provide actual graphs (igraph/network/matrix) as input")
  }
  
  for (g in 2:n_graphs) {
    lijst[[g]] <- eval(kall[[g]]) |> 
      snafun::to_igraph()
  }

  intersct <- utils::getFromNamespace(".igraph.graph.union.or.intersection", "igraph")
  intersctd <- intersct("intersection", lijst, byname = byname, keep.all.vertices = keep.all.vertices)
  
  # set output graph type to that of the first network provided
  if (klasse == "igraph") {
    return(intersctd)
  } else if (klasse == "network") {
      snafun::to_network(intersctd)
  } else if (klasse == "matrix") {
      snafun::to_matrix(intersctd)
  }
}






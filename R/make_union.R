
#' Union of graphs
#' 
#' The union of two or more graphs are created. 
#' The graphs may have identical or overlapping vertex sets.
#' 
#' \code{make_union()} creates the union of two or more graphs. The graphs to be 
#' combined can be of class \code{igraph}, \code{network}, or \code{matrix}. 
#' Edges which are included in at least one graph will be part of the new graph. 
#' 
#' If the \code{byname} argument is \code{TRUE} (or "auto" and all graphs are named), 
#' the operation is performed on symbolic vertex names instead of the internal 
#' numeric vertex ids.
#' 
#' \code{make_union()} keeps the attributes of all graphs. All graph, 
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
#' The actual union is performed through the \code{\link[igraph]{union}} function, 
#' which also is the basis of the documentation of this function.
#' 
#' What \code{make_union()} mainly adds is the opportunity to combine graphs of 
#' different classes, not just of class \code{igraph}. 
#' In fact, the intrepid can even combine graphs of different types together 
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
#'
#' @return a graph object of class \code{igraph}, \code{network}, or \code{matrix}.
#' @export
#'
#' @examples
#' net1 <- snafun::create_manual_graph(
#'   D - A:B:F:G, A - C - F - A, B - E - G - B, A - B, F - G,
#'   H - F:G, H - I - J
#' )
#' net2 <- snafun::create_manual_graph(D - A:F:Y, B - A - X - F - H - Z, F - Y)
#' net1_net2 <- make_union(net1, net2)
#' \dontrun{
#'   opar <- par()
#'   par("mfrow")
#'   snafun::plot(net1)
#'   snafun::plot(net2)
#'   snafun::plot(net1_net2)
#'   snafun::plot(igraph::union(net1, net2))
#'   par(opar)
#' }
#' 
#' make_union(snafun::to_network(net1), net2)
#' make_union(net1, snafun::to_network(net2))
#' make_union(snafun::to_matrix(net1), net2)
#' make_union(net1, snafun::to_matrix(net2))
#' 
#' g <- snafun::create_manual_graph(1-2, 1-3, 2-3, 2-4, 3-5, 4-5, 4-6, 4-7, 5-6, 6-7)
#' # Obtaining subgraphs
#' h <- snafun::extract_subgraph(g, v_to_keep = 1:5) # subgraph only contains vertices 1 to 5 
#' k <- snafun::create_manual_graph(4-6, 4-7, 5-6, 6-7) # create a new graph "k"
#' h_k <- make_union(h,k) # combine "h" and "k" graphs 
#' \dontrun{
#'   # Plotting the graphs 
#'   opar <- par()
#'   par(mfrow=c(1, 3), mar = c(0,0,0,1))
#'   set.seed(689)
#'   snafun::plot(h)
#'   snafun::plot(k)
#'   snafun::plot(h_k)
#'   par(opar)
#' }
#' 
#' # more than two graphs
#' # Note: the vertices end up in different orders, but the graphs are the same
#' net3 <- snafun::create_manual_graph(H-I:K, I-E, X-Q-Z)
#' snafun::make_union(net1, net2, net3)
#' snafun::make_union(net1, net3, net2)
#' snafun::make_union(net3, net2, net1)
make_union <- function(..., byname = "auto") {
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

  unite <- utils::getFromNamespace(".igraph.graph.union.or.intersection", "igraph")
  united <- unite("union", lijst, byname = byname, keep.all.vertices = TRUE)
  
  # set output graph type to that of the first network provided
  if (klasse == "igraph") {
    return(united)
  } else if (klasse == "network") {
      snafun::to_network(united)
  } else if (klasse == "matrix") {
      snafun::to_matrix(united)
  }
}






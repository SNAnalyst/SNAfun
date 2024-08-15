#' Find the Bottleneck centrality score
#'
#' How often is a vertex a bottleneck for other vertices in the graph? 
#' 
#' @details 
#' Consider the geodesics from \eqn{i} to all other vertices. 
#' If node \eqn{v} is part of at least 1/n-th of these geodesics, \eqn{v} is said 
#' to be a bottleneck for \eqn{i}.
#' By default, \code{n == 4}, so \eqn{v} is a bottleneck for \eqn{i}
#' if \eqn{v} is part of least one quarter of all of the geodesics from \eqn{i}.
#' 
#' The bottleneck centrality of \eqn{v} is the number of vertices that \eqn{v}
#' is a bottleneck for. 
#' 
#' The score runs from 0 (ie. the vertex is a bottleneck for no other vertex)
#' to \eqn{g - 1}, with \code{g} being the number of vertices in \code{graph.}
#' 
#' The calculation does not use edge weights, even if the graph is weighted 
#' (but the implementation could easily be altered to include weight as well).
#' 
#' Especially when densely connected subgroups exist, 
#' multiple shortest paths are possible between two vertices. 
#' If a vertex appear on at least one of them, that path counts as part of the 
#' calculation. 
#' This implies that being a _bottleneck_ does not take into account 
#' whether there are alternative geodesics between the two vertices; 
#' in other words, it does not matter whether the geodesic that \eqn{v} is 
#' part of is redundant or not and alternative geodesics exist between the 
#' pair of vertices that \eqn{v} is not part of and have the same length as 
#' the geodesic(s) that \eqn{v} is on. 
#' Hence, removal of the bottleneck node from the graph may not affect the 
#' efficiency of the graph in these cases.
#' 
#' Note that geodesics that end at \eqn{v} also count in the calculation. 
#' So, when there are multiple geodesics from \eqn{i} to \eqn{v}, 
#' it is likely that \eqn{v} will count as a bottleneck for \eqn{i}, 
#' although this may not be realistic. 
#' An alteration of this measure to discard geodesics to \eqn{v} might be advisable.
#' 
#' Also note that the implementation of this measure in the \code{centiserve} package 
#' is incorrect, so use our function and not theirs.
#' 
#' Also note that geodesics that end at \eqn{v} also count in the calculation. 
#' So, when there are multiple geodesics from \eqn{i} to \eqn{v}, 
#' it is likely that \eqn{v} will count as a bottleneck for \eqn{i}, 
#' although this may not be realistic. 
#' An alteration of this measure to discard geodesics to \eqn{v} might be advisable.
#' 
#' @param graph The input graph as igraph object, a network object, or a, 
#' adjacency matrix
#' @param mode Character constant, gives whether the shortest paths to or from 
#' the given vertices should be calculated for directed graphs. 
#' If \code{out} then the shortest paths from the vertex, if \code{in} then to 
#' it will be considered. If \code{all}, the default, then the corresponding 
#' undirected graph will be used. This argument is ignored for undirected graphs.
#' @param vids Numeric vertex sequence, the vertices that should be considered.
#' Default is all vertices. Otherwise, the operation is performed on the 
#' subgraph only containing vertices \code{vids}.
#' @param n scalar, defaults to 4.
#' @return A data.frame with vertex names (if they exist) and bottleneck 
#' centrality scores. 
#' Otherwise, a numeric vector contaning the centrality scores for the 
#' selected vertices.
#' @references Przulj, N., Dennis A. Wigle, and Igor Jurisica. 
#' "Functional topology in a network of protein interactions." 
#' Bioinformatics 20.3 (2004): 340-348.
#' @examples
#' g <- igraph::graph(c(1,2,2,3,3,4,4,2))
#' v_bottleneck(g)
#' v_bottleneck(g, vids = c(1, 2, 4))
#' v_bottleneck(g, mode = "out")
#' v_bottleneck(g, mode = "in")
#' 
#' g <- igraph::make_star(10, mode = "undirected")
#' v_bottleneck(g) 
#' g <- igraph::make_ring(10)
#' v_bottleneck(g) # all 0
#' v_bottleneck(g, n = Inf) # all 9
#' @export
v_bottleneck <- function (graph, 
                                   mode = c("all", "out", "in"), 
                                   vids = igraph::V(graph),
                                   n = 4
                                   ){
  
  if (inherits(graph, "network")) {
    graph <- snafun::to_igraph(graph)
  }
  
  if (inherits(graph, "matrix")) {
    graph <- snafun::to_igraph(graph)
  }
  
  if (!inherits(graph, "igraph")) {
    stop("'graph' should be an igraph object or be convertible to one")
  }
  
  induced <- FALSE
  if (!is.numeric(vids)) stop("'vids' must be a numeric vector")
  if (length(unique(vids)) < length(igraph::V(graph))) {
    graph <- igraph::induced_subgraph(graph, vids = vids)
    # igraph hernummert nu de vertices, daarvoor moeten we dus corrigeren
    vids_orig <- sort(vids) # voor de zekerheid, omdat igraph vids in numerieke volgorde toepast
    vids <- igraph::V(graph)
    induced <- TRUE
  } else if (any(!vids %in% igraph::V(graph))) {
      stop("You asked for vertices that are not present in the graph")
  }
  
  if (!igraph::is_igraph(graph)) {
    stop("Not a graph object", call. = FALSE)
  }

  # check vertex names
  v <- vids
  if (is.character(v) && "name" %in% igraph::list.vertex.attributes(graph)) {
    v <- as.numeric(match(v, igraph::V(graph)$name))
    if (any(is.na(v))) {
      stop("Invalid vertex names: there are NA's in the names")
    }
    vids <- v
  } else {
    if (is.logical(v)) {
      res <- as.vector(igraph::V(graph))[v]
    }
    else if (is.numeric(v) && any(v < 0)) {
      res <- as.vector(igraph::V(graph))[v]
    }
    else {
      res <- as.numeric(v)
    }
    if (any(is.na(res))) {
      stop("Invalid vertex name(s): there are NA's in the names")
    }
    vids = res
  }
  
  res <- matrix(0, igraph::vcount(graph), 1)
  rownames(res) <- igraph::V(graph)

  for(v in igraph::V(graph)){
    s <- table(unlist(igraph::all_shortest_paths(graph, from = v, 
                                                     to = igraph::V(graph), 
                                                     mode = mode[1], 
                                                     weights = NA)$res))
    v = as.character(v)
    for(i in names(s)[s > (length(s)/n)]){
      i <- as.character(i)
      if(i != v) res[i,1] <- res[i,1] + 1
    }
  }
  res <- res[, 1]
  if (induced) {names(res) <- vids_orig}
  
  if (igraph::is_named(graph)) {
    fin <- data.frame(name = igraph::V(graph)$name,
                      bottleneck = res)
  } else {
    fin <- data.frame(name = vids,
                      bottleneck = res)
  }
  
  fin
}


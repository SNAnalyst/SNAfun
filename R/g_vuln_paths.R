#' Vulnerability: paths
#' 
#' The vulnerability of a network, in terms of the vanishing of 
#' of a path between vertices.
#' 
#' When a vertex is removed from the network, some other vertices 
#' might not be able to reach each other anymore. 
#' This algorithm calculates the number of pairs of vertices 
#' that can no longer reach each other without vertex \code{i} 
#' in the network, but that were able to reach each other 
#' with \code{i} in the network. 
#' 
#' More formally, we compare the number of pairs of vertices that 
#' do not have some connecting path before and after removing 
#' vertex \code{i}. 
#' The function returns this change score for 
#' each vertex. 
#' The \code{i}th value is the number of pairs of vertices 
#' that become disconnected when vertex \code{i} is removed 
#' from the network (leaving everything else the same).
#' 
#' The output includes a column \code{vuln_prop} that 
#' is the ratio of the number of disconnected pairs after 
#' after 
#' removing the vertex and that before removing it (
#' correcting for the paths to and from \code{i}). 
#' For example, if this ratio is 1.5, then the number 
#' of disconnected pairs of vertices increases by 50% 
#' after removing vertex \code{i} (discarding of the paths 
#' to and from \code{i} itself).
#' A score of 1 means that there is no effect on the 
#' vertex-pair disconnectedness when the vertex is removed
#' from the graph.
#' A value of \code{Inf} occurs when all vertices were 
#' connected before \code{i} was removed, but after removing
#' \code{i} some became disconnected. 
#' This makes the proportion Infinite since the number 
#' is relative to the original number of disconnected 
#' vertices, which was 0.
#' 
#' @param g graph, as an object of class \code{igraph}, \code{network}, or
#' \code{matrix}
#' @param mode Character constant, gives whether the shortest paths to or from 
#' the vertices should be calculated for directed graphs. If \code{out} then the 
#' shortest paths in the direction of the edges is taken, if \code{in} then 
#' paths will go against the direction of the edge. 
#' If \code{all}, the default, then the corresponding undirected graph will be 
#' used, ie. edge direction is not taken into account. 
#' This argument is ignored for undirected graphs.
#' @param weight Possibly a numeric vector giving edge weights. 
#' If this is \code{NULL}--which is the default--and the graph 
#' has a \code{weight} edge attribute, then the attribute is used 
#' in the calculation of the distances. 
#' If this is \code{NA} then no weights are used 
#' (even if the graph has a \code{weight} attribute).
#' @param digits number of decimals for \code{vuln_prop}.
#'
#' @note This function is inspired by
#'  \code{\link[NetSwan]{swan_connectivity}}. 
#'  However, that function 
#' is incorrect when the network is not fully connected. 
#' Moreover, it does not correct for the ties to and from the 
#' vertex that is deleted. 
#' 
#' The implementation in the \code{snafun} package corrects this. 
#' In addition, it is more robust and more useful. 
#' 
#' @family vulnerability measures
#' @return a data.frame with a score per vertex
#' @export
g_vuln_paths <- function(g, 
                       mode = c("all", "out", "in"),
                       weight = NULL, digits = 3) {
  
  if (inherits(g, "network")) {
    g <- snafun::to_igraph(g)
  }
  
  if (inherits(g, "matrix")) {
    g <- snafun::to_igraph(g)
  }
  
  if (!inherits(g, "igraph")) {
    stop("'g' should be an igraph object or be convertible to one")
  }

  n <- length(igraph::V(g))
  fin <- prop <- matrix(ncol = 1, nrow = n, 0)
  dist <- igraph::distances(g, weight = weight)
  # 0 if reachable
  # 1 if not reachable
  dist[dist != Inf] <- 0
  dist[dist == Inf] <- 1
  # number of pairs of vertices that can not reach each other
  no_path <- sum(dist)
  mode <- mode[1]
  
  for (i in 1:n) {
    g2 <- g
    g2 <- igraph::delete_vertices(g2, i)
    dist2 <- igraph::distances(g2, mode = mode, weight = weight)
    dist2[dist2 != Inf] <- 0
    dist2[dist2 == Inf] <- 1
    no_path2 <- sum(dist2)
    # tel de "niet-paden" van i zelf in de oorsponkelijke graph
    # niet mee
    no_path_i <- sum(c(dist[, i], dist[i, ]))
    fin[i] <- no_path2 - (no_path - no_path_i)
    if (fin[i] == 0) {
      prop[i] <- 0   # als er geen wijziging is, dan 0 invullen
    } else {
      prop[i] <- no_path2/(no_path - no_path_i)
    }
  }

  if (igraph::is_named(g)) {
    fin <- data.frame(name = igraph::V(g)$name,
                      vulnerability = fin,
                      vuln_prop = prop)
  } else {
    fin <- data.frame(vulnerability = fin,
                      vuln_prop = prop)
  }
  
  fin$vuln_prop <- round(fin$vuln_prop, digits = digits)
  
  return(fin)
  }




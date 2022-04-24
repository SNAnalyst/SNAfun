
#' Make igraph object
#' 
#' Make an igraph object from various input types.
#' 
#' When a matrix is used as input, the function the number of rows is equal  
#' to the number of columns. If they do not, the function assumes the matrix 
#' refers to a bipartite network. This assumption can be overridden by 
#' the \code{bipartite} argument.
#' 
#' 
#'
#' @param x input object
#' @param bipartite logical, whether an adjacency matrix represents a bipartite 
#' network. Forces the creation of a bipartite igraph x. This argument is 
#' only used when a matrix is converted to an \code{igraph} object and is 
#' ignored otherwise.
#' @#rdname make_igraph
#' @export
#' @note The functions are largely based upon the \code{\link[migraph]{as_igraph}} 
#' functions. The versions in this package do not require tidyverse dependencies 
#' and do not deal with tidygraph.
#' @examples
#' # from a matrix
#' g <- igraph::sample_gnp(10, 2/10)
#' mat <- igraph::as_adjacency_matrix(g, sparse = FALSE)
#' make_igraph(mat)
#' 
#' g <- igraph::make_ring(10)
#' igraph::E(g)$weight <- seq_len(igraph::ecount(g))
#' mat <- igraph::as_adjacency_matrix(g, sparse = FALSE, attr = "weight")
#' make_igraph(mat)
#' 
#' # bipartite network, even nodes are one type, odd vertices another type
#' g <- igraph::make_bipartite_graph( rep(0:1,length=10), c(1:10))
#' mat <- igraph::as_adjacency_matrix(g, sparse = FALSE)
#' make_igraph(mat)  # same network, but not officially bipartite
#' mat <- igraph::as_incidence_matrix(g, sparse = FALSE)
#' make_igraph(mat, bipartite = TRUE)
make_igraph <- function(x, bipartite = FALSE) {
  UseMethod("make_igraph")
}



#' @export
#' @describeIn make_igraph Make an igraph object from a matrix
make_igraph.matrix <- function(x, bipartite = FALSE) {
  if (nrow(x) != ncol(x) | bipartite) {
    if (!(all(x %in% c(0, 1)))) {
      graph <- igraph::graph_from_incidence_matrix(x,
                          weighted = TRUE, directed = FALSE)
    } else {
      graph <- igraph::graph_from_incidence_matrix(x, directed = FALSE)
    }
  } else {
    if (!(all(x %in% c(0, 1)))) {
      graph <- igraph::graph_from_adjacency_matrix(x,
                          mode = ifelse(all(x == t(x)), "undirected", "directed"),
                          weighted = TRUE)
    } else {
      graph <- igraph::graph_from_adjacency_matrix(x,
                          mode = ifelse(all(x == t(x)), "undirected", "directed"))
    }
  }
  graph
}



#' @export
#' @describeIn make_igraph Make an igraph object from a network object
make_igraph.network <- function (x, bipartite = FALSE) {
  if (network::is.hyper(x)) 
    stop("hypergraphs are not supported")
  attr <- names(x[[3]][[1]])
  isna <- which(attr == "na")
  if (length(isna) > 0) {
    attr <- attr[-isna]
  }
  
  
  if (network::is.bipartite(x)) {
    if ("weight" %in% network::list.edge.attributes(x)) {
      graph <- sna::as.sociomatrix.sna(x, attrname = "weight")
      graph <- igraph::graph_from_incidence_matrix(graph, weighted = TRUE)
    } else {
      graph <- sna::as.sociomatrix.sna(x)
      graph <- igraph::graph_from_incidence_matrix(graph)
    }
  } else {
    if ("weight" %in% network::list.edge.attributes(x)) {
      graph <- sna::as.sociomatrix.sna(x, attrname = "weight")
      graph <- igraph::graph_from_adjacency_matrix(graph, 
                    weighted = TRUE, mode = ifelse(x$gal$directed, "directed", "undirected"))
    } else if (length(network::list.edge.attributes(x)) > 1) {
      x$gal$multiple <- FALSE
      graph <- sna::as.sociomatrix.sna(x, attrname = network::list.edge.attributes(x)[1])
      graph <- igraph::graph_from_adjacency_matrix(graph, 
                    weighted = TRUE, mode = ifelse(x$gal$directed, "directed", "undirected"))
    } else {
      graph <- sna::as.sociomatrix.sna(x)
      graph <- igraph::graph_from_adjacency_matrix(graph, 
                    mode = ifelse(x$gal$directed, "directed", "undirected"))
    }
  }
  
  if (!inherits(graph, "igraph")) {
    graph <- make_igraph(graph)
  }
  
  if (length(attr) > 1) {
    for (a in attr[1:length(attr)]) {
      values <-  sapply(x[[3]], "[[", a)
      igraph::vertex_attr(graph, name = a) <- values
    }
  }
  graph
}



#' @export
#' @describeIn make_igraph In case you feed an igraph object to this function: the object is returned unaltered
make_igraph.igraph <- function(x, bipartite = FALSE) {
  x
}


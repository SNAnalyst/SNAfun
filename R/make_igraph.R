
#' Make igraph object
#' 
#' Make an igraph object from various input types.
#' 
#' The following inputs are supported:
#' \describe{
#' \item{network object}{\code{network} object created by the network package}
#' \item{matrix}{This is a base R type matrix. When a matrix is used as input, 
#' the function the number of rows is equal to the number of columns. 
#' If they do not, the function assumes the matrix refers to a bipartite network. 
#' This assumption can be overridden by the \code{bipartite} argument.}
#' \item{data.frame}{The \code{data.frame} contains an edge list. 
#' The data.frame requires the first column to contain the senders and the 
#' second column contains the receivers. If there are any additional 
#' columns, these are considered to be edge attributes.
#' 
#' NOTE: The created \code{igraph} object is considered to be directed. 
#' If an undirected network is required, run \code{\link[igraph]{as.undirected}} 
#' on the output from this function.}
#' }
#' 
#' 
#' 
#' When a matrix is used as input, the function the number of rows is equal  
#' to the number of columns. If they do not, the function assumes the matrix 
#' refers to a bipartite network. This assumption can be overridden by 
#' the \code{bipartite} argument.
#' 
#' 
#' #### add vertices = NULL argument!!!!!
#' #### add directed = F argument???
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
#' 
#' relations <- data.frame(from = c("Bob", "Cecil", "Cecil", "David", 
#'     "David", "Esmeralda"), 
#'     to = c("Alice", "Bob", "Alice", "Alice", "Bob", "Alice"),
#'     same.dept = c(FALSE, FALSE, TRUE, FALSE, FALSE, TRUE), 
#'     friendship = c(4, 5, 5, 2, 1, 1), advice = c(4, 5, 5, 4, 2, 3))
#' make_igraph(relations)
#' 
#' aa <- data.frame(from = c(1,1,2,2,3,3,4,4), 
#'     to = c(11, 12, 13, 14, 15, 16, 17, 18))
#' make_igraph(aa)  # message is given if this should ne bipartite
#' make_igraph(aa, bipartite = TRUE)  
make_igraph <- function(x, bipartite = FALSE) {
  UseMethod("make_igraph")
}



#' @export
make_igraph.default <- function(x, bipartite = FALSE) {
  stop("'x' should be of class 'matrix', 'network', 'igraph', or 'data.frame'")
}



#' @export
#' @describeIn make_igraph
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
#' @describeIn make_igraph
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
#' @describeIn make_igraph
make_igraph.igraph <- function(x, bipartite = FALSE) {
  x
}



#' @export
#' @describeIn make_igraph
make_igraph.data.frame <- function(x,
                                 bipartite = FALSE) {
  # just in case this is done by a tidyverse user
  if (inherits(x, "tbl_df")) object <- as.data.frame(x)
  graph <- igraph::graph_from_data_frame(x, directed = TRUE)
  # make bipartite 
  if (bipartite) {
    if (length(intersect(c(x[,1]), c(x[, 2]))) == 0) { 
      igraph::V(graph)$type <- igraph::V(graph)$name %in% x[, 2]
    } else {
      stop("'You asked for a bipartite network, but there are overlapping 
           sender and receiver names. 
           Please correct this.'")
    }
  } else {
    if (length(intersect(c(x[,1]), c(x[, 2]))) == 0) { 
      message("You ask for a unipartite network, but you have non-overlapping 
              senders and receivers. 
              This is fine if the network should indeed not be bipartite. 
              If it should, please specify 'bipartite == TRUE'.")
    }
  }
  return(graph)
}




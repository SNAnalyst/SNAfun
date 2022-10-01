
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
#' NOTE: The created \code{igraph} object is considered to be directed, 
#' depending on the structure of the input. 
#' If an undirected network is required, 
#' run \code{\link[igraph]{as.undirected}} on the output from this function.}
#' }
#' 
#' When a matrix is used as input, the function the number of rows is equal  
#' to the number of columns. If they do not, the function assumes the matrix 
#' refers to a bipartite network. This assumption can be overridden by 
#' the \code{bipartite} argument.
#' 
#' The \code{vertices} argument is only used when \code{x} is a 
#' \code{data.frame} and is ignored otherwise. 
#' If \code{vertices} is NULL, then the first two columns of \code{x} are used 
#' as a symbolic edge list and additional columns as edge attributes. 
#' The names of the attributes are taken from the names of the columns.
#' 
#' If \code{vertices} is not \code{NULL}, then it must be a data frame 
#' giving vertex metadata. The first column of vertices is assumed to contain
#' symbolic vertex names, this will be added to the graphs as the \code{‘name’} 
#' vertex attribute. Other columns will be added as additional 
#' vertex attributes. If \code{vertices} is not \code{NULL} 
#' then the symbolic edge list given in \code{x} is checked to contain only
#' vertex names listed in vertices. 
#' See \code{\link[igraph]{graph_from_data_frame}} for the underlying function.
#' 
#' @param x input object
#' @param bipartite logical, whether an adjacency matrix represents a bipartite 
#' network. Forces the creation of a bipartite igraph x. This argument is 
#' only used when a matrix is converted to an \code{igraph} object and is 
#' ignored otherwise.
#' @param vertices A data frame with vertex metadata, or \code{NULL}. See details below.
#' @export
#' @note The functions are largely based upon the \code{\link[migraph]{as_igraph}} 
#' functions. The versions in the \code{snafun} package do not require 
#' the tidyverse dependencies of \code{migraph}
#' and do not deal with tidygraph.
#' @examples
#' # from a matrix
#' g <- igraph::sample_gnp(10, 2/10)
#' mat <- igraph::as_adjacency_matrix(g, sparse = FALSE)
#' to_igraph(mat)
#' 
#' g <- igraph::make_ring(10)
#' igraph::E(g)$weight <- seq_len(igraph::ecount(g))
#' mat <- igraph::as_adjacency_matrix(g, sparse = FALSE, attr = "weight")
#' to_igraph(mat)
#' 
#' # bipartite network, even nodes are one type, odd vertices another type
#' g <- igraph::make_bipartite_graph( rep(0:1,length=10), c(1:10))
#' mat <- igraph::as_adjacency_matrix(g, sparse = FALSE)
#' to_igraph(mat)  # same network, but not officially bipartite
#' mat <- igraph::as_incidence_matrix(g, sparse = FALSE)
#' to_igraph(mat, bipartite = TRUE)
#' 
#' relations <- data.frame(from = c("Bob", "Cecil", "Cecil", "David", 
#'     "David", "Esmeralda"), 
#'     to = c("Alice", "Bob", "Alice", "Alice", "Bob", "Alice"),
#'     same.dept = c(FALSE, FALSE, TRUE, FALSE, FALSE, TRUE), 
#'     friendship = c(4, 5, 5, 2, 1, 1), advice = c(4, 5, 5, 4, 2, 3))
#' to_igraph(relations)
#' 
#' \dontrun{
#' aa <- data.frame(from = c(1,1,2,2,3,3,4,4), 
#'     to = c(11, 12, 13, 14, 15, 16, 17, 18))
#' to_igraph(aa)  # message is given if this should ne bipartite
#' to_igraph(aa, bipartite = TRUE)  
#' }
to_igraph <- function(x, bipartite = FALSE,
                      vertices = NULL) {
  UseMethod("to_igraph")
}



#' @export
to_igraph.default <- function(x, bipartite = FALSE,
                              vertices = NULL) {
  txt <- methods_error_message("x", "to_igraph")
  stop(txt)
}



#' @export
to_igraph.matrix <- function(x, bipartite = FALSE,
                             vertices = NULL) {
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
                          mode = ifelse(isSymmetric(x), "undirected", "directed"),
                          weighted = TRUE)
    } else {
      graph <- igraph::graph_from_adjacency_matrix(x,
                          mode = ifelse(isSymmetric(x), "undirected", "directed"))
    }
  }
  # simplify, because loops will otherwise occur twice each
  graph <- igraph::simplify(graph, remove.multiple = TRUE, remove.loops = FALSE)
  graph
}



#' @export
to_igraph.network <- function (x, bipartite = FALSE,
                               vertices = NULL) {
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
    # } else if (length(network::list.edge.attributes(x)) > 1) {
    #   x$gal$multiple <- FALSE
    #   graph <- sna::as.sociomatrix.sna(x, attrname = network::list.edge.attributes(x)[1])
    #   graph <- igraph::graph_from_adjacency_matrix(graph, 
    #                 weighted = TRUE, mode = ifelse(x$gal$directed, "directed", "undirected"))
    } else {
      graph <- sna::as.sociomatrix.sna(x)
      graph <- igraph::graph_from_adjacency_matrix(graph, 
                    mode = ifelse(x$gal$directed, "directed", "undirected"))
    }
  }
  
  # edge attribs 
  if (has_edge_attributes(x)) { # there are edge attribs to copy over
    edges_network <- to_edgelist(x)
    # note the need for overwrite = TRUE (try flomar_network)
    graph <- add_edge_attributes(graph, edgelist = edges_network, overwrite = TRUE)
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
to_igraph.igraph <- function(x, bipartite = FALSE,
                             vertices = NULL) {
  x
}



#' @export
to_igraph.data.frame <- function(x,
                                 bipartite = FALSE,
                                 vertices = NULL) {
  # just in case this is done by a tidyverse user
  if (inherits(x, "tbl_df")) x <- as.data.frame(x)
  graph <- tryCatch(igraph::graph_from_data_frame(x, directed = TRUE, vertices = vertices), error = function(e) e)
  if (inherits(graph, "simpleError")) {
    if (graph$message == "Some vertex names in edge list are not listed in vertex data frame") {
      stop("Some vertices that occur in your edgelist are 
           missing in 'vertices'. Make sure all vertices are 
           included in 'vertices'.")
    }
  }
  
  # make bipartite 
  if (bipartite) {
    if (length(intersect(c(x[,1]), c(x[, 2]))) == 0) { 
      igraph::V(graph)$type <- igraph::V(graph)$name %in% x[, 2]
    } else {
      stop("'NOTE: You asked for a bipartite network, but there are overlapping 
           sender and receiver names. 
           Please correct this.'")
    }
  } else {
    if (length(intersect(c(x[,1]), c(x[, 2]))) == 0) { 
      message("NOTE: You ask for a unipartite network, but you have 
              non-overlapping senders and receivers. 
              This is fine if the network should indeed not be bipartite. 
              If it should, please specify 'bipartite == TRUE'.")
    }
  }
  return(graph)
}




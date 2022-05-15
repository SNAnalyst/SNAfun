


#' Make network object
#' 
#' Make a network object from various input types.
#' 
#' The following inputs are supported:
#' \describe{
#' \item{\code{network} object}{\code{network} object}
#' \item{\code{igraph} object}{\code{igraph} object}
#' \item{\code{matrix}}{This is a base R type \code{matrix}. 
#' When a matrix is used as input, 
#' the function the number of rows is equal to the number of columns. 
#' If they are not, the function assumes the matrix refers to a bipartite network. 
#' This assumption can be overridden by the \code{bipartite} argument.}
#' \item{\code{data.frame}}{The \code{data.frame} contains an edge list. 
#' The data.frame requires the first column to contain the senders and the 
#' second column contains the receivers. If there are any additional 
#' columns, these are considered to be edge attributes.
#' 
#' NOTE: The created \code{network} object is considered to be directed. 
#' If an undirected network is required, run \code{\link[igraph]{as.undirected}} 
#' on the output from this function.}
#' }
#' 
#' NOTE: as far as we know, this function is the only function that correctly 
#' deals with vertex attributes that themselves are lists in the conversion 
#' from \code{igraph} (which allows lists) to \code{network} (which can not 
#' deal with lists as attributes).
#' 
#' @param x graph data object
#' @param bipartite logical, should the graph be considered to be bipartite? 
#' This overrides the function's default choices.
#'
#' @return a \code{network} object
#' @export
to_network <- function(x, bipartite = FALSE) {
  UseMethod("to_network")
}


#' @export
to_network.default <- function(x, bipartite = FALSE) {
  txt <- methods_error_message("x", "to_network")
  stop(txt)
}


#' @export
to_network.network <- function(x, bipartite = FALSE) {
  x
}



#' @export
to_network.matrix <- function(x, bipartite = FALSE) {
  if ((ncol(x) != nrow(x)) && !bipartite) {
    stop("'x' should be square if a onemode network is to be created")
  }
  
  if (bipartite || is.integer(bipartite)) {
    network::as.network(x, 
                        directed = is_directed(x),
                        bipartite   = bipartite,
                        ignore.eval = ifelse(is_weighted(x), FALSE, TRUE),
                        names.eval  = ifelse(is_weighted(x), "weight", NULL))
  } else {   # onemode network
    network::as.network(x, 
                      directed = is_directed(x),
                      bipartite   = FALSE,
                      loops = ifelse(sum(diag(x)) > 0, TRUE, FALSE),
                      ignore.eval = ifelse(is_weighted(x), FALSE, TRUE),
                      names.eval  = ifelse(is_weighted(x), "weight", NULL))
  }
}



##############################
##############################edge attributes aren't copied over yet!!!

#' @export
to_network.igraph <- function(x, bipartite = NULL) {
  bip <- (snafun::is_bipartite(x) || isTRUE(bipartite))
  name <- type <- NULL
  attrs <- extract_all_vertex_attributes(x)
  
  mat <- withWarnings(to_matrix(x))
  if (!is.null(mat$warnings)) {
    if (grepl("within partitions", mat$warnings[[1]])) {
      warning("There are edges between vertices of the same type, 
              this is typically undesirable in bipartite networks")
    }
  }
  mat <- mat$value
  
  if (bip) {
    res <- to_network(mat, bipartite = nrow(mat))
  } else {
    res <- to_network(mat, bipartite = FALSE)
  }
  
  if (is.null(attrs)) {
    res <- mat
  } else {
    already_included <- which(colnames(attrs) %in% c("name", "type"))
    if (length(already_included) > 0) {
      attrs <- attrs[-which(colnames(attrs) %in% c("name", "type"))]
    }
    if (ncol(attrs) > 0) {
      res <- network::set.vertex.attribute(res, names(attrs), attrs)
    }
  }
  if (inherits(res, "matrix")) {res <- network::as.network.matrix(res)}
  res
}



#' @export
to_network.data.frame <- function(x, bipartite = FALSE) {
  if (inherits(x, "tbl_df")) x <- as.data.frame(x)
  out <- network::as.network(x)
  if (bipartite) {
    mat <- network::as.matrix.network.adjacency(out)
    out$gal$bipartite <- nrow(mat)
  }
  out
}

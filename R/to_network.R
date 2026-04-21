


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
#' Edgelists created by \code{snafun::to_edgelist()} preserve the original
#' directedness through hidden metadata. For plain external edgelists,
#' \code{snafun} infers an undirected one-mode network only when every visible
#' row has an exact reciprocal counterpart; otherwise the safest default is a
#' directed network. Use \code{directed = FALSE} when a plain external edgelist
#' lists each undirected edge only once.}
#' }
#' 
#' The argument \code{bipartite} is used to tell the function whether the input 
#' represents a bipartite network or not. If it does, then set \code{bipartite} 
#' to \code{TRUE} and the function will attempt to convert the input to a 
#' bipartite network. 
#' However, the argument is NOT intended to convert a bipartite network to 
#' a unipartite network and will NOT do that conversion! 
#' 
#' NOTE: as far as we know, this function is the only function that correctly 
#' deals with vertex attributes that themselves are lists in the conversion 
#' from \code{igraph} (which allows lists) to \code{network} (which can not 
#' deal with lists as attributes).
#' 
#' When the input edgelist was created by \code{snafun::to_edgelist()}, hidden
#' vertex metadata are reused automatically so isolates can survive a graph ->
#' edgelist -> network roundtrip.
#' 
#' @param x graph data object
#' @param bipartite logical, should \code{x} be considered to be bipartite? 
#' This overrides the function's default choices.
#' @param directed Optional logical override that is only used for
#' \code{data.frame} edgelists. Leave \code{NULL} to let \code{snafun} infer
#' the most plausible interpretation from hidden metadata and the visible rows.
#' Set \code{FALSE} to force an undirected one-mode network when a plain
#' external edgelist only lists each undirected edge once.
#'
#' @return a \code{network} object
#' @export
to_network <- function(x, bipartite = FALSE, directed = NULL) {
  UseMethod("to_network")
}


#' @export
to_network.default <- function(x, bipartite = FALSE, directed = NULL) {
  txt <- methods_error_message("x", "to_network")
  stop(txt)
}


#' @export
to_network.network <- function(x, bipartite = FALSE, directed = NULL) {
  network::network(x, bipartite = bipartite, directed = is_directed(x))
}



#' @export
to_network.matrix <- function(x, bipartite = FALSE, directed = NULL) {
  if ((ncol(x) != nrow(x)) && !bipartite) {
    stop("'x' should be square if a onemode network is to be created")
  }
  
  if (isTRUE(bipartite)) {
    # The network package expects the bipartite argument to be the size of the
    # first partition, not merely a logical flag. snafun users understandably
    # pass bipartite = TRUE for incidence matrices, so we translate that here
    # to the row partition size and keep the public API consistent.
    bipartite <- nrow(x)
  }
  
  if (is.integer(bipartite) || (is.numeric(bipartite) && length(bipartite) == 1 && !is.na(bipartite) && bipartite > 0)) {
    # Rectangular matrices should become bipartite networks. The network
    # package needs matrix.type = "bipartite" here so it initializes the full
    # row+column vertex set instead of treating the matrix as a one-mode
    # incidence object with too few vertices.
    network::as.network.matrix(
      x,
      matrix.type = "bipartite",
      directed = FALSE,
      bipartite = bipartite,
      ignore.eval = ifelse(is_weighted(x), FALSE, TRUE),
      names.eval = ifelse(is_weighted(x), "weight", NULL)
    )
  } else {   # onemode network
    network::as.network.matrix(
      x,
      matrix.type = "adjacency",
      directed = is_directed(x),
      bipartite = FALSE,
      loops = ifelse(sum(diag(x)) > 0, TRUE, FALSE),
      ignore.eval = ifelse(is_weighted(x), FALSE, TRUE),
      names.eval = ifelse(is_weighted(x), "weight", NULL)
    )
  }
}






#' @export
to_network.igraph <- function(x, bipartite = NULL, directed = NULL) {
  bip <- (snafun::is_bipartite(x) || isTRUE(bipartite))
  name <- type <- NULL
  vattrs <- extract_all_vertex_attributes(x)
  eattrs <- to_edgelist(x)
  
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
  
  if (!is.null(vattrs)) {
    # `res` already has the correct directed/bipartite structure from the
    # matrix conversion above. We only need this block to copy over additional
    # vertex attributes; rebuilding the network here would risk dropping the
    # bipartite interpretation for rectangular matrices.
    already_included <- which(colnames(vattrs) %in% c("name", "type"))
    if (length(already_included) > 0) {
      vattrs <- vattrs[-which(colnames(vattrs) %in% c("name", "type"))]
    }
    if (ncol(vattrs) > 0) {
      # network::set.vertex.attribute() expects one attribute/value vector per
      # call. Passing the full data.frame at once can silently recycle the
      # values, which becomes especially visible for bipartite graphs. We
      # therefore copy each vertex attribute column explicitly.
      for (attribute_name in colnames(vattrs)) {
        res <- network::set.vertex.attribute(
          res,
          attrname = attribute_name,
          value = vattrs[[attribute_name]]
        )
      }
    }
  }

  # add edge attributes if they are present in the data
  if (ncol(eattrs) > 2) {
    edges <- eattrs[, c("from", "to")]
    namen <- network::get.vertex.attribute(res, "vertex.names")
    if (!is.numeric(edges$from)) {
      edges$from <- match(edges$from, namen)
    }
    if (!is.numeric(edges$to)) {
      edges$to <- match(edges$to, namen)
    }
    eids <- extract_edge_id(res, edgelist = edges, ordered = FALSE)[, "eid"]
    eatt_names <- setdiff(colnames(eattrs), c("from", "to"))
    for (eat in eatt_names) {
      network::set.edge.attribute(res, attrname = eat,
                                  value = eattrs[, eat], 
                                  e = eids)
    }
  }
  if (inherits(res, "matrix")) {res <- network::as.network.matrix(res)}
  res
}



#' @export
to_network.data.frame <- function(x, bipartite = FALSE, directed = NULL) {
  bipartite_was_missing <- missing(bipartite)
  if (inherits(x, "tbl_df")) x <- as.data.frame(x)
  graph <- suppressMessages(
    snafun::to_igraph(
      x,
      bipartite = if (bipartite_was_missing) NULL else bipartite,
      directed = directed
    )
  )
  snafun::to_network(
    graph,
    bipartite = if (bipartite_was_missing) NULL else bipartite
  )
}


#' Make edgelist
#' 
#' Make an edgelist from a set of input objects. This function doesn't do 
#' anything that \code{igraph} or \code{network}/\code{sna} can't also already 
#' do. The usefulness of this function is its consistent API.
#' 
#' Currently, input can be a matrix, \code{igraph} object or \code{network} object.
#' 
#' @param x input object
#' @param named logical, should vertex names be returned (if they exist) (\code{TRUE})
#' or not (\code{FALSE})
#' @param sort character, name of the column to sort on. Defaults to "from".
#'
#' @return data frame
#' @export
#' @rdname to_edgelist
#' @examples
#' ## from an igraph object
#' g_i <- igraph::sample_gnp(10, 2/10)
#' to_edgelist(g_i)
#' 
#' # add vertex names
#' g1_i <- g_i
#' igraph::V(g1_i)$name <- LETTERS[seq_len(igraph::gorder(g1_i))]
#' to_edgelist(g1_i)
#' # add weights
#' igraph::E(g1_i)$weight <- seq_len(igraph::ecount(g1_i))
#' to_edgelist(g1_i)
#' # add further edge attribute
#' g2_i <- g1_i |> 
#'     igraph::set_edge_attr("color", value = "red")
#' to_edgelist(g2_i)
#' 
#' ## from a matrix
#' g_n <- sapply(runif(10, 0, 1), rep, 10)
#' g_n <- sna::rgraph(10, tprob = g_n)
#' to_edgelist(g_n)
#' g_b <- igraph::make_bipartite_graph(c(rep(0, 5), rep(1, 3)), c(1,6,2,7,3,6,3,7,4,8,5,8))
#' to_edgelist(g_b)
#' 
#' ## from a network object
#' g2_n <- network::as.network.matrix(g_n)
#' to_edgelist(g2_n)
to_edgelist <- function(x, named = TRUE, sort = "from") {
  UseMethod("to_edgelist")
}


#' @export
to_edgelist.default <- function(x, named = TRUE, sort = "from") {
  txt <- methods_error_message("x", "to_edgelist")
  stop(txt)
}


#' @export
to_edgelist.igraph <- function(x, named = TRUE, sort = "from") {
  if (!named & has_vertexnames(x)) {
    x <- igraph::delete_vertex_attr(x, "name")
  }
  el <- igraph::get.data.frame(x)

  if (!sort %in% colnames(el)) {
    stop("You did not provide an appropriate name for the 'sort' column")
  }
  rownames(el) <- NULL
  el[order(el[, sort]), ]
}


#' @export
to_edgelist.network <- function(x, named = TRUE, sort = "from") {
  if (!named) {
    network::delete.vertex.attribute(x, "vertex.names")
  }
  el <- network::as.data.frame.network(x, unit = "edges", na.rm = FALSE)
  colnames(el)[c(1, 2)] <- c("from", "to")
  if (!named) { # needed for the sorting
    el$from <- as.integer(el$from)
    el$to <- as.integer(el$to)
  }
  if (!sort %in% colnames(el)) {
    stop("You did not provide an appropriate name for the 'sort' column")
  }
  rownames(el) <- NULL
  el[order(el[, sort]), ]
}



#' @export
to_edgelist.matrix <- function(x, named = TRUE, sort = "from"){
  x <- to_igraph(x)
  el <- to_edgelist(x, named = named)
  if (!sort %in% colnames(el)) {
    stop("You did not provide an appropriate name for the 'sort' column")
  }
  rownames(el) <- NULL
  el[order(el[, sort]), ]
}


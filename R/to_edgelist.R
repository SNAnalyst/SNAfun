
#' Make edgelist
#' 
#' Make an edgelist from a set of input objects. This function doesn't do 
#' anything that \code{igraph} or \code{network}/\code{sna} can't also already 
#' do. The usefulness of this function is its consistent API.
#' 
#' Currently, input can be a matrix, \code{igraph} object or \code{network} object.
#' 
#' When the input is a graph object, the returned data.frame silently stores
#' vertex metadata as attributes. This allows \code{snafun::to_igraph()},
#' \code{snafun::to_network()}, and \code{snafun::to_matrix()} to reconstruct
#' isolates during a roundtrip through an edgelist. These hidden attributes are
#' intentionally lightweight and will often disappear when the edgelist is
#' heavily edited, subsetted, or written to disk, in which case the conversion
#' falls back to the information visible in the edgelist itself.
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
#' g_i <- snafun::create_random_graph(10, "gnp", p = 2/10, graph = "igraph")
#' to_edgelist(g_i)
#' 
#' # add vertex names
#' g1_i <- g_i
#' g1_i <- snafun::add_vertex_names(
#'   g1_i,
#'   LETTERS[seq_len(snafun::count_vertices(g1_i))]
#' )
#' to_edgelist(g1_i)
#' # add weights
#' g1_i <- snafun::add_edge_attributes(
#'   g1_i,
#'   "weight",
#'   value = seq_len(snafun::count_edges(g1_i))
#' )
#' to_edgelist(g1_i)
#' # add further edge attribute
#' g2_i <- snafun::add_edge_attributes(
#'   g1_i,
#'   "color",
#'   value = rep("red", snafun::count_edges(g1_i))
#' )
#' to_edgelist(g2_i)
#' 
#' ## from a matrix
#' g_n <- snafun::to_matrix(
#'   snafun::create_random_graph(10, "gnp", p = .2, graph = "igraph")
#' )
#' to_edgelist(g_n)
#' g_b <- snafun::create_bipartite(5, 3, strategy = "gnm", m = 6)
#' to_edgelist(g_b)
#' 
#' ## from a network object
#' g2_n <- snafun::to_network(g_n)
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
  vertex_metadata <- collect_edgelist_vertex_metadata(x = x, named = named)
  if (!named & has_vertexnames(x)) {
    x <- igraph::delete_vertex_attr(x, "name")
  }
  el <- igraph::get.data.frame(x)

  if (!sort %in% colnames(el)) {
    stop("You did not provide an appropriate name for the 'sort' column")
  }
  rownames(el) <- NULL
  el <- el[order(el[, sort]), ]
  attach_edgelist_metadata(
    edgelist = el,
    vertex_metadata = vertex_metadata,
    bipartite = snafun::is_bipartite(x),
    directed = snafun::is_directed(x)
  )
}


#' @export
to_edgelist.network <- function(x, named = TRUE, sort = "from") {
  vertex_metadata <- collect_edgelist_vertex_metadata(x = x, named = named)
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
  if (snafun::is_bipartite(x)) {
    partition_size <- network::get.network.attribute(x, "bipartite")
    first_partition <- seq_len(partition_size)
    if (named) {
      vertex_names <- network::get.vertex.attribute(x, "vertex.names")
      first_partition <- vertex_names[first_partition]
    }
    from_in_first <- el$from %in% first_partition
    to_in_first <- el$to %in% first_partition
    swap_rows <- !from_in_first & to_in_first
    if (any(swap_rows)) {
      tmp_from <- el$from[swap_rows]
      el$from[swap_rows] <- el$to[swap_rows]
      el$to[swap_rows] <- tmp_from
    }
  }
  rownames(el) <- NULL
  el <- el[order(el[, sort]), ]
  attach_edgelist_metadata(
    edgelist = el,
    vertex_metadata = vertex_metadata,
    bipartite = snafun::is_bipartite(x),
    directed = snafun::is_directed(x)
  )
}



#' @export
to_edgelist.matrix <- function(x, named = TRUE, sort = "from"){
  x <- to_igraph(x)
  el <- to_edgelist(x, named = named)
  if (!sort %in% colnames(el)) {
    stop("You did not provide an appropriate name for the 'sort' column")
  }
  vertex_metadata <- extract_stored_edgelist_vertices(el)
  bipartite <- extract_stored_edgelist_bipartite(el)
  rownames(el) <- NULL
  el <- el[order(el[, sort]), ]
  attach_edgelist_metadata(
    edgelist = el,
    vertex_metadata = vertex_metadata,
    bipartite = bipartite,
    directed = extract_stored_edgelist_directed(el)
  )
}


#' Collect vertex metadata for edgelist roundtrips
#'
#' Build the hidden vertex table that \code{snafun} attaches to edgelists so
#' isolates can survive a graph -> edgelist -> graph conversion. The first
#' column deliberately mirrors the representation used in the visible edgelist:
#' vertex names when \code{named = TRUE}, otherwise numeric vertex ids.
#'
#' Any existing vertex-name attribute is removed from the auxiliary attribute
#' table to avoid duplicate columns that would confuse the reconstruction step.
#' For bipartite \code{network} objects we explicitly derive a logical
#' \code{type} indicator, because the \code{network} class stores the partition
#' size as a graph attribute rather than a per-vertex attribute.
#'
#' @param x graph object of class \code{igraph} or \code{network}
#' @param named logical, should the stored key column use visible vertex names?
#'
#' @return A data.frame with one row per vertex.
#' @keywords internal
#' @noRd
collect_edgelist_vertex_metadata <- function(x, named = TRUE) {
  vertex_count <- snafun::count_vertices(x)
  vertex_key <- seq_len(vertex_count)
  if (named && snafun::has_vertexnames(x)) {
    vertex_key <- snafun::extract_vertex_names(x)
  }
  
  vertices <- data.frame(name = vertex_key, stringsAsFactors = FALSE)
  vertex_attributes <- snafun::extract_all_vertex_attributes(x)
  
  if (!is.null(vertex_attributes)) {
    removable_names <- intersect(colnames(vertex_attributes), c("name", "vertex.names"))
    if (length(removable_names) > 0) {
      vertex_attributes <- vertex_attributes[, !colnames(vertex_attributes) %in% removable_names, drop = FALSE]
    }
    if (ncol(vertex_attributes) > 0) {
      vertices <- cbind(vertices, vertex_attributes, stringsAsFactors = FALSE)
    }
  }
  
  if (inherits(x, "network") && snafun::is_bipartite(x) && !"type" %in% colnames(vertices)) {
    partition_size <- network::get.network.attribute(x, "bipartite")
    vertices$type <- c(
      rep(FALSE, partition_size),
      rep(TRUE, vertex_count - partition_size)
    )
  }
  
  vertices
}


#' Attach hidden reconstruction metadata to an edgelist
#'
#' The visible edgelist remains an ordinary data.frame. We only add two
#' attributes so the \code{to_*} methods can recover isolates when this exact
#' object is converted back into another graph representation.
#'
#' @param edgelist data.frame containing an edgelist
#' @param vertex_metadata data.frame with one row per vertex
#' @param bipartite logical, whether the source graph is bipartite
#' @param directed logical, whether the source graph is directed
#'
#' @return The same data.frame, with hidden metadata attributes added.
#' @keywords internal
#' @noRd
attach_edgelist_metadata <- function(edgelist, vertex_metadata, bipartite, directed) {
  attr(edgelist, "snafun_vertices") <- vertex_metadata
  attr(edgelist, "snafun_bipartite") <- bipartite
  attr(edgelist, "snafun_directed") <- directed
  edgelist
}


#' Extract stored vertex metadata from an edgelist
#'
#' Retrieve the hidden vertex metadata that \code{snafun::to_edgelist()} stores
#' on its return value. If the attribute is missing or malformed, \code{NULL} is
#' returned so the caller can safely fall back to the visible edgelist only.
#'
#' @param x data.frame that may contain hidden \code{snafun} metadata
#'
#' @return A data.frame with vertex metadata or \code{NULL}.
#' @keywords internal
#' @noRd
extract_stored_edgelist_vertices <- function(x) {
  vertices <- attr(x, "snafun_vertices", exact = TRUE)
  if (is.data.frame(vertices) && ncol(vertices) >= 1) {
    return(vertices)
  }
  NULL
}


#' Extract the stored bipartite flag from an edgelist
#'
#' @param x data.frame that may contain hidden \code{snafun} metadata
#'
#' @return logical scalar, \code{TRUE} when the source graph was bipartite.
#' @keywords internal
#' @noRd
extract_stored_edgelist_bipartite <- function(x) {
  isTRUE(attr(x, "snafun_bipartite", exact = TRUE))
}


#' Extract the stored directedness flag from an edgelist
#'
#' @param x data.frame that may contain hidden \code{snafun} metadata
#'
#' @return logical scalar, \code{TRUE} when the source graph was directed.
#' @keywords internal
#' @noRd
extract_stored_edgelist_directed <- function(x) {
  isTRUE(attr(x, "snafun_directed", exact = TRUE))
}


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
#' Edgelists created by \code{snafun::to_edgelist()} preserve the original
#' directedness through hidden metadata. For plain external edgelists,
#' \code{snafun} infers an undirected one-mode graph only when every visible row
#' has an exact reciprocal counterpart; otherwise the safest default is a
#' directed graph. Use \code{directed = FALSE} when a plain external edgelist
#' lists each undirected edge only once.}
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
#' 
#' When \code{x} originates from \code{snafun::to_edgelist()}, these vertex
#' metadata are recovered automatically from hidden attributes. That lets
#' isolates survive a graph -> edgelist -> igraph roundtrip without changing
#' the visible edgelist columns.
#' See \code{\link[igraph]{graph_from_data_frame}} for the underlying function.
#' 
#' @param x input object
#' @param bipartite logical, whether an adjacency matrix represents a bipartite 
#' network. Forces the creation of a bipartite igraph x. This argument is 
#' only used when a matrix is converted to an \code{igraph} object and is 
#' ignored otherwise.
#' @param vertices A data frame with vertex metadata, or \code{NULL}. See details below.
#' @param directed Optional logical override that is only used for
#' \code{data.frame} edgelists. Leave \code{NULL} to let \code{snafun} infer
#' the most plausible interpretation from hidden metadata and the visible rows.
#' Set \code{FALSE} to force an undirected one-mode graph when a plain external
#' edgelist only lists each undirected edge once.
#' @export
#' @note The functions are largely based upon \code{as_igraph} functions from 
#' the \code{migraph} package. 
#' The versions in the \code{snafun} package do not require 
#' the tidyverse dependencies of \code{migraph}
#' and do not deal with \code{tidygraph}.
#' @examples
#' # from a matrix
#' g <- snafun::create_random_graph(10, "gnp", p = 2/10, graph = "igraph")
#' mat <- snafun::to_matrix(g)
#' to_igraph(mat)
#' 
#' g <- snafun::create_manual_graph(1 -- 2 -- 3 -- 4 -- 5 -- 6 -- 7 -- 8 -- 9 -- 10 -- 1)
#' g <- snafun::add_edge_attributes(g, "weight", value = seq_len(snafun::count_edges(g)))
#' mat <- snafun::to_matrix(g)
#' to_igraph(mat)
#' 
#' # bipartite network, even nodes are one type, odd vertices another type
#' g <- snafun::create_bipartite(5, 5, strategy = "gnm", m = 5)
#' mat <- snafun::to_matrix(snafun::to_igraph(g))
#' to_igraph(mat)  # same network, but not officially bipartite
#' mat <- snafun::to_matrix(g)
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
                      vertices = NULL,
                      directed = NULL) {
  UseMethod("to_igraph")
}



#' @export
to_igraph.default <- function(x, bipartite = FALSE,
                              vertices = NULL,
                              directed = NULL) {
  txt <- methods_error_message("x", "to_igraph")
  stop(txt)
}



#' @export
to_igraph.matrix <- function(x, bipartite = FALSE,
                             vertices = NULL,
                             directed = NULL) {
  # PERFORMANCE (2026-09-16): the binary and symmetry predicates below were
  # previously all(x %in% c(0, 1)) and isSymmetric(x). Both are O(n^2), and on
  # large matrices %in% (hashing) and isSymmetric() (tolerance-based all.equal)
  # dominated the runtime. matrix_is_binary() / matrix_is_symmetric_fast() are
  # vectorized equivalents; each predicate is also computed only once here.
  is_binary <- matrix_is_binary(x)
  if (nrow(x) != ncol(x) | bipartite) {
    if (!is_binary) {
      graph <- igraph::graph_from_incidence_matrix(x,
                          weighted = TRUE, directed = FALSE)
    } else {
      graph <- igraph::graph_from_incidence_matrix(x, directed = FALSE)
    }
  } else {
    mode <- if (matrix_is_symmetric_fast(x)) "undirected" else "directed"
    if (!is_binary) {
      graph <- igraph::graph_from_adjacency_matrix(x, mode = mode, weighted = TRUE)
    } else {
      graph <- igraph::graph_from_adjacency_matrix(x, mode = mode)
    }
  }
  # simplify, because loops will otherwise occur twice each
  graph <- igraph::simplify(graph, remove.multiple = TRUE, remove.loops = FALSE)
  graph
}


#' Fast test whether a matrix contains only 0/1 values
#'
#' Vectorized replacement for \code{all(x \%in\% c(0, 1))}. Returns the same
#' result (including \code{FALSE} when \code{NA}s are present) but avoids the
#' hashing overhead of \code{\%in\%} on large matrices.
#'
#' @param x a matrix
#' @return logical scalar
#' @keywords internal
#' @noRd
matrix_is_binary <- function(x) {
  isTRUE(all(x == 0 | x == 1))
}


#' Fast symmetry test for adjacency matrices
#'
#' Used to decide directed vs. undirected in \code{to_igraph.matrix()}. For
#' matrices without \code{NA}s (the common case, including all 0/1 adjacency
#' matrices and cleanly symmetric weighted matrices) an exact, vectorized
#' comparison is used, which is far faster than \code{isSymmetric()}'s
#' tolerance-based \code{all.equal()} path. When \code{NA}s are present we defer
#' to \code{isSymmetric()} on the unnamed matrix.
#'
#' @param x a matrix
#' @return logical scalar
#' @keywords internal
#' @noRd
matrix_is_symmetric_fast <- function(x) {
  if (nrow(x) != ncol(x)) {
    return(FALSE)
  }
  if (anyNA(x)) {
    return(isSymmetric(unname(x)))
  }
  all(x == t(x))
}



#' @export
to_igraph.network <- function (x, bipartite = FALSE,
                               vertices = NULL,
                               directed = NULL) {
  if (network::is.hyper(x)) 
    stop("hypergraphs are not supported")
  attr <- names(x[[3]][[1]])
  isna <- which(attr == "na")
  if (length(isna) > 0) {
    attr <- attr[-isna]
  }
  
  # Build the graph STRUCTURE from the sparse edge list instead of a dense
  # sociomatrix (2026-09-16). The old path used sna::as.sociomatrix.sna() +
  # graph_from_adjacency_matrix()/graph_from_incidence_matrix(), which
  # materializes and scans an n x n matrix; graph_from_adjacency_matrix() alone
  # cost ~4s on the enwiki graph. network::as.edgelist() is O(number of edges).
  # Vertex names, edge attributes (including weight) and vertex attributes are
  # attached below exactly as before, so the resulting graph is identical (this
  # is verified bit-for-bit against the old implementation in the tests).
  n <- network::network.size(x)
  el <- network::as.edgelist(x)
  graph <- igraph::make_empty_graph(n = n, directed = isTRUE(x$gal$directed))
  if (!is.null(el) && nrow(el) > 0) {
    graph <- igraph::add_edges(graph, as.vector(t(el[, 1:2, drop = FALSE])))
  }
  # vertex names (the dense path set these from the sociomatrix dimnames)
  vertex_names <- network::get.vertex.attribute(x, "vertex.names")
  if (!is.null(vertex_names)) {
    igraph::V(graph)$name <- as.character(vertex_names)
  }
  # bipartite: mark the two partitions with the logical 'type' attribute, as
  # graph_from_incidence_matrix() would have done.
  if (network::is.bipartite(x)) {
    partition_size <- network::get.network.attribute(x, "bipartite")
    igraph::V(graph)$type <- c(rep(FALSE, partition_size),
                               rep(TRUE, n - partition_size))
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



#' Build a bare structural igraph from a network object (sparse)
#'
#' Internal helper for the measure methods (transitivity, triad/dyad census,
#' mean distance) that only need graph STRUCTURE, not attributes. It builds the
#' igraph from the network's sparse edge list instead of materializing a dense
#' sociomatrix via \code{sna::as.sociomatrix.sna()} (which does not scale -- e.g.
#' it hangs / needs ~n^2 memory on the enwiki graph). The vertex count (hence
#' isolates), the directedness, and -- for bipartite inputs -- the vertex
#' \code{type} are preserved; edge/vertex attributes and weights are dropped, as
#' the callers ignore them. Loops are kept and multiple edges removed, matching
#' \code{to_igraph.matrix()}.
#'
#' @param x a \code{network} object
#' @return an \code{igraph} object
#' @keywords internal
#' @noRd
network_structure_to_igraph <- function(x) {
  n <- network::network.size(x)
  directed <- network::is.directed(x)
  el <- network::as.edgelist(x)
  g <- igraph::make_empty_graph(n = n, directed = directed)
  if (!is.null(el) && nrow(el) > 0) {
    g <- igraph::add_edges(g, as.vector(t(el[, 1:2, drop = FALSE])))
  }
  g <- igraph::simplify(g, remove.multiple = TRUE, remove.loops = FALSE)
  if (network::is.bipartite(x)) {
    partition_size <- network::get.network.attribute(x, "bipartite")
    igraph::V(g)$type <- c(rep(FALSE, partition_size),
                           rep(TRUE, n - partition_size))
  }
  g
}


#' @export
to_igraph.igraph <- function(x, bipartite = FALSE,
                             vertices = NULL,
                             directed = NULL) {
  x
}



#' @export
to_igraph.data.frame <- function(x,
                                 bipartite = FALSE,
                                 vertices = NULL,
                                 directed = NULL) {
  bipartite_was_missing <- missing(bipartite)
  vertices_were_missing <- missing(vertices)
  
  # just in case this is done by a tidyverse user
  if (inherits(x, "tbl_df")) x <- as.data.frame(x)
  
  # Edgelists cannot display isolates directly. When the edgelist comes from
  # snafun::to_edgelist(), we recover the stored vertex table here so the full
  # vertex set is retained during the conversion back to a graph object.
  if (vertices_were_missing) {
    vertices <- extract_stored_edgelist_vertices(x)
  }
  semantics <- resolve_edgelist_conversion(
    x,
    directed = directed,
    bipartite = if (bipartite_was_missing) NULL else bipartite
  )
  x <- semantics$x
  bipartite <- semantics$bipartite
  directed <- semantics$directed

  # Numeric (double) vertex ids >= 1e5 stringify in scientific notation
  # ("1e+05") when igraph::graph_from_data_frame() matches them against the
  # (integer) vertex table, which broke the edge-list -> igraph roundtrip on
  # large, unnamed networks such as enwiki. Coerce whole-number id columns to
  # integer so the endpoints match the vertex table exactly. (2026-09-14)
  if (is.numeric(x[[1]])) x[[1]] <- as.integer(x[[1]])
  if (is.numeric(x[[2]])) x[[2]] <- as.integer(x[[2]])
  if (!is.null(vertices) && is.numeric(vertices[[1]])) {
    vertices[[1]] <- as.integer(vertices[[1]])
  }

  graph <- tryCatch(
    igraph::graph_from_data_frame(x, directed = directed, vertices = vertices),
    error = function(e) e
  )
  
  # Recent igraph versions signal this as an rlang error with updated text,
  # while older versions used a base-style simpleError and a different
  # message. We normalize both cases to the same user-facing snafun message.
  if (inherits(graph, "error")) {
    graph_message <- conditionMessage(graph)
    if (grepl("Some vertex names.*not listed in.*vertices", graph_message)) {
      stop(
        paste0(
          "Some vertices that occur in your edgelist are missing in ",
          "'vertices'. Make sure all vertices are included in 'vertices'."
        ),
        call. = FALSE
      )
    }
    stop(graph)
  }
  
  # make bipartite 
  if (bipartite) {
    if (length(intersect(c(x[,1]), c(x[, 2]))) == 0) { 
      # For ordinary edgelists we infer the second partition from the receiver
      # column. When stored vertex metadata are available, that metadata are
      # more reliable because isolated vertices may never occur in the edge list.
      if (is.null(vertices) || !("type" %in% colnames(vertices))) {
        igraph::V(graph)$type <- igraph::V(graph)$name %in% x[, 2]
      }
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

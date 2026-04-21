#' Make a bipartite projection
#'
#' Project a bipartite network into one or two one-mode networks.
#'
#' This function follows the semantics of \code{igraph::bipartite_projection()}
#' but exposes a slightly clearer \code{which} argument across the supported
#' \code{snafun} input types. In particular, \code{"row"} and \code{"column"}
#' refer to the row and column partitions of the underlying bipartite incidence
#' matrix. For compatibility with igraph, the aliases \code{"false"},
#' \code{"true"}, \code{"one"}, and \code{"two"} are accepted as well.
#'
#' The \code{probe1} argument from \code{igraph::bipartite_projection()} is not
#' included here. In igraph it only controls the order of the two projections in
#' the returned list. \code{snafun::make_projection()} instead returns a named
#' list with elements \code{row} and \code{column}, so the ordering ambiguity is
#' removed without needing an extra argument.
#'
#' Edge directions in the input are ignored during the projection, matching the
#' behaviour of \code{igraph::bipartite_projection()}.
#'
#' Supported inputs are:
#' \describe{
#'   \item{\code{igraph}}{The graph must be marked as bipartite via the
#'   \code{type} vertex attribute.}
#'   \item{\code{network}}{The network must be marked as bipartite via its
#'   \code{bipartite} graph attribute.}
#'   \item{\code{matrix}}{The matrix must be rectangular and is interpreted as a
#'   bipartite incidence matrix.}
#'   \item{\code{data.frame}}{The data frame is interpreted as a bipartite
#'   edgelist. Hidden \code{snafun} metadata from \code{\link{to_edgelist}} are
#'   reused when available. Otherwise, the edgelist is treated as bipartite only
#'   when its sender and receiver sets do not overlap.}
#' }
#'
#' The projection keeps vertex attributes. If \code{multiplicity = TRUE}, the
#' number of shared neighbours is stored as an edge attribute called
#' \code{weight}, matching igraph. If \code{multiplicity = FALSE}, the
#' projection is unweighted.
#'
#' @param x bipartite network of class \code{igraph}, \code{network},
#' \code{matrix}, or \code{data.frame}
#' @param which Character scalar. One of \code{"both"}, \code{"row"},
#' \code{"column"}, \code{"one"}, \code{"two"}, \code{"false"}, or
#' \code{"true"}.
#' @param multiplicity Logical scalar, should the number of shared neighbours be
#' stored as an edge attribute called \code{weight}?
#' @param remove.type Logical scalar, should the \code{type} vertex attribute be
#' removed from the projection(s)? This matters mainly for \code{igraph} and for
#' edgelists that keep hidden vertex metadata.
#'
#' @return If \code{which = "both"}, a named list with elements \code{row} and
#' \code{column}. Otherwise, a single projected object of the same broad format
#' as the input.
#' @export
#'
#' @examples
#' m <- matrix(
#'   c(
#'     1, 1,
#'     1, 1,
#'     0, 1
#'   ),
#'   nrow = 3,
#'   byrow = TRUE
#' )
#' rownames(m) <- c("Alice", "Bob", "Cecil")
#' colnames(m) <- c("Project1", "Project2")
#'
#' make_projection(m, which = "row")
#' make_projection(m, which = "column")
#'
#' g <- snafun::to_igraph(m, bipartite = TRUE)
#' make_projection(g, which = "both")
make_projection <- function(x,
                            which = c("both", "row", "column", "one", "two", "false", "true"),
                            multiplicity = TRUE,
                            remove.type = TRUE) {
  UseMethod("make_projection")
}


#' @export
make_projection.default <- function(x,
                                    which = c("both", "row", "column", "one", "two", "false", "true"),
                                    multiplicity = TRUE,
                                    remove.type = TRUE) {
  txt <- methods_error_message("x", "make_projection")
  stop(txt)
}


#' Normalize projection labels
#'
#' @param which User-supplied projection label
#'
#' @return One of \code{"both"}, \code{"row"}, or \code{"column"}.
#' @keywords internal
#' @noRd
normalize_projection_which <- function(which) {
  which <- snafun.match.arg(
    which,
    choices = c("both", "row", "column", "one", "two", "false", "true")
  )
  
  switch(
    which,
    both = "both",
    row = "row",
    one = "row",
    false = "row",
    column = "column",
    two = "column",
    true = "column"
  )
}


#' Map projection labels to igraph labels
#'
#' @param which Normalized projection label
#'
#' @return Character scalar accepted by \code{igraph::bipartite_projection()}.
#' @keywords internal
#' @noRd
projection_which_to_igraph <- function(which) {
  switch(
    which,
    both = "both",
    row = "false",
    column = "true"
  )
}


#' Check whether an edgelist can be treated as bipartite
#'
#' @param x data.frame interpreted as an edgelist
#'
#' @return Logical scalar.
#' @keywords internal
#' @noRd
is_bipartite_edgelist_input <- function(x) {
  if (extract_stored_edgelist_bipartite(x)) {
    return(TRUE)
  }
  length(intersect(unique(x[[1]]), unique(x[[2]]))) == 0
}


#' Convert a projection graph back to a target format
#'
#' @param x projected igraph object
#' @param target Character scalar naming the desired output format
#' @param keep_type Logical scalar, should the type vertex attribute be kept
#' when possible?
#'
#' @return Projected object in the requested target format.
#' @keywords internal
#' @noRd
coerce_projection_output <- function(x, target, keep_type = FALSE) {
  type_values <- NULL
  graph_plain <- x
  if ("type" %in% igraph::vertex_attr_names(x)) {
    type_values <- igraph::vertex_attr(x, "type")
    graph_plain <- igraph::delete_vertex_attr(x, "type")
  }
  
  if (target == "igraph") {
    if (keep_type && !is.null(type_values)) {
      return(x)
    }
    return(graph_plain)
  }
  
  if (target == "network") {
    out <- snafun::to_network(graph_plain)
    if (keep_type && !is.null(type_values)) {
      out <- network::set.vertex.attribute(out, attrname = "type", value = type_values)
    }
    return(out)
  }
  
  if (target == "matrix") {
    return(snafun::to_matrix(graph_plain))
  }
  
  if (target == "data.frame") {
    out <- snafun::to_edgelist(graph_plain)
    if (keep_type && !is.null(type_values)) {
      vertices <- attr(out, "snafun_vertices", exact = TRUE)
      if (is.data.frame(vertices)) {
        vertices$type <- type_values
        attr(out, "snafun_vertices") <- vertices
      }
    }
    return(out)
  }
  
  stop("Unknown target format in projection conversion.", call. = FALSE)
}


#' Project an igraph object and convert the result
#'
#' @param x bipartite igraph object
#' @param which User-facing projection selector
#' @param multiplicity Logical scalar
#' @param remove.type Logical scalar
#' @param target Character scalar naming the desired output format
#'
#' @return Projected object or named list of projected objects.
#' @keywords internal
#' @noRd
project_from_igraph <- function(x,
                                which,
                                multiplicity,
                                remove.type,
                                target) {
  which <- normalize_projection_which(which)
  projection <- igraph::bipartite_projection(
    graph = x,
    multiplicity = multiplicity,
    which = projection_which_to_igraph(which),
    remove.type = ifelse(target == "igraph", remove.type, FALSE)
  )
  
  if (which == "both") {
    out <- list(
      row = coerce_projection_output(
        x = projection[[1]],
        target = target,
        keep_type = !remove.type
      ),
      column = coerce_projection_output(
        x = projection[[2]],
        target = target,
        keep_type = !remove.type
      )
    )
    return(out)
  }
  
  coerce_projection_output(
    x = projection,
    target = target,
    keep_type = !remove.type
  )
}


#' @export
make_projection.igraph <- function(x,
                                   which = c("both", "row", "column", "one", "two", "false", "true"),
                                   multiplicity = TRUE,
                                   remove.type = TRUE) {
  if (!snafun::is_bipartite(x)) {
    stop("The input graph should be bipartite.", call. = FALSE)
  }
  
  project_from_igraph(
    x = x,
    which = which,
    multiplicity = multiplicity,
    remove.type = remove.type,
    target = "igraph"
  )
}


#' @export
make_projection.network <- function(x,
                                    which = c("both", "row", "column", "one", "two", "false", "true"),
                                    multiplicity = TRUE,
                                    remove.type = TRUE) {
  if (!snafun::is_bipartite(x)) {
    stop("The input graph should be bipartite.", call. = FALSE)
  }
  
  graph <- snafun::to_igraph(x)
  project_from_igraph(
    x = graph,
    which = which,
    multiplicity = multiplicity,
    remove.type = remove.type,
    target = "network"
  )
}


#' @export
make_projection.matrix <- function(x,
                                   which = c("both", "row", "column", "one", "two", "false", "true"),
                                   multiplicity = TRUE,
                                   remove.type = TRUE) {
  if (nrow(x) == ncol(x)) {
    stop("For matrix input, 'x' should be a rectangular bipartite incidence matrix.", call. = FALSE)
  }
  
  graph <- snafun::to_igraph(x, bipartite = TRUE)
  project_from_igraph(
    x = graph,
    which = which,
    multiplicity = multiplicity,
    remove.type = remove.type,
    target = "matrix"
  )
}


#' @export
make_projection.data.frame <- function(x,
                                       which = c("both", "row", "column", "one", "two", "false", "true"),
                                       multiplicity = TRUE,
                                       remove.type = TRUE) {
  if (inherits(x, "tbl_df")) {
    x <- as.data.frame(x)
  }
  if (ncol(x) < 2) {
    stop("'x' should have at least two columns.", call. = FALSE)
  }
  if (!is_bipartite_edgelist_input(x)) {
    stop("The edgelist should describe a bipartite network.", call. = FALSE)
  }
  
  graph <- suppressMessages(snafun::to_igraph(x, bipartite = TRUE))
  project_from_igraph(
    x = graph,
    which = which,
    multiplicity = multiplicity,
    remove.type = remove.type,
    target = "data.frame"
  )
}

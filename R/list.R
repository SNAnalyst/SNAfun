

#' List the attributes in the graph object
#' 
#' List the attributes in the graph object using a consistent API
#'
#' For static \code{igraph} and \code{network} objects this function returns
#' the regular vertex, edge, or graph attributes. For \code{networkDynamic}
#' objects you can also request the attributes that are active at a specific
#' time point or over a time interval by setting \code{active = TRUE}.
#'
#' With the default \code{active = FALSE}, a \code{networkDynamic} object is
#' treated like a regular \code{network} object. That means you get the
#' attribute names exactly as they are stored inside the object. Static
#' attributes keep their usual names, while temporally extended attributes are
#' listed by their raw TEA names such as \code{weight.active}.
#'
#' When \code{active = TRUE}, the \code{networkDynamic} backend is used and
#' additional timing arguments such as \code{at}, \code{onset}, and
#' \code{terminus} are forwarded to the corresponding
#' \code{networkDynamic::list.*.attributes.active()} function.
#'
#' @param x graph object of class \code{igraph}, \code{network}, or
#' \code{networkDynamic}
#' @param active logical; should active attributes be listed for a
#' \code{networkDynamic} object? Defaults to \code{FALSE}.
#' @param onset start of the queried time interval. Only used when
#' \code{active = TRUE} and \code{x} is a \code{networkDynamic} object.
#' @param terminus end of the queried time interval. Only used when
#' \code{active = TRUE} and \code{x} is a \code{networkDynamic} object.
#' @param length interval length. Only used when \code{active = TRUE} and
#' \code{x} is a \code{networkDynamic} object.
#' @param at single time point to query. Only used when \code{active = TRUE}
#' and \code{x} is a \code{networkDynamic} object.
#' @param ... additional arguments passed to the underlying
#' \code{networkDynamic} active-attribute accessors, for example
#' \code{dynamic.only}.
#'
#' @return the names of the requested attributes (if any)
#' @name list_attributes
NULL


#' @export
#' @rdname list_attributes
list_vertex_attributes <- function(x, active = FALSE, onset = NULL, terminus = NULL,
                                   length = NULL, at = NULL, ...) {
  UseMethod("list_vertex_attributes")
}

#' @export
list_vertex_attributes.default <- function(x, active = FALSE, onset = NULL, terminus = NULL,
                                           length = NULL, at = NULL, ...) {
  txt <- methods_error_message("x", "list_vertex_attributes")
  stop(txt)
}

#' @export
list_vertex_attributes.igraph <- function(x, active = FALSE, onset = NULL, terminus = NULL,
                                          length = NULL, at = NULL, ...) {
  if (isTRUE(active)) {
    stop("Active attribute queries are only implemented for 'networkDynamic' objects")
  }
  igraph::vertex_attr_names(x)
}


#' @export
list_vertex_attributes.network <- function(x, active = FALSE, onset = NULL, terminus = NULL,
                                           length = NULL, at = NULL, ...) {
  if (isTRUE(active) && networkDynamic::is.networkDynamic(x)) {
    return(
      networkDynamic::list.vertex.attributes.active(
        x = x,
        onset = onset,
        terminus = terminus,
        length = length,
        at = at,
        ...
      )
    )
  }
  if (isTRUE(active)) {
    stop("Active attribute queries are only implemented for 'networkDynamic' objects")
  }
  network::list.vertex.attributes(x)
}


#' @export
list_vertex_attributes.networkDynamic <- function(x, active = FALSE, onset = NULL,
                                                  terminus = NULL, length = NULL,
                                                  at = NULL, ...) {
  if (!isTRUE(active)) {
    return(network::list.vertex.attributes(x))
  }
  networkDynamic::list.vertex.attributes.active(
    x = x,
    onset = onset,
    terminus = terminus,
    length = length,
    at = at,
    ...
  )
}



#' @export
#' @rdname list_attributes
list_edge_attributes <- function(x, active = FALSE, onset = NULL, terminus = NULL,
                                 length = NULL, at = NULL, ...) {
  UseMethod("list_edge_attributes")
}

#' @export
list_edge_attributes.default <- function(x, active = FALSE, onset = NULL, terminus = NULL,
                                         length = NULL, at = NULL, ...) {
  txt <- methods_error_message("x", "list_edge_attributes")
  stop(txt)
}

#' @export
list_edge_attributes.igraph <- function(x, active = FALSE, onset = NULL, terminus = NULL,
                                        length = NULL, at = NULL, ...) {
  if (isTRUE(active)) {
    stop("Active attribute queries are only implemented for 'networkDynamic' objects")
  }
  igraph::edge_attr_names(x)
}


#' @export
list_edge_attributes.network <- function(x, active = FALSE, onset = NULL, terminus = NULL,
                                         length = NULL, at = NULL, ...) {
  if (isTRUE(active) && networkDynamic::is.networkDynamic(x)) {
    return(
      networkDynamic::list.edge.attributes.active(
        x = x,
        onset = onset,
        terminus = terminus,
        length = length,
        at = at,
        ...
      )
    )
  }
  if (isTRUE(active)) {
    stop("Active attribute queries are only implemented for 'networkDynamic' objects")
  }
  network::list.edge.attributes(x)
}


#' @export
list_edge_attributes.networkDynamic <- function(x, active = FALSE, onset = NULL,
                                                terminus = NULL, length = NULL,
                                                at = NULL, ...) {
  if (!isTRUE(active)) {
    return(network::list.edge.attributes(x))
  }
  networkDynamic::list.edge.attributes.active(
    x = x,
    onset = onset,
    terminus = terminus,
    length = length,
    at = at,
    ...
  )
}


#' @export
#' @rdname list_attributes
list_graph_attributes <- function(x) {
  UseMethod("list_graph_attributes")
}

#' @export
list_graph_attributes.default <- function(x) {
  txt <- methods_error_message("x", "list_graph_attributes")
  stop(txt)
}

#' @export
list_graph_attributes.igraph <- function(x) {
  igraph::graph_attr_names(x)
}


#' @export
list_graph_attributes.network <- function(x) {
  network::list.network.attributes(x)
}



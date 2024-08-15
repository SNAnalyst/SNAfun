

#' List the attributes in the graph object
#' 
#' List the attributes in the graph object using a consistent API
#'
#' @param x graph object of class \code{igraph} or \code{network}
#'
#' @return the names of the requested attributes (if any)
#' @name list_attributes
NULL


#' @export
#' @rdname list_attributes
list_vertex_attributes <- function(x) {
  UseMethod("list_vertex_attributes")
}

#' @export
list_vertex_attributes.default <- function(x) {
  txt <- methods_error_message("x", "list_vertex_attributes")
  stop(txt)
}

#' @export
list_vertex_attributes.igraph <- function(x) {
  igraph::vertex_attr_names(x)
}


#' @export
list_vertex_attributes.network <- function(x) {
  network::list.vertex.attributes(x)
}



#' @export
#' @rdname list_attributes
list_edge_attributes <- function(x) {
  UseMethod("list_edge_attributes")
}

#' @export
list_edge_attributes.default <- function(x) {
  txt <- methods_error_message("x", "list_edge_attributes")
  stop(txt)
}

#' @export
list_edge_attributes.igraph <- function(x) {
  igraph::edge_attr_names(x)
}


#' @export
list_edge_attributes.network <- function(x) {
  network::list.edge.attributes(x)
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



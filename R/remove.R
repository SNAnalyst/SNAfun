
#' Remove vertices
#' 
#' Remove specific vertices from the graph
#' 
#' Removes specific vertices from a graph (potentially bipartite) of 
#' class \code{network} or \code{igraph}.
#' 
#' The vertices to be removed can either be their id's or their names (if they 
#' exist in the graph object). The algorithm will first check if the 
#' \code{vertices} coincide with vertex names that are present in the object 
#' (if they exist) and, if they match, the vertices are removed according to 
#' their names. If they do not match, or if there are no vertex names in the 
#' graph object, \code{vertices} is assumed to refer to vertex id's.
#'
#' @param x graph object of class \code{network} or \code{igraph}
#' @param vertices vector with either vertex names or vertex id's.
#'
#' @return graph without the removed vertices
#' @export 
remove_vertices <- function(x, vertices) {
  UseMethod("remove_vertices")
}



#' @export
remove_vertices.default <- function(x, vertices) {
  txt <- methods_error_message("x", "remove_vertices")
  stop(txt)
}




#' @export
remove_vertices.igraph <- function(x, vertices) {
  igraph::delete_vertices(x, v = vertices)
}


#' @export
remove_vertices.network <- function(x, vertices) {
  namen <- network::get.vertex.attribute(x, "vertex.names")
  if (all(vertices %in% namen)) {
    vertices <- match(vertices, namen)
  } else if (!is.numeric(vertices)) {
    stop("You need to specify only vertex names or vertex numbers and they should all exist in the network")
  }
  graph <- network::delete.vertices(x, vid = vertices)
  return(graph)
}





#' Remove all isolates from the graph
#' 
#' Remove all isolates from the graph
#' 
#' Removes the isolates from an object of class \code{igraph} or \code{network}.
#' First, the algorithm identifies the isolates, using the \code{\link{find_isolates}} 
#' function. Then, the identified isolates are removed and the new, smaller, 
#' graph is returned. Obviously, if \code{x} does not contain any isolates, 
#' \code{x} will be returned unaltered.
#' 
#' @param x graph of class \code{igraph} or \code{network}
#' @param loops should self-loops count when deciding if a vertex is an isolate? 
#' See  \code{\link{find_isolates}} for details.
#'
#' @return graph with the isolates (if any) removed
#' @export
#' @seealso Finding isolates: \code{\link{find_isolates}}
remove_isolates <- function(x, loops = FALSE) {
  UseMethod("remove_isolates")
}




#' @export
remove_isolates.default <- function(x, loops = FALSE) {
  txt <- methods_error_message("x", "remove_isolates")
  stop(txt)
}



#' @export
remove_isolates.igraph <- function(x, loops = FALSE) {
  isols <- find_isolates.igraph(x, names = FALSE, loops = loops)
  if (length(isols) > 0) {
    remove_vertices.igraph(x, isols)
  } else {
    x
  }
}


#' @export
remove_isolates.network <- function(x, loops = FALSE) {
  isols <- find_isolates.network(x, names = FALSE, loops = loops)
  if (length(isols) > 0) {
    remove_vertices.network(x, isols)
  } else {
    x
  }
}








#' Remove parts of the graph
#' 
#' Remove parts of the graph
#' 
#' @param x input graph (class \code{igraph} or \code{network})
#' @param attr_name name of the attribute to be removed
#'
#' @return graph, without the removed elements
#' @export
#'
#' @examples
#' data(emon, package = "network")
#' texas <- emon$Texas
#' remove_edge_attribute(texas, "Frequency")
#' remove_vertex_attribute(texas, "Formalization")
#' remove_graph_attribute(texas, "directed")
#' 
#' texas_i <- to_igraph(texas)
#' remove_edge_attribute(texas_i, "Frequency")
#' remove_vertex_attribute(texas_i, "Formalization")
#' @name remove
NULL

#' @rdname remove
#' @export
remove_edge_attribute <- function(x, attr_name) {
  UseMethod("remove_edge_attribute")
}


#' @export
remove_edge_attribute.default <- function(x, attr_name) {
  txt <- methods_error_message("x", "remove_edge_attribute")
  stop(txt)
}



#' @export
remove_edge_attribute.igraph <- function(x, attr_name) {
  x <- igraph::delete_edge_attr(graph = x, name = attr_name)
  x
}


#' @export
remove_edge_attribute.network <- function(x, attr_name) {
  network::delete.edge.attribute(x, attrname = attr_name)
  x
}






#' @rdname remove
#' @export
remove_vertex_attribute <- function(x, attr_name) {
  UseMethod("remove_vertex_attribute")
}


#' @export
remove_vertex_attribute.default <- function(x, attr_name) {
  txt <- methods_error_message("x", "remove_vertex_attribute")
  stop(txt)
}



#' @export
remove_vertex_attribute.igraph <- function(x, attr_name) {
  x <- igraph::delete_vertex_attr(graph = x, name = attr_name)
  x
}


#' @export
remove_vertex_attribute.network <- function(x, attr_name) {
  network::delete.vertex.attribute(x, attrname = attr_name)
  x
}






#' @rdname remove
#' @export
remove_graph_attribute <- function(x, attr_name) {
  UseMethod("remove_graph_attribute")
}


#' @export
remove_graph_attribute.default <- function(x, attr_name) {
  txt <- methods_error_message("x", "remove_graph_attribute")
  stop(txt)
}



#' @export
remove_graph_attribute.igraph <- function(x, attr_name) {
  x <- igraph::delete_graph_attr(graph = x, name = attr_name)
  x
}


#' @export
remove_graph_attribute.network <- function(x, attr_name) {
  network::delete.network.attribute(x, attrname = attr_name)
  x
}


# remove_loops -----------------------------------------------------------------

#' Remove loops
#' 
#' Remove loops from the graph
#' 
#' Remove loops (ie. a tie from a vertex to itself) from the graph object
#'
#' @param x graph of class \code{igraph}, \code{network}, or \code{matrix}
#'
#' @return graph with the same class as the input
#' @export
#'
#' @examples
#' x <- matrix(c(1, 1, 0, 0, 0, 1, 1, 0, 1), ncol = 3, byrow = TRUE)
#' remove_loops(x)
#' g_n <- snafun::to_network(x)
#' snafun::to_matrix(g_n)
#' remove_loops(g_n) |> snafun::to_matrix()
#' remove_loops(snafun::to_igraph(x)) |> snafun::to_matrix()
remove_loops <- function(x) {
  UseMethod("remove_loops")
}

#' @export
remove_loops.default <- function(x) {
  txt <- methods_error_message("x", "remove_loops")
  stop(txt)
}

#' @export
remove_loops.igraph <- function(x) {
  igraph::simplify(x, remove.multiple = FALSE, remove.loops = TRUE)
}


#' @export
remove_loops.network <- function(x) {
  el <- to_edgelist(x)
  loops <- which(el$from == el$to)
  x <- network::delete.edges(x, eid = loops)
  x
}


#' @export
remove_loops.matrix <- function(x) {
  diag(x) <- 0
  x
}



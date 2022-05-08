
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


#' @describeIn remove_vertices
#' @export
remove_vertices.default <- function(x, vertices) {
  stop("'x' should be an 'igraph' object or a 'network' object")
}



#' @describeIn remove_vertices
#' @export
remove_vertices.igraph <- function(x, vertices) {
  igraph::delete_vertices(x, v = vertices)
}

#' @describeIn remove_vertices
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



#' @describeIn remove_isolates
#' @export
remove_isolates.default <- function(x, loops = FALSE) {
  stop("'x' should be an 'igraph' object or a 'network' object")
}



#' @export
#' @describeIn remove_isolates
remove_isolates.igraph <- function(x, loops = FALSE) {
  isols <- find_isolates.igraph(x, names = FALSE, loops = loops)
  if (length(isols) > 0) {
    remove_vertices.igraph(x, isols)
  } else {
    x
  }
}


#' @export
#' @describeIn remove_isolates
remove_isolates.network <- function(x, loops = FALSE) {
  isols <- find_isolates.network(x, names = FALSE, loops = loops)
  if (length(isols) > 0) {
    remove_vertices.network(x, isols)
  } else {
    x
  }
}



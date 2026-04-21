



#' Check for the existence of edge attributes in the graph
#' 
#' Check for the existence of edge attributes in the graph
#' 
#' Simple utility function that returns \code{TRUE} if the graph has 
#' edge attributes and \code{FALSE} otherwise.
#' 
#' @param x graph of class \code{igraph} or \code{network}
#' @param attrname name of the attribute to check for
#'
#' @return logical
#' @name has
#' @examples
#' data(florentine, package = "snafun")
#' has_vertexnames(florentine$flobusiness)      # TRUE
#' 
#' has_edge_attributes(florentine$flobusiness)  # FALSE
#' has_edge_attributes(florentine$flomarriage)  # TRUE
#' 
#' has_loops(florentine$flobusiness)
#' g <- igraph::graph( c(1,1,2,2,3,3,4,5))
#' has_loops(g)
#' m <- matrix(c(1, 0, 0, 0, 0, 1, 0, 1, 1), byrow = TRUE, ncol = 3)
#' has_loops(m)                                 # TRUE
#' diag(m) <- 0
#' has_loops(m)                                 # FALSE
#'
#' g_multi <- igraph::make_empty_graph(n = 3, directed = TRUE)
#' g_multi <- igraph::add_edges(g_multi, c(1, 2, 1, 2, 2, 3))
#' has_multiple_edges(g_multi)                  # TRUE
NULL



#' @export
#' @describeIn has Check whether a graph has edge attributes
has_edge_attributes <- function(x) {
  UseMethod("has_edge_attributes")
}


#' @export
has_edge_attributes.default <- function(x) {
  txt <- methods_error_message("x", "has_edge_attributes")
  stop(txt)
}


#' @export
has_edge_attributes.igraph <- function(x) {
  eattrs <- igraph::edge_attr_names(x)
  length(eattrs) > 0   # TRUE if there is at least one edge attribute
}


#' @export
has_edge_attributes.network <- function(x) {
  # there is always a edge attribute called "na"
  # if that is the only one, then there are no edge attrs
  !identical(network::list.edge.attributes(x), "na")
}




#' @export
#' @describeIn has Check whether a graph has vertex attributes (where 
#' the attribute called "na" does not count, as that is only an internal attribute 
#' by the network package)
has_vertex_attributes <- function(x) {
  UseMethod("has_vertex_attributes")
}


#' @export
has_vertex_attributes.default <- function(x) {
  txt <- methods_error_message("x", "has_vertex_attributes")
  stop(txt)
}


#' @export
has_vertex_attributes.igraph <- function(x) {
  vattrs <- igraph::vertex_attr_names(x)
  length(vattrs) > 0   # TRUE if there is at least one vertex attribute
}


#' @export
has_vertex_attributes.network <- function(x) {
  # there is (almost?) always a vertex attribute called "na"
  # if that is the only one, then there are no vertex attrs
  !identical(network::list.vertex.attributes(x), "na")
}





#' @export
#' @describeIn has Check whether a graph has a specific vertex attribute (where 
#' the attribute called "na" does not count, as that is only an internal attribute 
#' by the network package)
has_vertex_attribute <- function(x, attrname) {
  UseMethod("has_vertex_attribute")
}


#' @export
has_vertex_attribute.default <- function(x, attrname) {
  txt <- methods_error_message("x", "has_vertex_attribute")
  stop(txt)
}


#' @export
has_vertex_attribute.igraph <- function(x, attrname) {
  attrname %in% igraph::vertex_attr_names(x)
}


#' @export
has_vertex_attribute.network <- function(x, attrname) {
  attrname %in% network::list.vertex.attributes(x)
}







#' @export
#' @describeIn has Check whether a graph has a specific edge attribute
has_edge_attribute <- function(x, attrname) {
  UseMethod("has_edge_attribute")
}


#' @export
has_edge_attribute.default <- function(x, attrname) {
  txt <- methods_error_message("x", "has_edge_attribute")
  stop(txt)
}


#' @export
has_edge_attribute.igraph <- function(x, attrname) {
  attrname %in% igraph::edge_attr_names(x)
}


#' @export
has_edge_attribute.network <- function(x, attrname) {
  attrname %in% network::list.edge.attributes(x)
}









#' @export
#' @describeIn has Check whether a graph has vertex names. The function merely checks 
#' for the presence of the vertex attribute \code{name} (for a network of
#' class \code{igraph}) or \code{vertex.names} (for a network of
#' class \code{network}). 
#' 
#' Note that the \code{network} package tends to create vertex names by 
#' default, even if they were not added as separate attributes. 
#' In that case, the vertices are named as integers running  from 1 to the 
#' number of vertices. Of course, this function can not distinguish 
#' whether these are names the researcher want to use as names, or purely 
#' the result of the default behavior of the \code{network} package (and 
#' not meaningful names per se).
has_vertexnames <- function(x) {
  UseMethod("has_vertexnames")
}


#' @export
has_vertexnames.default <- function(x) {
  txt <- methods_error_message("x", "has_vertexnames")
  stop(txt)
}



#' @export
has_vertexnames.igraph <- function(x) {
  "name" %in% igraph::vertex_attr_names(x)
}


#' @export
has_vertexnames.network <- function(x) {
  "vertex.names" %in% network::list.vertex.attributes(x)
}






#' @export
#' @describeIn has Check whether a graph contains at least one loop (ie. 
#' an edge from a vertex to itself). Also works on a \code{matrix}.
has_loops <- function(x) {
  UseMethod("has_loops")
}


#' @export
has_loops.default <- function(x) {
  txt <- methods_error_message("x", "has_loops")
  stop(txt)
}



#' @export
has_loops.igraph <- function(x) {
  igraph::any_loop(x)
}


#' @export
has_loops.network <- function(x) {
  network::has.loops(x)
}

#' @export
has_loops.matrix <- function(x) {
  any(diag(x) != 0)
}








#' @export
#' @describeIn has Check whether a graph contains at least one isolate vertex. 
#' Returns \code{TRUE} or \code{FAlsE}
has_isolates <- function(x) {
  UseMethod("has_isolates")
}


#' @export
has_isolates.default <- function(x) {
  txt <- methods_error_message("x", "has_isolates")
  stop(txt)
}


#' @export
has_isolates.igraph <- function(x) {
  length(extract_isolates(x)) != 0
}


#' @export
has_isolates.network <- function(x) {
  length(extract_isolates(x)) != 0
}

#' @export
has_isolates.matrix <- function(x) {
  length(extract_isolates(x)) != 0
}





#' Build stable dyad keys for multiple-edge detection
#'
#' Multiple-edge checks need a backend-independent way to decide whether two
#' edges connect the same dyad. For directed graphs we keep the sender/receiver
#' ordering. For undirected graphs we canonicalize each dyad by sorting its two
#' endpoints, so A--B and B--A are treated as the same edge.
#'
#' The keys are intentionally based on character values because that works for
#' both numeric vertex ids and vertex names without changing the calling code.
#'
#' @param from vector of senders / tails
#' @param to vector of receivers / heads
#' @param directed logical, whether edge direction matters
#'
#' @return Character vector with one canonical key per edge.
#' @keywords internal
#' @noRd
multiple_edge_keys <- function(from, to, directed) {
  from_chr <- as.character(from)
  to_chr <- as.character(to)
  
  if (!directed) {
    swap_rows <- from_chr > to_chr
    if (any(swap_rows)) {
      from_tmp <- from_chr[swap_rows]
      from_chr[swap_rows] <- to_chr[swap_rows]
      to_chr[swap_rows] <- from_tmp
    }
  }
  
  paste(from_chr, to_chr, sep = "\r")
}





#' @export
#' @describeIn has Check whether a graph contains multiple edges between at
#' least one dyad. For matrices, values larger than 1 are interpreted as
#' collapsed multiple edges; this is useful for multigraph-style adjacency or
#' incidence matrices, but it is ambiguous for genuinely weighted matrices.
has_multiple_edges <- function(x) {
  UseMethod("has_multiple_edges")
}


#' @export
has_multiple_edges.default <- function(x) {
  txt <- methods_error_message("x", "has_multiple_edges")
  stop(txt)
}


#' @export
has_multiple_edges.igraph <- function(x) {
  igraph::any_multiple(x)
}


#' @export
has_multiple_edges.network <- function(x) {
  !is.null(snafun::extract_multiple_edges(x))
}


#' @export
has_multiple_edges.matrix <- function(x) {
  any(x > 1)
}


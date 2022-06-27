
#' has_vertexnames
#' 
#' Check whether the object contais vertex names 
#'
#' A simple check, yielding \code{TRUE} or \code{FALSE}. It merely checks 
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
#' 
#' 
#' @param x the network, either of class \code{igraph} or \code{network}
#'
#' @return \code{TRUE} or \code{FALSE}
#' @export
#' @examples
#' data(florentine, package = "snafun")
#' has_vertexnames(florentine$flobusiness)
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
  "name" %in% igraph::list.vertex.attributes(x)
}


#' @export
has_vertexnames.network <- function(x) {
  "vertex.names" %in% network::list.vertex.attributes(x)
}



#' Find the isolates
#' 
#' Who are the isolates in the network?
#' 
#' Identifies the isolates (if any) in the network. 
#' This function works for objects of class \code{network} or \code{graph}, 
#' potentially bipartite.
#' 
#' The output is a vector with the numbers of the isolates or their names. 
#' The latter is the default, but the numbers are returned if there are no 
#' names in the network object.
#' 
#' If there are self-loops in the network (ie. a vertex has a tie 
#' to or from himself), vertices who do not have ties with others will not 
#' be seen as isolates. Therefore, the user can decide whether these self-ties 
#' should be taken into account. If \code{FALSE}, the default, any self-loops 
#' will be ignored and vertices with no ties with others will be identified as 
#' isolates. However, in the (uncommon) case where a tie with oneself should 
#' no longer make the vertex an isolate, one can set the \code{loops} 
#' argument to \code{TRUE}. 
#'
#' @param x network of class \code{network} or \code{igraph}
#' @param names logical, should the names of the isolates be returned (
#' \code{TRUE} or their IDs (\code{FALSE})? The default is \code{TRUE}.
#' @param loops logical, should self-loops (if there are any) be taken into 
#' account? Default is \code{FALSE}, which is (almost) always what you want.
#'
#' @return vector with the isolates
#' @export
#' @seealso \code{\link{remove_isolates}}
#'
#' @examples
#' mat <- matrix(0, nrow = 4, ncol = 4)
#' # edges, incl one self-loop
#' mat[1, 3] <- mat[4,4] <- 1
#' ig <- igraph::graph_from_adjacency_matrix(mat)
#' find_isolates(ig)  # 2 4
#' # 4 has a loop to itself, including this removes its isolate-ship
#' find_isolates(ig, loops = TRUE)  # 2
#' igraph::V(ig)$name <-  LETTERS[1:4]
#' find_isolates(ig)  # B D
#' find_isolates(ig, names = FALSE)  # 2 4
#' 
#' nw <- network::as.network(mat, loops = TRUE)
#' find_isolates(nw)  # 2 4
#' network::set.vertex.attribute(nw, "vertex.names", LETTERS[1:4])
#' find_isolates(nw)  # B D
#' find_isolates(nw, names = FALSE)  # 2 4
find_isolates <- function(x, names = TRUE, loops = FALSE) {
  UseMethod("find_isolates")
}




#' @export
#' @describeIn find_isolates
find_isolates.default <- function(x, names = TRUE, loops = FALSE) {
  stop("'x' should be of class 'igraph' or 'network'")
}




#' @export
#' @describeIn find_isolates
find_isolates.igraph <- function(x, names = TRUE, loops = FALSE) {
  degs <- igraph::degree(x, mode = "all", loops = loops)
  isols <- which(degs == 0)
  if (names & has_vertexnames.igraph(x)) {
    names(isols)
  } else if (!names | !has_vertexnames.igraph(x)) {
    unname(isols)
  }
}



#' @export
#' @describeIn find_isolates
find_isolates.network <- function(x, names = TRUE, loops = FALSE) {
  welke <- sna::isolates(x, diag = loops)
  if (names & has_vertexnames.network(x)) {
    welke <- network::get.vertex.attribute(x, "vertex.names")[welke]
  }
  welke
}


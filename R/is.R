
#' Is the network bipartite?
#' 
#' Check if the network is bipartite
#' 
#' This function simply checks if \code{x} is a bipartite network.
#' This is only possible by checking whether the bipartite attributes are 
#' set in the x. 
#' Hence, if the network is bipartite, but the researcher neglected to include 
#' this information in the graph x, the result of this functions will be 
#' \code{FALSE}. Of course, this is how it should be: neglecting to include 
#' the bipartite attribute in the x is bad practice.
#'
#' @param x graph x of class \code{network} or \code{igraph}
#'
#' @return \code{TRUE} or \code{FALSE}
#' @export
#'
#' @examples
#' net <- igraph::sample_bipartite(10, 5, p =.1)
#' is_bipartite(net)  # TRUE
#' 
#' net <- igraph::erdos.renyi.game(10, p.or.m = .1, type = "gnp")
#' is_bipartite(net)  # FALSE
#' 
#' mat <- sna::rgraph(10, m = 1, tprob = .1)
#' net <- network::as.network.matrix(mat)
#' is_bipartite(net)  # FALSE
is_bipartite <- function(x) {
  UseMethod("is_bipartite")
}


#' @export
is_bipartite.default <- function(x) {
  txt <- methods_error_message("x", "is_bipartite")
  stop(txt)
}


#' @export
is_bipartite.network <- function(x) {
  bip <- network::get.network.attribute(x, "bipartite")
  if (is.null(bip)) {
    return(FALSE)
  } else if (is.logical(bip)) {
    return(bip)
  } else {
    return(bip >= 0)
  }
}


#' @export
is_bipartite.igraph <- function(x) {
  "type" %in% igraph::vertex_attr_names(x)
}












#' Is the network weighted?
#' 
#' Check if the network is weighted
#' 
#' This function checks if \code{x} is a weighted network.
#' 
#' For an x of class \code{igraph}or \code{network}, the function checks 
#' for the appropiate attribute in the graph x.
#' 
#' For a \code{matrix} the function returns \code{TRUE} if any of the 
#' elements is not or 1.
#'  
#' For a \code{data.frame} the function returns \code{TRUE} if it contains 
#' a numeric column called "weight", in addition to the first two columns 
#' of senders and receivers.
#'
#' @param x graph x of class \code{network}, \code{igraph}, 
#' \code{matrix}, or \code{data.frame}
#'
#' @return \code{TRUE} or \code{FALSE}
#' @export
is_weighted <- function(x) {
  UseMethod("is_weighted")
}


#' @export
is_weighted.default <- function(x) {
  txt <- methods_error_message("x", "is_weighted")
  stop(txt)
}


#' @export
is_weighted.network <- function(x) {
  "weight" %in% network::list.edge.attributes(x)
}

#' @export
is_weighted.igraph <- function(x) {
  igraph::is_weighted(x)
}


#' @export
is_weighted.matrix <- function(x) {
  !all(x == 0 | x == 1)
}

#' @export
is_weighted.data.frame <- function(x) {
  ncol(x) >= 3 && ("weight" %in% names(x) | is.numeric(x[, 3]))
}









#' Is the network directed?
#' 
#' Check if the network is directed
#' 
#' This function checks if \code{x} is a directed network.
#' 
#' For an x of class \code{igraph} or \code{network}, the function checks 
#' for the appropriate attribute in the graph x.
#' 
#' For a \code{matrix} the function returns \code{TRUE} if the matrix is 
#' not symmetric.
#'  
#' For a \code{data.frame} the function returns \code{TRUE} if the reciprocity 
#' of the network is not exactly 0 or 1.
#'
#' @param x graph x
#'
#' @return logical, \code{TRUE} or \code{FALSE}
#' @export
#' @examples
#' data(florentine, package = "snafun")
#' is_weighted(florentine$flobusiness)   # FALSE
#' @references largely based on similar functions from the excellent 
#' \code{migraph} package. 
#' These methods are licensed through the 
#' \href{https://choosealicense.com/licenses/mit/}{MIT license}
#' in combination with copyright for James Hollway.
is_directed <- function(x) {
  UseMethod("is_directed")
}


#' @export
is_directed.default <- function(x) {
  txt <- methods_error_message("x", "is_directed")
  stop(txt)
}


#' @export
is_directed.data.frame <- function(x) {
  net <- to_igraph.data.frame(x)
  !(igraph::reciprocity(net, ignore.loops = TRUE, mode = "default") == 0 | 
      igraph::reciprocity(net, ignore.loops = TRUE, mode = "default") == 1)
}

#' @export
is_directed.igraph <- function(x) {
  igraph::is.directed(x)
}


#' @export
is_directed.matrix <- function(x) {
  isSymmetric(x)
}


#' @export
is_directed.network <- function(x) {
  network::get.network.attribute(x, "directed")
}







#' Is the network signed?
#' 
#' Check if the network is signed
#' 
#' A signed \code{igraph} or \code{network} object has a "sign" attribute.
#' 
#' For a matrix or data.frame, a signed network is recognized by the presence 
#' of positive and negative ties, where values are integers.
#' A signed graph that only has positive integer values will not be recognized 
#' as such (but is pathologial anyway).
#' 
#' Specifially, for a \code{data.frame}, this check is conducted on its 
#' third column, which is the most logical column to hold the signs. 
#'
#' @param x graph object
#'
#' @return logical, \code{TRUE} or \code{FALSE}
#' @export
#' @references  functions from the excellent \code{migraph} package. 
#' These methods are licensed through the 
#' \href{https://choosealicense.com/licenses/mit/}{MIT license}
#' in combination with copyright for James Hollway.
#' @examples
#' data(florentine, package = "snafun")
#' is_signed(florentine$flobusiness)   # FALSE
is_signed <- function(x) {
  UseMethod("is_signed")
}


#' @export
is_signed.default <- function(x) {
  txt <- methods_error_message("x", "is_signed")
  stop(txt)
}



#' @export
is_signed.data.frame <- function(x) {
  is.integer(x[,3]) && any(x[,3] < 0)
}



#' @export
is_signed.matrix <- function(x) {
  is.integer(c(x)) && any(x < 0)
}


#' @export
is_signed.igraph <- function(x) {
  "sign" %in% igraph::edge_attr_names(x)
}



#' @export
is_signed.network <- function(x) {
  "sign" %in% network::list.edge.attributes(x)
}



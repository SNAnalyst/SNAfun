
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
  !isSymmetric(x)
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






#' Is the network connected?
#' 
#' Check if the network is connected
#'
#' The rule to determined if a graph is connected can be either "weak" or 
#' "strong". If "weak", vertex $i$ is connected to $j$ iff there exists a 
#' semi-path from $i$ to $j$ (i.e., a path in the weakly symmetrized graph).
#' If "strong", vertex $i$ is connected to $j$ iff there exists a 
#' direct path from $i$ to $j$ OR  a direct path from $j$ to $i$. 
#' 
#' In other words: a graph is said to be "strongly connected" if every pair of 
#' vertices($i$, $j$) in the graph contains a path between each other following 
#' the directions of the edges.  
#' In an unweighted directed graph G, every pair of vertices $i$ and $j$ should 
#' have a path in each direction between them i.e., a bidirectional path. 
#' The elements of the path matrix of such a graph will contain all 1’s.
#' A graph is "weakly connected" if when considering it as an $undirected$ graph 
#' it is connected, e.e., for every pair of distinct vertices $i$ and $j$ there 
#' exists an undirected path (potentially running opposite the direction of an 
#' edge) from $i$ to $j$.
#' 
#' A strongly connected graph is also weakly connected.
#' 
#' Note that the above rules are distinct for directed graphs only; if \code{x} 
#' is symmetric, the rule has no effect. An undirected graph ought not get the 
#' label of "strongly" or "weakly" connected, but it is connected or not.
#'
#' @param x graph of type \code{igraph} or \code{network}
#' @param rule character, either "strong" or "weak"
#'
#' @return logical
#' @export
#'
#' @examples
#' strong_i <- igraph::graph_from_literal(a --+ b --+ c --+ a)
#' is_connected(strong_i, "weak")  # TRUE
#' is_connected(strong_i, "strong")  # TRUE
#' strong_n <- snafun::to_network(strong_i)
#' is_connected(strong_n, "weak")  # TRUE
#' is_connected(strong_n, "strong")  # TRUE
#' 
#' weak_i <- igraph::graph_from_literal(a --+ b +-- c)
#' is_connected(weak_i, "weak")  # TRUE
#' is_connected(weak_i, "strong")  # FALSE
#' weak_n <- snafun::to_network(weak_i)
#' is_connected(weak_n, "weak")  # TRUE
#' is_connected(weak_n, "strong")  # FALSE
is_connected <- function(x, rule = c("weak", "strong")) {
  UseMethod("is_connected")
}


#' @export
is_connected.default <- function(x, rule = c("weak", "strong")) {
  txt <- methods_error_message("x", "is_connected")
  stop(txt)
}


#' @export
is_connected.network <- function(x, rule = c("weak", "strong")) {
  rule = snafun.match.arg(rule)
  sna::is.connected(g = x, connected = rule, comp.dist.precomp = NULL)
}


#' @export
is_connected.igraph <- function(x, rule = c("weak", "strong")) {
  rule = snafun.match.arg(rule)
  igraph::is.connected(x, mode = rule)
}





# is (describeIn) --------------------------------------------------------------






#' Is something?
#' 
#' Check if the network is something specific.
#' 
#' Check for various "identities" of the input object
#' 
#' @param x graph object
#'
#' @return logical, \code{TRUE} or \code{FALSE}
#' @name is_something
#' @examples
#' data(florentine, package = "snafun")
#' is_network(florentine$flobusiness)   # FALSE
#' is_igraph(florentine$flobusiness)   # TRUE
#' data(emon, package = "network")
#' is_network(emon$Cheyenne)   # TRUE
#' is_igraph(emon$Cheyenne)   # FALSE
NULL



#' @export
#' @describeIn is_something Check whether a graph is of class \code{network}
is_network <- function(x) {
  UseMethod("is_network")
}


#' @export
is_network.default <- function(x) {
  txt <- methods_error_message("x", "is_network")
  stop(txt)
}


#' @export
is_network.igraph <- function(x) {
  FALSE
}


#' @export
is_network.network <- function(x) {
  TRUE
}

#' @export
is_network.matrix <- function(x) {
  FALSE
}




#' @export
#' @describeIn is_something Check whether a graph is of class \code{igraph}
is_igraph <- function(x) {
  UseMethod("is_igraph")
}


#' @export
is_igraph.default <- function(x) {
  txt <- methods_error_message("x", "is_igraph")
  stop(txt)
}


#' @export
is_igraph.network <- function(x) {
  FALSE
}


#' @export
is_igraph.igraph <- function(x) {
  TRUE
}

#' @export
is_igraph.matrix <- function(x) {
  FALSE
}









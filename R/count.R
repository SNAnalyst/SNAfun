


#' Number of vertices in the graph
#' 
#' Count the number of vertices in the graph
#' 
#' Returns the number of vertices in an object of class \code{network} 
#' or \code{igraph}.
#'
#' @param x graph
#'
#' @return integer
#' @export
#'
#' @examples
#' net <- snafun::create_random_graph(10, "gnp", p = .15, graph = "igraph")
#' count_vertices(net)
#'
#' net <- snafun::create_random_graph(10, "gnp", p = .15, graph = "network")
#' count_vertices(net)
count_vertices <- function(x) {
  UseMethod("count_vertices")
}

#' @export
count_vertices.default <- function(x) {
  txt <- methods_error_message("x", "count_vertices")
  stop(txt)
}

#' @export
count_vertices.igraph <- function(x) {
  as.integer(igraph::gorder(x))
}


#' @export
count_vertices.network <- function(x) {
  as.integer(network::network.size(x))
}




#' Number of edges in the graph
#' 
#' Count the number of edges in the graph
#' 
#' Returns the number of edges in an object of class \code{network} 
#' or \code{igraph}.
#'
#' @param x graph
#'
#' @return integer
#' @export
#'
#' @examples
#' net <- snafun::create_random_graph(10, "gnp", p = .15, graph = "igraph")
#' count_edges(net)
#'
#' net <- snafun::create_random_graph(10, "gnp", p = .15, graph = "network")
#' count_edges(net)
count_edges <- function(x) {
  UseMethod("count_edges")
}

#' @export
count_edges.default <- function(x) {
  txt <- methods_error_message("x", "count_edges")
  stop(txt)
}

#' @export
count_edges.igraph <- function(x) {
  as.integer(igraph::gsize(x))
}


#' @export
count_edges.network <- function(x) {
  as.integer(network::network.edgecount(x))
}





#' Count connected components
#'
#' Count the number of connected components in a graph.
#'
#' Components are exact reachability-based parts of a graph. They are therefore
#' conceptually different from communities: community detection is heuristic,
#' whereas component membership is fully determined by the existence of paths.
#'
#' For directed graphs, \code{type = "weak"} counts weakly connected
#' components and \code{type = "strong"} counts strongly connected components.
#' For undirected graphs this argument is ignored.
#'
#' The function accepts the graph representations that are most common in
#' \code{snafun}: \code{igraph}, \code{network}, \code{matrix}, and
#' \code{data.frame} edgelists.
#'
#' @param x graph data
#' @param type character scalar, either \code{"weak"} or \code{"strong"}.
#'
#' @return integer scalar
#' @export
#'
#' @examples
#' g <- snafun::create_components_graph(
#'   n_vertices = 6,
#'   membership = c(1, 1, 1, 2, 2, 3),
#'   directed = FALSE,
#'   graph = "igraph"
#' )
#' count_components(g)
#'
#' g_d <- snafun::create_manual_graph(A +-+ B -+ C)
#' count_components(g_d, type = "weak")
#' count_components(g_d, type = "strong")
count_components <- function(x, type = c("weak", "strong")) {
  UseMethod("count_components")
}


#' @export
count_components.default <- function(x, type = c("weak", "strong")) {
  txt <- methods_error_message("x", "count_components")
  stop(txt)
}


#' @export
count_components.igraph <- function(x, type = c("weak", "strong")) {
  type <- snafun.match.arg(type)
  as.integer(igraph::components(x, mode = type)$no)
}


#' @export
count_components.network <- function(x, type = c("weak", "strong")) {
  type <- snafun.match.arg(type)
  count_components(snafun::to_igraph(x), type = type)
}


#' @export
count_components.matrix <- function(x, type = c("weak", "strong")) {
  type <- snafun.match.arg(type)
  bipartite <- nrow(x) != ncol(x)
  count_components(snafun::to_igraph(x, bipartite = bipartite), type = type)
}


#' @export
count_components.data.frame <- function(x, type = c("weak", "strong")) {
  type <- snafun.match.arg(type)
  count_components(snafun::to_igraph(x), type = type)
}










#' Count dyad types
#' 
#' Perform a dyad census
#' 
#' Perform a dyad census on the network data. As input, \code{x} can be 
#' any of \code{igraph}, \code{network}, or \code{matrix}.
#' 
#' \code{Mutual} represents the number of reciprocated dyads
#' 
#' \code{Asymmetric} represents the number of unreciprocated dyads. This is 
#' always 0 for undirected graphs.
#' 
#' \code{Null} represents the number of dyads without a relation between the vertices
#' 
#' The output is invisibly returned and (if \code{echo} is \code{TRUE}) 
#' also printed in the console.
#' 
#' @param x graph data
#' @param echo logical, should the result be printed in the console? Default is 
#' \code{TRUE}.
#'
#' @return a data.frame
#' @export
#'
#' @examples
#' data(judge_net, package = "snafun")
#' count_dyads(judge_net)
count_dyads <- function(x, echo = TRUE) {
  UseMethod("count_dyads")
}


#' @export
count_dyads.default <- function(x, echo = TRUE) {
  txt <- methods_error_message("x", "count_dyads")
  stop(txt)
}


#' @export
count_dyads.igraph <- function(x, echo = TRUE) {
  if (snafun::is_bipartite(x)) {
    stop("A bipartite option for a dyad census is not yet implemented.")
  } else {
    out <- as.data.frame(suppressWarnings(igraph::dyad_census(x)))
    names(out) <- c("Mutual", "Asymmetric", "Null")
    if (!snafun::is_directed(x)) out <- out[c(1, 3)]
  }
  if (echo) print(out, row.names = FALSE)
  invisible(out)
}


#' @export
count_dyads.network <- function(x, echo = TRUE) {
  if (snafun::is_bipartite(x)) {
    stop("A bipartite option for a dyad census is not yet implemented.")
  } else {
    out <- as.data.frame(suppressWarnings(sna::dyad.census(x)))
    names(out) <- c("Mutual", "Asymmetric", "Null")
    if (!snafun::is_directed(x)) out <- out[c(1, 3)]
  }
  if (echo) print(out, row.names = FALSE)
  invisible(out)
}



#' @export
count_dyads.matrix <- function(x, echo = TRUE) {
  if (nrow(x) != ncol(x)) {
    stop("'x' should be a square matrix, a bipartite option for a dyad census is not yet implemented.")
  } else {
    out <- as.data.frame(suppressWarnings(sna::dyad.census(snafun::to_network(x))))
    names(out) <- c("Mutual", "Asymmetric", "Null")
    if (!snafun::is_directed(x)) out <- out[c(1, 3)]
  }
  if (echo) print(out, row.names = FALSE)
  invisible(out)
}





#' Count triad types
#' 
#' Perform a trad census
#' 
#' Perform a trad census on the network data. As input, \code{x} can be 
#' any of \code{igraph}, \code{network}, or \code{matrix}.
#' 
#' The output is a data.frame that countains the counts for each of the 16 triad 
#' types.
#' 
#' The output is invisibly returned and (if \code{echo} is \code{TRUE}) 
#' also printed in the console.
#' 
#' @param x graph data
#' @param echo logical, should the result be printed in the console? Default is 
#' \code{TRUE}.
#'
#' @return a data.frame
#' @export
#'
#' @examples
#' data(judge_net, package = "snafun")
#' count_triads(judge_net)
count_triads <- function(x, echo = TRUE) {
  UseMethod("count_triads")
}

#' @export
count_triads.default <- function(x, echo = TRUE) {
  txt <- methods_error_message("x", "count_dyads")
  stop(txt)
}


#' @export
count_triads.igraph <- function(x, echo = TRUE) {
  if (snafun::is_bipartite(x)) {
    stop("A bipartite option for a triad census is not yet implemented.")
  } else {
    out <- suppressWarnings(igraph::triad_census(x))
    out <- data.frame(matrix(out, nrow = 1))
    names(out) <- c("003", "012", "102", "021D",
                    "021U", "021C", "111D", "111U",
                    "030T", "030C", "201", "120D",
                    "120U", "120C", "210", "300")
  }
  if (echo) print(out, row.names = FALSE)
  invisible(out)
}


#' @export
count_triads.network <- function(x, echo = TRUE) {
  if (snafun::is_bipartite(x)) {
    stop("A bipartite option for a triad census is not yet implemented.")
  } else {
    if (snafun::is_directed(x)) {
      out <- suppressWarnings(sna::triad.census(x, mode = "digraph")) |> as.data.frame()
    } else {
      triads <- suppressWarnings(sna::triad.census(x, mode = "graph")) |> as.data.frame()
      out <- data.frame(matrix(0, ncol = 16, nrow = 1))
      names(out) <- c("003", "012", "102", "021D",
                      "021U", "021C", "111D", "111U",
                      "030T", "030C", "201", "120D",
                      "120U", "120C", "210", "300")
      out[1, c("003", "102", "201", "300")] <- triads
    }
    if (echo) print(out, row.names = FALSE)
    invisible(out)
  }
}


#' @export
count_triads.matrix <- function(x, echo = TRUE) {
  if (ncol(x) != nrow(x)) {
    stop("'x' should be square, a bipartite option for a triad census is not yet implemented.")
  } else {
    out <- count_triads(snafun::to_network(x), echo = FALSE)
    if (echo) print(out, row.names = FALSE)
    invisible(out)
  }
}

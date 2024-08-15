

#' Distances to and from a vertex
#' 
#' Distances to and from a vertex
#' 
#' For each vertex, this function returns the average distance to all other 
#' vertices, the standard deviation of the distances, the maximum distance, 
#' the median distance, and the smmed distances.
#' 
#' The distances are determined either following the direction of the edges (
#' "out" or "in")--in case of a directed network, or irrespective of edge 
#' direction. 
#' 
#' Distances can be weighted against an edge attribute. 
#' 
#' When the graph is not connected, it is not obvious how to include the distances 
#' to/from unconnected vertices. If \code{count_unnconnected} is \code{TRUE}, the 
#' distances will count as infinite and most of the statistics will hence be 
#' infinite too. If \code{count_unnconnected} is \code{TRUE}, the paths to and from 
#' unconnected vertices are not included. Isolates will then have zero and NA 
#' scores, obviously.
#'
#' @param x input graph
#' @param mode character, how to follow edge direction: "all", "out", "in". 
#' Irrelevant for undirected graphs
#' @param weights if not \code{NULL}, the name of an edge attribute to use 
#' as edge weights
#' @param count_unnconnected logical, should unconnected vertices be included in 
#' the path lengths?
#'
#' @return data frame
#' @export
#'
#' @examples
#' data("florentine", package = "snafun")
#' g <- florentine$flobusiness
#' v_distance(g, mode = "all", count_unnconnected = FALSE)
#' v_distance(g, mode = "all", count_unnconnected = TRUE)
#' v_distance(g, mode = "in", count_unnconnected = FALSE)
#' v_distance(g, mode = "out", count_unnconnected = FALSE)
#' v_distance(snafun::to_network(g), mode = "all", count_unnconnected = FALSE)
#' v_distance(snafun::to_network(g), mode = "all", count_unnconnected = TRUE)
#' v_distance(snafun::to_network(g), mode = "in", count_unnconnected = FALSE)
#' v_distance(snafun::to_network(g), mode = "out", count_unnconnected = FALSE)
#' 
#' data(emon, package = "network")
#' g_n <- emon$LakePomona
#' v_distance(g_n, mode = "all")
#' v_distance(g_n, mode = "in")
#' v_distance(g_n, mode = "out")
#' g_i <- snafun::to_igraph(g_n)
#' v_distance(g_i, mode = "all")
#' v_distance(g_i, mode = "in")
#' v_distance(g_i, mode = "out")
v_distance <- function(x, mode = c("all", "out", "in"),
                              weights = NULL, count_unnconnected = FALSE) {
  UseMethod("v_distance")
}


#' @export
v_distance.default <- function(x, mode = c("all", "out", "in"),
                              weights = NULL, count_unnconnected = FALSE) {
  txt <- methods_error_message("x", "v_distance")
  stop(txt)
}


#' @export
v_distance.igraph <- function(x, mode = c("all", "out", "in"),
                       weights = NULL, count_unnconnected = FALSE) {
  if (!is.null(weights)) {
    if (!weights %in% igraph::edge_attr_names(x)) {
      stop("The 'weights' attribute does not occur in x")
    }
  }
  mode <- snafun.match.arg(mode)
  
  if (mode %in% c("out", "in")) {
    if (!is_connected(x, rule = "weak")) {
      warning("The graph is not fully weakly connected, interpret result with prudence.",
              call. = FALSE, immediate. = TRUE)
    }
  } else if (mode == "all") {
    if (!is_connected(x, rule = "strong")) {
      warning("The graph is not fully strongly connected, interpret result with prudence.",
              call. = FALSE, immediate. = TRUE)
    }
  } else {
    stop("Make sure to provide a correct 'mode' value")
  }
  dists <- igraph::distances(x, mode = mode, weights = weights)
  if (!count_unnconnected) dists[is.infinite(dists)] <- NA
  n <- igraph::vcount(x)
  average <- round(rowSums(dists, na.rm = TRUE)/n, digits = 2)
  sum <- rowSums(dists, na.rm = TRUE)
  stdev <- round(apply(dists, 1, stats::sd, na.rm = TRUE), digits = 2)
  max <- apply(dists, 1, max, na.rm = TRUE)
  median <- apply(dists, 1, stats::median, na.rm = TRUE)
  data.frame(average = average, median = median,
             stdev = stdev, max = max, sum = sum)
}






#' @export
v_distance.network <- function(x, mode = c("all", "out", "in"),
                               weights = NULL, count_unnconnected = FALSE) {
  x <- snafun::to_igraph(x)
  mode <- snafun.match.arg(mode)
  v_distance(x, 
             mode = mode, 
             weights = weights, 
             count_unnconnected = count_unnconnected)
}



#' Graph level indices
#' 
#' Graph level indices
#' 
#' Calculate several graph level indices. 
#' 
#' @param x graph object
#' @param mode Character constant, gives whether the shortest paths to or from 
#' the given vertices should be calculated for directed graphs. 
#' If \code{out} then the shortest paths from the vertex, if \code{in} then to 
#' it will be considered. If \code{all}, the default, then the corresponding 
#' undirected graph will be used, edge directions will be ignored. 
#' This argument is ignored for undirected graphs.
#' @param loops logical, should loops/diagonal be included? Default is \code{FALSE}.
#' This is almost always what you want.
#' @param type character, either "density" or "count"
#' @param cumulative logical, should the raw densities be returned or their 
#' cumulative sum
#' @param digits the maximal number of decimals in the result
#' @param directed Logical, whether directed or undirected paths are to be 
#' considered. This is ignored for undirected graphs. The default is \code{TRUE} 
#' is \code{x} is directed and \code{FALSE} otherwise
#' @param unconnected Logical, what to do if the graph is unconnected. 
#' If \code{FALSE}, the function will return a number that is one larger the 
#' largest possible diameter, which is always the number of vertices. 
#' If \code{TRUE}, the diameters of the connected components will be calculated 
#' and the largest one will be returned.
#' @param g1 an input graph of class \code{igraph}, \code{network}, \code{matrix}
#' @param g2 an input graph of class \code{igraph}, \code{network}, \code{matrix}
#' @param diag logical, indicating whether or not the diagonal should be treated 
#' as valid data. Set this \code{TRUE} if and only if the data can contain 
#' loops. Is \code{FALSE} by default.
#' @examples 
#' m <- matrix(rbinom(25, 1, 0.5), 5, 5)
#' diag(m) <- 0
#' g_density(snafun::to_network(m))
#' g_density(snafun::to_igraph(m))
#' # when loops matter
#' g <- igraph::graph( c(1,2, 2,2, 2,3) )
#' g_density(g, loops = FALSE)   # this is wrong, if loops matter
#' g_density(g, loops = TRUE)   # this is correct, if loops matter
#' 
#' g <- sna::rgraph(10, mode = "digraph")
#' g_n <- snafun::to_network(g)
#' g_mean_distance(g_n)
#' g_i <- snafun::to_igraph(g)
#' g_mean_distance(g_i)
#' 
#' g <- sna::rgraph(10, mode = "graph")
#' g_n <- snafun::to_network(g)
#' g_mean_distance(g_n)
#' g_i <- snafun::to_igraph(g)
#' g_mean_distance(g_i)
#' @name gli
NULL





####----------------------------------------------------------------------------
#' @describeIn gli Summary indices of a graph.
#'
#' This function provides a few very basic summary indices of a graph.
#' Only the default settings for the underlying functions (from this package)
#' are used. For further investigation, consider those settings.
#'
#' If \code{directed == TRUE} (the default), the graph will be treated as
#' directed if it is (and as undirected if it is not). When
#' \code{directed == FALSE} a directed graph will be forced to become an undirected
#' graph using \link{to_symmetric_matrix} with \code{rule = "weak"}.
#'
#' It can sometimes be informative to compare the summary stats for a directed
#' graph with its undirected equivalent.
#' @export
g_summary <- function(x, directed = TRUE) {
  if (!inherits(x, "igraph") & !inherits(x, "network")) {
    stop("Make sure 'x' is an 'igraph' or a 'network' object")
  }
  if (!directed) {
    x <- to_matrix(x) |>
      to_symmetric_matrix(rule = "weak") |>
      to_igraph()
  }

  density <- g_density(x)
  reciprocity <- g_reciprocity(x)
  transitivity <- g_transitivity(x)
  mean_distance <- g_mean_distance(x)
  dyad_census <- count_dyads(x, echo = FALSE)
  triad_census <- count_triads(x, echo = FALSE)
  number_of_isolates = length(find_isolates(x, names = FALSE))
  list(number_of_vertices = count_vertices(x),
       number_of_edges = count_edges(x),
       density = round(density, 3),
       reciprocity = round(reciprocity, 3),
       transitivity = round(transitivity, 3),
       mean_distance = round(mean_distance, 3),
       number_of_isolates = number_of_isolates,
       dyad_census = dyad_census,
       triad_census = triad_census)
}
#' 
#' 


####----------------------------------------------------------------------------
#' @describeIn gli Density of a graph. Weights are discarded. 
#' Use \code{\link[sna]{gden}} if edge weights are to be used and \code{code} is 
#' a graph of class \code{network}.
#' @export
g_density <- function(x, loops = FALSE) {
  UseMethod("g_density")
}


#' @export
g_density.default <- function(x, loops = FALSE) {
  txt <- methods_error_message("x", "g_density")
  stop(txt)
}


#' @export
g_density.igraph <- function(x, loops = FALSE) {
  igraph::edge_density(x, loops = loops)
}


#' @export
g_density.network <- function(x, loops = FALSE) {
  sna::gden(dat = x, diag = loops, 
            mode = ifelse(snafun::is_directed(x), "digraph", "graph"),
            ignore.eval = TRUE)
}



####----------------------------------------------------------------------------
#' @describeIn gli Mean path distance
#' @export
g_mean_distance <- function(x) {
  UseMethod("g_mean_distance")
}


#' @export
g_mean_distance.default <- function(x) {
  txt <- methods_error_message("x", "g_mean_distance")
  stop(txt)
}


#' @export
g_mean_distance.igraph <- function(x) {
  igraph::mean_distance(x, weights = NULL, directed = TRUE,
                        unconnected = TRUE,
                        details = FALSE)
}


#' @export
g_mean_distance.network <- function(x) {
  dists <- sna::geodist(x, count.paths = FALSE,
                        ignore.eval = TRUE)$gdist
  n <- network::network.size(x)
  sum(dists)/(n * (n-1))
}








####----------------------------------------------------------------------------
#' @describeIn gli Product-moment correlation between two networks. 
#' Missing values are permitted. Takes into account whether the 
#' graph is (un)directed (judged from \code{g1}). 
#' 
#' NOTE: The input graphs should be of class 
#' \code{igraph},\code{network} or 
#' \code{matrix}. It is possible to mis graph claases, so correlations can 
#' be calculated between graphs of class \code{network} and \code{igraph}, for 
#' example.
#' 
#' It is possible and the most common to provide two graphs directly, 
#' but the function also accepts a list of two graphs or an array 
#' of two graph (so, an array of size 2 x n x n).
#' 
#' Internally, the graphs are converted to matrices before correlation is 
#' calculated.
#' @export
#' @examples
#' # 
#' # correlation
#' # matrices
#' g1 <- sna::rgraph(10,1,tprob=c(0.2,0.2,0.5,0.5,0.8,0.8))
#' g2 <- sna::rgraph(10,1,tprob=c(0.2,0.2,0.5,0.5,0.8,0.8))
#' g_correlation(g1, g2)
#' 
#' g1 <- to_network(g1); g2 <- to_network(g2)
#' g_correlation(g1, g2)
#' 
#' g1 <- to_igraph(g1); g2 <- to_igraph(g2)
#' g_correlation(g1, g2)
g_correlation <- function(g1, g2, diag = FALSE) {
  UseMethod("g_correlation")
}


#' @export
g_correlation.default <- function(g1, g2, diag = FALSE) {
  txt <- methods_error_message("g1", "g_correlation")
  stop(txt)
}


#' @export
g_correlation.igraph <- function(g1, g2, diag = FALSE) {
  if (!inherits(g2, c("igraph", "network", "matrix"))) {
    stop("'g2' should be of class 'igraph', 'network', or 'matrix'")
  }
  x1 <- to_matrix(g1)
  x2 <- to_matrix(g2)
  directed <- is_directed(g1)
  sna::gcor(dat = x1, dat2 = x2, diag = diag, 
            mode = ifelse(directed, "digraph", "graph"))
}


#' @export
g_correlation.network <- function(g1, g2, diag = FALSE) {
  if (!inherits(g2, c("igraph", "network", "matrix"))) {
    stop("'g2' should be of class 'igraph', 'network', or 'matrix'")
  }
  if (inherits(g2, "igraph")) g2 <- to_matrix(g2)
  directed <- is_directed(g1)
  sna::gcor(dat = g1, dat2 = g2, diag = diag, 
            mode = ifelse(directed, "digraph", "graph"))
}

#' @export
g_correlation.matrix <- function(g1, g2, diag = FALSE) {
  if (!inherits(g2, c("igraph", "network", "matrix"))) {
    stop("'g2' should be of class 'igraph', 'network', or 'matrix'")
  }
  if (inherits(g2, "igraph")) g2 <- to_matrix(g2)
  directed <- is_directed(g1)
  sna::gcor(dat = g1, dat2 = g2, diag = diag, 
            mode = ifelse(directed, "digraph", "graph"))
}

#' @export
g_correlation.list <- function(g1, g2, diag = FALSE) {
  if (length(g1) != 2) {
    stop("When a list is provided, include exactly two graphs")
  }
  g_correlation(g1 = g1[[1]], g2 = g1[[2]], diag = diag)
}


#' @export
g_correlation.array <- function(g1, g2, diag = FALSE) {
  if (dim(g1)[1] != 2) {
    stop("When an array is provided, include exactly two graphs")
  }
  g_correlation(g1 = g1[1, , ], g2 = g1[2, , ], diag = diag)
}




####----------------------------------------------------------------------------
#' @describeIn gli Reciprocity
#' @export
#' @examples
#' # 
#' # reciprocity
#' g <- igraph::erdos.renyi.game(10, .3, type = "gnp", directed = TRUE)
#' g_reciprocity(g)
#' g_reciprocity(snafun::to_network(g))
#' 
#' g <- snafun::create_random_graph(10, strategy = "gnm", m = 2, directed = FALSE, graph = "igraph")
#' g_reciprocity(g)
#' g_reciprocity(snafun::to_network(g))
g_reciprocity <- function(x) {
  UseMethod("g_reciprocity")
}


#' @export
g_reciprocity.default <- function(x) {
  txt <- methods_error_message("x", "g_reciprocity")
  stop(txt)
}


#' @export
g_reciprocity.igraph <- function(x) {
  igraph::reciprocity(x, ignore.loops = TRUE, mode = "default")
}


#' @export
g_reciprocity.network <- function(x) {
  sna::grecip(dat = x, measure = "edgewise") |> unname()
}


####----------------------------------------------------------------------------
#' @describeIn gli Transitivity Transitivity is a triadic, algebraic structural 
#' constraint. In its weak form (which is the common form), the transitive constraint 
#' corresponds to a -> b -> c implying a -> c. This measure returns  the fraction 
#' of potentially intransitive triads obeying the weak condition. 
#' In other words, we count the number of triplets in which i -> j, 
#' j -> k, and i -> k, and divide by the number of triplets in which 
#' i -> j and j -> k (regardless of whether there is an i -> k edge).
#' 
#' Weights are discarded. Specific functions that can alternatively be used (and are 
#' called by this function) include \code{\link[sna]{gtrans}} (for objects of 
#' class \code{network}) and \code{\link[igraph]{transitivity}} (for objects of 
#' class \code{igraph}).
#' 
#' The \code{network} and \code{igraph} implementations differ and can give 
#' somewhat different results. 
#' 
#' @export
#' @examples
#' #
#' # transitivity
#' data("emon", package = "network")
#' g <- emon$Cheyenne
#' is_directed(g)  # TRUE
#' g_transitivity(g)
#' g_transitivity(to_network(g))
#' 
#' data(florentine, package = "snafun")
#' g <- florentine$flobusiness
#' is_directed(g)  # FALSE
#' g_transitivity(g)
#' g_transitivity(to_network(g))
g_transitivity <- function(x) {
  UseMethod("g_transitivity")
}


#' @export
g_transitivity.default <- function(x) {
  txt <- methods_error_message("x", "g_transitivity")
  stop(txt)
}


#' @export
g_transitivity.igraph <- function(x) {
  igraph::transitivity(x, type = "global")
}


#' @export
g_transitivity.network <- function(x) {
  sna::gtrans(x, 
              mode = ifelse(is_directed(x), "digraph", "graph"),
              measure = "weak",
              use.adjacency = TRUE)
}




####----------------------------------------------------------------------------
#' @describeIn gli Diameter of a graph. Weights are discarded. The diameter is 
#' equal to the maximum eccentricity scores across all vertices. 
#' Substantively, this measures the maximum number of steps that are needed to 
#' connect any two vertices in the graph (given that they are connected).
#' Use \code{\link[igraph]{diameter}} if edge weights are to be used or specific 
#' options are needed. 
#' @export
#' @examples 
#' #
#' # diameter
#' g <- igraph::make_ring(10)
#' g2 <- igraph::delete_edges(g, c(1,2,1,10))
#' igraph::diameter(g2, unconnected=TRUE)
#' igraph::diameter(g2, unconnected=FALSE)
#' g_diameter(g2)  # 7
#' g_diameter(g2, unconnected = FALSE)  # Inf
#' 
#' g_diameter(to_network(g2)) # 7
#' g_diameter(to_network(g2), unconnected = FALSE) # Inf
g_diameter <- function(x, 
                       directed = is_directed(x),
                       unconnected = TRUE) {
  UseMethod("g_diameter")
}


#' @export
g_diameter.default <- function(x, 
                               directed = is_directed(x),
                               unconnected = TRUE) {
  txt <- methods_error_message("x", "g_diameter")
  stop(txt)
}


#' @export
g_diameter.igraph <- function(x, 
                              directed = is_directed(x),
                              unconnected = TRUE) {
  igraph::diameter(x, directed = directed,
                   unconnected = unconnected,
                   weights = NA)
}


#' @export
g_diameter.network <- function(x, 
                               directed = is_directed(x),
                               unconnected = TRUE) {
  g <- to_igraph(x)
  g_diameter.igraph(x = g, directed = directed,
             unconnected = unconnected)
}






####----------------------------------------------------------------------------
#' @describeIn gli Radius of a graph. Weights are discarded. The diameter is 
#' equal to the minimum eccentricity scores across all vertices. 
#' Substantively, this measures the minimum number of steps that are needed to 
#' connect any two vertices in the graph (given that they are connected). 
#' In other words, regardless of which vertex is considered, it is not possible 
#' to reach all other vertices from it within fewer than this number of steps.
#' @export
#' @examples 
#' #
#' # radius
#' g_i <- snafun::create_random_graph(10, strategy = "gnp", p = .2, 
#'    directed = TRUE, graph = "igraph")
#' # add isolate
#' g_i_iso <- igraph::add_vertices(g_i, nv = 1)
#' igraph::radius(g_i)
#' snafun::v_eccentricity(g_i)
#' snafun::find_isolates(g_i_iso)
#' snafun::v_eccentricity(g_i_iso)  # the isolate has eccentricity 0
#' igraph::radius(g_i_iso)  # also 0, should raise a flag about isolates
#' g_radius(g_i, mode = "all")
#' g_radius(g_i, mode = "in")
#' g_radius(g_i, mode = "out")
#' 
#' g_n <- snafun::to_network(g_i)
#' g_radius(g_n, mode = "all")
#' g_radius(g_n, mode = "in")
#' g_radius(g_n, mode = "out")
g_radius <- function(x, mode = c("all", "out", "in")) {
  UseMethod("g_radius")
}


#' @export
g_radius.default <- function(x, mode = c("all", "out", "in")) {
  txt <- methods_error_message("x", "g_radius")
  stop(txt)
}


#' @export
g_radius.igraph <- function(x, mode = c("all", "out", "in")) {
  mode = snafun.match.arg(mode)
  igraph::radius(x, mode = mode)
}


#' @export
g_radius.network <- function(x, mode = c("all", "out", "in")) {
  mode = snafun.match.arg(mode)
  g <- to_igraph(x)
  g_radius.igraph(x = g, mode = mode)
}


####----------------------------------------------------------------------------
#' @describeIn gli Compactness of a graph. Weights are discarded. 
#' 
#' The compactness of a graph is the average of the inverse distances across 
#' all dyads in the graph. Mathematically:
#' 
#' \deqn{\frac{\sum_{i,j : i \neq j} (\frac{1}{d_{ij}})}{n(n-1)}}
#' 
#' where the denominator is altered according to the number of dyads in the graph.
#' 
#' If all vertices are directly tied to each other, compactness is 1. 
#' The shorter the paths between vertices, the larger the inverse distances become. 
#' Hence, the shorter the paths, the higher compactness. As such, it is often 
#' seen as a measure of cohesion.
#' 
#' This measure works well for disconnected graphs. Inverse distances between 
#' disconnected vertices are 0, so the more disconnected the graph, the lower its 
#' compactness. 
#' 
#' Thus, the measure runs between 0 (abolutely "uncompact") to 1 (maximally compact).
#' @export
#' @examples 
#' #
#' # compactness
#' g_i <- snafun::create_random_graph(10, strategy = "gnp", p = .2, 
#'    directed = TRUE, graph = "igraph")
#' # add isolate
#' g_i_iso <- igraph::add_vertices(g_i, nv = 1)
#' g_n <- snafun::to_network(g_i)
#' g_n_iso <- snafun::to_network(g_i_iso)
#' g_compactness(g_i)
#' g_compactness(g_n)
#' g_compactness(g_i, mode = "all")   # if direction is irrelevant
#' g_compactness(g_i_iso)
#' g_compactness(g_n_iso)
#' 
#' g1 <- igraph::graph_from_literal(A-B-C-D-E-F)
#' g2 <- igraph::delete_edges(g1, 5)
#' g3 <- igraph::delete.edges(g1, 3)
#' g4 <- igraph::delete_edges(g1, c(4, 5))
#' g5 <- igraph::delete_edges(g1, c(2, 4))
#' g_compactness(g1)
#' g_compactness(g2)
#' g_compactness(g3)
#' g_compactness(g4)
#' g_compactness(g5)
g_compactness <- function(x, mode = c("out", "in", "all")) {
  UseMethod("g_compactness")
}


#' @export
g_compactness.default <- function(x, mode = c("out", "in", "all")) {
  txt <- methods_error_message("x", "g_compactness")
  stop(txt)
}


#' @export
g_compactness.igraph <- function (x, mode = c("out", "in", "all")) {
  mode <- snafun.match.arg(mode)
  dist <- 1/igraph::distances(x, weights = NA, mode = mode)
  diag(dist) <- NA
  summed <- sum(dist, na.rm = T)  # 16.33333
  dyads <- sum(!is.na(dist))   # 90
  summed/dyads
}


#' @export
g_compactness.network <- function(x, mode = c("out", "in", "all")) {
  mode = snafun.match.arg(mode)
  g <- snafun::to_igraph(x)
  g_compactness.igraph(x = g, mode = mode)
}







####----------------------------------------------------------------------------
#' @describeIn gli Degree distribution. 
#' 
#' The distribution of the degrees of the vertices in the graph.
#' 
#' Returns a matrix with the counts or densities for each degree and the 
#' degrees themselves as column names.
#' The result can be raw or cumulative (starting from degree 0.
#' @export
#' @examples 
#' #
#' # degree distribution
#' g <- snafun::create_random_graph(1000, "gnp", p = .01)
#' g_degree_distribution(g)
g_degree_distribution <- function(x, mode = c("out", "in", "all"),
                                  type = c("density", "count"),
                                  cumulative = FALSE, 
                                  loops = FALSE,
                                  digits = 3) {
  UseMethod("g_degree_distribution")
}


#' @export
g_degree_distribution.default <- function(x, mode = c("out", "in", "all"),
                                          type = c("density", "count"),
                                          cumulative = FALSE, 
                                          loops = FALSE,
                                          digits = 3) {
  txt <- methods_error_message("x", "g_degree_distribution")
  stop(txt)
}


#' @export
g_degree_distribution.igraph <- function(x, mode = c("out", "in", "all"),
                                          type = c("density", "count"),
                                          cumulative = FALSE, 
                                          loops = FALSE,
                                          digits = 3) {
  mode <- snafun.match.arg(mode)
  type <- snafun.match.arg(type)
  degree_dist(x = x, mode = mode, loops = loops, type = type,
                          cumulative = cumulative, digits = digits) 
}



#' @export
g_degree_distribution.network <- function(x, mode = c("out", "in", "all"),
                                          type = c("density", "count"),
                                          cumulative = FALSE, 
                                          loops = FALSE,
                                          digits = 3) {
  mode = snafun.match.arg(mode)
  type <- snafun.match.arg(type)
  degree_dist(x = x, mode = mode, loops = loops, type = type,
              cumulative = cumulative, digits = digits) 
}


degree_dist <- function(x, mode, loops, cumulative, digits, type) {
  cs <- v_degree(x = x, mode = mode, loops = loops)
  hi <- graphics::hist(cs, -1:max(cs), plot = FALSE)
  if (type == "density") {
    hi <- hi$density
  } else {
    hi <- hi$counts
  }
  
  if (!cumulative) {
    res <- hi
  } else {
    res <- cumsum(hi)
  }
  res <- matrix(res, nrow = 1)
  colnames(res) <- 1:length(res)
  round(res, digits = digits)
}





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
#' @param attrname optional character scalar naming the vertex attribute for
#' assortativity.
#' @param values optional vector with one value per vertex. This is mainly
#' useful for matrices and plain edge lists, but can also be used with graph
#' objects.
#' @param vertices optional vertex metadata table used only for
#' \code{data.frame} edgelists. The first column should contain the vertex
#' names that appear in the edgelist; further columns are treated as vertex
#' attributes.
#' @param normalized logical; should the assortativity be normalized?
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
#' @param weight local, should weight be included? Is \code{TRUE} by default.
#' @param wf not to be set by the user, for internal use only
#' @examples 
#' m <- matrix(rbinom(25, 1, 0.5), 5, 5)
#' diag(m) <- 0
#' g_density(snafun::to_network(m))
#' g_density(snafun::to_igraph(m))
#' # when loops matter
#' g <- snafun::to_igraph(data.frame(from = c(1, 2, 2), to = c(2, 2, 3)))
#' g_density(g, loops = FALSE)   # this is wrong, if loops matter
#' g_density(g, loops = TRUE)   # this is correct, if loops matter
#' 
#' g <- snafun::create_random_graph(10, "gnp", p = .2, directed = TRUE, graph = "igraph")
#' g_n <- snafun::to_network(g)
#' g_mean_distance(g_n)
#' g_mean_distance(g)
#' 
#' g <- snafun::create_random_graph(10, "gnp", p = .2, directed = FALSE, graph = "igraph")
#' g_n <- snafun::to_network(g)
#' g_mean_distance(g_n)
#' g_mean_distance(g)
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
  number_of_isolates = length(extract_isolates(x, names = FALSE))
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
#' Note that this function computes the density of a bipartite network without 
#' symmetrizing, so the density that is reported is half that of the density 
#' reported by the \code{\link[sna]{gden}} in this specific case.
#' Also note that \code{\link[igraph]{edge_density}} does not correctly deal with 
#' loops, so the current function reports correct density values whereas 
#' \code{\link[igraph]{edge_density}} reports incorrect values.
#' @export
g_density <- function(x, loops = FALSE) {
  if (!(snafun::is_igraph(x) || snafun::is_network(x) || is.matrix(x))) {
    stop("'g_density' requires an graph object of class 'igraph', 'network', or 'matrix'")
  }
  
  is_directed <- snafun::is_directed(x)
  
  g <- snafun::to_matrix(x)
  is_bipartite <- (nrow(g) != ncol(g))
  if (!loops & !is_bipartite) {diag(g) <- 0}
  # a regular matrix is symmetrized, this is not applicable for a bipartite networtk
  if (!is_bipartite & !is_directed) {
    g <- snafun::to_symmetric_matrix(g, rule = "weak")
  }
  
  no_of_vertices <- nrow(g)
  no_of_edges <- sum(g != 0)    # value is discarded
  
  if (loops & is_bipartite) {
    stop("You want loops to be included, but 'x' is a bipartite network and is therefore not allowed to have loops.")
  }
  
  if (no_of_vertices == 0) {
    return(NaN)
  } else if (no_of_vertices == 1) {
    if (!loops) {  # no loops allowed, so no edges are possible
      return(NaN)
    } else {       # 1 vertex, so max 1 edge (=loop) is possible, so density is 1 or 0
      return(no_of_edges)
    }
  }
  
  if (!loops & !is_bipartite) {
    return(no_of_edges / (no_of_vertices * (no_of_vertices - 1)))
  }
  if (loops & !is_bipartite) {
    return(no_of_edges / (no_of_vertices  * no_of_vertices))
  }
  
  if (!loops & is_bipartite) {
    return(no_of_edges / (nrow(g) * ncol(g)))
  }
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
#' \code{matrix}. It is possible to mix graph classes, so correlations can 
#' be calculated between graphs of class \code{network} and \code{igraph}, for 
#' example.
#' 
#' If a weight attribute is included in the graphs, these are used by default.
#' 
#' It is possible and the most common to provide two graphs directly, 
#' but the function also accepts a list of two graphs or an array 
#' of two graph (so, an array of size 2 x n x n). 
#' In this case, provide the list or array as the \code{g1} argument. 
#' The \code{g2} is ignored if \code{g1} is a list or array.
#' 
#' Internally, the graphs are converted to matrices before correlation is 
#' calculated.
#' @export
#' @examples
#' #
#' # correlation
#' # matrices
#' # 
#' g1 <- snafun::to_matrix(
#'   snafun::create_random_graph(10, "gnp", p = .3, directed = TRUE, graph = "igraph")
#' )
#' g2 <- snafun::to_matrix(
#'   snafun::create_random_graph(10, "gnp", p = .3, directed = TRUE, graph = "igraph")
#' )
#' g_correlation(g1, g2)
#' g1 <- to_network(g1); g2 <- to_network(g2)
#' g_correlation(g1, g2)
#' g1 <- to_igraph(g1); g2 <- to_igraph(g2)
#' g_correlation(g1, g2)              
g_correlation <- function(g1, g2, diag = FALSE, weight = TRUE, wf = NULL) {
  UseMethod("g_correlation")
}



#' @export
g_correlation.default <- function(g1, g2, diag = FALSE, weight = TRUE, wf = NULL) {
  txt <- methods_error_message("g1", "g_correlation")
  stop(txt)
}




#' @export
g_correlation.igraph <- function(g1, g2, diag = FALSE, weight = TRUE, wf = NULL) {
  wf <- 0  # is -999, this shows that weights have already been fixed/dealt with
  if (!weight) {
    if (is_weighted(g1)) g1 <- remove_edge_weight(g1)
    if (is_weighted(g2)) g2 <- remove_edge_weight(g2)
    wf <- -999  # weird number, not normally just tried by an investigative user
  } else {
    if (is_weighted(g1) && !is_weighted(g2)) {
      warning("'g1' is weighted, but 'g2' is not; correlation will assume weights")
    } else if (!is_weighted(g1) && is_weighted(g2)) {
      warning("'g2' is weighted, but 'g1' is not; correlation will assume weights")
    }
  }
  g1 <- to_matrix(g1)
  g_correlation.matrix(g1, g2, diag = diag, weight = weight, wf = wf)
}


#' @export
g_correlation.network <- function(g1, g2, diag = FALSE, weight = TRUE, wf = NULL) {
  wf <- 0
  if (!weight) {
    if (is_weighted(g1)) g1 <- remove_edge_weight(g1)
    if (is_weighted(g2)) g2 <- remove_edge_weight(g2)
    wf <- -999
  } else {
    if (is_weighted(g1) && !is_weighted(g2)) {
      warning("'g1' is weighted, but 'g2' is not; correlation will assume weights")
    } else if (!is_weighted(g1) && is_weighted(g2)) {
      warning("'g2' is weighted, but 'g1' is not; correlation will assume weights")
    }
  }
  g1 <- to_matrix(g1)
  g_correlation.matrix(g1, g2, diag = diag, weight = weight, wf = wf)
}


#' @export
g_correlation.matrix <- function(g1, g2, diag = FALSE, weight = TRUE, wf = NULL) {
  if (!inherits(g2, c("igraph", "network", "matrix"))) {
    stop("'g2' should be of class 'igraph', 'network', or 'matrix'")
  }
  
  x1 <- g1
  x2 <- to_matrix(g2)
  
  if (is.null(wf) || wf != -999) {  # other functions have not dealt with weight already
    if (!weight) {
      if (is_weighted(g1)) x1[x1 != 0] <- 1
      if (is_weighted(g2)) x2[x2 != 0] <- 1
    } else {
      if (is_weighted(g1) && !is_weighted(g2)) {
        warning("'g1' appears weighted, but 'g2' is not; correlation will assume weights")
      } else if (!is_weighted(x1) && is_weighted(x2)) {
        warning("'g2' appears weighted, but 'g1' is not; correlation will assume weights")
      }
    }
  }
  
  if (any(dim(x1) != dim(x2))) {stop("The input graphs should have the same sizes")}
  directed <- is_directed(g1)
  if (!diag) {
    diag(x1) <- NA
    diag(x2) <- NA
  }
  if (!directed) {
    x1[upper.tri(x1)] <- NA
    x2[upper.tri(x2)] <- NA
  }
  cor(as.vector(x1), as.vector(x2), use = "complete.obs")
}




#' @export
g_correlation.list <- function(g1, g2, diag = FALSE, weight = TRUE, wf = NULL) {
  if (length(g1) != 2) {
    stop("When a list is provided, include exactly two graphs in 'g1'")
  }
  g_correlation(g1 = g1[[1]], g2 = g1[[2]], diag = diag, weight = weight, wf = wf)
}


#' @export
g_correlation.array <- function(g1, g2, diag = FALSE, weight = TRUE, wf = NULL) {
  if (dim(g1)[1] != 2) {
    stop("When an array is provided, include exactly two graphs inside 'g1'")
  }
  g_correlation(g1 = g1[1, , ], g2 = g1[2, , ], diag = diag, weight = weight, wf = wf)
}








####----------------------------------------------------------------------------
#' @describeIn gli Reciprocity
#' @export
#' @examples
#' # 
#' # reciprocity
#' g <- snafun::create_random_graph(10, "gnp", p = .3, directed = TRUE, graph = "igraph")
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
#' class \code{igraph}). Internally, \code{snafun} now computes weak
#' transitivity from a binary adjacency matrix for all supported input formats.
#' This keeps the result consistent across backends and also allows the
#' function to work for one-mode matrices and edge lists.
#' 
#' Self-loops are ignored, weights are discarded, and the result is
#' \code{NaN} when there are no valid length-2 paths that could potentially be
#' transitive. Bipartite inputs are not supported.
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
#'
#' m <- snafun::to_matrix(g)
#' g_transitivity(m)
#'
#' el <- snafun::to_edgelist(g)
#' g_transitivity(el)
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
  g_transitivity_matrix_backend(snafun::to_matrix(x))
}


#' @export
g_transitivity.network <- function(x) {
  g_transitivity_matrix_backend(snafun::to_matrix(x))
}


#' @export
g_transitivity.matrix <- function(x) {
  g_transitivity_matrix_backend(x)
}


#' @export
g_transitivity.data.frame <- function(x) {
  g_transitivity_matrix_backend(snafun::to_matrix(x))
}



#' Compute weak transitivity from a binary adjacency matrix
#'
#' Internal helper that standardizes the transitivity calculation across the
#' supported one-mode backends. Each non-zero entry is treated as a tie,
#' self-loops are ignored, and the result is returned as the share of
#' transitive length-2 paths among all length-2 paths.
#'
#' @param x adjacency matrix for a one-mode network
#'
#' @return numeric scalar with the weak transitivity, or \code{NaN} if there
#' are no valid length-2 paths
#' @keywords internal
g_transitivity_matrix_backend <- function(x) {
  if (!is.matrix(x)) {
    x <- as.matrix(x)
  }
  
  if (nrow(x) != ncol(x)) {
    stop("'g_transitivity' is only defined for one-mode networks; bipartite inputs are not supported")
  }
  
  x <- matrix(
    data = x,
    nrow = nrow(x),
    ncol = ncol(x),
    dimnames = dimnames(x)
  )
  storage.mode(x) <- "numeric"
  x[is.na(x)] <- 0
  x[x != 0] <- 1
  
  # Weak transitivity is defined on ties between distinct vertices, so loops do
  # not create valid 2-paths or transitive closures here.
  if (nrow(x) > 0) {
    diag(x) <- 0
  }
  
  if (nrow(x) == 0) {
    return(NaN)
  }
  
  two_paths <- x %*% x
  diag(two_paths) <- 0
  
  denominator <- sum(two_paths)
  if (denominator == 0) {
    return(NaN)
  }
  
  numerator <- sum(two_paths * x)
  numerator / denominator
}



####----------------------------------------------------------------------------
#' @describeIn gli Assortativity by vertex attribute or supplied vertex values.
#'
#' Assortativity captures the tendency of connected vertices to resemble one
#' another on a vertex-level characteristic. For numeric characteristics,
#' assortativity is a correlation-like coefficient. For categorical
#' characteristics, it summarizes the extent to which ties stay within versus
#' between categories.
#'
#' The graph itself can be supplied as an \code{igraph}, \code{network},
#' \code{matrix}, or \code{data.frame} edgelist. The vertex information can be
#' supplied in two ways:
#' \itemize{
#' \item via \code{attrname}, when the graph object already stores the required
#' vertex attribute;
#' \item via \code{values}, a vector of length equal to the number of vertices.
#' }
#'
#' For plain \code{data.frame} edgelists, \code{attrname} only works when
#' either hidden vertex metadata are present (for example after
#' \code{snafun::to_edgelist()}) or when a \code{vertices} table is supplied.
#'
#' This function is intended for one-mode networks. Bipartite inputs are
#' rejected.
#' @export
#' @examples
#' g <- snafun::create_manual_graph(A -- B -- C -- D -- E -- F -- A)
#' g <- snafun::add_vertex_attributes(g, "group", c("A", "A", "A", "B", "B", "B"))
#' g_assortativity(g, attrname = "group")
#'
#' m <- snafun::to_matrix(g)
#' g_assortativity(m, values = c("A", "A", "A", "B", "B", "B"))
#'
#' vertices <- data.frame(
#'   name = LETTERS[1:6],
#'   group = c("A", "A", "A", "B", "B", "B")
#' )
#' el <- snafun::to_edgelist(g)
#' g_assortativity(el, attrname = "group", vertices = vertices)
g_assortativity <- function(x,
                            attrname = NULL,
                            values = NULL,
                            vertices = NULL,
                            directed = TRUE,
                            normalized = TRUE) {
  UseMethod("g_assortativity")
}


#' @export
g_assortativity.default <- function(x,
                                    attrname = NULL,
                                    values = NULL,
                                    vertices = NULL,
                                    directed = TRUE,
                                    normalized = TRUE) {
  txt <- methods_error_message("x", "g_assortativity")
  stop(txt)
}


#' @export
g_assortativity.igraph <- function(x,
                                   attrname = NULL,
                                   values = NULL,
                                   vertices = NULL,
                                   directed = TRUE,
                                   normalized = TRUE) {
  assortativity_from_graph(
    graph = x,
    attrname = attrname,
    values = values,
    directed = directed,
    normalized = normalized
  )
}


#' @export
g_assortativity.network <- function(x,
                                    attrname = NULL,
                                    values = NULL,
                                    vertices = NULL,
                                    directed = TRUE,
                                    normalized = TRUE) {
  assortativity_from_graph(
    graph = snafun::to_igraph(x),
    attrname = attrname,
    values = values,
    directed = directed,
    normalized = normalized
  )
}


#' @export
g_assortativity.matrix <- function(x,
                                   attrname = NULL,
                                   values = NULL,
                                   vertices = NULL,
                                   directed = TRUE,
                                   normalized = TRUE) {
  if (!is.null(attrname)) {
    stop(
      "Matrix inputs do not store vertex attributes. Please supply 'values' instead.",
      call. = FALSE
    )
  }
  assortativity_from_graph(
    graph = snafun::to_igraph(x, bipartite = nrow(x) != ncol(x)),
    attrname = attrname,
    values = values,
    directed = directed,
    normalized = normalized
  )
}


#' @export
g_assortativity.data.frame <- function(x,
                                       attrname = NULL,
                                       values = NULL,
                                       vertices = NULL,
                                       directed = TRUE,
                                       normalized = TRUE) {
  graph <- if (is.null(vertices)) {
    snafun::to_igraph(x)
  } else {
    snafun::to_igraph(x, vertices = vertices)
  }
  assortativity_from_graph(
    graph = graph,
    attrname = attrname,
    values = values,
    directed = directed,
    normalized = normalized
  )
}


assortativity_from_graph <- function(graph,
                                     attrname = NULL,
                                     values = NULL,
                                     directed = TRUE,
                                     normalized = TRUE) {
  if (!xor(is.null(attrname), is.null(values))) {
    stop(
      "Please provide exactly one of 'attrname' or 'values'.",
      call. = FALSE
    )
  }
  
  if (snafun::is_bipartite(graph)) {
    stop(
      "'g_assortativity' is only implemented for one-mode networks.",
      call. = FALSE
    )
  }
  
  vertex_values <- resolve_assortativity_values(
    graph = graph,
    attrname = attrname,
    values = values
  )
  
  directed <- isTRUE(directed) && snafun::is_directed(graph)
  
  if (is.factor(vertex_values) || is.character(vertex_values) || is.logical(vertex_values)) {
    vertex_codes <- as.integer(factor(vertex_values))
    return(
      igraph::assortativity_nominal(
        graph = graph,
        types = vertex_codes,
        directed = directed,
        normalized = normalized
      )
    )
  }
  
  if (!is.numeric(vertex_values)) {
    stop(
      "Assortativity values should be numeric, logical, character, or factor.",
      call. = FALSE
    )
  }
  
  igraph::assortativity(
    graph = graph,
    values = as.numeric(vertex_values),
    directed = directed,
    normalized = normalized
  )
}


resolve_assortativity_values <- function(graph,
                                         attrname = NULL,
                                         values = NULL) {
  if (!is.null(attrname)) {
    if (!snafun::has_vertex_attribute(graph, attrname = attrname)) {
      stop(
        "This vertex attribute does not occur on the graph object.",
        call. = FALSE
      )
    }
    values <- snafun::extract_vertex_attribute(graph, name = attrname)
  }
  
  if (length(values) != snafun::count_vertices(graph)) {
    stop(
      "The supplied values should have length equal to the number of vertices.",
      call. = FALSE
    )
  }
  
  if (any(is.na(values))) {
    stop(
      "Assortativity can not be calculated with missing vertex values.",
      call. = FALSE
    )
  }
  
  if (!is.null(names(values)) && snafun::has_vertexnames(graph)) {
    vertex_names <- snafun::extract_vertex_names(graph)
    if (setequal(names(values), vertex_names)) {
      values <- values[vertex_names]
    }
  }
  
  if (is.character(values)) {
    return(as.character(values))
  }
  
  values
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
#' g2 <- snafun::create_manual_graph(
#'   1 -- 2 -- 3 -- 4 -- 5 -- 6 -- 7 -- 8,
#'   9 -- 10
#' )
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
#' g_i <- snafun::create_manual_graph(2:3:4:5:6:7:8:9:10 -+ 1)
#' g_i_iso <- snafun::create_manual_graph(2:3:4:5:6:7:8:9:10 -+ 1, 11)
#' snafun::v_eccentricity(g_i)
#' snafun::extract_isolates(g_i_iso)
#' snafun::v_eccentricity(g_i_iso)  # the isolate has eccentricity 0
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
#' g_i <- snafun::create_manual_graph(2:3:4:5:6:7:8:9:10 -+ 1)
#' g_i_iso <- snafun::create_manual_graph(2:3:4:5:6:7:8:9:10 -+ 1, 11)
#' g_n <- snafun::to_network(g_i)
#' g_n_iso <- snafun::to_network(g_i_iso)
#' g_compactness(g_i)
#' g_compactness(g_n)
#' g_compactness(g_i, mode = "all")   # if direction is irrelevant
#' g_compactness(g_i_iso)
#' g_compactness(g_n_iso)
#' 
#' g1 <- snafun::create_manual_graph(A -- B -- C -- D -- E -- F)
#' g2 <- snafun::create_manual_graph(A -- B -- C -- D -- E, F)
#' g3 <- snafun::create_manual_graph(A -- B, C -- D -- E -- F)
#' g4 <- snafun::create_manual_graph(A -- B -- C -- D, E -- F)
#' g5 <- snafun::create_manual_graph(A -- B, C -- D, E -- F)
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




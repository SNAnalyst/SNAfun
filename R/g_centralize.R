
#' Centralization
#' 
#' Centralization around several vertex measures
#' 
#' Centralization is a method for creating a graph level centralization
#' measure from the centrality scores of the vertices.
#' 
#' Centralization is a general method for calculating a graph-level
#' centrality score based on vertex-level centrality measure. 
#' 
#' There are many ways of aggregating vertex-level centrality scores to
#' graph-level centralization. A very common method is the Freeman method. 
#' 
#' The formula for this is
#' \deqn{C(G)=\sum_v (\max_w c_w - c_v)}
#' where \eqn{c_v} is the centrality of vertex \eqn{v}.
#' 
#' This is the summed absolute deviation from the maximum of C on G. 
#' Generally, this value is normalized by the theoretical maximum centralization 
#' score, conditional on the graph and the values of the arguments (such as 
#' the choice for \code{mode} and \code{directed}).
#' 
#' The Freeman method is implemented in \code{method == "freeman"}. This is 
#' the default. When this 
#' method is chosen, the function returns a list with the following components:
#' \describe{
#'   \item{centralization}{The graph level centrality index.}
#'   \item{theoretical_max}{The maximum theoretical graph level
#'     centralization score for a graph with the given number of vertices,
#'     using the same parameters. If the \code{normalized} argument was
#'     \code{TRUE}, then the result was divided by this number.}
#' }
#' 
#' For calculating Freeman centralization with the native packages, see 
#' \code{\link[igraph]{centralize}} (for \code{igraph} objects) or 
#' \code{\link[sna]{centralization}} (for \code{network} objects).
#' 
#' The "freeman" "method is implemented for "betweenness", "closeness", "degree", 
#' and "eigenvector".#' 
#' 
#' An alternative way of calculating centralization is by taking the standard 
#' deviation of the vertex-level scores. This method is implemented in 
#' \code{method == "sd"}.
#' 
#' When this method is chosen, the function returns the value of the standard 
#' deviation.
#' 
#' The "sd" method has been implemented for the centrality indices 
#' "betweenness", "closeness", "degree", "eccentricity", 
#' "eigenvector", "geokpath", "harmonic", "pagerank", 
#' "shapley", and "stress".
#' 
#' @param x graph 
#' @param measure character, name of which centrality index needs to be used
#' @param directed logical, should the graph be considered to be directed (if it 
#' is undirected, this is ignored)
#' @param mode parameter for several centrality indices
#' @param k parameter for \link{v_geokpath}
#' @param damping parameter for \link{v_pagerank}
#' @param normalized Logical scalar. Whether to normalize the graph level centrality 
#' score by dividing by the theoretical maximum. Only used when \code{method == "freeman"}.
#' @param method character, either "freeman" (the default) or "sd"
#' @return for \code{method == "freeman"} a list, for \code{method == "sd"} a numeric. 
#' See Details for further info.
#' @examples 
#' # A BA graph is quite centralized
#' g <- igraph::sample_pa(1000, m = 4)
#' g_centralize(g, "degree", method = "freeman")
#' g_centralize(snafun::to_network(g), "degree", method = "freeman")
#' g_centralize(g, "betweenness", method = "freeman")
#' g_centralize(g, "betweenness", method = "freeman", directed = FALSE)
#' g_centralize(g, "betweenness", method = "freeman")
#' g_centralize(g, "closeness", method = "freeman")
#' g_centralize(g, "closeness", method = "freeman", mode = "out") # can be NaN
#' # eigenvector does not work for this directed graph, so make it undirected
#' g_centralize(g, "eigenvector", method = "freeman", directed = FALSE) 
#' 
#' # The most centralized graph according to eigenvector centrality
#' g0 <- igraph::make_graph(c(2,1), n = 10, dir = FALSE)
#' g1 <- igraph::make_star(10, mode = "undirected")
#' g_centralize(g0, "eigenvector", method = "freeman")$centralization
#' g_centralize(g1, "eigenvector", method = "freeman")$centralization
#' 
#' # method sd
#' g_centralize(g, "degree", method = "sd")
#' g_centralize(g, "degree", method = "sd", mode = "out")
#' g_centralize(g, "betweenness", method = "sd")
#' g_centralize(g, "betweenness", method = "sd", directed = FALSE)
#' g_centralize(g, "betweenness", method = "sd")
#' g_centralize(g, "closeness", method = "sd")
#' g_centralize(g, "closeness", method = "sd", mode = "out") 
#' g_centralize(g, "eccentricity", method = "sd")
#' g_centralize(g, "eigenvector", directed = FALSE, method = "sd")
#' g_centralize(g, "geokpath", method = "sd", k = 4)
#' g_centralize(g, "geokpath", method = "sd", k = 2)
#' g_centralize(g, "harmonic", method = "sd")
#' g_centralize(g, "pagerank", method = "sd", damping = .9)
#' g_centralize(g, "pagerank", method = "sd", damping = .5)
#' g_centralize(g, "shapley", method = "sd")
#' g_centralize(g, "stress", method = "sd")
#' @export
g_centralize <- function(x, 
                         measure = "betweenness",
                         directed = TRUE,
                         mode = c("all", "out", "in"),
                         k = 3,
                         damping = 0.85,
                         normalized = TRUE,
                         method = c("freeman", "sd")) {
  UseMethod("g_centralize")
}


#' @export
g_centralize.default <- function(x, 
                                 measure = "betweenness",
                                 directed = TRUE,
                                 mode = c("all", "out", "in"),
                                 k = 3,
                                 damping = 0.85,
                                 normalized = TRUE,
                                 method = c("freeman", "sd")) {
  txt <- methods_error_message("x", "g_centralize")
  stop(txt)
}


#' @export
g_centralize.igraph <- function(x, 
                                  measure = "betweenness",
                                  directed = TRUE,
                                  mode = c("all", "out", "in"),
                                  k = 3,
                                  damping = 0.85,
                                  normalized = TRUE,
                                  method = c("freeman", "sd")) {
  
  method <- snafun.match.arg(method)
  if (!method %in% c("freeman", "sd")) {
    stop("'method' needs to be either 'freeman' or 'sd'")
  }
  if (length(measure) > 1) stop("Please specify only one 'measure'")
  mode <- snafun.match.arg(mode)
  
  if (method == "freeman") {
    implemented <- c("betweenness", "closeness", "degree", "eigenvector")
    if (!measure %in% implemented) {
      stop('Only ', paste(implemented, collapse = ", "), ' are currently implemented for "freeman".')
    }
    if (measure == "betweenness") {
      ret <- igraph::centr_betw(x, directed = directed, normalized = normalized)
    } else if (measure == "closeness") {
      ret <- igraph::centr_clo(x, mode = mode, normalized = normalized)
    } else if (measure == "degree") {
      ret <- igraph::centr_degree(x, normalized = normalized, loops = FALSE)
    } else if (measure == "eigenvector") {
      ret <- igraph::centr_eigen(x, directed = directed, normalized = normalized, 
                                 scale = TRUE)
    }
    ret <- ret[c("centralization", "theoretical_max")]
  } else {    # method == "sd"
    implemented <- c("betweenness", "closeness", "degree", "eccentricity",
                     "eigenvector", "geokpath", "harmonic", "pagerank", 
                     "shapley", "stress")
    if (!measure %in% implemented) {
      stop('Only ', paste(implemented, collapse = ", "), ' are currently implemented for "freeman".')
    }
    if (measure == "betweenness") {
      ret <- stats::sd(v_betweenness(x, directed = directed, rescaled = FALSE), na.rm = TRUE)
    } else if (measure == "closeness") {
      ret <- stats::sd(v_closeness(x, mode = mode, rescaled = FALSE), na.rm = TRUE)
    } else if (measure == "degree") {
      ret <- stats::sd(v_degree(x, mode = mode, rescaled = FALSE, loops = FALSE), na.rm = TRUE)
    } else if (measure == "eccentricity") {
      ret <- stats::sd(v_eccentricity(x, mode = mode, rescaled = FALSE), na.rm = TRUE)
    } else if (measure == "eigenvector") {
      ret <- stats::sd(v_eigenvector(x, directed = directed, rescaled = FALSE), na.rm = TRUE)
    } else if (measure == "geokpath") {
      ret <- stats::sd(v_geokpath(x, mode = mode, k = k, rescaled = FALSE), na.rm = TRUE)
    } else if (measure == "harmonic") {
      ret <- stats::sd(v_harmonic(x, mode = mode, rescaled = FALSE), na.rm = TRUE)
    } else if (measure == "pagerank") {
      ret <- stats::sd(v_pagerank(x, directed = directed, damping = damping, rescaled = FALSE), na.rm = TRUE)
    } else if (measure == "shapley") {
      ret <- stats::sd(v_shapley(x, add.vertex.names = FALSE, vids = NULL, rescaled = FALSE), na.rm = TRUE)
    } else if (measure == "stress") {
      ret <- stats::sd(v_stress(x, directed = directed, rescaled = FALSE), na.rm = TRUE)
    } 
  }
  
  ret
}





#' @export
g_centralize.network <- function(x, 
                                 measure = "betweenness",
                                 directed = TRUE,
                                 mode = c("all", "out", "in"),
                                 k = 3,
                                 damping = 0.85,
                                 normalized = TRUE,
                                 method = c("freeman", "sd")) {
  g <- snafun::to_igraph(x)
  method <- snafun.match.arg(method)
  if (!method %in% c("freeman", "sd")) {
    stop("'method' needs to be either 'freeman' or 'sd'")
  }
  if (length(measure) > 1) stop("Please specify only one 'measure'")
  mode <- snafun.match.arg(mode)
  
  g_centralize.igraph(x = g, 
                      measure = measure,
                      directed = directed,
                      mode = mode,
                      k = k,
                      damping = damping,
                      normalized = normalized,
                      method = method)
}




#' Community structure via short random walks
#' 
#' Extract communities using the walktrap algorithm
#' 
#' Find community structure in the graph by using random walks across the 
#' edges of the graph. The idea is that short random walks tend to stay in the 
#' same community.
#' 
#' The result of this function can be queried using the utility functions 
#' documented here: \code{\link[igraph]{membership}},
#' 
#' @param x The input graph, edge directions are ignored in directed graphs.
#' @param weights The weights of the edges. It must be a positive numeric vector, 
#' \code{NULL} or \code{NA}. If it is \code{NULL} and the input graph has a 
#' ‘weight’ edge attribute, then that attribute will be used. If \code{NULL} and 
#' no such attribute is present, then the edges will have equal weights. 
#' Set this to \code{NA} if the graph was a ‘weight’ edge attribute, but you 
#' don't want to use it for community detection. Larger edge weights increase 
#' the probability that an edge is selected by the random walker. 
#' In other words, larger edge weights correspond to stronger connections.
#' @param steps The length of the random walks to perform.
#' @param modularity Logical scalar, whether to include the vector of the 
#' modularity scores in the result. If the membership argument is \code{TRUE}, 
#' then it will always be calculated.
#' @param merges Logical scalar, whether to include the merge matrix in the result.
#' @param membership Logical scalar, whether to calculate the membership vector 
#' for the split corresponding to the highest modularity value.
#' @references this function just wraps the \code{\link[igraph]{cluster_walktrap}}
#' function and documentation.
#' @return a \code{communities} object
#' @export
extract_comm_walktrap <- function(x,
                                    weights = NA,
                                    steps = 4,
                                    modularity = TRUE,
                                    merges = TRUE,
                                    membership = TRUE) {
  UseMethod("extract_comm_walktrap")
}



#' @export
extract_comm_walktrap.default <- function(x,
                                            weights = NA,
                                            steps = 4,
                                            modularity = TRUE,
                                            merges = TRUE,
                                            membership = TRUE) {
  txt <- methods_error_message("x", "extract_comm_walktrap")
  stop(txt)
}


#' @export
extract_comm_walktrap.igraph <- function(x,
                                         weights = NA,
                                         steps = 4,
                                         modularity = TRUE,
                                         merges = TRUE,
                                         membership = TRUE) {
  igraph::cluster_walktrap(graph = x, weights = weights, 
                           steps = steps, modularity = modularity,
                           merges = merges, membership = membership)
}


#' @export
extract_comm_walktrap.network <- function(x,
                                          weights = NA,
                                          steps = 4,
                                          modularity = TRUE,
                                          merges = TRUE,
                                          membership = TRUE) {
  g <- snafun::to_igraph(x)
  igraph::cluster_walktrap(graph = g, weights = weights, 
                           steps = steps, modularity = modularity,
                           merges = merges, membership = membership)
}



####----------------------------------------------------------------------------


#' Community structure via multi-level optimization of modularity
#' 
#' Extract communities using the Louvain algorithm for undirected graphs.
#' 
#' NOTE: this algorithm only works with undirected graphs.
#' 
#' This function implements the multi-level modularity optimization algorithm for 
#' finding community structure, also known as the "Louvain" algorithm. 
#' It is based on the modularity measure and a hierarchical approach.
#' Initially, each vertex is assigned to a community on its own. In every step, 
#' vertices are re-assigned to communities in a local, greedy way: each vertex 
#' is moved to the community with which it achieves the highest contribution to 
#' modularity. When no vertices can be reassigned, each community is considered 
#' a vertex on its own, and the process starts again with the merged communities. 
#' The process stops when there is only a single vertex left or when the 
#' modularity cannot be increased any more in a step.
#'
#' @param x The undirected input graph, edge directions are ignored in directed graphs.
#' @param weights The weights of the edges. It must be a positive numeric vector, 
#' \code{NULL} or \code{NA}. If it is \code{NULL} and the input graph has a 
#' ‘weight’ edge attribute, then that attribute will be used. If \code{NULL} and 
#' no such attribute is present, then the edges will have equal weights. 
#' Set this to \code{NA} if the graph was a ‘weight’ edge attribute, but you 
#' don't want to use it for community detection. Larger edge weights increase 
#' the probability that an edge is selected by the random walker. 
#' In other words, larger edge weights correspond to stronger connections.
#' @param resolution Optional resolution parameter that allows the user to adjust 
#' the resolution parameter of the modularity function that the algorithm uses 
#' internally. Lower values typically yield fewer, larger clusters. The original 
#' definition of modularity is recovered when the resolution parameter is set to 1.
#' @references this function just wraps the \code{\link[igraph]{cluster_louvain}}
#' function and documentation.
#' @return a \code{communities} object
#' @export
extract_comm_louvain <- function(x,
                                 weights = NA,
                                 resolution = 1) {
  UseMethod("extract_comm_louvain")
}



#' @export
extract_comm_louvain.default <- function(x,
                                         weights = NA,
                                         resolution = 1) {
  txt <- methods_error_message("x", "extract_comm_louvain")
  stop(txt)
}


#' @export
extract_comm_louvain.igraph <- function(x,
                                        weights = NA,
                                        resolution = 1) {
  if (snafun::is_directed(x)) {
    stop("The Louvain community detection works for undirected graphs only,
         please provide an undirected graph.")
  }
  igraph::cluster_louvain(graph = x, weights = weights, 
                           resolution = resolution)
}


#' @export
extract_comm_louvain.network <- function(x,
                                         weights = NA,
                                         resolution = 1) {
  if (snafun::is_directed(x)) {
    stop("The Louvain community detection works for undirected graphs only,
         please provide an undirected graph.")
  }
  g <- snafun::to_igraph(x)
  igraph::cluster_louvain(graph = g, weights = weights, 
                          resolution = resolution)
}







####----------------------------------------------------------------------------

#' Community structure via greedy optimization of modularity
#' 
#' Extract communities via greedy optimization of modularity
#' 
#' NOTE: this algorithm only works with undirected graphs.
#' 
#' This function tries to find dense subgraph, also called communities in graphs 
#' via directly optimizing a modularity score.
#' 
#' The result of this function can be queried using the utility functions 
#' documented here: \code{\link[igraph]{membership}},
#' 
#' @param x The undirected input graph, edge directions are ignored in directed graphs.
#' @param weights The weights of the edges. It must be a positive numeric vector, 
#' \code{NULL} or \code{NA}. If it is \code{NULL} and the input graph has a 
#' ‘weight’ edge attribute, then that attribute will be used. If \code{NULL} and 
#' no such attribute is present, then the edges will have equal weights. 
#' Set this to \code{NA} if the graph was a ‘weight’ edge attribute, but you 
#' don't want to use it for community detection. Larger edge weights increase 
#' the probability that an edge is selected by the random walker. 
#' In other words, larger edge weights correspond to stronger connections.
#' @param modularity Logical scalar, whether to include the vector of the 
#' modularity scores in the result. If the membership argument is \code{TRUE}, 
#' then it will always be calculated.
#' @param merges Logical scalar, whether to include the merge matrix in the result.
#' @param membership Logical scalar, whether to calculate the membership vector 
#' for the split corresponding to the highest modularity value.
#' @references this function just wraps the \code{\link[igraph]{cluster_fast_greedy}}
#' function and documentation.
#' @return a \code{communities} object
#' @export
extract_comm_fastgreedy <- function(x,
                                  weights = NA,
                                  modularity = TRUE,
                                  merges = TRUE,
                                  membership = TRUE) {
  UseMethod("extract_comm_fastgreedy")
}



#' @export
extract_comm_fastgreedy.default <- function(x,
                                          weights = NA,
                                          modularity = TRUE,
                                          merges = TRUE,
                                          membership = TRUE) {
  txt <- methods_error_message("x", "extract_comm_fastgreedy")
  stop(txt)
}


#' @export
extract_comm_fastgreedy.igraph <- function(x,
                                         weights = NA,
                                         modularity = TRUE,
                                         merges = TRUE,
                                         membership = TRUE) {
  if (snafun::is_directed(x)) {
    stop("Fast greedy community detection works for undirected graphs only,
         please provide an undirected graph.")
  }
  igraph::cluster_fast_greedy(graph = x, weights = weights, 
                           modularity = modularity,
                           merges = merges, membership = membership)
}


#' @export
extract_comm_fastgreedy.network <- function(x,
                                            weights = NA,
                                            modularity = TRUE,
                                            merges = TRUE,
                                            membership = TRUE) {
  if (snafun::is_directed(x)) {
    stop("Fast greedy community detection works for undirected graphs only,
         please provide an undirected graph.")
  }
  g <- snafun::to_igraph(x)
  igraph::cluster_fast_greedy(graph = g, weights = weights, 
                              modularity = modularity,
                              merges = merges, membership = membership)
}





####----------------------------------------------------------------------------

#' Community structure based on edge betweenness
#' 
#' Extract communities via the Girvan Newman method
#' 
#' The edge betweenness score of an edge measures the number of shortest paths 
#' through it. The idea of the edge betweenness-based community structure 
#' detection is that it is likely that edges connecting separate modules have 
#' high edge betweenness as all the shortest paths from one module to another 
#' must traverse through them. So if we gradually remove the edge with the 
#' highest edge betweenness score we will get a hierarchical map, a rooted tree, 
#' called a dendrogram of the graph. The leafs of the tree are the individual 
#' vertices and the root of the tree represents the whole graph.
#' 
#' The result of this function can be queried using the utility functions 
#' documented here: \code{\link[igraph]{membership}},
#' 
#' @param x The input graph, edge directions are ignored in directed graphs.
#' @param weights The weights of the edges. It must be a positive numeric vector, 
#' \code{NULL} or \code{NA}. If it is \code{NULL} and the input graph has a 
#' ‘weight’ edge attribute, then that attribute will be used. If \code{NULL} and 
#' no such attribute is present, then the edges will have equal weights. 
#' Set this to \code{NA} if the graph was a ‘weight’ edge attribute, but you 
#' don't want to use it for community detection. Larger edge weights increase 
#' the probability that an edge is selected by the random walker. 
#' In other words, larger edge weights correspond to stronger connections.
#' @param directed Logical constant, whether to calculate directed edge betweenness 
#' for directed graphs. It is ignored for undirected graphs.
#' @param modularity Logical scalar, whether to include the vector of the 
#' modularity scores in the result. If the membership argument is \code{TRUE}, 
#' then it will always be calculated.
#' @param edge.betweenness Logical constant, whether to return the edge betweenness 
#' of the edges at the time of their removal.
#' @param merges Logical scalar, whether to include the merge matrix in the result.
#' @param bridges	Logical constant, whether to return a list the edge removals 
#' which actually splitted a component of the graph.
#' @param membership Logical scalar, whether to calculate the membership vector 
#' for the split corresponding to the highest modularity value.
#' @references this function just wraps the \code{\link[igraph]{edge_betweenness}}
#' function and documentation.
#' @return a communities object that can be handled with the functions at 
#' \code{\link[igraph]{membership}}.
#' @export
extract_comm_girvan <- function(x,
                                weights = NA,
                                directed = TRUE,
                                modularity = TRUE,
                                edge.betweenness = FALSE,
                                bridges = FALSE,
                                merges = TRUE,
                                membership = TRUE) {
  UseMethod("extract_comm_girvan")
}



#' @export
extract_comm_girvan.default <- function(x,
                                        weights = NA,
                                        directed = TRUE,
                                        modularity = TRUE,
                                        edge.betweenness = FALSE,
                                        bridges = FALSE,
                                        merges = TRUE,
                                        membership = TRUE) {
  txt <- methods_error_message("x", "extract_comm_girvan")
  stop(txt)
}


#' @export
extract_comm_girvan.igraph <- function(x,
                                       weights = NA,
                                       directed = TRUE,
                                       modularity = TRUE,
                                       edge.betweenness = FALSE,
                                       bridges = FALSE,
                                       merges = TRUE,
                                       membership = TRUE) {
  # if (snafun::is_directed(x)) {
  #   stop("Fast greedy community detection works for undirected graphs only,
  #        please provide an undirected graph.")
  # }
  igraph::cluster_edge_betweenness(graph = x, weights = weights, 
                              modularity = modularity,
                              merges = merges, membership = membership,
                              edge.betweenness = edge.betweenness,
                              bridges = bridges)
}


#' @export
extract_comm_girvan.network <- function(x,
                                        weights = NA,
                                        directed = TRUE,
                                        modularity = TRUE,
                                        edge.betweenness = FALSE,
                                        bridges = FALSE,
                                        merges = TRUE,
                                        membership = TRUE) {
  # if (snafun::is_directed(x)) {
  #   stop("Fast greedy community detection works for undirected graphs only,
  #        please provide an undirected graph.")
  # }
  g <- snafun::to_igraph(x)
  igraph::cluster_edge_betweenness(graph = g, weights = weights, 
                              modularity = modularity,
                              merges = merges, membership = membership,
                              edge.betweenness = edge.betweenness,
                              bridges = bridges)
}



####----------------------------------------------------------------------------

#' Plot a dendrogram of the community structure
#' 
#' Plot a hierarchical cluster analysis on a communities object
#' 
#' Plot the result of one of the community detection algorithms as a nice 
#' dendrogram. Except for \code{x}, all arguments are used for the \code{plot} 
#' function.
#' 
#' This works for the results from the \code{\link{extract_comm_walktrap}}, 
#' \code{\link{extract_comm_fastgreedy}}, and \code{\link{extract_comm_girvan}}. 
#' It (currently) does not work for the results from \code{\link{extract_comm_louvain}}.
#' 
#' @param x a \code{communities} object
#' @param labels A character vector of labels for the leaves of the tree. 
#' By default the row names or row numbers of the original data are used. 
#' If \code{labels = FALSE} no labels at all are plotted.
#' @param hang The fraction of the plot height by which labels should hang 
#' below the rest of the plot. A negative value will cause the labels to hang 
#' down from 0.
#' @param axes,frame_plot,ann logical flags as in \code{\link[graphics]{plot.default}}.
#' @param main,sub,xlab,ylab character strings for \code{\link[graphics]{title}}.
#' @param ... Further graphical arguments. E.g., \code{cex} controls the size of 
#' the labels (if plotted) in the same way as \code{\link[graphics]{text}}.
#'
#' @return nothing, only the plot is shown
#' @export
plot_comm_dendrogram <- function(x, labels = NULL, hang = 0.1, 
                                  axes = TRUE, frame_plot = FALSE, ann = TRUE,
                                  main = "Communities dendrogram",
                                  sub = "", xlab = "", ylab = "", ...) {
  if (!inherits(x, "communities")) {
    stop("Make sure to input a 'communities' object for 'x'.")
  }
  stats::as.hclust(x) |> graphics::plot(labels = labels, hang = hang, check = FALSE,
                                     axes = axes, frame.plot = frame_plot, ann = ann,
                                     main = main, sub = sub, xlab = NULL, 
                                     ylab = "", ...)
}

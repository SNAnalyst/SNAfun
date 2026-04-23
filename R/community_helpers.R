#' Community-object helpers
#'
#' Utility functions for working with \code{igraph} community objects.
#'
#' The community detection functions in \pkg{snafun}, such as
#' \code{\link{extract_comm_fastgreedy}} and \code{\link{extract_comm_louvain}},
#' return objects of class \code{communities}. Those objects already work with a
#' number of \pkg{igraph} helper functions, but students then need to remember a
#' second mini-API. The helpers documented here provide a small and consistent
#' \pkg{snafun} layer for the most commonly used community-object queries.
#'
#' \code{extract_comm_modularity()} deserves one extra remark. When it is called
#' without a graph, it returns the modularity value stored in the community
#' object itself. This mirrors \code{igraph::modularity(coms)} and is usually
#' what you want for an untouched result from a community-detection algorithm.
#' However, if the membership vector was modified afterwards, for example via
#' \code{\link{merge_membership}}, the stored modularity can become stale. In
#' that case, pass the graph as well so the modularity is recomputed from the
#' current membership assignment.
#'
#' @param x object of class \code{communities}
#' @param graph graph used with the community object. This can be an
#'   \code{igraph}, \code{network}, \code{matrix}, or \code{data.frame}
#'   edgelist.
#' @param ... additional arguments passed on to the underlying plotting method.
#'
#' @return Depending on the helper: a membership vector, an integer count, a
#'   named vector of community sizes, a merge matrix, a modularity value, a
#'   logical edge vector, or a plot.
#' @name comm_helpers
NULL


#' Extract community membership
#'
#' Return the community id of each vertex.
#'
#' This is the \pkg{snafun} wrapper around \code{igraph::membership()}.
#'
#' @rdname comm_helpers
#' @export
#'
#' @examples
#' data("judge_net", package = "snafun")
#' coms <- snafun::extract_comm_fastgreedy(judge_net)
#' snafun::extract_comm_membership(coms)
extract_comm_membership <- function(x) {
  UseMethod("extract_comm_membership")
}


#' @export
extract_comm_membership.default <- function(x) {
  txt <- methods_error_message("x", "extract_comm_membership")
  stop(txt)
}


#' @export
extract_comm_membership.communities <- function(x) {
  igraph::membership(x)
}


#' Count detected communities
#'
#' Return how many communities the object currently contains.
#'
#' This is the \pkg{snafun} equivalent of \code{length(coms)} for a
#' \code{communities} object.
#'
#' @rdname comm_helpers
#' @export
#'
#' @examples
#' data("judge_net", package = "snafun")
#' coms <- snafun::extract_comm_fastgreedy(judge_net)
#' snafun::count_communities(coms)
count_communities <- function(x) {
  UseMethod("count_communities")
}


#' @export
count_communities.default <- function(x) {
  txt <- methods_error_message("x", "count_communities")
  stop(txt)
}


#' @export
count_communities.communities <- function(x) {
  as.integer(length(x))
}


#' Extract community sizes
#'
#' Return the number of vertices in each community.
#'
#' This is the \pkg{snafun} wrapper around \code{igraph::sizes()}.
#'
#' @rdname comm_helpers
#' @export
#'
#' @examples
#' data("judge_net", package = "snafun")
#' coms <- snafun::extract_comm_fastgreedy(judge_net)
#' snafun::extract_comm_sizes(coms)
extract_comm_sizes <- function(x) {
  UseMethod("extract_comm_sizes")
}


#' @export
extract_comm_sizes.default <- function(x) {
  txt <- methods_error_message("x", "extract_comm_sizes")
  stop(txt)
}


#' @export
extract_comm_sizes.communities <- function(x) {
  igraph::sizes(x)
}


#' Extract community merges
#'
#' Return the merge matrix stored in a hierarchical community object.
#'
#' For methods without a merge history, \code{NULL} is returned.
#'
#' @rdname comm_helpers
#' @export
#'
#' @examples
#' data("judge_net", package = "snafun")
#' coms <- snafun::extract_comm_fastgreedy(judge_net)
#' snafun::extract_comm_merges(coms)
extract_comm_merges <- function(x) {
  UseMethod("extract_comm_merges")
}


#' @export
extract_comm_merges.default <- function(x) {
  txt <- methods_error_message("x", "extract_comm_merges")
  stop(txt)
}


#' @export
extract_comm_merges.communities <- function(x) {
  igraph::merges(x)
}


#' Extract the community algorithm name
#'
#' Return the name of the algorithm that created the community object.
#'
#' @rdname comm_helpers
#' @export
#'
#' @examples
#' data("judge_net", package = "snafun")
#' coms <- snafun::extract_comm_fastgreedy(judge_net)
#' snafun::extract_comm_algorithm(coms)
extract_comm_algorithm <- function(x) {
  UseMethod("extract_comm_algorithm")
}


#' @export
extract_comm_algorithm.default <- function(x) {
  txt <- methods_error_message("x", "extract_comm_algorithm")
  stop(txt)
}


#' @export
extract_comm_algorithm.communities <- function(x) {
  igraph::algorithm(x)
}


#' Extract the community modularity
#'
#' Return the modularity value associated with the community assignment.
#'
#' If \code{graph} is omitted, the modularity stored inside the community object
#' is returned. If \code{graph} is supplied, the modularity is recomputed from
#' the current membership vector on that graph.
#'
#' @rdname comm_helpers
#' @export
#'
#' @examples
#' data("judge_net", package = "snafun")
#' coms <- snafun::extract_comm_fastgreedy(judge_net)
#' snafun::extract_comm_modularity(coms)
extract_comm_modularity <- function(x, graph = NULL) {
  UseMethod("extract_comm_modularity")
}


#' @export
extract_comm_modularity.default <- function(x, graph = NULL) {
  txt <- methods_error_message("x", "extract_comm_modularity")
  stop(txt)
}


#' @export
extract_comm_modularity.communities <- function(x, graph = NULL) {
  if (is.null(graph)) {
    return(as.numeric(igraph::modularity(x)))
  }

  graph_i <- coerce_graph_to_igraph_for_communities(graph)
  membership <- align_community_membership_to_graph(x = x, graph = graph_i)
  igraph::modularity(graph_i, membership = unname(membership))
}


#' Extract the crossing indicator for edges
#'
#' Determine for each edge whether it runs between two different communities.
#'
#' This wraps \code{igraph::crossing()}, but the graph can be supplied in the
#' common \pkg{snafun} formats.
#'
#' @rdname comm_helpers
#' @export
#'
#' @examples
#' data("judge_net", package = "snafun")
#' coms <- snafun::extract_comm_fastgreedy(judge_net)
#' snafun::extract_comm_crossing(coms, judge_net)
extract_comm_crossing <- function(x, graph) {
  UseMethod("extract_comm_crossing")
}


#' @export
extract_comm_crossing.default <- function(x, graph) {
  txt <- methods_error_message("x", "extract_comm_crossing")
  stop(txt)
}


#' @export
extract_comm_crossing.communities <- function(x, graph) {
  community_crossing_values(graph = graph, x = x)
}


#' Plot a community object
#'
#' Plot a community assignment on top of the corresponding graph.
#'
#' This is a thin \pkg{snafun} wrapper around \code{igraph}'s internal
#' \code{plot.communities()} helper. It keeps the familiar community-plotting
#' behaviour, but accepts the same graph classes as the rest of \pkg{snafun}.
#'
#' @rdname comm_helpers
#' @export
#'
#' @examples
#' \dontrun{
#' data("judge_net", package = "snafun")
#' coms <- snafun::extract_comm_fastgreedy(judge_net)
#' snafun::plot_communities(coms, judge_net)
#' }
plot_communities <- function(x, graph, ...) {
  UseMethod("plot_communities")
}


#' @export
plot_communities.default <- function(x, graph, ...) {
  txt <- methods_error_message("x", "plot_communities")
  stop(txt)
}


#' @export
plot_communities.communities <- function(x, graph, ...) {
  graph_i <- coerce_graph_to_igraph_for_communities(graph)
  plot_fun <- utils::getFromNamespace("plot.communities", "igraph")
  plot_fun(x, graph_i, ...)
}


#' Add community membership to a graph
#'
#' Add the community id of each vertex back to a graph-like object.
#'
#' For \code{igraph} and \code{network} objects the membership is stored as a
#' regular vertex attribute. For \code{data.frame} edgelists it is stored in the
#' hidden vertex metadata that \pkg{snafun} uses for faithful roundtrips, so the
#' attribute will reappear after converting the edgelist back to a richer graph
#' class. Matrices are not supported because they have no place to store vertex
#' attributes.
#'
#' @param x graph-like object that should receive the community information
#' @param coms object of class \code{communities}
#' @param attr name of the attribute to add back to the graph
#'
#' @return graph object with the community information added
#' @rdname add_comm
#' @export
#'
#' @examples
#' data("judge_net", package = "snafun")
#' coms <- snafun::extract_comm_fastgreedy(judge_net)
#'
#' g1 <- snafun::add_comm_membership(judge_net, coms)
#' snafun::extract_vertex_attribute(g1, "community")
#'
#' el <- snafun::to_edgelist(judge_net)
#' el <- snafun::add_comm_membership(el, coms)
#' snafun::extract_vertex_attribute(snafun::to_igraph(el), "community")
add_comm_membership <- function(x, coms, attr = "community") {
  UseMethod("add_comm_membership")
}


#' @export
add_comm_membership.default <- function(x, coms, attr = "community") {
  txt <- methods_error_message("x", "add_comm_membership")
  stop(txt)
}


#' @export
add_comm_membership.igraph <- function(x, coms, attr = "community") {
  ensure_communities_object(coms = coms, fun = "add_comm_membership")
  membership <- align_community_membership_to_graph(x = coms, graph = x)
  snafun::add_vertex_attributes(x, attr_name = attr, value = unname(membership))
}


#' @export
add_comm_membership.network <- function(x, coms, attr = "community") {
  ensure_communities_object(coms = coms, fun = "add_comm_membership")
  membership <- align_community_membership_to_graph(
    x = coms,
    graph = snafun::to_igraph(x)
  )
  snafun::add_vertex_attributes(x, attr_name = attr, value = unname(membership))
}


#' @export
add_comm_membership.matrix <- function(x, coms, attr = "community") {
  stop(
    "Matrices cannot store vertex attributes. Convert the matrix to an 'igraph', ",
    "'network', or edgelist first."
  )
}


#' @export
add_comm_membership.data.frame <- function(x, coms, attr = "community") {
  ensure_communities_object(coms = coms, fun = "add_comm_membership")

  vertex_table <- extract_stored_edgelist_vertices(x)
  if (is.null(vertex_table)) {
    membership <- extract_comm_membership(coms)
    if (!is.null(names(membership))) {
      vertex_table <- data.frame(name = names(membership), stringsAsFactors = FALSE)
    } else if (nrow(x) > 0L && ncol(x) >= 2L &&
               length(unique(c(x[[1]], x[[2]]))) == length(membership)) {
      vertex_table <- data.frame(name = unique(c(x[[1]], x[[2]])), stringsAsFactors = FALSE)
    } else {
      stop(
        "Cannot align an unnamed community membership vector to this edgelist. ",
        "Please use an edgelist created by 'snafun::to_edgelist()' or start from ",
        "a graph with vertex names."
      )
    }
  }

  membership <- align_community_membership_to_keys(
    x = coms,
    keys = vertex_table[[1]]
  )
  vertex_table[[attr]] <- unname(membership)

  store_edgelist_vertex_table(
    edgelist = x,
    vertex_table = vertex_table,
    keep_bipartite = !is.null(base::attr(x, "snafun_bipartite", exact = TRUE)),
    bipartite = base::attr(x, "snafun_bipartite", exact = TRUE),
    keep_directed = !is.null(base::attr(x, "snafun_directed", exact = TRUE)),
    directed = base::attr(x, "snafun_directed", exact = TRUE)
  )
}


#' Add a community crossing indicator to a graph
#'
#' Add an edge-level indicator showing whether each edge connects two different
#' communities.
#'
#' For \code{igraph} and \code{network} objects the indicator is stored as a
#' regular edge attribute. For \code{data.frame} edgelists it is added as a
#' visible column. Matrices are not supported because they cannot store
#' arbitrary edge attributes beyond the numeric cell value itself.
#'
#' @param x graph-like object that should receive the crossing indicator
#' @param coms object of class \code{communities}
#' @param attr name of the attribute to add back to the graph
#'
#' @return graph object with the crossing information added
#' @rdname add_comm
#' @export
#'
#' @examples
#' data("judge_net", package = "snafun")
#' coms <- snafun::extract_comm_fastgreedy(judge_net)
#'
#' g1 <- snafun::add_comm_crossing(judge_net, coms)
#' snafun::extract_edge_attribute(g1, "crossing")
#'
#' el <- snafun::to_edgelist(judge_net)
#' snafun::add_comm_crossing(el, coms)
add_comm_crossing <- function(x, coms, attr = "crossing") {
  UseMethod("add_comm_crossing")
}


#' @export
add_comm_crossing.default <- function(x, coms, attr = "crossing") {
  txt <- methods_error_message("x", "add_comm_crossing")
  stop(txt)
}


#' @export
add_comm_crossing.igraph <- function(x, coms, attr = "crossing") {
  ensure_communities_object(coms = coms, fun = "add_comm_crossing")
  values <- community_crossing_values(graph = x, x = coms)
  igraph::set_edge_attr(graph = x, name = attr, value = values)
}


#' @export
add_comm_crossing.network <- function(x, coms, attr = "crossing") {
  ensure_communities_object(coms = coms, fun = "add_comm_crossing")
  values <- community_crossing_values(graph = x, x = coms)
  network::set.edge.attribute(x, attrname = attr, value = values)
  x
}


#' @export
add_comm_crossing.matrix <- function(x, coms, attr = "crossing") {
  stop(
    "Matrices cannot store arbitrary edge attributes. Convert the matrix to an ",
    "'igraph', 'network', or edgelist first."
  )
}


#' @export
add_comm_crossing.data.frame <- function(x, coms, attr = "crossing") {
  ensure_communities_object(coms = coms, fun = "add_comm_crossing")
  x[[attr]] <- community_crossing_values(graph = x, x = coms)
  x
}


#' Summarize a community partition as a table
#'
#' Create a compact community-level summary table.
#'
#' The table always contains the community id, its size, and its share of the
#' total number of vertices. If the original graph is supplied, the table is
#' expanded with three simple structure measures that are especially useful in
#' teaching: the number of internal edges, the number of possible internal
#' edges, the resulting internal density, and the number of crossing edges
#' incident to the community.
#'
#' @param x object of class \code{communities}
#' @param graph optional graph corresponding to the community object
#'
#' @return data.frame with one row per community
#' @rdname summarize_communities
#' @export
#'
#' @examples
#' data("judge_net", package = "snafun")
#' coms <- snafun::extract_comm_fastgreedy(judge_net)
#' snafun::summarize_communities(coms)
#' snafun::summarize_communities(coms, graph = judge_net)
summarize_communities <- function(x, graph = NULL) {
  UseMethod("summarize_communities")
}


#' @export
summarize_communities.default <- function(x, graph = NULL) {
  txt <- methods_error_message("x", "summarize_communities")
  stop(txt)
}


#' @export
summarize_communities.communities <- function(x, graph = NULL) {
  sizes <- extract_comm_sizes(x)
  out <- data.frame(
    community = as.integer(names(sizes)),
    size = as.integer(unname(sizes)),
    proportion = as.numeric(unname(sizes)) / sum(unname(sizes)),
    stringsAsFactors = FALSE
  )

  if (is.null(graph)) {
    return(out)
  }

  graph_i <- coerce_graph_to_igraph_for_communities(graph)
  graph_stats <- summarize_community_structure(graph = graph_i, x = x)
  merge(out, graph_stats, by = "community", sort = FALSE)
}


#' Evaluate a community partition
#'
#' Compute a small set of cluster-quality measures for a community partition.
#'
#' The returned table follows the structure often used in applied community
#' analysis: one row per detected community, with its size, average silhouette
#' width, the overall modularity score of the partition, conductance, and
#' within-community density.
#'
#' Silhouette widths are based on the geodesic distance matrix of the supplied
#' graph. When the graph is disconnected and some shortest-path distances are
#' infinite, those infinities are replaced by a large finite value equal to one
#' plus the largest finite distance observed in the network before the
#' silhouette widths are calculated. This keeps the function usable on ordinary
#' social networks that contain multiple components or isolates.
#'
#' Conductance is computed as the number of cut edges incident to a community
#' divided by the smaller of the total degrees inside and outside that
#' community. Density is computed on the induced subgraph of the community after
#' removing loops and collapsing any multiple edges, so it stays on the usual
#' 0-to-1 scale.
#'
#' Interpretation:
#'
#' In the per-vertex \code{sil_width} table, values close to \code{1} indicate
#' that a vertex is much closer to the vertices in its own community than to
#' vertices in other communities. Values around \code{0} indicate a boundary
#' case: the vertex fits its own community only about as well as a neighbouring
#' community. Negative values indicate that the vertex is on average closer to
#' another community than to the one to which it was assigned. In practice this
#' makes silhouette width a convenient diagnostic for individual vertices:
#' strongly positive values suggest a well-placed vertex, values near zero
#' suggest an ambiguous or bridge-like position, and negative values suggest
#' that the vertex may be misclassified by the community detection method.
#'
#' In the per-community \code{results} table:
#' \itemize{
#'   \item \code{Silhouette}: the average silhouette width of all vertices in
#'   that community. Higher values indicate a cleaner and better-separated
#'   community. As a rough rule of thumb, values clearly above zero indicate
#'   that most vertices sit closer to their own community than to competing
#'   communities.
#'   \item \code{Modularity}: the overall modularity of the full partition, so
#'   the same value is repeated for each community row. Higher values indicate
#'   that, overall, there are relatively many within-community ties and
#'   relatively few between-community ties.
#'   \item \code{Conductance}: the fraction of ties leaving the community
#'   relative to the smaller of the degree volume inside and outside the
#'   community. Lower values are usually better: they indicate a community that
#'   is more self-contained and less outward-facing.
#'   \item \code{Density}: the density of the induced subgraph of the community.
#'   Higher values indicate that the members of the community are more tightly
#'   connected to one another.
#' }
#'
#' @param x object of class \code{communities}
#' @param graph graph corresponding to the community object. This can be an
#'   \code{igraph}, \code{network}, \code{matrix}, or \code{data.frame}
#'   edgelist.
#' @param sil_width logical; if \code{TRUE}, return both the per-vertex
#'   silhouette-width table and the per-community results table. If
#'   \code{FALSE}, return only the per-community results table.
#' @param digits number of digits used to round the returned numeric summaries.
#'
#' @return If \code{sil_width = FALSE}, a \code{data.frame} with one row per
#'   community. If \code{sil_width = TRUE}, a list with elements
#'   \code{sil_width} and \code{results}. The \code{sil_width} element is a
#'   \code{data.frame} with one row per vertex. Its \code{vertex} column
#'   contains the vertex names when the graph has them; otherwise it contains
#'   the vertex positions \code{1, 2, 3, ...} in the order used by the supplied
#'   graph object.
#' @export
#'
#' @examples
#' data("judge_net", package = "snafun")
#' coms <- snafun::extract_comm_fastgreedy(judge_net)
#'
#' ev <- snafun::evaluate_communities(coms, judge_net)
#' ev
#' plot(ev)
#'
#' ev2 <- snafun::evaluate_communities(coms, judge_net, sil_width = TRUE)
#' head(ev2$sil_width)
#' ev2$results
evaluate_communities <- function(x, graph, sil_width = FALSE, digits = 3) {
  UseMethod("evaluate_communities")
}


#' @export
evaluate_communities.default <- function(x, graph, sil_width = FALSE, digits = 3) {
  txt <- methods_error_message("x", "evaluate_communities")
  stop(txt)
}


#' @export
evaluate_communities.communities <- function(x, graph, sil_width = FALSE, digits = 3) {
  if (missing(graph)) {
    stop("'graph' should be supplied to 'evaluate_communities()'")
  }
  if (!is.logical(sil_width) || length(sil_width) != 1L || is.na(sil_width)) {
    stop("'sil_width' should be either TRUE or FALSE")
  }
  if (length(digits) != 1L || is.na(digits) || digits < 0) {
    stop("'digits' should be a single non-negative number")
  }

  graph_i <- coerce_graph_to_igraph_for_communities(graph)
  membership <- align_community_membership_to_graph(x = x, graph = graph_i)
  cluster_ids <- sort(unique(as.integer(unname(membership))))
  cluster_sizes <- as.numeric(table(factor(
    as.integer(unname(membership)),
    levels = cluster_ids
  )))
  modularity_score <- extract_comm_modularity(x, graph = graph_i)

  sil_table <- community_silhouette_table(
    graph = graph_i,
    membership = membership
  )
  mean_sil_per_cluster <- tapply(
    sil_table$sil_width,
    INDEX = sil_table$cluster,
    FUN = function(z) {
      if (all(is.na(z))) {
        NA_real_
      } else {
        mean(z, na.rm = TRUE)
      }
    }
  )
  mean_sil_per_cluster <- mean_sil_per_cluster[as.character(cluster_ids)]

  conductance_values <- stats::setNames(
    sapply(
      cluster_ids,
      FUN = function(k) {
        community_conductance(
          graph = graph_i,
          membership = membership,
          target_cluster = k
        )
      }
    ),
    nm = as.character(cluster_ids)
  )

  density_values <- stats::setNames(
    sapply(
      cluster_ids,
      FUN = function(k) {
        community_density(
          graph = graph_i,
          membership = membership,
          target_cluster = k
        )
      }
    ),
    nm = as.character(cluster_ids)
  )

  results <- data.frame(
    Cluster = paste0("C", cluster_ids),
    Size = as.numeric(cluster_sizes),
    Silhouette = round(as.numeric(mean_sil_per_cluster), digits = digits),
    Modularity = round(rep(modularity_score, length(cluster_ids)), digits = digits),
    Conductance = round(as.numeric(conductance_values), digits = digits),
    Density = round(as.numeric(density_values), digits = digits),
    stringsAsFactors = FALSE
  )

  base::attr(results, "sil_width") <- sil_table
  class(results) <- c("evaluate_communities", class(results))

  if (!isTRUE(sil_width)) {
    return(results)
  }

  sil_table$sil_width <- round(sil_table$sil_width, digits = digits)
  out <- list(
    sil_width = sil_table,
    results = results
  )
  class(out) <- "evaluate_communities"
  out
}


#' Plot silhouette widths from a community evaluation
#'
#' Draw a silhouette plot in base R for an object returned by
#' \code{\link{evaluate_communities}}.
#'
#' The plot is built from the per-vertex silhouette widths contained in the
#' evaluation object. Vertices are grouped by community and sorted within each
#' community by their silhouette width. The subtitle shows the overall average
#' silhouette width, and the legend can optionally display for each community
#' both its size and its average silhouette width.
#'
#' The light grey horizontal line marks a silhouette width of \code{0}. Bars
#' above that line represent vertices that fit their own community better than
#' neighbouring communities; bars below it represent problematic assignments.
#' The red horizontal line marks the overall average silhouette width of the
#' partition. The legend follows the style \code{Ck (Size): Width}, so each
#' colour can be matched directly to a community id, its size, and its average
#' silhouette width.
#'
#' @param x object returned by \code{\link{evaluate_communities}}
#' @param col optional vector of colours, one for each community
#' @param label logical; if \code{TRUE}, print the vertex identifiers on the
#'   x-axis
#' @param identify logical; if \code{TRUE}, allow the user to click on bars
#'   after plotting to identify the corresponding vertices. Clicked vertices are
#'   labelled in the plot and also printed to the console.
#' @param summary.legend logical; if \code{TRUE}, show the legend as
#'   \code{Cluster (Size): Width}
#' @param grayscale logical; if \code{TRUE}, use a grayscale palette instead of
#'   colours
#' @param linetype line type used for the horizontal average silhouette line
#' @param border border colour for the bars
#' @param main plot title
#' @param xlab x-axis label
#' @param ... additional arguments passed on to low-level graphics functions
#'
#' @return invisibly returns \code{x}
#' @export
#'
#' @examples
#' data("judge_net", package = "snafun")
#' coms <- snafun::extract_comm_fastgreedy(judge_net)
#' ev <- snafun::evaluate_communities(coms, judge_net)
#' plot(ev)
plot.evaluate_communities <- function(x,
                                      col = NULL,
                                      label = FALSE,
                                      identify = FALSE,
                                      summary.legend = TRUE,
                                      grayscale = FALSE,
                                      linetype = "dashed",
                                      border = "white",
                                      main = "Clusters Silhouette Plot",
                                      xlab = "",
                                      ...) {
  sil_table <- extract_silhouette_from_evaluation(x)
  sil_table <- sil_table[!is.na(sil_table$sil_width), , drop = FALSE]

  if (nrow(sil_table) == 0L) {
    stop(
      "No finite silhouette widths are available to plot. ",
      "This usually happens when the partition has fewer than two clusters ",
      "or when every vertex forms its own cluster."
    )
  }
  if (!is.logical(identify) || length(identify) != 1L || is.na(identify)) {
    stop("'identify' should be either TRUE or FALSE")
  }

  sil_table$cluster <- as.integer(sil_table$cluster)
  sil_table$vertex <- as.character(sil_table$vertex)
  sil_table <- sil_table[
    order(sil_table$cluster, -sil_table$sil_width, sil_table$vertex),
    ,
    drop = FALSE
  ]

  cluster_ids <- sort(unique(sil_table$cluster))
  n_clusters <- length(cluster_ids)
  results_table <- extract_results_from_evaluation(x)
  cluster_sizes <- results_table$Size[match(paste0("C", cluster_ids), results_table$Cluster)]
  cluster_avg <- results_table$Silhouette[match(paste0("C", cluster_ids), results_table$Cluster)]
  overall_avg <- mean(sil_table$sil_width, na.rm = TRUE)

  if (is.null(col)) {
    if (isTRUE(grayscale)) {
      col <- grDevices::gray.colors(n_clusters, start = 0.2, end = 0.8)
    } else {
      col <- grDevices::hcl.colors(n_clusters, palette = "Set 2")
    }
  }
  if (length(col) < n_clusters) {
    col <- rep(col, length.out = n_clusters)
  }
  names(col) <- as.character(cluster_ids)

  gap <- 0.8
  x_left <- numeric(nrow(sil_table))
  x_right <- numeric(nrow(sil_table))
  current_x <- 0

  for (i in seq_along(cluster_ids)) {
    current_cluster <- cluster_ids[[i]]
    cluster_rows <- which(sil_table$cluster == current_cluster)

    for (j in cluster_rows) {
      x_left[[j]] <- current_x
      current_x <- current_x + 1
      x_right[[j]] <- current_x
    }

    current_x <- current_x + gap
  }

  y_range <- range(c(0, sil_table$sil_width, 1), finite = TRUE)
  y_padding <- diff(y_range) * 0.05
  if (!is.finite(y_padding) || y_padding == 0) {
    y_padding <- 0.05
  }

  old_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old_par), add = TRUE)
  graphics::par(mar = c(5, 4, 4, 11), xpd = NA)

  graphics::plot.new()
  graphics::plot.window(
    xlim = c(0, current_x),
    ylim = c(y_range[[1]] - y_padding, y_range[[2]] + y_padding)
  )
  graphics::abline(h = 0, col = "grey80", lty = 1)
  graphics::abline(h = overall_avg, col = "red", lty = linetype)

  for (i in seq_len(nrow(sil_table))) {
    current_cluster <- as.character(sil_table$cluster[[i]])
    graphics::rect(
      xleft = x_left[[i]],
      ybottom = min(0, sil_table$sil_width[[i]]),
      xright = x_right[[i]],
      ytop = max(0, sil_table$sil_width[[i]]),
      col = col[[current_cluster]],
      border = border,
      ...
    )
  }

  if (isTRUE(label)) {
    graphics::axis(
      side = 1,
      at = (x_left + x_right) / 2,
      labels = sil_table$vertex,
      las = 2,
      cex.axis = 0.7
    )
  } else {
    graphics::axis(1, labels = FALSE, tick = FALSE)
  }
  graphics::axis(2, las = 1)
  graphics::box()
  graphics::title(
    main = main,
    sub = paste0("Average silhouette width: ", round(overall_avg, 4)),
    xlab = xlab,
    ylab = "Silhouette Width"
  )

  legend_labels <- if (isTRUE(summary.legend)) {
    paste0(
      "C", cluster_ids, " (", cluster_sizes, "): ",
      format(round(cluster_avg, 4), nsmall = 4)
    )
  } else {
    paste0("C", cluster_ids)
  }
  legend_title <- if (isTRUE(summary.legend)) {
    "Cluster (Size): Width"
  } else {
    "Cluster"
  }
  usr <- graphics::par("usr")
  legend_x <- usr[[2]] + 0.03 * diff(usr[1:2])
  legend_y <- usr[[4]]
  graphics::legend(
    x = legend_x,
    y = legend_y,
    legend = legend_labels,
    title = legend_title,
    fill = unname(col[as.character(cluster_ids)]),
    border = NA,
    bty = "n",
    cex = 0.9,
    xjust = 0,
    yjust = 1
  )

  if (isTRUE(identify)) {
    x_mid <- (x_left + x_right) / 2
    selected <- graphics::identify(
      x = x_mid,
      y = sil_table$sil_width,
      labels = sil_table$vertex,
      plot = TRUE
    )
    if (length(selected) > 0L) {
      cat(
        "Selected vertices:",
        paste(sil_table$vertex[selected], collapse = ", "),
        "\n"
      )
    }
  }

  invisible(x)
}


#' Summarize a community object
#'
#' Summarize the main information contained in a \code{communities} object.
#'
#' The summary contains the algorithm name, number of communities, community
#' sizes, the stored modularity value, and optionally a recomputed modularity
#' and community-level structure table when \code{graph} is provided.
#'
#' @param object object of class \code{communities}
#' @param graph optional graph corresponding to the community object
#' @param ... ignored
#'
#' @return object of class \code{summary.communities}
#' @export
#'
#' @examples
#' data("judge_net", package = "snafun")
#' coms <- snafun::extract_comm_fastgreedy(judge_net)
#' summary(coms)
#' summary(coms, graph = judge_net)
summary.communities <- function(object, graph = NULL, ...) {
  sizes <- extract_comm_sizes(object)
  out <- list(
    algorithm = extract_comm_algorithm(object),
    n_communities = count_communities(object),
    sizes = sizes,
    membership = extract_comm_membership(object),
    merges = extract_comm_merges(object),
    modularity = extract_comm_modularity(object),
    modularity_recomputed = if (is.null(graph)) NULL else extract_comm_modularity(object, graph = graph),
    community_table = summarize_communities(object, graph = graph),
    call = match.call()
  )
  class(out) <- "summary.communities"
  out
}


#' Print a community object
#'
#' Print the main characteristics of a \code{communities} object in a compact
#' student-friendly format.
#'
#' @param x object of class \code{communities}
#' @param digits number of significant digits to print
#' @param ... ignored
#'
#' @return invisibly returns \code{x}
#' @export
#'
#' @examples
#' data("judge_net", package = "snafun")
#' coms <- snafun::extract_comm_fastgreedy(judge_net)
#' print(coms)
print.communities <- function(x, digits = 4, ...) {
  sizes <- extract_comm_sizes(x)

  cat("\nCommunity structure object\n\n")
  cat("Algorithm:", extract_comm_algorithm(x), "\n")
  cat("Communities:", count_communities(x), "\n")
  cat("Vertices:", length(extract_comm_membership(x)), "\n")
  cat("Sizes:", paste(unname(sizes), collapse = ", "), "\n")
  cat(
    "Stored modularity:",
    format(signif(extract_comm_modularity(x), digits = digits)),
    "\n"
  )
  cat("Merge history:", if (is.null(extract_comm_merges(x))) "no" else "yes", "\n")
  invisible(x)
}


#' Print a community summary
#'
#' Print the object returned by \code{\link[base:summary]{summary()}} for a
#' \code{communities} object.
#'
#' @param x object of class \code{summary.communities}
#' @param digits number of significant digits to print
#' @param ... ignored
#'
#' @return invisibly returns \code{x}
#' @export
#'
#' @examples
#' data("judge_net", package = "snafun")
#' coms <- snafun::extract_comm_fastgreedy(judge_net)
#' print(summary(coms))
print.summary.communities <- function(x, digits = 4, ...) {
  cat("\nSummary of community structure\n\n")
  cat("Algorithm:", x$algorithm, "\n")
  cat("Communities:", x$n_communities, "\n")
  cat("Vertices:", length(x$membership), "\n")
  cat("Sizes:", paste(unname(x$sizes), collapse = ", "), "\n")
  cat("Stored modularity:", format(signif(x$modularity, digits = digits)), "\n")
  if (!is.null(x$modularity_recomputed)) {
    cat(
      "Recomputed modularity:",
      format(signif(x$modularity_recomputed, digits = digits)),
      "\n"
    )
  }
  cat("Merge history:", if (is.null(x$merges)) "no" else "yes", "\n\n")
  print(x$community_table, row.names = FALSE)
  invisible(x)
}


#' Convert supported graph inputs to igraph for community helpers
#'
#' @param graph graph object
#'
#' @return \code{igraph} object
#' @keywords internal
#' @noRd
coerce_graph_to_igraph_for_communities <- function(graph) {
  if (inherits(graph, "igraph")) {
    return(graph)
  }
  if (inherits(graph, "network")) {
    return(snafun::to_igraph(graph))
  }
  if (is.matrix(graph)) {
    return(snafun::to_igraph(graph, bipartite = nrow(graph) != ncol(graph)))
  }
  if (inherits(graph, "data.frame")) {
    return(snafun::to_igraph(graph))
  }
  txt <- methods_error_message("graph", "plot_communities")
  stop(txt)
}


#' Check community-object input
#'
#' @keywords internal
#' @noRd
ensure_communities_object <- function(coms, fun) {
  if (!inherits(coms, "communities")) {
    stop("'coms' should be a 'communities' object for '", fun, "()'.")
  }
}


#' Extract graph vertex keys for community alignment
#'
#' @keywords internal
#' @noRd
community_graph_vertex_keys <- function(graph) {
  if (is.matrix(graph)) {
    graph <- coerce_graph_to_igraph_for_communities(graph)
    return(community_graph_vertex_keys(graph))
  }

  if (inherits(graph, "igraph")) {
    if (snafun::has_vertexnames(graph)) {
      return(snafun::extract_vertex_names(graph))
    }
    return(seq_len(snafun::count_vertices(graph)))
  }

  if (inherits(graph, "network")) {
    if (snafun::has_vertexnames(graph)) {
      return(snafun::extract_vertex_names(graph))
    }
    return(seq_len(snafun::count_vertices(graph)))
  }

  if (inherits(graph, "data.frame")) {
    stored_vertices <- extract_stored_edgelist_vertices(graph)
    if (!is.null(stored_vertices)) {
      return(stored_vertices[[1]])
    }
    if (nrow(graph) == 0L || ncol(graph) < 2L) {
      return(character(0))
    }
    return(unique(c(graph[[1]], graph[[2]])))
  }

  stop("Unsupported graph type in 'community_graph_vertex_keys()'.")
}


#' Align a community membership vector to a set of vertex keys
#'
#' @keywords internal
#' @noRd
align_community_membership_to_keys <- function(x, keys) {
  membership <- extract_comm_membership(x)
  key_labels <- as.character(keys)

  if (is.null(names(membership))) {
    if (length(membership) != length(keys)) {
      stop(
        "The community object and the supplied graph do not refer to the same ",
        "number of vertices."
      )
    }
    names(membership) <- key_labels
    return(membership)
  }

  index <- match(key_labels, as.character(names(membership)))
  if (anyNA(index)) {
    stop("The community object and the supplied graph do not refer to the same vertices.")
  }

  out <- membership[index]
  names(out) <- key_labels
  out
}


#' Align community membership to a graph object
#'
#' @keywords internal
#' @noRd
align_community_membership_to_graph <- function(x, graph) {
  align_community_membership_to_keys(
    x = x,
    keys = community_graph_vertex_keys(graph)
  )
}


#' Extract edge endpoints in their current storage order
#'
#' @keywords internal
#' @noRd
community_edge_frame <- function(graph) {
  if (is.matrix(graph)) {
    graph <- coerce_graph_to_igraph_for_communities(graph)
    return(community_edge_frame(graph))
  }

  if (inherits(graph, "igraph")) {
    if (igraph::ecount(graph) == 0L) {
      return(data.frame(from = numeric(0), to = numeric(0)))
    }
    edge_frame <- igraph::as_data_frame(graph, what = "edges")
    return(edge_frame[, 1:2, drop = FALSE])
  }

  if (inherits(graph, "network")) {
    if (network::network.edgecount(graph) == 0L) {
      return(data.frame(from = numeric(0), to = numeric(0)))
    }
    edge_frame <- network::as.data.frame.network(graph, unit = "edges", na.rm = FALSE)
    colnames(edge_frame)[1:2] <- c("from", "to")
    return(edge_frame[, 1:2, drop = FALSE])
  }

  if (inherits(graph, "data.frame")) {
    if (nrow(graph) == 0L || ncol(graph) < 2L) {
      return(data.frame(from = numeric(0), to = numeric(0)))
    }
    edge_frame <- graph[, 1:2, drop = FALSE]
    colnames(edge_frame) <- c("from", "to")
    return(edge_frame)
  }

  stop("Unsupported graph type in 'community_edge_frame()'.")
}


#' Compute edge crossing indicators from endpoints and membership
#'
#' @keywords internal
#' @noRd
community_crossing_values <- function(graph, x) {
  edge_frame <- community_edge_frame(graph)
  if (nrow(edge_frame) == 0L) {
    return(logical(0))
  }

  membership <- align_community_membership_to_keys(
    x = x,
    keys = community_graph_vertex_keys(graph)
  )
  lookup_names <- names(membership)
  from_index <- match(as.character(edge_frame$from), lookup_names)
  to_index <- match(as.character(edge_frame$to), lookup_names)

  if (anyNA(from_index) || anyNA(to_index)) {
    stop("The community object and the supplied graph do not refer to the same vertices.")
  }

  unname(membership[from_index] != membership[to_index])
}


#' Summarize community-level graph structure
#'
#' @keywords internal
#' @noRd
summarize_community_structure <- function(graph, x) {
  membership <- align_community_membership_to_graph(x = x, graph = graph)
  keys <- community_graph_vertex_keys(graph)
  edge_frame <- community_edge_frame(graph)
  communities <- sort(unique(as.integer(unname(membership))))
  directed <- snafun::is_directed(graph)

  out <- vector("list", length(communities))
  for (index in seq_along(communities)) {
    community_id <- communities[[index]]
    inside <- unname(membership == community_id)
    size <- sum(inside)
    possible_internal_edges <- if (size < 2L) {
      0
    } else if (directed) {
      size * (size - 1L)
    } else {
      size * (size - 1L) / 2
    }

    if (nrow(edge_frame) == 0L) {
      internal_edges <- 0L
      crossing_edges_incident <- 0L
    } else {
      from_membership <- membership[match(as.character(edge_frame$from), as.character(keys))]
      to_membership <- membership[match(as.character(edge_frame$to), as.character(keys))]
      non_loop <- as.character(edge_frame$from) != as.character(edge_frame$to)

      internal_edges <- sum(
        non_loop & from_membership == community_id & to_membership == community_id
      )
      crossing_edges_incident <- sum(
        xor(from_membership == community_id, to_membership == community_id)
      )
    }

    out[[index]] <- data.frame(
      community = community_id,
      internal_edges = as.integer(internal_edges),
      possible_internal_edges = as.integer(possible_internal_edges),
      internal_density = if (possible_internal_edges == 0) NA_real_ else internal_edges / possible_internal_edges,
      crossing_edges_incident = as.integer(crossing_edges_incident),
      stringsAsFactors = FALSE
    )
  }

  do.call("rbind", out)
}


#' Extract the stored silhouette table from an evaluation object
#'
#' @keywords internal
#' @noRd
extract_silhouette_from_evaluation <- function(x) {
  if (is.list(x) && !is.data.frame(x) && "sil_width" %in% names(x)) {
    return(x$sil_width)
  }

  sil_table <- base::attr(x, "sil_width", exact = TRUE)
  if (is.null(sil_table)) {
    stop(
      "No stored silhouette information was found in this object. ",
      "Please create it with 'snafun::evaluate_communities()'."
    )
  }

  sil_table
}


#' Extract the stored results table from an evaluation object
#'
#' @keywords internal
#' @noRd
extract_results_from_evaluation <- function(x) {
  if (is.list(x) && !is.data.frame(x) && "results" %in% names(x)) {
    return(x$results)
  }

  as.data.frame(x)
}


#' Build a per-vertex silhouette table for a community partition
#'
#' @keywords internal
#' @noRd
community_silhouette_table <- function(graph, membership) {
  cluster_ids <- as.integer(unname(membership))
  vertex_names <- as.character(community_graph_vertex_keys(graph))
  n_clusters <- length(unique(cluster_ids))
  n_vertices <- length(cluster_ids)

  if (n_clusters < 2L || n_clusters >= n_vertices) {
    return(data.frame(
      vertex = vertex_names,
      cluster = cluster_ids,
      neighbor = NA_integer_,
      sil_width = NA_real_,
      stringsAsFactors = FALSE
    ))
  }

  dist_matrix <- community_distance_matrix(graph = graph)
  names(cluster_ids) <- vertex_names
  sil_obj <- cluster::silhouette(cluster_ids, dist_matrix)
  sil_matrix <- unclass(sil_obj)
  sil_vertex_names <- rownames(sil_matrix)
  if (is.null(sil_vertex_names) || length(sil_vertex_names) != nrow(sil_matrix)) {
    sil_vertex_names <- vertex_names
  }
  sil_df <- data.frame(
    vertex = sil_vertex_names,
    cluster = sil_matrix[, 1],
    neighbor = sil_matrix[, 2],
    sil_width = sil_matrix[, 3],
    stringsAsFactors = FALSE
  )
  rownames(sil_df) <- NULL
  sil_df
}


#' Compute a finite geodesic distance matrix for silhouette widths
#'
#' @keywords internal
#' @noRd
community_distance_matrix <- function(graph) {
  dist_matrix <- igraph::distances(graph)
  finite_values <- dist_matrix[is.finite(dist_matrix)]

  if (length(finite_values) == 0L) {
    dist_matrix[!is.finite(dist_matrix)] <- 1
  } else if (any(!is.finite(dist_matrix))) {
    replacement <- max(finite_values) + 1
    dist_matrix[!is.finite(dist_matrix)] <- replacement
  }

  dist_matrix
}


#' Compute conductance for one community
#'
#' @keywords internal
#' @noRd
community_conductance <- function(graph, membership, target_cluster) {
  cluster_ids <- as.integer(unname(membership))
  in_cluster <- which(cluster_ids == as.integer(target_cluster))
  out_cluster <- which(cluster_ids != as.integer(target_cluster))

  if (length(in_cluster) == 0L || length(out_cluster) == 0L) {
    return(NA_real_)
  }

  edge_frame <- community_edge_frame(graph)
  if (nrow(edge_frame) == 0L) {
    return(0)
  }

  keys <- as.character(community_graph_vertex_keys(graph))
  from_membership <- membership[match(as.character(edge_frame$from), keys)]
  to_membership <- membership[match(as.character(edge_frame$to), keys)]
  cut_size <- sum(xor(
    from_membership == as.integer(target_cluster),
    to_membership == as.integer(target_cluster)
  ))

  degree_values <- igraph::degree(graph, mode = "all")
  vol_in <- sum(degree_values[in_cluster])
  vol_out <- sum(degree_values[out_cluster])

  if (min(vol_in, vol_out) == 0) {
    return(NA_real_)
  }

  cut_size / min(vol_in, vol_out)
}


#' Compute internal density for one community
#'
#' @keywords internal
#' @noRd
community_density <- function(graph, membership, target_cluster) {
  vertices_in_cluster <- which(unname(membership) == as.integer(target_cluster))

  if (length(vertices_in_cluster) < 2L) {
    return(0)
  }

  subgraph <- igraph::induced_subgraph(graph, vids = vertices_in_cluster)
  subgraph <- igraph::simplify(
    graph = subgraph,
    remove.multiple = TRUE,
    remove.loops = TRUE
  )

  density <- snafun::g_density(subgraph, loops = FALSE)
  if (!is.finite(density) || is.na(density)) {
    return(0)
  }
  density
}

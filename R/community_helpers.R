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

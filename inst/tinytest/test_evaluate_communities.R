report_side_effects()


canonical_eval_table <- function(x) {
  x[order(x$Cluster), , drop = FALSE]
}


canonical_sil_table <- function(x) {
  x[order(x$vertex), , drop = FALSE]
}


toy_graph <- igraph::graph_from_edgelist(
  matrix(
    c(
      "A", "B",
      "B", "C",
      "C", "A",
      "D", "E",
      "E", "F",
      "F", "D",
      "C", "D"
    ),
    byrow = TRUE,
    ncol = 2
  ),
  directed = FALSE
)
toy_coms <- igraph::make_clusters(toy_graph, membership = c(1, 1, 1, 2, 2, 2))


# format parity ---------------------------------------------------------------
toy_eval_igraph <- snafun::evaluate_communities(toy_coms, toy_graph, sil_width = TRUE)
toy_eval_network <- snafun::evaluate_communities(
  toy_coms,
  snafun::to_network(toy_graph),
  sil_width = TRUE
)
toy_eval_matrix <- snafun::evaluate_communities(
  toy_coms,
  snafun::to_matrix(toy_graph),
  sil_width = TRUE
)
toy_eval_edgelist <- snafun::evaluate_communities(
  toy_coms,
  snafun::to_edgelist(toy_graph),
  sil_width = TRUE
)

expect_equal(
  canonical_eval_table(toy_eval_network$results),
  canonical_eval_table(toy_eval_igraph$results)
)
expect_equal(
  canonical_eval_table(toy_eval_matrix$results),
  canonical_eval_table(toy_eval_igraph$results)
)
expect_equal(
  canonical_eval_table(toy_eval_edgelist$results),
  canonical_eval_table(toy_eval_igraph$results)
)

expect_equal(
  canonical_sil_table(toy_eval_network$sil_width),
  canonical_sil_table(toy_eval_igraph$sil_width)
)
expect_equal(
  canonical_sil_table(toy_eval_matrix$sil_width),
  canonical_sil_table(toy_eval_igraph$sil_width)
)
expect_equal(
  canonical_sil_table(toy_eval_edgelist$sil_width),
  canonical_sil_table(toy_eval_igraph$sil_width)
)

toy_eval_default <- snafun::evaluate_communities(toy_coms, toy_graph)
expect_true(inherits(toy_eval_default, "evaluate_communities"))
expect_true(is.data.frame(toy_eval_default))
expect_true(!is.null(base::attr(toy_eval_default, "sil_width", exact = TRUE)))
expect_equal(
  unclass(canonical_eval_table(as.data.frame(toy_eval_default))),
  unclass(canonical_eval_table(toy_eval_igraph$results))
)


# disconnected graphs and distance replacement --------------------------------
disconnected_graph <- igraph::make_empty_graph(n = 4, directed = FALSE)
disconnected_graph <- igraph::set_vertex_attr(
  disconnected_graph,
  "name",
  value = c("A", "B", "C", "D")
)
disconnected_graph <- igraph::add_edges(disconnected_graph, c("A", "B"))
disconnected_coms <- igraph::make_clusters(disconnected_graph, membership = c(1, 1, 2, 2))

distance_matrix <- snafun:::community_distance_matrix(disconnected_graph)
expect_equal(distance_matrix["A", "C"], 2)
expect_equal(distance_matrix["C", "D"], 2)
expect_true(all(is.finite(distance_matrix)))

disconnected_eval <- snafun::evaluate_communities(
  disconnected_coms,
  disconnected_graph,
  sil_width = TRUE
)
expect_equal(nrow(disconnected_eval$results), 2)
expect_true(all(is.finite(disconnected_eval$results$Silhouette)))
expect_equal(
  disconnected_eval$sil_width$vertex,
  c("A", "B", "C", "D")
)


# weighted graph parity -------------------------------------------------------
weighted_graph <- toy_graph
weighted_graph <- igraph::set_edge_attr(
  weighted_graph,
  "weight",
  value = c(1, 2, 3, 1, 4, 2, 5)
)
weighted_coms <- toy_coms

weighted_eval_igraph <- snafun::evaluate_communities(
  weighted_coms,
  weighted_graph,
  sil_width = TRUE
)
weighted_eval_network <- snafun::evaluate_communities(
  weighted_coms,
  snafun::to_network(weighted_graph),
  sil_width = TRUE
)
weighted_eval_matrix <- snafun::evaluate_communities(
  weighted_coms,
  snafun::to_matrix(weighted_graph),
  sil_width = TRUE
)
weighted_eval_edgelist <- snafun::evaluate_communities(
  weighted_coms,
  snafun::to_edgelist(weighted_graph),
  sil_width = TRUE
)

expect_equal(
  canonical_eval_table(weighted_eval_network$results),
  canonical_eval_table(weighted_eval_igraph$results)
)
expect_equal(
  canonical_eval_table(weighted_eval_matrix$results),
  canonical_eval_table(weighted_eval_igraph$results)
)
expect_equal(
  canonical_eval_table(weighted_eval_edgelist$results),
  canonical_eval_table(weighted_eval_igraph$results)
)
expect_equal(
  canonical_sil_table(weighted_eval_network$sil_width),
  canonical_sil_table(weighted_eval_igraph$sil_width)
)


# directed graph parity -------------------------------------------------------
directed_graph <- igraph::graph_from_edgelist(
  matrix(
    c(
      "A", "B",
      "B", "C",
      "C", "A",
      "D", "E",
      "E", "F",
      "F", "D",
      "C", "D"
    ),
    byrow = TRUE,
    ncol = 2
  ),
  directed = TRUE
)
directed_coms <- igraph::make_clusters(directed_graph, membership = c(1, 1, 1, 2, 2, 2))

directed_eval_igraph <- snafun::evaluate_communities(
  directed_coms,
  directed_graph,
  sil_width = TRUE
)
directed_eval_network <- snafun::evaluate_communities(
  directed_coms,
  snafun::to_network(directed_graph),
  sil_width = TRUE
)
directed_eval_matrix <- snafun::evaluate_communities(
  directed_coms,
  snafun::to_matrix(directed_graph),
  sil_width = TRUE
)
directed_eval_edgelist <- snafun::evaluate_communities(
  directed_coms,
  snafun::to_edgelist(directed_graph),
  sil_width = TRUE
)

expect_equal(
  canonical_eval_table(directed_eval_network$results),
  canonical_eval_table(directed_eval_igraph$results)
)
expect_equal(
  canonical_eval_table(directed_eval_matrix$results),
  canonical_eval_table(directed_eval_igraph$results)
)
expect_equal(
  canonical_eval_table(directed_eval_edgelist$results),
  canonical_eval_table(directed_eval_igraph$results)
)


# edge-case partitions --------------------------------------------------------
single_cluster_coms <- igraph::make_clusters(toy_graph, membership = c(1, 1, 1, 1, 1, 1))
single_cluster_eval <- snafun::evaluate_communities(
  single_cluster_coms,
  toy_graph,
  sil_width = TRUE
)
expect_equal(nrow(single_cluster_eval$results), 1)
expect_true(is.na(single_cluster_eval$results$Silhouette))
expect_true(all(is.na(single_cluster_eval$sil_width$sil_width)))

singleton_coms <- igraph::make_clusters(toy_graph, membership = seq_len(igraph::vcount(toy_graph)))
singleton_eval <- snafun::evaluate_communities(
  singleton_coms,
  toy_graph,
  sil_width = TRUE
)
expect_equal(nrow(singleton_eval$results), igraph::vcount(toy_graph))
expect_true(all(is.na(singleton_eval$results$Silhouette)))
expect_true(all(is.na(singleton_eval$sil_width$sil_width)))


# digits and return shape -----------------------------------------------------
rounded_eval <- snafun::evaluate_communities(toy_coms, toy_graph, digits = 1)
expect_true(all(grepl("^[0-9.-]+$", format(rounded_eval$Silhouette, trim = TRUE))))
expect_equal(
  rounded_eval$Silhouette,
  round(toy_eval_igraph$results$Silhouette, digits = 1)
)

expect_error(
  snafun::evaluate_communities(toy_coms, toy_graph, sil_width = NA),
  "TRUE or FALSE"
)
expect_error(
  snafun::evaluate_communities(toy_coms, toy_graph, digits = -1),
  "non-negative"
)


# plotting --------------------------------------------------------------------
tmp_plot_default <- tempfile(fileext = ".pdf")
grDevices::pdf(tmp_plot_default)
plot(toy_eval_default)
grDevices::dev.off()
expect_true(file.exists(tmp_plot_default))

tmp_plot_list <- tempfile(fileext = ".pdf")
grDevices::pdf(tmp_plot_list)
plot(toy_eval_igraph)
grDevices::dev.off()
expect_true(file.exists(tmp_plot_list))

expect_error(
  plot(singleton_eval),
  "No finite silhouette widths"
)
expect_error(
  plot(toy_eval_default, identify = NA),
  "TRUE or FALSE"
)

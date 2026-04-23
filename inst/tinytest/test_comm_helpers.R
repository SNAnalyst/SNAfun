report_side_effects()


canonical_community_table <- function(x) {
  x[order(x$community), , drop = FALSE]
}


data("judge_net", package = "snafun")

coms <- snafun::extract_comm_fastgreedy(judge_net)


# basic accessors --------------------------------------------------------------
expect_equal(snafun::extract_comm_membership(coms), igraph::membership(coms))
expect_equal(snafun::count_communities(coms), as.integer(length(coms)))
expect_equal(snafun::extract_comm_sizes(coms), igraph::sizes(coms))
expect_equal(snafun::extract_comm_merges(coms), igraph::merges(coms))
expect_equal(snafun::extract_comm_algorithm(coms), igraph::algorithm(coms))
expect_equal(
  snafun::extract_comm_modularity(coms),
  as.numeric(igraph::modularity(coms))
)
expect_equal(
  snafun::extract_comm_modularity(coms, graph = judge_net),
  as.numeric(igraph::modularity(judge_net, membership = igraph::membership(coms)))
)


# crossing helper parity across graph classes ----------------------------------
expect_equal(
  unname(snafun::extract_comm_crossing(coms, judge_net)),
  unname(igraph::crossing(coms, judge_net))
)
expect_equal(
  unname(snafun::extract_comm_crossing(coms, snafun::to_network(judge_net))),
  unname(igraph::crossing(coms, judge_net))
)
expect_equal(
  unname(snafun::extract_comm_crossing(coms, snafun::to_matrix(judge_net))),
  unname(igraph::crossing(coms, judge_net))
)
judge_el <- snafun::to_edgelist(judge_net)
expect_equal(
  unname(snafun::extract_comm_crossing(coms, judge_el)),
  unname(igraph::crossing(coms, snafun::to_igraph(judge_el)))
)


# modified memberships ---------------------------------------------------------
merged_coms <- snafun::merge_membership(coms, merges = list(c(1, 2)))
expect_equal(
  snafun::extract_comm_membership(merged_coms),
  igraph::membership(merged_coms)
)
expect_equal(
  snafun::count_communities(merged_coms),
  as.integer(length(merged_coms))
)
expect_equal(
  snafun::extract_comm_sizes(merged_coms),
  igraph::sizes(merged_coms)
)
expect_equal(
  snafun::extract_comm_modularity(merged_coms),
  as.numeric(igraph::modularity(merged_coms))
)
expect_equal(
  snafun::extract_comm_modularity(merged_coms, graph = judge_net),
  as.numeric(
    igraph::modularity(
      judge_net,
      membership = igraph::membership(merged_coms)
    )
  )
)
expect_true(
  !isTRUE(all.equal(
    snafun::extract_comm_modularity(merged_coms),
    snafun::extract_comm_modularity(merged_coms, graph = judge_net)
  ))
)


# summary and print methods ----------------------------------------------------
community_summary <- summary(coms, graph = judge_net)
expect_inherits(community_summary, "summary.communities")
expect_equal(community_summary$algorithm, snafun::extract_comm_algorithm(coms))
expect_equal(community_summary$n_communities, snafun::count_communities(coms))
expect_equal(community_summary$modularity, snafun::extract_comm_modularity(coms))
expect_equal(
  community_summary$modularity_recomputed,
  snafun::extract_comm_modularity(coms, graph = judge_net)
)
expect_equal(nrow(community_summary$community_table), snafun::count_communities(coms))
expect_equal(sum(community_summary$community_table$size), length(snafun::extract_comm_membership(coms)))
expect_equal(sum(community_summary$community_table$proportion), 1)

print_output <- capture.output(snafun:::print.communities(coms))
expect_true(any(grepl("Community structure object", print_output, fixed = TRUE)))
expect_true(any(grepl("Algorithm:", print_output, fixed = TRUE)))

summary_output <- capture.output(snafun:::print.summary.communities(community_summary))
expect_true(any(grepl("Summary of community structure", summary_output, fixed = TRUE)))
expect_true(any(grepl("Stored modularity:", summary_output, fixed = TRUE)))


# exact community table on a hand-crafted graph --------------------------------
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

expect_equal(
  canonical_community_table(snafun::summarize_communities(toy_coms)),
  data.frame(
    community = c(1L, 2L),
    size = c(3L, 3L),
    proportion = c(0.5, 0.5)
  )
)

expect_equal(
  canonical_community_table(snafun::summarize_communities(toy_coms, graph = toy_graph)),
  data.frame(
    community = c(1L, 2L),
    size = c(3L, 3L),
    proportion = c(0.5, 0.5),
    internal_edges = c(3L, 3L),
    possible_internal_edges = c(3L, 3L),
    internal_density = c(1, 1),
    crossing_edges_incident = c(1L, 1L)
  )
)


# community evaluation ---------------------------------------------------------
toy_membership <- igraph::membership(toy_coms)
toy_dist <- igraph::distances(toy_graph)
toy_sil <- cluster::silhouette(toy_membership, toy_dist)
toy_mean_sil <- tapply(toy_sil[, "sil_width"], toy_membership, mean)
toy_modularity <- igraph::modularity(toy_graph, toy_membership)
toy_degree <- igraph::degree(toy_graph)
toy_edge_frame <- igraph::as_data_frame(toy_graph, what = "edges")[, 1:2, drop = FALSE]

toy_conductance <- sapply(sort(unique(toy_membership)), function(k) {
  in_cluster <- which(toy_membership == k)
  out_cluster <- which(toy_membership != k)
  cut_size <- sum(xor(
    toy_membership[match(toy_edge_frame[[1]], igraph::V(toy_graph)$name)] == k,
    toy_membership[match(toy_edge_frame[[2]], igraph::V(toy_graph)$name)] == k
  ))
  cut_size / min(sum(toy_degree[in_cluster]), sum(toy_degree[out_cluster]))
})

toy_density <- sapply(sort(unique(toy_membership)), function(k) {
  sub <- igraph::induced_subgraph(toy_graph, vids = which(toy_membership == k))
  snafun::g_density(sub, loops = FALSE)
})

expected_eval <- data.frame(
  Cluster = c("C1", "C2"),
  Size = c(3, 3),
  Silhouette = round(as.numeric(toy_mean_sil), 3),
  Modularity = round(rep(toy_modularity, 2), 3),
  Conductance = round(as.numeric(toy_conductance), 3),
  Density = round(as.numeric(toy_density), 3),
  stringsAsFactors = FALSE
)

expect_equal(
  snafun::evaluate_communities(toy_coms, toy_graph),
  expected_eval
)

eval_with_sil <- snafun::evaluate_communities(toy_coms, toy_graph, sil_width = TRUE)
expect_true(is.list(eval_with_sil))
expect_true(all(c("sil_width", "results") %in% names(eval_with_sil)))
expect_equal(eval_with_sil$results, expected_eval)
expect_equal(nrow(eval_with_sil$sil_width), igraph::vcount(toy_graph))
expect_true(all(c("vertex", "cluster", "neighbor", "sil_width") %in% colnames(eval_with_sil$sil_width)))

disconnected_graph <- igraph::make_empty_graph(n = 4, directed = FALSE)
disconnected_graph <- igraph::set_vertex_attr(
  disconnected_graph,
  "name",
  value = c("A", "B", "C", "D")
)
disconnected_graph <- igraph::add_edges(disconnected_graph, c("A", "B"))
disconnected_coms <- igraph::make_clusters(disconnected_graph, membership = c(1, 1, 2, 3))
disconnected_eval <- snafun::evaluate_communities(
  disconnected_coms,
  disconnected_graph,
  sil_width = TRUE
)
expect_equal(nrow(disconnected_eval$results), 3)
expect_equal(disconnected_eval$results$Density[disconnected_eval$results$Cluster %in% c("C2", "C3")], c(0, 0))
expect_equal(
  disconnected_eval$sil_width$vertex,
  c("A", "B", "C", "D")
)


# add community membership -----------------------------------------------------
g_comm <- snafun::add_comm_membership(judge_net, coms)
expect_equal(
  as.numeric(snafun::extract_vertex_attribute(g_comm, "community")),
  as.numeric(igraph::membership(coms))
)

nw_comm <- snafun::add_comm_membership(snafun::to_network(judge_net), coms)
expect_equal(
  as.numeric(snafun::extract_vertex_attribute(nw_comm, "community")),
  as.numeric(igraph::membership(coms))
)

graph_with_isolate <- igraph::make_empty_graph(n = 3, directed = FALSE)
graph_with_isolate <- igraph::set_vertex_attr(
  graph = graph_with_isolate,
  name = "name",
  value = c("A", "B", "C")
)
graph_with_isolate <- igraph::add_edges(graph_with_isolate, c("A", "B"))
coms_with_isolate <- snafun::extract_comm_fastgreedy(graph_with_isolate)

edgelist_with_isolate <- snafun::to_edgelist(graph_with_isolate)
edgelist_with_membership <- snafun::add_comm_membership(edgelist_with_isolate, coms_with_isolate)
expect_true("community" %in% colnames(base::attr(edgelist_with_membership, "snafun_vertices")))
expect_equal(
  as.numeric(snafun::extract_vertex_attribute(snafun::to_igraph(edgelist_with_membership), "community")),
  c(1, 1, 2)
)

bare_edgelist <- data.frame(from = "A", to = "B", stringsAsFactors = FALSE)
bare_edgelist <- snafun::add_comm_membership(bare_edgelist, coms_with_isolate)
expect_equal(
  sort(snafun::extract_vertex_names(snafun::to_igraph(bare_edgelist))),
  c("A", "B", "C")
)


# add crossing indicator -------------------------------------------------------
g_cross <- snafun::add_comm_crossing(judge_net, coms)
expect_equal(
  snafun::extract_edge_attribute(g_cross, "crossing"),
  snafun::extract_comm_crossing(coms, judge_net)
)

nw_cross <- snafun::add_comm_crossing(snafun::to_network(judge_net), coms)
expect_equal(
  snafun::extract_edge_attribute(nw_cross, "crossing"),
  snafun::extract_comm_crossing(coms, snafun::to_network(judge_net))
)

edgelist_cross <- snafun::add_comm_crossing(judge_el, coms)
expect_true("crossing" %in% colnames(edgelist_cross))
expect_equal(
  edgelist_cross$crossing,
  snafun::extract_comm_crossing(coms, judge_el)
)


# plotting --------------------------------------------------------------------
tmp_pdf <- tempfile(fileext = ".pdf")
grDevices::pdf(tmp_pdf)
snafun::plot_communities(coms, judge_net)
grDevices::dev.off()
expect_true(file.exists(tmp_pdf))


# error handling ---------------------------------------------------------------
expect_error(
  snafun::extract_comm_membership(judge_net),
  "communities"
)
expect_error(
  snafun::extract_comm_algorithm(judge_net),
  "communities"
)
expect_error(
  snafun::add_comm_membership(snafun::to_matrix(judge_net), coms),
  "Matrices cannot store vertex attributes"
)
expect_error(
  snafun::add_comm_crossing(snafun::to_matrix(judge_net), coms),
  "Matrices cannot store arbitrary edge attributes"
)

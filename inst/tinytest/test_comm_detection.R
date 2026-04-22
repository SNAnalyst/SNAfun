report_side_effects()


data("judge_net", package = "snafun")

judge_network <- snafun::to_network(judge_net)


# walktrap parity --------------------------------------------------------------
walktrap_igraph <- snafun::extract_comm_walktrap(judge_net)
walktrap_network <- snafun::extract_comm_walktrap(judge_network)

expect_inherits(walktrap_igraph, "communities")
expect_inherits(walktrap_network, "communities")
expect_equal(
  unname(snafun::extract_comm_membership(walktrap_igraph)),
  unname(snafun::extract_comm_membership(walktrap_network))
)
expect_equal(
  snafun::extract_comm_sizes(walktrap_igraph),
  snafun::extract_comm_sizes(walktrap_network)
)


# louvain parity and directed errors ------------------------------------------
set.seed(123)
louvain_igraph <- snafun::extract_comm_louvain(judge_net)
set.seed(123)
louvain_network <- snafun::extract_comm_louvain(judge_network)

expect_inherits(louvain_igraph, "communities")
expect_inherits(louvain_network, "communities")
expect_equal(
  unname(snafun::extract_comm_membership(louvain_igraph)),
  unname(snafun::extract_comm_membership(louvain_network))
)

directed_graph <- igraph::graph_from_edgelist(
  matrix(
    c(
      "A", "B",
      "B", "C",
      "C", "A",
      "C", "D"
    ),
    byrow = TRUE,
    ncol = 2
  ),
  directed = TRUE
)

expect_error(
  snafun::extract_comm_louvain(directed_graph),
  "undirected graphs only"
)
expect_error(
  snafun::extract_comm_louvain(snafun::to_network(directed_graph)),
  "undirected graphs only"
)


# fast greedy parity and directed errors --------------------------------------
fastgreedy_igraph <- snafun::extract_comm_fastgreedy(judge_net)
fastgreedy_network <- snafun::extract_comm_fastgreedy(judge_network)

expect_inherits(fastgreedy_igraph, "communities")
expect_inherits(fastgreedy_network, "communities")
expect_equal(
  unname(snafun::extract_comm_membership(fastgreedy_igraph)),
  unname(snafun::extract_comm_membership(fastgreedy_network))
)
expect_equal(
  snafun::extract_comm_sizes(fastgreedy_igraph),
  snafun::extract_comm_sizes(fastgreedy_network)
)

expect_error(
  snafun::extract_comm_fastgreedy(directed_graph),
  "undirected graphs only"
)
expect_error(
  snafun::extract_comm_fastgreedy(snafun::to_network(directed_graph)),
  "undirected graphs only"
)


# girvan directed passthrough --------------------------------------------------
girvan_graph <- igraph::graph_from_edgelist(
  matrix(
    c(
      1, 2,
      2, 3,
      3, 1,
      3, 4,
      4, 2,
      4, 5,
      5, 4
    ),
    byrow = TRUE,
    ncol = 2
  ),
  directed = TRUE
)
girvan_network <- snafun::to_network(girvan_graph)

backend_true <- igraph::cluster_edge_betweenness(
  girvan_graph,
  directed = TRUE,
  merges = TRUE,
  membership = TRUE,
  modularity = TRUE
)
backend_false <- igraph::cluster_edge_betweenness(
  girvan_graph,
  directed = FALSE,
  merges = TRUE,
  membership = TRUE,
  modularity = TRUE
)

wrapper_true <- snafun::extract_comm_girvan(girvan_graph, directed = TRUE)
wrapper_false <- snafun::extract_comm_girvan(girvan_graph, directed = FALSE)

expect_true(
  !identical(
    unname(snafun::extract_comm_membership(backend_true)),
    unname(snafun::extract_comm_membership(backend_false))
  )
)
expect_equal(
  unname(snafun::extract_comm_membership(wrapper_true)),
  unname(snafun::extract_comm_membership(backend_true))
)
expect_equal(
  unname(snafun::extract_comm_membership(wrapper_false)),
  unname(snafun::extract_comm_membership(backend_false))
)
expect_equal(
  snafun::extract_comm_merges(wrapper_true),
  igraph::merges(backend_true)
)
expect_equal(
  snafun::extract_comm_merges(wrapper_false),
  igraph::merges(backend_false)
)

wrapper_network_true <- snafun::extract_comm_girvan(girvan_network, directed = TRUE)
wrapper_network_false <- snafun::extract_comm_girvan(girvan_network, directed = FALSE)

expect_equal(
  unname(snafun::extract_comm_membership(wrapper_network_true)),
  unname(snafun::extract_comm_membership(backend_true))
)
expect_equal(
  unname(snafun::extract_comm_membership(wrapper_network_false)),
  unname(snafun::extract_comm_membership(backend_false))
)


# dendrogram plotting arguments ------------------------------------------------
dendrogram_body <- paste(deparse(body(snafun::plot_comm_dendrogram)), collapse = "\n")
expect_true(grepl("xlab = xlab", dendrogram_body, fixed = TRUE))
expect_true(grepl("ylab = ylab", dendrogram_body, fixed = TRUE))
expect_true(!grepl("xlab = NULL", dendrogram_body, fixed = TRUE))
expect_true(!grepl("ylab = \"\"", dendrogram_body, fixed = TRUE))

expect_error(
  snafun::plot_comm_dendrogram(judge_net),
  "communities"
)

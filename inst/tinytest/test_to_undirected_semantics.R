report_side_effects()


drop_snafun_edgelist_metadata <- function(x) {
  attr(x, "snafun_vertices") <- NULL
  attr(x, "snafun_bipartite") <- NULL
  attr(x, "snafun_directed") <- NULL
  x
}


expect_same_adjacency <- function(actual, expected) {
  actual <- actual[order(rownames(actual)), order(colnames(actual)), drop = FALSE]
  expected <- expected[order(rownames(expected)), order(colnames(expected)), drop = FALSE]
  expect_identical(actual, expected)
}


# hidden metadata should preserve undirected roundtrips ------------------------
set.seed(20260417)
for (i in seq_len(20)) {
  graph <- igraph::sample_gnp(10, 0.25, directed = FALSE)
  igraph::V(graph)$name <- LETTERS[seq_len(igraph::vcount(graph))]
  edgelist <- snafun::to_edgelist(graph)
  expected_matrix <- snafun::to_matrix(graph)
  
  expect_false(snafun::is_directed(snafun::to_igraph(edgelist)))
  expect_false(snafun::is_directed(snafun::to_network(edgelist)))
  expect_same_adjacency(snafun::to_matrix(edgelist), expected_matrix)
}


# plain single-row edge lists stay ambiguous by default, but can be forced ----
plain_undirected <- data.frame(
  from = c("A", "A", "C"),
  to = c("B", "C", "D"),
  stringsAsFactors = FALSE
)

default_matrix <- snafun::to_matrix(plain_undirected)
expect_false(isSymmetric(default_matrix))
expect_true(snafun::is_directed(snafun::to_igraph(plain_undirected)))
expect_true(snafun::is_directed(snafun::to_network(plain_undirected)))

forced_matrix <- snafun::to_matrix(plain_undirected, directed = FALSE)
expect_true(isSymmetric(forced_matrix))
expect_false(snafun::is_directed(snafun::to_igraph(plain_undirected, directed = FALSE)))
expect_false(snafun::is_directed(snafun::to_network(plain_undirected, directed = FALSE)))
expect_equal(sum(forced_matrix), 2 * nrow(plain_undirected))


# reciprocal plain edge lists should auto-collapse to one undirected tie ------
reciprocal_plain <- data.frame(
  from = c("A", "B", "C", "D", "A", "C"),
  to = c("B", "A", "D", "C", "C", "A"),
  stringsAsFactors = FALSE
)

reciprocal_matrix <- snafun::to_matrix(reciprocal_plain)
expect_true(isSymmetric(reciprocal_matrix))
expect_equal(sum(reciprocal_matrix), 6)

reciprocal_igraph <- snafun::to_igraph(reciprocal_plain)
expect_false(snafun::is_directed(reciprocal_igraph))
expect_equal(igraph::ecount(reciprocal_igraph), 3)

reciprocal_network <- snafun::to_network(reciprocal_plain)
expect_false(snafun::is_directed(reciprocal_network))
expect_equal(network::network.edgecount(reciprocal_network), 3)


# exact reciprocal edge attributes should remain intact after collapsing -------
reciprocal_attrs <- data.frame(
  from = c("A", "B", "A", "C"),
  to = c("B", "A", "C", "A"),
  weight = c(4, 4, 7, 7),
  color = c("red", "red", "blue", "blue"),
  stringsAsFactors = FALSE
)

reciprocal_attr_graph <- snafun::to_igraph(reciprocal_attrs)
expect_false(snafun::is_directed(reciprocal_attr_graph))
expect_equal(igraph::ecount(reciprocal_attr_graph), 2)
expect_equal(sort(igraph::edge_attr(reciprocal_attr_graph, "weight")), c(4, 7))
expect_equal(sort(igraph::edge_attr(reciprocal_attr_graph, "color")), c("blue", "red"))

reciprocal_attr_network <- snafun::to_network(reciprocal_attrs)
expect_false(snafun::is_directed(reciprocal_attr_network))
expect_equal(network::network.edgecount(reciprocal_attr_network), 2)
expect_equal(
  sort(network::get.edge.attribute(reciprocal_attr_network, "weight")),
  c(4, 7)
)
expect_equal(
  sort(network::get.edge.attribute(reciprocal_attr_network, "color")),
  c("blue", "red")
)

reciprocal_attr_matrix <- snafun::to_matrix(reciprocal_attrs)
expect_true(isSymmetric(reciprocal_attr_matrix))
expect_equal(sum(reciprocal_attr_matrix), 22)


# conflicting reciprocal attributes remain directed because collapsing is unsafe
conflicting_reciprocals <- data.frame(
  from = c("A", "B"),
  to = c("B", "A"),
  weight = c(4, 9),
  color = c("red", "blue"),
  stringsAsFactors = FALSE
)

expect_true(snafun::is_directed(snafun::to_igraph(conflicting_reciprocals)))
expect_true(snafun::is_directed(snafun::to_network(conflicting_reciprocals)))
expect_false(isSymmetric(snafun::to_matrix(conflicting_reciprocals)))


# disjoint sender/receiver sets should infer bipartite, even when square ------
bip_square <- data.frame(
  from = c("p1", "p2"),
  to = c("e1", "e2"),
  stringsAsFactors = FALSE
)

bip_square_matrix <- snafun::to_matrix(bip_square)
expect_identical(dim(bip_square_matrix), c(2L, 2L))
expect_identical(rownames(bip_square_matrix), c("p1", "p2"))
expect_identical(colnames(bip_square_matrix), c("e1", "e2"))

bip_square_igraph <- snafun::to_igraph(bip_square)
expect_true(snafun::is_bipartite(bip_square_igraph))
expect_false(snafun::is_directed(bip_square_igraph))

bip_square_network <- snafun::to_network(bip_square)
expect_true(snafun::is_bipartite(bip_square_network))
expect_false(snafun::is_directed(bip_square_network))


# simulations for explicit undirected overrides on plain external edgelists ----
set.seed(20260418)
for (i in seq_len(20)) {
  graph <- igraph::sample_gnp(9, 0.2, directed = FALSE)
  igraph::V(graph)$name <- LETTERS[seq_len(igraph::vcount(graph))]
  plain_edgelist <- drop_snafun_edgelist_metadata(snafun::to_edgelist(graph))
  visible_vertices <- sort(unique(c(plain_edgelist$from, plain_edgelist$to)))
  
  forced_graph <- snafun::to_igraph(plain_edgelist, directed = FALSE)
  forced_network <- snafun::to_network(plain_edgelist, directed = FALSE)
  forced_matrix <- snafun::to_matrix(plain_edgelist, directed = FALSE)
  
  expect_false(snafun::is_directed(forced_graph))
  expect_false(snafun::is_directed(forced_network))
  expect_identical(sort(snafun::extract_vertex_names(forced_graph)), visible_vertices)
  expect_identical(sort(snafun::extract_vertex_names(forced_network)), visible_vertices)
  expect_same_adjacency(
    forced_matrix,
    snafun::to_matrix(graph)[visible_vertices, visible_vertices, drop = FALSE]
  )
}


# simulations for reciprocal external encodings --------------------------------
set.seed(20260419)
for (i in seq_len(15)) {
  graph <- igraph::sample_gnp(8, 0.3, directed = FALSE)
  igraph::V(graph)$name <- LETTERS[seq_len(igraph::vcount(graph))]
  plain_edgelist <- drop_snafun_edgelist_metadata(snafun::to_edgelist(graph))
  
  if (nrow(plain_edgelist) == 0) {
    next
  }
  
  reciprocal_encoding <- rbind(
    plain_edgelist,
    data.frame(
      from = plain_edgelist$to,
      to = plain_edgelist$from,
      stringsAsFactors = FALSE
    )
  )
  
  inferred_graph <- snafun::to_igraph(reciprocal_encoding)
  inferred_network <- snafun::to_network(reciprocal_encoding)
  inferred_matrix <- snafun::to_matrix(reciprocal_encoding)
  
  expect_false(snafun::is_directed(inferred_graph))
  expect_false(snafun::is_directed(inferred_network))
  expect_identical(
    sort(snafun::extract_vertex_names(inferred_graph)),
    sort(unique(c(plain_edgelist$from, plain_edgelist$to)))
  )
  expect_identical(
    sort(snafun::extract_vertex_names(inferred_network)),
    sort(unique(c(plain_edgelist$from, plain_edgelist$to)))
  )
  expect_same_adjacency(
    inferred_matrix,
    snafun::to_matrix(graph)[
      sort(unique(c(plain_edgelist$from, plain_edgelist$to))),
      sort(unique(c(plain_edgelist$from, plain_edgelist$to))),
      drop = FALSE
    ]
  )
}

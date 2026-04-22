report_side_effects()


canonical_membership <- function(x) {
  x <- data.frame(x, stringsAsFactors = FALSE)
  x$vertex <- as.character(x$vertex)
  x$component <- as.integer(x$component)
  x <- x[order(x$vertex), , drop = FALSE]
  new_ids <- match(x$component, unique(x$component))
  x$component <- as.integer(new_ids)
  rownames(x) <- NULL
  x
}


canonical_edge_pairs <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  x <- data.frame(x, stringsAsFactors = FALSE)
  x <- x[, seq_len(2), drop = FALSE]
  colnames(x) <- c("from", "to")
  x$from <- as.character(x$from)
  x$to <- as.character(x$to)
  x <- x[order(x$from, x$to), , drop = FALSE]
  rownames(x) <- NULL
  x
}


make_component_graphs <- function() {
  undirected <- matrix(
    0,
    nrow = 6,
    ncol = 6,
    dimnames = list(LETTERS[1:6], LETTERS[1:6])
  )
  undirected["A", "B"] <- 1
  undirected["B", "A"] <- 1
  undirected["B", "C"] <- 1
  undirected["C", "B"] <- 1
  undirected["D", "E"] <- 1
  undirected["E", "D"] <- 1
  
  directed <- matrix(
    0,
    nrow = 4,
    ncol = 4,
    dimnames = list(letters[1:4], letters[1:4])
  )
  directed["a", "b"] <- 1
  directed["b", "a"] <- 1
  directed["b", "c"] <- 1
  
  list(
    undirected_matrix = undirected,
    undirected_igraph = snafun::to_igraph(undirected),
    undirected_network = snafun::to_network(undirected),
    undirected_edgelist = snafun::to_edgelist(snafun::to_igraph(undirected)),
    directed_matrix = directed,
    directed_igraph = snafun::to_igraph(directed),
    directed_network = snafun::to_network(directed),
    directed_edgelist = snafun::to_edgelist(snafun::to_igraph(directed))
  )
}


graphs <- make_component_graphs()


# count_components -------------------------------------------------------------

for (graph_name in c("undirected_igraph", "undirected_network", "undirected_matrix", "undirected_edgelist")) {
  expect_equal(snafun::count_components(graphs[[graph_name]]), 3L)
}

for (graph_name in c("directed_igraph", "directed_network", "directed_matrix", "directed_edgelist")) {
  expect_equal(snafun::count_components(graphs[[graph_name]], type = "weak"), 2L)
  expect_equal(snafun::count_components(graphs[[graph_name]], type = "strong"), 3L)
}


# extract_component_membership -------------------------------------------------

expected_undirected_membership <- data.frame(
  vertex = LETTERS[1:6],
  component = c(1L, 1L, 1L, 2L, 2L, 3L),
  stringsAsFactors = FALSE
)

for (graph_name in c("undirected_igraph", "undirected_network", "undirected_matrix", "undirected_edgelist")) {
  actual <- snafun::extract_component_membership(graphs[[graph_name]])
  expect_identical(canonical_membership(actual), canonical_membership(expected_undirected_membership))
}

expected_directed_weak <- data.frame(
  vertex = letters[1:4],
  component = c(1L, 1L, 1L, 2L),
  stringsAsFactors = FALSE
)

expected_directed_strong <- data.frame(
  vertex = letters[1:4],
  component = c(1L, 1L, 2L, 3L),
  stringsAsFactors = FALSE
)

for (graph_name in c("directed_igraph", "directed_network", "directed_matrix", "directed_edgelist")) {
  actual_weak <- snafun::extract_component_membership(graphs[[graph_name]], type = "weak")
  expect_identical(canonical_membership(actual_weak), canonical_membership(expected_directed_weak))
  
  actual_strong <- snafun::extract_component_membership(graphs[[graph_name]], type = "strong")
  expect_identical(canonical_membership(actual_strong), canonical_membership(expected_directed_strong))
}


# extract_components -----------------------------------------------------------

igraph_components <- snafun::extract_components(graphs$undirected_igraph)
network_components <- snafun::extract_components(graphs$undirected_network)
matrix_components <- snafun::extract_components(graphs$undirected_matrix)
edgelist_components <- snafun::extract_components(graphs$undirected_edgelist)

expect_equal(length(igraph_components), 3L)
expect_equal(length(network_components), 3L)
expect_equal(length(matrix_components), 3L)
expect_equal(length(edgelist_components), 3L)

expect_true(all(vapply(igraph_components, inherits, logical(1), what = "igraph")))
expect_true(all(vapply(network_components, inherits, logical(1), what = "network")))
expect_true(all(vapply(matrix_components, is.matrix, logical(1))))
expect_true(all(vapply(edgelist_components, is.data.frame, logical(1))))

component_sizes_expected <- c(3L, 2L, 1L)
expect_identical(unname(vapply(igraph_components, snafun::count_vertices, integer(1))), component_sizes_expected)
expect_identical(unname(vapply(network_components, snafun::count_vertices, integer(1))), component_sizes_expected)
expect_identical(unname(vapply(matrix_components, nrow, integer(1))), component_sizes_expected)
expect_identical(unname(vapply(edgelist_components, function(x) nrow(attr(x, "snafun_vertices", exact = TRUE)), integer(1))), component_sizes_expected)

expect_identical(
  unname(sort(unlist(lapply(edgelist_components, function(x) attr(x, "snafun_vertices", exact = TRUE)[, 1])))),
  LETTERS[1:6]
)


# cut vertices and bridges -----------------------------------------------------

bridge_matrix <- matrix(
  0,
  nrow = 5,
  ncol = 5,
  dimnames = list(LETTERS[1:5], LETTERS[1:5])
)
bridge_matrix["A", "B"] <- 1
bridge_matrix["B", "A"] <- 1
bridge_matrix["B", "C"] <- 1
bridge_matrix["C", "B"] <- 1
bridge_matrix["C", "D"] <- 1
bridge_matrix["D", "C"] <- 1

bridge_igraph <- snafun::to_igraph(bridge_matrix)
bridge_network <- snafun::to_network(bridge_matrix)
bridge_edgelist <- snafun::to_edgelist(bridge_igraph)

for (graph in list(bridge_igraph, bridge_network, bridge_matrix, bridge_edgelist)) {
  expect_identical(sort(snafun::extract_cut_vertices(graph)), c("B", "C"))
}

expected_bridges <- data.frame(
  from = c("A", "B", "C"),
  to = c("B", "C", "D"),
  stringsAsFactors = FALSE
)

for (graph in list(bridge_igraph, bridge_network, bridge_matrix, bridge_edgelist)) {
  actual_bridges <- snafun::extract_bridges(graph)
  expect_identical(canonical_edge_pairs(actual_bridges), canonical_edge_pairs(expected_bridges))
}

directed_bridge <- matrix(
  0,
  nrow = 4,
  ncol = 4,
  dimnames = list(letters[1:4], letters[1:4])
)
directed_bridge["a", "b"] <- 1
directed_bridge["b", "c"] <- 1
directed_bridge["c", "d"] <- 1

for (graph in list(
  snafun::to_igraph(directed_bridge),
  snafun::to_network(directed_bridge),
  directed_bridge,
  snafun::to_edgelist(snafun::to_igraph(directed_bridge))
)) {
  expect_identical(sort(snafun::extract_cut_vertices(graph)), c("b", "c"))
  expect_identical(
    canonical_edge_pairs(snafun::extract_bridges(graph)),
    canonical_edge_pairs(data.frame(from = c("a", "b", "c"), to = c("b", "c", "d"), stringsAsFactors = FALSE))
  )
}

triangle <- igraph::make_full_graph(3, directed = FALSE)
igraph::V(triangle)$name <- c("x", "y", "z")
expect_null(snafun::extract_cut_vertices(triangle))
expect_null(snafun::extract_bridges(triangle))

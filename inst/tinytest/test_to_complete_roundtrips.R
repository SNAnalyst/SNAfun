report_side_effects()


drop_edgelist_metadata <- function(x) {
  attr(x, "snafun_vertices") <- NULL
  attr(x, "snafun_bipartite") <- NULL
  attr(x, "snafun_directed") <- NULL
  x
}


expect_visible_edgelist_equal <- function(actual, expected) {
  rownames(actual) <- NULL
  rownames(expected) <- NULL
  expect_identical(drop_edgelist_metadata(actual), drop_edgelist_metadata(expected))
}


expect_stored_vertex_metadata <- function(edgelist,
                                          expected_names,
                                          bipartite = FALSE,
                                          directed = FALSE,
                                          expected_attributes = list()) {
  stored_vertices <- attr(edgelist, "snafun_vertices", exact = TRUE)
  expect_true(is.data.frame(stored_vertices))
  expect_true(is.character(stored_vertices[[1]]) || is.numeric(stored_vertices[[1]]))
  expect_identical(as.character(stored_vertices[[1]]), as.character(expected_names))
  expect_identical(isTRUE(attr(edgelist, "snafun_bipartite", exact = TRUE)), bipartite)
  expect_identical(isTRUE(attr(edgelist, "snafun_directed", exact = TRUE)), directed)
  
  if (length(expected_attributes) > 0) {
    for (attribute_name in names(expected_attributes)) {
      expect_true(attribute_name %in% colnames(stored_vertices))
      expect_identical(
        as.vector(stored_vertices[[attribute_name]]),
        as.vector(expected_attributes[[attribute_name]])
      )
    }
  }
}


expect_graph_matches_reference <- function(x,
                                           expected_matrix,
                                           expected_edgelist,
                                           expected_isolates,
                                           directed,
                                           bipartite,
                                           expected_vertex_names,
                                           expected_vertex_attributes = list()) {
  expected_vertex_count <- if (bipartite) {
    nrow(expected_matrix) + ncol(expected_matrix)
  } else {
    nrow(expected_matrix)
  }
  
  expect_equal(snafun::count_vertices(x), expected_vertex_count)
  expect_equal(snafun::count_edges(x), nrow(expected_edgelist))
  expect_identical(snafun::is_directed(x), directed)
  expect_identical(snafun::is_bipartite(x), bipartite)
  expect_identical(snafun::extract_vertex_names(x), expected_vertex_names)
  expect_identical(snafun::extract_isolates(x), expected_isolates)
  expect_identical(snafun::to_matrix(x), expected_matrix)
  expect_visible_edgelist_equal(snafun::to_edgelist(x), expected_edgelist)
  
  if (length(expected_vertex_attributes) > 0) {
    for (attribute_name in names(expected_vertex_attributes)) {
      expect_true(attribute_name %in% snafun::list_vertex_attributes(x))
      expect_identical(
        as.vector(snafun::extract_vertex_attribute(x, attribute_name)),
        as.vector(expected_vertex_attributes[[attribute_name]])
      )
    }
  }
}


make_onemode_reference <- function() {
  graph <- igraph::make_empty_graph(n = 5, directed = TRUE)
  graph <- igraph::add_edges(graph, c(1, 2, 2, 1, 2, 2, 3, 4))
  graph <- igraph::set_vertex_attr(graph, "name", value = LETTERS[1:5])
  graph <- igraph::set_vertex_attr(graph, "group", value = c("g1", "g1", "g2", "g2", "g3"))
  graph <- igraph::set_vertex_attr(graph, "score", value = c(10, 20, 30, 40, 50))
  graph <- igraph::set_edge_attr(graph, "weight", value = c(2, 5, 7, 11))
  graph <- igraph::set_edge_attr(graph, "color", value = c("red", "blue", "green", "orange"))
  
  matrix_reference <- snafun::to_matrix(graph)
  rich_edgelist <- data.frame(
    from = c("A", "B", "B", "C"),
    to = c("B", "A", "B", "D"),
    weight = c(2, 5, 7, 11),
    color = c("red", "blue", "green", "orange"),
    stringsAsFactors = FALSE
  )
  matrix_edgelist <- rich_edgelist[, c("from", "to", "weight")]
  
  list(
    graph = graph,
    network = snafun::to_network(graph),
    matrix = matrix_reference,
    edgelist = snafun::to_edgelist(graph),
    vertex_names = LETTERS[1:5],
    isolates = "E",
    rich_edgelist = rich_edgelist,
    matrix_edgelist = matrix_edgelist,
    rich_vertex_attributes = list(
      group = c("g1", "g1", "g2", "g2", "g3"),
      score = c(10, 20, 30, 40, 50)
    )
  )
}


make_bipartite_reference <- function() {
  matrix_reference <- matrix(
    c(1, 0, 0, 0,
      0, 0, 0, 0,
      0, 3, 0, 0),
    nrow = 3,
    byrow = TRUE
  )
  rownames(matrix_reference) <- c("p1", "p2", "p3")
  colnames(matrix_reference) <- c("e1", "e2", "e3", "e4")
  
  graph <- snafun::to_igraph(matrix_reference, bipartite = TRUE)
  graph <- igraph::set_vertex_attr(graph, "label2", value = paste0("L", seq_len(igraph::vcount(graph))))
  graph <- igraph::set_edge_attr(graph, "color", value = c("red", "blue"))
  
  rich_edgelist <- data.frame(
    from = c("p1", "p3"),
    to = c("e1", "e2"),
    weight = c(1, 3),
    color = c("red", "blue"),
    stringsAsFactors = FALSE
  )
  matrix_edgelist <- rich_edgelist[, c("from", "to", "weight")]
  
  list(
    graph = graph,
    network = snafun::to_network(graph),
    matrix = matrix_reference,
    edgelist = snafun::to_edgelist(graph),
    vertex_names = c("p1", "p2", "p3", "e1", "e2", "e3", "e4"),
    isolates = c("p2", "e3", "e4"),
    rich_edgelist = rich_edgelist,
    matrix_edgelist = matrix_edgelist,
    rich_vertex_attributes = list(
      label2 = paste0("L", seq_len(7))
    ),
    type_indicator = c(FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE)
  )
}


onemode <- make_onemode_reference()
bipartite <- make_bipartite_reference()

# Rectangular incidence matrices should remain a stable special case
# throughout the conversion code. These direct checks catch regressions in the
# low-level helpers before they cascade into the larger roundtrip matrix below.
expect_false(snafun::is_directed(bipartite$matrix))
expect_identical(
  network::get.network.attribute(snafun::to_network(bipartite$matrix, bipartite = TRUE), "bipartite"),
  nrow(bipartite$matrix)
)


# onemode direct conversions ---------------------------------------------------
expect_graph_matches_reference(
  snafun::to_igraph(onemode$graph),
  expected_matrix = onemode$matrix,
  expected_edgelist = onemode$rich_edgelist,
  expected_isolates = onemode$isolates,
  directed = TRUE,
  bipartite = FALSE,
  expected_vertex_names = onemode$vertex_names,
  expected_vertex_attributes = onemode$rich_vertex_attributes
)

expect_graph_matches_reference(
  snafun::to_igraph(onemode$network),
  expected_matrix = onemode$matrix,
  expected_edgelist = onemode$rich_edgelist,
  expected_isolates = onemode$isolates,
  directed = TRUE,
  bipartite = FALSE,
  expected_vertex_names = onemode$vertex_names,
  expected_vertex_attributes = onemode$rich_vertex_attributes
)

expect_graph_matches_reference(
  snafun::to_igraph(onemode$matrix),
  expected_matrix = onemode$matrix,
  expected_edgelist = onemode$matrix_edgelist,
  expected_isolates = onemode$isolates,
  directed = TRUE,
  bipartite = FALSE,
  expected_vertex_names = onemode$vertex_names
)

expect_graph_matches_reference(
  snafun::to_igraph(onemode$edgelist),
  expected_matrix = onemode$matrix,
  expected_edgelist = onemode$rich_edgelist,
  expected_isolates = onemode$isolates,
  directed = TRUE,
  bipartite = FALSE,
  expected_vertex_names = onemode$vertex_names,
  expected_vertex_attributes = onemode$rich_vertex_attributes
)

expect_graph_matches_reference(
  snafun::to_network(onemode$graph),
  expected_matrix = onemode$matrix,
  expected_edgelist = onemode$rich_edgelist,
  expected_isolates = onemode$isolates,
  directed = TRUE,
  bipartite = FALSE,
  expected_vertex_names = onemode$vertex_names,
  expected_vertex_attributes = onemode$rich_vertex_attributes
)

expect_graph_matches_reference(
  snafun::to_network(onemode$network),
  expected_matrix = onemode$matrix,
  expected_edgelist = onemode$rich_edgelist,
  expected_isolates = onemode$isolates,
  directed = TRUE,
  bipartite = FALSE,
  expected_vertex_names = onemode$vertex_names,
  expected_vertex_attributes = onemode$rich_vertex_attributes
)

expect_graph_matches_reference(
  snafun::to_network(onemode$matrix),
  expected_matrix = onemode$matrix,
  expected_edgelist = onemode$matrix_edgelist,
  expected_isolates = onemode$isolates,
  directed = TRUE,
  bipartite = FALSE,
  expected_vertex_names = onemode$vertex_names
)

expect_graph_matches_reference(
  snafun::to_network(onemode$edgelist),
  expected_matrix = onemode$matrix,
  expected_edgelist = onemode$rich_edgelist,
  expected_isolates = onemode$isolates,
  directed = TRUE,
  bipartite = FALSE,
  expected_vertex_names = onemode$vertex_names,
  expected_vertex_attributes = onemode$rich_vertex_attributes
)

expect_identical(snafun::to_matrix(onemode$graph), onemode$matrix)
expect_identical(snafun::to_matrix(onemode$network), onemode$matrix)
expect_identical(snafun::to_matrix(onemode$matrix), onemode$matrix)
expect_warning(
  expect_identical(snafun::to_matrix(onemode$edgelist), onemode$matrix),
  "only column 3"
)


# onemode edgelist targets -----------------------------------------------------
edgelist_from_onemode_igraph <- snafun::to_edgelist(onemode$graph)
expect_visible_edgelist_equal(edgelist_from_onemode_igraph, onemode$rich_edgelist)
expect_stored_vertex_metadata(
  edgelist_from_onemode_igraph,
  expected_names = onemode$vertex_names,
  directed = TRUE,
  expected_attributes = onemode$rich_vertex_attributes
)

edgelist_from_onemode_network <- snafun::to_edgelist(onemode$network)
expect_visible_edgelist_equal(edgelist_from_onemode_network, onemode$rich_edgelist)
expect_stored_vertex_metadata(
  edgelist_from_onemode_network,
  expected_names = onemode$vertex_names,
  directed = TRUE,
  expected_attributes = onemode$rich_vertex_attributes
)

edgelist_from_onemode_matrix <- snafun::to_edgelist(onemode$matrix)
expect_visible_edgelist_equal(edgelist_from_onemode_matrix, onemode$matrix_edgelist)
expect_stored_vertex_metadata(
  edgelist_from_onemode_matrix,
  expected_names = onemode$vertex_names,
  directed = TRUE
)

onemode_edgelist_sources <- list(
  list(
    input = edgelist_from_onemode_igraph,
    expected_edgelist = onemode$rich_edgelist,
    expected_vertex_attributes = onemode$rich_vertex_attributes,
    expect_matrix_warning = TRUE
  ),
  list(
    input = edgelist_from_onemode_network,
    expected_edgelist = onemode$rich_edgelist,
    expected_vertex_attributes = onemode$rich_vertex_attributes,
    expect_matrix_warning = TRUE
  ),
  list(
    input = edgelist_from_onemode_matrix,
    expected_edgelist = onemode$matrix_edgelist,
    expected_vertex_attributes = list(),
    expect_matrix_warning = FALSE
  )
)

for (case in onemode_edgelist_sources) {
  expect_graph_matches_reference(
    snafun::to_igraph(case$input),
    expected_matrix = onemode$matrix,
    expected_edgelist = case$expected_edgelist,
    expected_isolates = onemode$isolates,
    directed = TRUE,
    bipartite = FALSE,
    expected_vertex_names = onemode$vertex_names,
    expected_vertex_attributes = case$expected_vertex_attributes
  )
  
  expect_graph_matches_reference(
    snafun::to_network(case$input),
    expected_matrix = onemode$matrix,
    expected_edgelist = case$expected_edgelist,
    expected_isolates = onemode$isolates,
    directed = TRUE,
    bipartite = FALSE,
    expected_vertex_names = onemode$vertex_names,
    expected_vertex_attributes = case$expected_vertex_attributes
  )
  
  if (isTRUE(case$expect_matrix_warning)) {
    expect_warning(
      expect_identical(snafun::to_matrix(case$input), onemode$matrix),
      "only column 3"
    )
  } else {
    expect_identical(snafun::to_matrix(case$input), onemode$matrix)
  }
}


# bipartite direct conversions -------------------------------------------------
expect_graph_matches_reference(
  snafun::to_igraph(bipartite$graph),
  expected_matrix = bipartite$matrix,
  expected_edgelist = bipartite$rich_edgelist,
  expected_isolates = bipartite$isolates,
  directed = FALSE,
  bipartite = TRUE,
  expected_vertex_names = bipartite$vertex_names,
  expected_vertex_attributes = bipartite$rich_vertex_attributes
)

expect_graph_matches_reference(
  snafun::to_igraph(bipartite$network),
  expected_matrix = bipartite$matrix,
  expected_edgelist = bipartite$rich_edgelist,
  expected_isolates = bipartite$isolates,
  directed = FALSE,
  bipartite = TRUE,
  expected_vertex_names = bipartite$vertex_names,
  expected_vertex_attributes = bipartite$rich_vertex_attributes
)

expect_graph_matches_reference(
  snafun::to_igraph(bipartite$matrix, bipartite = TRUE),
  expected_matrix = bipartite$matrix,
  expected_edgelist = bipartite$matrix_edgelist,
  expected_isolates = bipartite$isolates,
  directed = FALSE,
  bipartite = TRUE,
  expected_vertex_names = bipartite$vertex_names
)

expect_graph_matches_reference(
  suppressMessages(snafun::to_igraph(bipartite$edgelist)),
  expected_matrix = bipartite$matrix,
  expected_edgelist = bipartite$rich_edgelist,
  expected_isolates = bipartite$isolates,
  directed = FALSE,
  bipartite = TRUE,
  expected_vertex_names = bipartite$vertex_names,
  expected_vertex_attributes = bipartite$rich_vertex_attributes
)

expect_graph_matches_reference(
  snafun::to_network(bipartite$graph),
  expected_matrix = bipartite$matrix,
  expected_edgelist = bipartite$rich_edgelist,
  expected_isolates = bipartite$isolates,
  directed = FALSE,
  bipartite = TRUE,
  expected_vertex_names = bipartite$vertex_names,
  expected_vertex_attributes = bipartite$rich_vertex_attributes
)

expect_graph_matches_reference(
  snafun::to_network(bipartite$network),
  expected_matrix = bipartite$matrix,
  expected_edgelist = bipartite$rich_edgelist,
  expected_isolates = bipartite$isolates,
  directed = FALSE,
  bipartite = TRUE,
  expected_vertex_names = bipartite$vertex_names,
  expected_vertex_attributes = bipartite$rich_vertex_attributes
)

expect_graph_matches_reference(
  snafun::to_network(bipartite$matrix, bipartite = TRUE),
  expected_matrix = bipartite$matrix,
  expected_edgelist = bipartite$matrix_edgelist,
  expected_isolates = bipartite$isolates,
  directed = FALSE,
  bipartite = TRUE,
  expected_vertex_names = bipartite$vertex_names
)

expect_graph_matches_reference(
  snafun::to_network(bipartite$edgelist),
  expected_matrix = bipartite$matrix,
  expected_edgelist = bipartite$rich_edgelist,
  expected_isolates = bipartite$isolates,
  directed = FALSE,
  bipartite = TRUE,
  expected_vertex_names = bipartite$vertex_names,
  expected_vertex_attributes = bipartite$rich_vertex_attributes
)

expect_identical(snafun::to_matrix(bipartite$graph), bipartite$matrix)
expect_identical(snafun::to_matrix(bipartite$network), bipartite$matrix)
expect_identical(snafun::to_matrix(bipartite$matrix), bipartite$matrix)
expect_warning(
  expect_identical(snafun::to_matrix(bipartite$edgelist), bipartite$matrix),
  "only column 3"
)


# bipartite edgelist targets ---------------------------------------------------
edgelist_from_bip_igraph <- snafun::to_edgelist(bipartite$graph)
expect_visible_edgelist_equal(edgelist_from_bip_igraph, bipartite$rich_edgelist)
expect_stored_vertex_metadata(
  edgelist_from_bip_igraph,
  expected_names = bipartite$vertex_names,
  bipartite = TRUE,
  expected_attributes = c(
    list(type = bipartite$type_indicator),
    bipartite$rich_vertex_attributes
  )
)

edgelist_from_bip_network <- snafun::to_edgelist(bipartite$network)
expect_visible_edgelist_equal(edgelist_from_bip_network, bipartite$rich_edgelist)
expect_stored_vertex_metadata(
  edgelist_from_bip_network,
  expected_names = bipartite$vertex_names,
  bipartite = TRUE,
  expected_attributes = c(
    list(type = bipartite$type_indicator),
    bipartite$rich_vertex_attributes
  )
)

edgelist_from_bip_matrix <- snafun::to_edgelist(bipartite$matrix)
expect_visible_edgelist_equal(edgelist_from_bip_matrix, bipartite$matrix_edgelist)
expect_stored_vertex_metadata(
  edgelist_from_bip_matrix,
  expected_names = bipartite$vertex_names,
  bipartite = TRUE,
  expected_attributes = list(type = bipartite$type_indicator)
)

bipartite_edgelist_sources <- list(
  list(
    input = edgelist_from_bip_igraph,
    expected_edgelist = bipartite$rich_edgelist,
    expected_vertex_attributes = bipartite$rich_vertex_attributes,
    expect_matrix_warning = TRUE
  ),
  list(
    input = edgelist_from_bip_network,
    expected_edgelist = bipartite$rich_edgelist,
    expected_vertex_attributes = bipartite$rich_vertex_attributes,
    expect_matrix_warning = TRUE
  ),
  list(
    input = edgelist_from_bip_matrix,
    expected_edgelist = bipartite$matrix_edgelist,
    expected_vertex_attributes = list(),
    expect_matrix_warning = FALSE
  )
)

for (case in bipartite_edgelist_sources) {
  expect_graph_matches_reference(
    suppressMessages(snafun::to_igraph(case$input)),
    expected_matrix = bipartite$matrix,
    expected_edgelist = case$expected_edgelist,
    expected_isolates = bipartite$isolates,
    directed = FALSE,
    bipartite = TRUE,
    expected_vertex_names = bipartite$vertex_names,
    expected_vertex_attributes = case$expected_vertex_attributes
  )
  
  expect_graph_matches_reference(
    snafun::to_network(case$input),
    expected_matrix = bipartite$matrix,
    expected_edgelist = case$expected_edgelist,
    expected_isolates = bipartite$isolates,
    directed = FALSE,
    bipartite = TRUE,
    expected_vertex_names = bipartite$vertex_names,
    expected_vertex_attributes = case$expected_vertex_attributes
  )
  
  if (isTRUE(case$expect_matrix_warning)) {
    expect_warning(
      expect_identical(snafun::to_matrix(case$input), bipartite$matrix),
      "only column 3"
    )
  } else {
    expect_identical(snafun::to_matrix(case$input), bipartite$matrix)
  }
}

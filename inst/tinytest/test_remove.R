
report_side_effects()


# it is possible to only have few tests for remove_vertices, since this 
# function is used in remove_isolates. Hence, all tests for remove_isolates 
# also test remove_vertices.




mat <- structure(c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 
                   0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 
                   0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                   0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 
                   0, 1, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 1, 1, 0, 0, 0, 0, 1, 0), 
                 dim = c(10L, 10L))



### IGRAPH ----
ig <- igraph::graph_from_adjacency_matrix(mat)
expect_equal(length(extract_isolates(ig)), 4)
expect_equal(length(extract_isolates(ig, loops = TRUE)), 2)
expect_true(igraph::vcount(ig) == 10)
expect_true(igraph::ecount(ig) == 15)
expect_true(igraph::vcount(remove_isolates(ig)) == 6)
expect_true(igraph::vcount(remove_isolates(ig, loops = TRUE)) == 8)
expect_true(igraph::ecount(remove_isolates(ig)) == 13) # vanwege de self-loops
expect_true(igraph::ecount(remove_isolates(ig, loops = TRUE)) == 15)
# nothing should happen if the network does not have any isolates
ig_no_isolates <- remove_isolates(ig)
expect_identical(ig_no_isolates, remove_isolates(ig_no_isolates))

expect_error(remove_isolates(mat), "class")
expect_error(remove_isolates(mat), "igraph")
expect_error(remove_isolates(mat), "network")


expect_true(igraph::vcount(remove_vertices(ig, c(1, 2, 4))) == 7)
expect_true(igraph::ecount(remove_vertices(ig, c(1, 2, 4))) == 7)  # self-loop counts too
expect_true(igraph::ecount(remove_vertices(ig, c(1, 2, 8))) == 8)
expect_error(suppressWarnings(remove_vertices(ig, c("A", "B", "C"))), "Invalid")


# named vertices
igraph::V(ig)$name <- LETTERS[1:10]
expect_true(igraph::vcount(remove_isolates(ig)) == 6)
expect_true(igraph::vcount(remove_isolates(ig, loops = TRUE)) == 8)
expect_true(igraph::ecount(remove_isolates(ig)) == 13) # vanwege de self-loops
expect_true(igraph::ecount(remove_isolates(ig, loops = TRUE)) == 15)
expect_equal(igraph::V(remove_isolates(ig))$name, c("B", "C", "D", "G", "I", "J"))
expect_equal(igraph::V(remove_isolates(ig, loops = TRUE))$name, c("A", "B", "C", "D", "G", "H", "I", "J"))
ig_no_isolates <- remove_isolates(ig)
expect_identical(ig_no_isolates, remove_isolates(ig_no_isolates))



# bipartite
big <- igraph::make_bipartite_graph(c(rep(0, 6), rep(1, 8)), 
                                  edges = c(1, 10, 2, 11, 3, 12, 4, 10, 
                                            5, 7, 5, 8, 5, 12))

expect_equal(extract_isolates(big), c(6, 9, 13, 14))
expect_equal(extract_isolates(big, loops = TRUE), c(6, 9, 13, 14))
expect_true(igraph::vcount(big) == 14)
expect_true(igraph::ecount(big) == 7)
expect_true(igraph::vcount(remove_isolates(big)) == 10)
expect_true(igraph::ecount(remove_isolates(big)) == 7)
big_no_isolates <- remove_isolates(big)
expect_identical(big_no_isolates, remove_isolates(big_no_isolates))

igraph::V(big)$name <- c(paste("person", LETTERS[1:6]), 
                       paste("event", letters[1:8]))
expect_true(igraph::vcount(big) == 14)
expect_true(igraph::ecount(big) == 7)
expect_true(igraph::vcount(remove_isolates(big)) == 10)
expect_true(igraph::ecount(remove_isolates(big)) == 7)
expect_equal(igraph::V(remove_isolates(big))$name, 
                       c("person A", "person B", "person C", "person D", "person E",
                         "event a", "event b", "event d", "event e", "event f" ))
big_no_isolates <- remove_isolates(big)
expect_identical(big_no_isolates, remove_isolates(big_no_isolates))




### NETWORK ----
mat <- structure(c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 
                   0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 
                   0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                   0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 
                   0, 1, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 1, 1, 0, 0, 0, 0, 1, 0), 
                 dim = c(10L, 10L))


nw <- network::as.network.matrix(mat, loops = TRUE)
expect_equal(length(extract_isolates(nw)), 4)
expect_equal(length(extract_isolates(nw, loops = TRUE)), 2)
expect_true(network::network.size(nw) == 10)
expect_true(network::network.edgecount(nw) == 15)
expect_true(network::network.size(remove_isolates(nw)) == 6)
expect_true(network::network.size(remove_isolates(nw, loops = TRUE)) == 8)
expect_true(network::network.edgecount(remove_isolates(nw)) == 13) # vanwege de self-loops
expect_true(network::network.edgecount(remove_isolates(nw, loops = TRUE)) == 15)
# nothing should happen if the network does not have any isolates
nw_no_isolates <- remove_isolates(nw)
expect_identical(nw_no_isolates, remove_isolates(nw_no_isolates))

expect_true(network::network.size(remove_vertices(nw, c(1, 2, 4))) == 7)
expect_true(network::network.edgecount(remove_vertices(nw, c(1, 2, 4))) == 7)  # self-loop counts too
expect_true(network::network.edgecount(remove_vertices(nw, c(1, 2, 8))) == 8)
expect_error(remove_vertices(nw, c("A", "B", "C")))

# named vertices
network::set.vertex.attribute(nw, "vertex.names", LETTERS[1:10])
expect_true(network::network.size(remove_isolates(nw)) == 6)
expect_true(network::network.size(remove_isolates(nw, loops = TRUE)) == 8)
expect_true(network::network.edgecount(remove_isolates(nw)) == 13) # vanwege de self-loops
expect_true(network::network.edgecount(remove_isolates(nw, loops = TRUE)) == 15)
expect_equal(network::get.vertex.attribute(remove_isolates(nw), "vertex.names"), 
                       c("B", "C", "D", "G", "I", "J"))
expect_equal(network::get.vertex.attribute(remove_isolates(nw, loops = TRUE), "vertex.names"), 
                       c("A", "B", "C", "D", "G", "H", "I", "J"))
nw_no_isolates <- remove_isolates(nw)
expect_identical(nw_no_isolates, remove_isolates(nw_no_isolates))


# bipartite
bmat <- igraph::as_adjacency_matrix(big, sparse = FALSE)[1:6, 7:14]
colnames(bmat) <- rownames(bmat) <- NULL
bnw <- network::network.bipartite(bmat, network::as.network.matrix(bmat))
expect_equal(extract_isolates(bnw), c(6, 9, 13, 14))
expect_equal(extract_isolates(bnw, loops = TRUE), c(6, 9, 13, 14))
expect_true(network::network.size(bnw) == 14)
expect_true(network::network.edgecount(bnw) == 14) # since undirected, edges count twice in `network`
expect_true(network::network.size(remove_isolates(bnw)) == 10)
expect_true(network::network.edgecount(remove_isolates(bnw)) == 14)
bnw_no_isolates <- remove_isolates(bnw)
expect_identical(bnw_no_isolates, remove_isolates(bnw_no_isolates))

network::set.vertex.attribute(bnw, "vertex.names", 
                              c(paste("person", LETTERS[1:6]), 
                                paste("event", letters[1:8])))
expect_true(network::network.size(bnw) == 14)
expect_true(network::network.edgecount(bnw) == 14)  # counts twice!
expect_true(network::network.size(remove_isolates(bnw)) == 10)
expect_true(network::network.edgecount(remove_isolates(bnw)) == 14)
expect_equal(network::get.vertex.attribute(remove_isolates(bnw), "vertex.names"),
                       c("person A", "person B", "person C", "person D", "person E",
                         "event a", "event b", "event d", "event e", "event f" ))
bnw_no_isolates <- remove_isolates(bnw)
expect_identical(bnw_no_isolates, remove_isolates(bnw_no_isolates))






# remove_loops -----------------------------------------------------------------

x <- matrix(c(1, 1, 0, 0, 0, 1, 1, 0, 1), ncol = 3, byrow = TRUE)
expect_true(all(diag(remove_loops(x)) == c(0, 0, 0)))
g_n <- snafun::to_network(x)
expect_true(all(diag(snafun::to_matrix(g_n)) == c(1, 0, 1)))
expect_true(all(diag(remove_loops(g_n) |> snafun::to_matrix() == c(0, 0, 0))))
expect_true(all(diag(remove_loops(snafun::to_igraph(x)) |> snafun::to_matrix() == c(0, 0, 0))))




# remove_multiple_edges --------------------------------------------------------

canonicalize_edge_table <- function(x, directed = TRUE) {
  if (!is.data.frame(x)) {
    x <- snafun::to_edgelist(x, named = FALSE)
  }
  x <- data.frame(x, check.names = FALSE, stringsAsFactors = FALSE)
  if (!directed) {
    new_from <- pmin(x[[1]], x[[2]])
    new_to <- pmax(x[[1]], x[[2]])
    x[[1]] <- new_from
    x[[2]] <- new_to
  }
  x <- x[order(x[[1]], x[[2]]), , drop = FALSE]
  rownames(x) <- NULL
  x
}


extract_vertex_state <- function(x) {
  if (is.data.frame(x)) {
    state <- attr(x, "snafun_vertices", exact = TRUE)
    if (is.null(state)) {
      return(NULL)
    }
    return(data.frame(state, check.names = FALSE, stringsAsFactors = FALSE))
  }
  
  state <- snafun::extract_all_vertex_attributes(x)
  if (is.null(state)) {
    return(NULL)
  }
  data.frame(state, check.names = FALSE, stringsAsFactors = FALSE)
}


make_vertex_table <- function() {
  data.frame(
    name = 1:3,
    role = c("alpha", "beta", "gamma"),
    cohort = c("A", "B", "C"),
    stringsAsFactors = FALSE
  )
}


make_multiedge_table <- function(directed = TRUE, attr_mode = c("multiple", "single", "none")) {
  attr_mode <- match.arg(attr_mode)
  
  if (directed) {
    edges <- data.frame(
      from = c(1, 1, 1, 2, 3, 3),
      to = c(2, 2, 2, 1, 3, 3),
      stringsAsFactors = FALSE
    )
  } else {
    edges <- data.frame(
      from = c(1, 2, 1, 2, 3, 3),
      to = c(2, 1, 2, 3, 3, 3),
      stringsAsFactors = FALSE
    )
  }
  
  if (attr_mode == "multiple") {
    edges$weight <- c(2, 5, 11, 7, 13, 17)
    edges$label <- c("a", "b", "c", "d", "loop1", "loop2")
    edges$flag <- c(FALSE, TRUE, FALSE, TRUE, FALSE, TRUE)
    edges$score <- c(2, 5, 11, 7, 13, 17)
  } else if (attr_mode == "single") {
    edges$weight <- c(2, 5, 11, 7, 13, 17)
  }
  
  edges
}


make_rule_table <- function(attr_name, values) {
  out <- data.frame(
    from = c(1, 1, 1, 2),
    to = c(2, 2, 2, 3),
    stringsAsFactors = FALSE
  )
  out[[attr_name]] <- values
  out
}


make_backend_object <- function(backend,
                                edge_table,
                                directed = TRUE,
                                stored_metadata = TRUE) {
  vertices <- make_vertex_table()
  
  if (backend == "igraph") {
    out <- igraph::graph_from_data_frame(
      edge_table[, 1:2, drop = FALSE],
      directed = directed,
      vertices = vertices
    )
    if (ncol(edge_table) > 2) {
      for (one_attr in colnames(edge_table)[-(1:2)]) {
        out <- igraph::set_edge_attr(out, one_attr, value = edge_table[[one_attr]])
      }
    }
    return(out)
  }
  
  if (backend == "network") {
    out <- network::network.initialize(
      nrow(vertices),
      directed = directed,
      loops = TRUE,
      multiple = TRUE
    )
    out <- network::add.edges(out, tail = edge_table[[1]], head = edge_table[[2]])
    out <- network::set.vertex.attribute(out, "vertex.names", value = vertices$name)
    out <- network::set.vertex.attribute(out, "role", value = vertices$role)
    out <- network::set.vertex.attribute(out, "cohort", value = vertices$cohort)
    if (ncol(edge_table) > 2) {
      for (one_attr in colnames(edge_table)[-(1:2)]) {
        out <- network::set.edge.attribute(out, one_attr, value = edge_table[[one_attr]])
      }
    }
    return(out)
  }
  
  if (backend == "edgelist") {
    if (stored_metadata) {
      return(snafun::to_edgelist(
        make_backend_object("igraph", edge_table, directed = directed),
        named = FALSE
      ))
    }
    return(edge_table)
  }
  
  stop("Unknown backend in test helper")
}


expect_directed_state <- function(x, directed) {
  if (is.data.frame(x)) {
    if (!is.null(attr(x, "snafun_directed", exact = TRUE))) {
      expect_identical(attr(x, "snafun_directed", exact = TRUE), directed)
    }
  } else {
    expect_identical(snafun::is_directed(x), directed)
  }
}


expect_vertex_state_unchanged <- function(before, after) {
  expect_equal(extract_vertex_state(after), extract_vertex_state(before))
}


backends <- c("igraph", "network", "edgelist")


# multiple attributes, defaults ------------------------------------------------
expected_directed_default <- data.frame(
  from = c(1, 2, 3),
  to = c(2, 1, 3),
  weight = c(18, 7, 30),
  label = c("a", "d", "loop1"),
  flag = c(FALSE, TRUE, FALSE),
  score = c(2, 7, 13)
)
expected_undirected_default <- data.frame(
  from = c(1, 2, 3),
  to = c(2, 3, 3),
  weight = c(18, 7, 30),
  label = c("a", "d", "loop1"),
  flag = c(FALSE, TRUE, FALSE),
  score = c(2, 7, 13)
)

for (one_backend in backends) {
  original <- make_backend_object(one_backend, make_multiedge_table(TRUE, "multiple"), directed = TRUE)
  simplified <- snafun::remove_multiple_edges(original)
  if (is.data.frame(simplified)) {
    expect_false(snafun::has_multiple_edges(snafun::to_igraph(simplified)))
  } else {
    expect_false(snafun::has_multiple_edges(simplified))
  }
  expect_equal(
    canonicalize_edge_table(simplified, directed = TRUE),
    expected_directed_default
  )
  expect_vertex_state_unchanged(original, simplified)
  expect_directed_state(simplified, TRUE)
  
  original_u <- make_backend_object(one_backend, make_multiedge_table(FALSE, "multiple"), directed = FALSE)
  simplified_u <- snafun::remove_multiple_edges(original_u)
  if (is.data.frame(simplified_u)) {
    expect_false(snafun::has_multiple_edges(snafun::to_igraph(simplified_u)))
  } else {
    expect_false(snafun::has_multiple_edges(simplified_u))
  }
  expect_equal(
    canonicalize_edge_table(simplified_u, directed = FALSE),
    expected_undirected_default
  )
  expect_vertex_state_unchanged(original_u, simplified_u)
  expect_directed_state(simplified_u, FALSE)
}


# explicit collapse rule for one attribute; others remain correct -------------
expected_directed_explicit <- data.frame(
  from = c(1, 2, 3),
  to = c(2, 1, 3),
  weight = c(2, 7, 13),
  label = c("a", "d", "loop1"),
  flag = c(FALSE, TRUE, FALSE),
  score = c(11, 7, 17)
)
expected_undirected_explicit <- data.frame(
  from = c(1, 2, 3),
  to = c(2, 3, 3),
  weight = c(2, 7, 13),
  label = c("a", "d", "loop1"),
  flag = c(FALSE, TRUE, FALSE),
  score = c(11, 7, 17)
)

for (one_backend in backends) {
  original <- make_backend_object(one_backend, make_multiedge_table(TRUE, "multiple"), directed = TRUE)
  simplified <- snafun::remove_multiple_edges(
    original,
    edge_attr_combine = list(score = "max"),
    default = "first"
  )
  expect_equal(
    canonicalize_edge_table(simplified, directed = TRUE),
    expected_directed_explicit
  )
  expect_vertex_state_unchanged(original, simplified)
  
  original_u <- make_backend_object(one_backend, make_multiedge_table(FALSE, "multiple"), directed = FALSE)
  simplified_u <- snafun::remove_multiple_edges(
    original_u,
    edge_attr_combine = list(score = "max"),
    default = "first"
  )
  expect_equal(
    canonicalize_edge_table(simplified_u, directed = FALSE),
    expected_undirected_explicit
  )
  expect_vertex_state_unchanged(original_u, simplified_u)
}


# single attribute -------------------------------------------------------------
expected_single_directed <- data.frame(
  from = c(1, 2, 3),
  to = c(2, 1, 3),
  weight = c(18, 7, 30)
)
expected_single_undirected <- data.frame(
  from = c(1, 2, 3),
  to = c(2, 3, 3),
  weight = c(18, 7, 30)
)

for (one_backend in backends) {
  original <- make_backend_object(one_backend, make_multiedge_table(TRUE, "single"), directed = TRUE)
  simplified <- snafun::remove_multiple_edges(original)
  expect_equal(
    canonicalize_edge_table(simplified, directed = TRUE),
    expected_single_directed
  )
  expect_vertex_state_unchanged(original, simplified)
  
  original_u <- make_backend_object(one_backend, make_multiedge_table(FALSE, "single"), directed = FALSE)
  simplified_u <- snafun::remove_multiple_edges(original_u)
  expect_equal(
    canonicalize_edge_table(simplified_u, directed = FALSE),
    expected_single_undirected
  )
  expect_vertex_state_unchanged(original_u, simplified_u)
}


# no edge attributes -----------------------------------------------------------
expected_none_directed <- data.frame(
  from = c(1, 2, 3),
  to = c(2, 1, 3)
)
expected_none_undirected <- data.frame(
  from = c(1, 2, 3),
  to = c(2, 3, 3)
)

for (one_backend in backends) {
  original <- make_backend_object(one_backend, make_multiedge_table(TRUE, "none"), directed = TRUE)
  simplified <- snafun::remove_multiple_edges(original)
  expect_equal(
    canonicalize_edge_table(simplified, directed = TRUE),
    expected_none_directed
  )
  expect_vertex_state_unchanged(original, simplified)
  
  original_u <- make_backend_object(one_backend, make_multiedge_table(FALSE, "none"), directed = FALSE)
  simplified_u <- snafun::remove_multiple_edges(original_u)
  expect_equal(
    canonicalize_edge_table(simplified_u, directed = FALSE),
    expected_none_undirected
  )
  expect_vertex_state_unchanged(original_u, simplified_u)
}


# all combine rules ------------------------------------------------------------
rule_specs <- list(
  list(attr = "label", values = c("a", "b", "c", "z"), rule = "first", expected = c("a", "z")),
  list(attr = "label", values = c("a", "b", "c", "z"), rule = "last", expected = c("c", "z")),
  list(attr = "score", values = c(2, 5, 11, 7), rule = "sum", expected = c(18, 7)),
  list(attr = "score", values = c(2, 5, 11, 7), rule = "mean", expected = c(6, 7)),
  list(attr = "score", values = c(2, 5, 11, 7), rule = "min", expected = c(2, 7)),
  list(attr = "score", values = c(2, 5, 11, 7), rule = "max", expected = c(11, 7)),
  list(attr = "flag", values = c(FALSE, TRUE, FALSE, TRUE), rule = "any", expected = c(TRUE, TRUE)),
  list(attr = "flag", values = c(FALSE, TRUE, FALSE, TRUE), rule = "all", expected = c(FALSE, TRUE)),
  list(attr = "label", values = c("a", "b", "c", "z"), rule = "concat_unique", expected = c("a | b | c", "z")),
  list(
    attr = "label",
    values = c("a", "b", "c", "z"),
    rule = function(values) paste(rev(values), collapse = "/"),
    expected = c("c/b/a", "z")
  )
)

for (one_backend in backends) {
  for (one_spec in rule_specs) {
    original <- make_backend_object(
      one_backend,
      make_rule_table(one_spec$attr, one_spec$values),
      directed = TRUE
    )
    simplified <- snafun::remove_multiple_edges(
      original,
      edge_attr_combine = list(),
      default = one_spec$rule
    )
    expected <- data.frame(
      from = c(1, 2),
      to = c(2, 3),
      stringsAsFactors = FALSE
    )
    expected[[one_spec$attr]] <- one_spec$expected
    expect_equal(canonicalize_edge_table(simplified, directed = TRUE), expected)
    expect_vertex_state_unchanged(original, simplified)
  }
}


# edgelist semantics with and without hidden directed metadata -----------------
stored_undirected <- make_backend_object(
  "edgelist",
  make_multiedge_table(FALSE, "single"),
  directed = FALSE,
  stored_metadata = TRUE
)
stored_undirected_simple <- snafun::remove_multiple_edges(stored_undirected)
expect_equal(
  canonicalize_edge_table(stored_undirected_simple, directed = FALSE),
  expected_single_undirected
)
expect_vertex_state_unchanged(stored_undirected, stored_undirected_simple)
expect_identical(attr(stored_undirected_simple, "snafun_directed", exact = TRUE), FALSE)

external_edgelist <- make_backend_object(
  "edgelist",
  make_multiedge_table(FALSE, "single"),
  directed = FALSE,
  stored_metadata = FALSE
)
external_simple <- snafun::remove_multiple_edges(external_edgelist)
expect_equal(
  canonicalize_edge_table(external_simple, directed = TRUE),
  data.frame(
    from = c(1, 2, 2, 3),
    to = c(2, 1, 3, 3),
    weight = c(13, 5, 7, 30)
  )
)
expect_true(is.null(attr(external_simple, "snafun_vertices", exact = TRUE)))


# unchanged when there are no multiple edges ----------------------------------
for (one_backend in backends) {
  original <- make_backend_object(
    one_backend,
    data.frame(from = c(1, 2), to = c(2, 3), weight = c(4, 6)),
    directed = TRUE
  )
  simplified <- snafun::remove_multiple_edges(original)
  expect_identical(simplified, original)
}


expect_error(
  snafun::remove_multiple_edges(matrix(c(0, 2, 0, 0), nrow = 2)),
  "'x' should be of class"
)

for (one_backend in backends) {
  expect_error(
    snafun::remove_multiple_edges(
      make_backend_object(one_backend, make_multiedge_table(TRUE, "single"), directed = TRUE),
      edge_attr_combine = list(weight = "median")
    ),
    "Unknown edge attribute combination rule"
  )
}


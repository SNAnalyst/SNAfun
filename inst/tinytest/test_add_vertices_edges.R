report_side_effects()


# igraph -----------------------------------------------------------------------
g_i <- snafun::create_manual_graph(A -- B -- C)
g_i_added <- snafun::add_vertices(
  g_i,
  c("D", "E"),
  attributes = data.frame(group = c("g1", "g2"), score = c(10, 20))
)

expect_equal(snafun::count_vertices(g_i_added), 5L)
expect_equal(tail(snafun::extract_vertex_names(g_i_added), 2), c("D", "E"))
expect_equal(tail(snafun::extract_vertex_attribute(g_i_added, "group"), 2), c("g1", "g2"))
expect_equal(tail(snafun::extract_vertex_attribute(g_i_added, "score"), 2), c(10, 20))

g_i_added <- snafun::add_edges(
  g_i_added,
  data.frame(
    from = c("A", "C"),
    to = c("D", "E"),
    weight = c(2, 3),
    relation = c("new1", "new2")
  )
)

expect_equal(snafun::count_edges(g_i_added), 4L)
expect_equal(tail(snafun::extract_edge_attribute(g_i_added, "weight"), 2), c(2, 3))
expect_equal(tail(snafun::extract_edge_attribute(g_i_added, "relation"), 2), c("new1", "new2"))


# network ----------------------------------------------------------------------
g_n <- snafun::to_network(snafun::create_manual_graph(A -- B -- C))
g_n_added <- snafun::add_vertices(
  g_n,
  c("D", "E"),
  attributes = data.frame(group = c("g1", "g2"))
)

expect_equal(snafun::count_vertices(g_n_added), 5L)
expect_equal(tail(snafun::extract_vertex_names(g_n_added), 2), c("D", "E"))
expect_equal(tail(snafun::extract_vertex_attribute(g_n_added, "group"), 2), c("g1", "g2"))

g_n_added <- snafun::add_edges(
  g_n_added,
  data.frame(
    from = c("A", "C"),
    to = c("D", "E"),
    weight = c(4, 5),
    relation = c("new1", "new2")
  )
)

expect_equal(snafun::count_edges(g_n_added), 4L)
expect_equal(tail(snafun::extract_edge_attribute(g_n_added, "weight"), 2), c(4, 5))
expect_equal(tail(snafun::extract_edge_attribute(g_n_added, "relation"), 2), c("new1", "new2"))


# matrix, one-mode -------------------------------------------------------------
mat <- matrix(0, nrow = 2, ncol = 2)
rownames(mat) <- colnames(mat) <- c("A", "B")

mat_added <- snafun::add_vertices(mat, "C")
expect_equal(dim(mat_added), c(3, 3))
expect_equal(rownames(mat_added), c("A", "B", "C"))
expect_equal(colnames(mat_added), c("A", "B", "C"))

mat_added <- snafun::add_edges(
  mat_added,
  data.frame(from = "A", to = "C", weight = 7)
)
expect_equal(mat_added["A", "C"], 7)
expect_equal(mat_added["C", "A"], 7)

expect_error(
  snafun::add_edges(
    mat_added,
    data.frame(from = "A", to = "B", weight = 1, label = "x")
  ),
  "at most one edge value"
)
expect_error(
  snafun::add_vertices(
    mat_added,
    "D",
    attributes = data.frame(group = "x")
  ),
  "Matrices do not store arbitrary vertex attributes"
)


# matrix, bipartite ------------------------------------------------------------
mat_b <- matrix(c(1, 0), nrow = 1, byrow = TRUE)
rownames(mat_b) <- "p1"
colnames(mat_b) <- c("e1", "e2")

mat_b_added <- snafun::add_vertices(
  mat_b,
  c("p2", "e3"),
  type = c(FALSE, TRUE)
)

expect_equal(dim(mat_b_added), c(2, 3))
expect_equal(rownames(mat_b_added), c("p1", "p2"))
expect_equal(colnames(mat_b_added), c("e1", "e2", "e3"))

mat_b_added <- snafun::add_edges(
  mat_b_added,
  data.frame(from = "p2", to = "e3")
)
expect_equal(mat_b_added["p2", "e3"], 1)

expect_error(
  snafun::add_vertices(mat_b, "p2"),
  "Please specify 'type'"
)


# edgelist ---------------------------------------------------------------------
el <- data.frame(from = "A", to = "B", stringsAsFactors = FALSE)
el_added <- snafun::add_vertices(
  el,
  c("C", "D"),
  attributes = data.frame(group = c("g1", "g2"))
)

ig_from_el <- snafun::to_igraph(el_added)
expect_equal(snafun::count_vertices(ig_from_el), 4L)
expect_equal(snafun::extract_isolates(ig_from_el), c("C", "D"))
expect_equal(tail(snafun::extract_vertex_attribute(ig_from_el, "group"), 2), c("g1", "g2"))

el_added <- snafun::add_edges(
  el_added,
  data.frame(from = c("B", "C"), to = c("C", "D"), weight = c(4, 5))
)

ig_from_el2 <- snafun::to_igraph(el_added)
expect_equal(snafun::count_edges(ig_from_el2), 3L)
expect_equal(snafun::count_vertices(ig_from_el2), 4L)
expect_equal(tail(snafun::extract_vertex_attribute(ig_from_el2, "group"), 2), c("g1", "g2"))
expect_equal(el_added$weight, c(NA, 4, 5))


# edgelist with preserved metadata ---------------------------------------------
el_roundtrip <- snafun::to_edgelist(snafun::create_manual_graph(A -- B))
el_roundtrip <- snafun::add_vertices(el_roundtrip, "C")
el_roundtrip <- snafun::add_edges(el_roundtrip, data.frame(from = "B", to = "C"))
ig_roundtrip <- snafun::to_igraph(el_roundtrip)

expect_equal(snafun::count_vertices(ig_roundtrip), 3L)
expect_equal(snafun::count_edges(ig_roundtrip), 2L)
expect_equal(snafun::extract_vertex_names(ig_roundtrip), c("A", "B", "C"))


# error handling ----------------------------------------------------------------
expect_error(
  snafun::add_vertices(g_i, 0),
  "single positive integer"
)
expect_error(
  snafun::add_edges(g_i, c("A", "B", "C")),
  "even number of values"
)
expect_error(
  snafun::add_edges(g_i, data.frame(from = "A", to = "Z")),
  "does not exist"
)

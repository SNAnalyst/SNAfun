report_side_effects()


canonicalize_projection_edgelist <- function(x) {
  if (!is.data.frame(x)) {
    x <- snafun::to_edgelist(x)
  }
  x <- data.frame(x, check.names = FALSE, stringsAsFactors = FALSE)
  x <- x[order(as.character(x[[1]]), as.character(x[[2]])), , drop = FALSE]
  rownames(x) <- NULL
  x
}


base_incidence <- matrix(
  c(
    1, 1,
    1, 1,
    0, 1
  ),
  nrow = 3,
  byrow = TRUE
)
rownames(base_incidence) <- c("Alice", "Bob", "Cecil")
colnames(base_incidence) <- c("Project1", "Project2")

g_bip <- igraph::graph_from_biadjacency_matrix(base_incidence)
igraph::vertex_attr(g_bip, "name") <- c(rownames(base_incidence), colnames(base_incidence))
igraph::V(g_bip)$role <- c("student", "student", "staff", "seminar", "workshop")
igraph::V(g_bip)$score <- c(10, 20, 30, 100, 200)

g_bip_network <- snafun::to_network(g_bip)
g_bip_edgelist <- snafun::to_edgelist(g_bip)
g_bip_external_edgelist <- data.frame(
  from = c("Alice", "Alice", "Bob", "Bob", "Cecil"),
  to = c("Project1", "Project2", "Project1", "Project2", "Project2"),
  stringsAsFactors = FALSE
)

expected_row_weighted <- matrix(
  c(
    0, 2, 1,
    2, 0, 1,
    1, 1, 0
  ),
  nrow = 3,
  byrow = TRUE,
  dimnames = list(c("Alice", "Bob", "Cecil"), c("Alice", "Bob", "Cecil"))
)
expected_column_weighted <- matrix(
  c(
    0, 2,
    2, 0
  ),
  nrow = 2,
  byrow = TRUE,
  dimnames = list(c("Project1", "Project2"), c("Project1", "Project2"))
)
expected_row_binary <- expected_row_weighted
expected_row_binary[expected_row_binary > 0] <- 1
expected_column_binary <- expected_column_weighted
expected_column_binary[expected_column_binary > 0] <- 1


# igraph ----------------------------------------------------------------------
proj_both_i <- snafun::make_projection(g_bip)
expect_true(is.list(proj_both_i))
expect_identical(names(proj_both_i), c("row", "column"))
expect_true(inherits(proj_both_i$row, "igraph"))
expect_true(inherits(proj_both_i$column, "igraph"))
expect_equal(snafun::to_matrix(proj_both_i$row), expected_row_weighted)
expect_equal(snafun::to_matrix(proj_both_i$column), expected_column_weighted)
expect_equal(igraph::vertex_attr(proj_both_i$row, "role"), c("student", "student", "staff"))
expect_equal(igraph::vertex_attr(proj_both_i$column, "role"), c("seminar", "workshop"))
expect_false("type" %in% igraph::vertex_attr_names(proj_both_i$row))

for (one_alias in c("row", "one", "false")) {
  proj_row_i <- snafun::make_projection(g_bip, which = one_alias)
  expect_equal(snafun::to_matrix(proj_row_i), expected_row_weighted)
}

for (one_alias in c("column", "two", "true")) {
  proj_column_i <- snafun::make_projection(g_bip, which = one_alias)
  expect_equal(snafun::to_matrix(proj_column_i), expected_column_weighted)
}

proj_row_i_binary <- snafun::make_projection(g_bip, which = "row", multiplicity = FALSE)
expect_equal(snafun::to_matrix(proj_row_i_binary), expected_row_binary)
expect_false("weight" %in% igraph::edge_attr_names(proj_row_i_binary))

proj_row_i_keep_type <- snafun::make_projection(g_bip, which = "row", remove.type = FALSE)
expect_true("type" %in% igraph::vertex_attr_names(proj_row_i_keep_type))
expect_true(all(!igraph::vertex_attr(proj_row_i_keep_type, "type")))


# network ---------------------------------------------------------------------
proj_both_n <- snafun::make_projection(g_bip_network)
expect_true(is.list(proj_both_n))
expect_identical(names(proj_both_n), c("row", "column"))
expect_true(inherits(proj_both_n$row, "network"))
expect_true(inherits(proj_both_n$column, "network"))
expect_false(snafun::is_directed(proj_both_n$row))
expect_equal(snafun::to_matrix(proj_both_n$row), expected_row_weighted)
expect_equal(snafun::to_matrix(proj_both_n$column), expected_column_weighted)
expect_equal(network::get.vertex.attribute(proj_both_n$row, "role"), c("student", "student", "staff"))
expect_false(snafun::is_bipartite(proj_both_n$row))

proj_row_n_keep_type <- snafun::make_projection(g_bip_network, which = "row", remove.type = FALSE)
expect_equal(network::get.vertex.attribute(proj_row_n_keep_type, "type"), c(FALSE, FALSE, FALSE))
expect_false(snafun::is_bipartite(proj_row_n_keep_type))

proj_column_n_binary <- snafun::make_projection(g_bip_network, which = "column", multiplicity = FALSE)
expect_equal(snafun::to_matrix(proj_column_n_binary), expected_column_binary)
expect_false("weight" %in% network::list.edge.attributes(proj_column_n_binary))


# matrix ----------------------------------------------------------------------
proj_both_m <- snafun::make_projection(base_incidence)
expect_true(is.list(proj_both_m))
expect_identical(names(proj_both_m), c("row", "column"))
expect_true(is.matrix(proj_both_m$row))
expect_true(is.matrix(proj_both_m$column))
expect_equal(proj_both_m$row, expected_row_weighted)
expect_equal(proj_both_m$column, expected_column_weighted)

proj_row_m_binary <- snafun::make_projection(base_incidence, which = "row", multiplicity = FALSE)
expect_equal(proj_row_m_binary, expected_row_binary)


# edgelist with stored metadata -----------------------------------------------
proj_both_e <- snafun::make_projection(g_bip_edgelist)
expect_true(is.list(proj_both_e))
expect_identical(names(proj_both_e), c("row", "column"))
expect_true(is.data.frame(proj_both_e$row))
expect_true(is.data.frame(proj_both_e$column))
expect_equal(
  canonicalize_projection_edgelist(proj_both_e$row),
  canonicalize_projection_edgelist(snafun::to_edgelist(proj_both_i$row))
)
expect_equal(
  canonicalize_projection_edgelist(proj_both_e$column),
  canonicalize_projection_edgelist(snafun::to_edgelist(proj_both_i$column))
)
expect_equal(
  attr(proj_both_e$row, "snafun_vertices", exact = TRUE)$role,
  c("student", "student", "staff")
)
expect_false(isTRUE(attr(proj_both_e$row, "snafun_bipartite", exact = TRUE)))

proj_row_e_keep_type <- snafun::make_projection(g_bip_edgelist, which = "row", remove.type = FALSE)
expect_equal(
  attr(proj_row_e_keep_type, "snafun_vertices", exact = TRUE)$type,
  c(FALSE, FALSE, FALSE)
)
expect_false(isTRUE(attr(proj_row_e_keep_type, "snafun_bipartite", exact = TRUE)))

proj_column_e_binary <- snafun::make_projection(g_bip_edgelist, which = "column", multiplicity = FALSE)
expect_equal(
  canonicalize_projection_edgelist(proj_column_e_binary),
  data.frame(
    from = "Project1",
    to = "Project2",
    stringsAsFactors = FALSE
  )
)


# external edgelist without hidden metadata -----------------------------------
proj_row_external <- snafun::make_projection(g_bip_external_edgelist, which = "row")
expect_equal(
  canonicalize_projection_edgelist(proj_row_external),
  data.frame(
    from = c("Alice", "Alice", "Bob"),
    to = c("Bob", "Cecil", "Cecil"),
    weight = c(2, 1, 1),
    stringsAsFactors = FALSE
  )
)


# directed bipartite input should give the same projection --------------------
g_bip_directed <- igraph::graph_from_data_frame(
  d = g_bip_external_edgelist,
  directed = TRUE,
  vertices = data.frame(
    name = c("Alice", "Bob", "Cecil", "Project1", "Project2"),
    type = c(FALSE, FALSE, FALSE, TRUE, TRUE),
    stringsAsFactors = FALSE
  )
)
expect_equal(
  snafun::to_matrix(snafun::make_projection(g_bip_directed, which = "row")),
  expected_row_weighted
)


# errors ----------------------------------------------------------------------
g_non_bip <- igraph::make_ring(4)
expect_error(snafun::make_projection(g_non_bip), "bipartite")

n_non_bip <- snafun::to_network(g_non_bip)
expect_error(snafun::make_projection(n_non_bip), "bipartite")

expect_error(
  snafun::make_projection(matrix(c(0, 1, 1, 0), nrow = 2)),
  "rectangular bipartite incidence matrix"
)

expect_error(
  snafun::make_projection(data.frame(from = c("A", "B"), to = c("B", "A"))),
  "bipartite"
)

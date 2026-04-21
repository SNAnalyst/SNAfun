report_side_effects()


# igraph ----------------------------------------------------------------------
g_i <- igraph::make_empty_graph(n = 4, directed = TRUE)
g_i <- igraph::add_edges(g_i, c(1, 2, 1, 2, 2, 3, 4, 4, 4, 4))

expect_true(snafun::has_multiple_edges(g_i))
expect_identical(
  snafun::extract_multiple_edges(g_i),
  which(igraph::which_multiple(g_i))
)
expect_identical(snafun::extract_multiple_edges(g_i), c(2L, 5L))

g_i_simple <- igraph::delete_edges(g_i, snafun::extract_multiple_edges(g_i))
expect_false(snafun::has_multiple_edges(g_i_simple))
expect_identical(snafun::extract_multiple_edges(g_i_simple), NULL)


# undirected igraph -----------------------------------------------------------
g_i_u <- igraph::make_empty_graph(n = 3, directed = FALSE)
g_i_u <- igraph::add_edges(g_i_u, c(1, 2, 2, 1, 2, 3))

expect_true(snafun::has_multiple_edges(g_i_u))
expect_identical(
  snafun::extract_multiple_edges(g_i_u),
  which(igraph::which_multiple(g_i_u))
)
expect_identical(snafun::extract_multiple_edges(g_i_u), 2L)


# network ---------------------------------------------------------------------
g_n <- network::network.initialize(4, directed = TRUE, loops = TRUE, multiple = TRUE)
g_n <- network::add.edges(g_n, tail = c(1, 1, 2, 4, 4), head = c(2, 2, 3, 4, 4))

expect_true(snafun::has_multiple_edges(g_n))
expect_identical(snafun::extract_multiple_edges(g_n), c(2L, 5L))

g_n_u <- network::network.initialize(3, directed = FALSE, loops = FALSE, multiple = TRUE)
g_n_u <- network::add.edges(g_n_u, tail = c(1, 2, 2), head = c(2, 1, 3))

expect_true(snafun::has_multiple_edges(g_n_u))
expect_identical(snafun::extract_multiple_edges(g_n_u), 2L)

g_n_simple <- network::network.initialize(3, directed = TRUE, loops = TRUE, multiple = TRUE)
g_n_simple <- network::add.edges(g_n_simple, tail = c(1, 2), head = c(2, 3))

expect_false(snafun::has_multiple_edges(g_n_simple))
expect_identical(snafun::extract_multiple_edges(g_n_simple), NULL)


# matrix ----------------------------------------------------------------------
mat_simple <- matrix(
  c(0, 1, 0,
    0, 0, 1,
    0, 0, 0),
  nrow = 3,
  byrow = TRUE
)
mat_multi <- mat_simple
mat_multi[1, 2] <- 2

expect_false(snafun::has_multiple_edges(mat_simple))
expect_true(snafun::has_multiple_edges(mat_multi))
expect_error(
  snafun::extract_multiple_edges(mat_multi),
  "'x' should be of class `igraph`, `network`"
)

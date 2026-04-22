report_side_effects()


redist_equivalence <- function(x, mode = "digraph", diag = FALSE,
                               seed.partition = NULL, digits = 6) {
  round(
    1 - sna::redist(
      dat = x,
      mode = mode,
      diag = diag,
      seed.partition = seed.partition,
      code.diss = TRUE
    ),
    digits = digits
  )
}


expect_regular_equivalence_parity <- function(mat, mode, diag = FALSE,
                                              seed.partition = NULL) {
  reference <- redist_equivalence(
    x = mat,
    mode = mode,
    diag = diag,
    seed.partition = seed.partition,
    digits = 6
  )
  observed <- snafun::d_regular_equivalence(
    x = mat,
    mode = mode,
    diag = diag,
    seed.partition = seed.partition,
    digits = 6
  )
  expect_equal(unname(observed), unname(reference), tolerance = 1e-12)
}


directed_mat <- matrix(
  c(
    0, 1, 0, 1, 0,
    0, 0, 1, 0, 1,
    1, 0, 0, 0, 0,
    0, 1, 0, 0, 1,
    1, 0, 0, 1, 0
  ),
  nrow = 5,
  byrow = TRUE,
  dimnames = list(LETTERS[1:5], LETTERS[1:5])
)

undirected_mat <- matrix(
  c(
    0, 1, 1, 0, 0, 0,
    1, 0, 0, 1, 0, 0,
    1, 0, 0, 1, 1, 0,
    0, 1, 1, 0, 0, 1,
    0, 0, 1, 0, 0, 1,
    0, 0, 0, 1, 1, 0
  ),
  nrow = 6,
  byrow = TRUE,
  dimnames = list(letters[1:6], letters[1:6])
)

seed_partition <- c(1, 1, 2, 2, 3)


# Unweighted binary parity with sna::redist()
expect_regular_equivalence_parity(
  mat = directed_mat,
  mode = "digraph",
  diag = FALSE
)
expect_regular_equivalence_parity(
  mat = undirected_mat,
  mode = "graph",
  diag = FALSE
)
expect_regular_equivalence_parity(
  mat = directed_mat,
  mode = "digraph",
  diag = FALSE,
  seed.partition = seed_partition
)


# Output should be bounded and have self-equivalence 1.
default_result <- snafun::d_regular_equivalence(directed_mat)
expect_true(all(default_result >= 0))
expect_true(all(default_result <= 1))
expect_equal(unname(diag(default_result)), rep(1, nrow(directed_mat)))
expect_equal(rownames(default_result), rownames(directed_mat))
expect_equal(colnames(default_result), colnames(directed_mat))


# Input-format parity for binary networks.
directed_igraph <- snafun::to_igraph(directed_mat)
directed_network <- snafun::to_network(directed_mat)
directed_edgelist <- snafun::to_edgelist(directed_mat)

matrix_result <- snafun::d_regular_equivalence(directed_mat, digits = 6)
igraph_result <- snafun::d_regular_equivalence(directed_igraph, digits = 6)
network_result <- snafun::d_regular_equivalence(directed_network, digits = 6)
edgelist_result <- snafun::d_regular_equivalence(directed_edgelist, digits = 6)

expect_equal(igraph_result, matrix_result, tolerance = 1e-12)
expect_equal(network_result, matrix_result, tolerance = 1e-12)
expect_equal(edgelist_result, matrix_result, tolerance = 1e-12)


# Mode override should symmetrize directed matrices for graph-style analysis.
graph_override <- snafun::d_regular_equivalence(
  directed_mat,
  mode = "graph",
  digits = 6
)
graph_reference <- snafun::d_regular_equivalence(
  snafun::to_symmetric_matrix(directed_mat, rule = "weak"),
  mode = "graph",
  digits = 6
)
expect_equal(graph_override, graph_reference, tolerance = 1e-12)


# Weights should be optional and should affect the output when they carry extra information.
weighted_mat <- matrix(
  c(
    0, 2, 0, 1,
    0, 0, 3, 0,
    4, 0, 0, 0,
    0, 5, 0, 0
  ),
  nrow = 4,
  byrow = TRUE,
  dimnames = list(c("v1", "v2", "v3", "v4"), c("v1", "v2", "v3", "v4"))
)
weighted_igraph <- snafun::to_igraph(weighted_mat)
weighted_edgelist <- data.frame(
  from = c("v1", "v1", "v2", "v3", "v4"),
  to = c("v2", "v4", "v3", "v1", "v2"),
  weight = c(2, 1, 3, 4, 5),
  strength = c(20, 10, 30, 40, 50),
  stringsAsFactors = FALSE
)

weighted_matrix_result <- snafun::d_regular_equivalence(weighted_mat, digits = 6)
weighted_graph_result <- snafun::d_regular_equivalence(
  weighted_igraph,
  weights = "weight",
  digits = 6
)
weighted_edgelist_result <- snafun::d_regular_equivalence(
  weighted_edgelist,
  weights = "strength",
  digits = 6
)
unweighted_graph_result <- snafun::d_regular_equivalence(
  weighted_igraph,
  weights = NA,
  digits = 6
)

expect_equal(weighted_graph_result, weighted_matrix_result, tolerance = 1e-12)
expect_false(isTRUE(all.equal(unweighted_graph_result, weighted_graph_result, tolerance = 1e-12)))
expect_true(all(weighted_edgelist_result >= 0))
expect_true(all(weighted_edgelist_result <= 1))
expect_true(any(
  weighted_graph_result[upper.tri(weighted_graph_result)] > 0 &
    weighted_graph_result[upper.tri(weighted_graph_result)] < 1
))


# NULL should pick up the standard weight attribute when it exists.
auto_weight_result <- snafun::d_regular_equivalence(
  weighted_igraph,
  weights = NULL,
  digits = 6
)
expect_equal(auto_weight_result, weighted_graph_result, tolerance = 1e-12)


# Digits should round the output.
rounded_result <- snafun::d_regular_equivalence(directed_mat, digits = 2)
expect_equal(rounded_result, round(matrix_result, 2))


# Example syntax from the help file should stay valid.
g_example <- snafun::create_manual_graph(A -+ B, B -+ C, D -+ E, E -+ F)
example_result <- snafun::d_regular_equivalence(g_example)
expect_true(is.matrix(example_result))
expect_equal(nrow(example_result), 6)


# Error handling
expect_error(
  snafun::d_regular_equivalence(matrix(1, nrow = 2, ncol = 3)),
  "square adjacency matrix"
)
expect_error(
  snafun::d_regular_equivalence(directed_igraph, weights = "missing"),
  "does not occur"
)
expect_error(
  snafun::d_regular_equivalence(weighted_edgelist, weights = "missing"),
  "does not occur"
)
expect_error(
  snafun::d_regular_equivalence(directed_mat, seed.partition = c(1, 2)),
  "one value per vertex"
)

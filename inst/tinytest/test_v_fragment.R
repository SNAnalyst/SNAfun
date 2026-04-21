W <- matrix(
  c(0, 1, 3, 0, 0,
    0, 0, 0, 4, 0,
    1, 1, 0, 2, 0,
    0, 0, 0, 0, 3,
    0, 2, 0, 0, 0),
  nrow = 5,
  byrow = TRUE,
  dimnames = list(letters[1:5], letters[1:5])
)

A <- W
A[W != 0] <- 1 / W[W != 0]

# These reference values were verified against keyplayer::fragment().
expected_default <- c(
  a = 0.6365079,
  b = 0.7446429,
  c = 0.67335,
  d = 0.8333333,
  e = 0.725
)
expected_m1 <- c(
  a = 0.6365079,
  b = 0.7625,
  c = 0.7031746,
  d = 0.8333333,
  e = 0.725
)
expected_binary <- c(
  a = 0.4166667,
  b = 0.5555556,
  c = 0.4722222,
  d = 0.5833333,
  e = 0.4583333
)

expect_equal(snafun::v_fragment(A), expected_default, tolerance = 1e-7)
expect_equal(snafun::v_fragment(A, M = 1), expected_m1, tolerance = 1e-7)
expect_equal(snafun::v_fragment(A, binary = TRUE), expected_binary, tolerance = 1e-7)
expect_equal(snafun::v_fragment(A, large = FALSE), expected_default, tolerance = 1e-7)
expect_equal(snafun::v_fragment(A, vids = c(1, 3)), 0.4480159, tolerance = 1e-7)
expect_equal(snafun::v_fragment(A, vids = c("a", "c")), 0.4480159, tolerance = 1e-7)
expect_equal(snafun::v_fragment(A, vids = c(1, 3), binary = TRUE), 0.25, tolerance = 1e-7)
expect_equal(sum(snafun::v_fragment(A, rescaled = TRUE)), 1, tolerance = 1e-10)


geodist_precomp <- sna::geodist(A, ignore.eval = FALSE)$gdist
expect_equal(
  snafun::v_fragment(A, geodist.precomp = geodist_precomp),
  expected_default,
  tolerance = 1e-7
)


g_i <- snafun::to_igraph(A)
g_n <- snafun::to_network(A)
edge_index <- which(A > 0, arr.ind = TRUE)
edgelist <- data.frame(
  from = rownames(A)[edge_index[, 1]],
  to = colnames(A)[edge_index[, 2]],
  weight = A[edge_index]
)

expect_equal(snafun::v_fragment(g_i), expected_default, tolerance = 1e-7)
expect_equal(snafun::v_fragment(g_n), expected_default, tolerance = 1e-7)
expect_equal(snafun::v_fragment(edgelist), expected_default, tolerance = 1e-7)


bipartite_edges <- data.frame(
  from = c("a", "b"),
  to = c("x", "y"),
  weight = c(1, 2)
)

expect_error(
  snafun::v_fragment(matrix(1, nrow = 2, ncol = 3)),
  "square adjacency or distance matrix"
)
expect_error(
  snafun::v_fragment(bipartite_edges),
  "only defined for one-mode networks"
)
expect_error(
  snafun::v_fragment(A, vids = "z"),
  "not present in the graph"
)
expect_error(
  snafun::v_fragment(A, M = 0),
  "positive numeric value"
)


if (requireNamespace("keyplayer", quietly = TRUE)) {
  expect_equal(
    unname(snafun::v_fragment(A)),
    unname(as.vector(keyplayer::fragment(A))),
    tolerance = 1e-7
  )
  expect_equal(
    unname(snafun::v_fragment(A, M = 1)),
    unname(as.vector(keyplayer::fragment(A, M = 1))),
    tolerance = 1e-7
  )
  expect_equal(
    unname(snafun::v_fragment(A, binary = TRUE)),
    unname(as.vector(keyplayer::fragment(A, binary = TRUE))),
    tolerance = 1e-7
  )
  expect_equal(
    snafun::v_fragment(A, vids = c(1, 3)),
    keyplayer::fragment(A, nodes = c(1, 3)),
    tolerance = 1e-7
  )
}

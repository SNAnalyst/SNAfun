P <- matrix(
  c(0, 0.2, 0.6, 0,   0,
    0, 0,   0,   0.8, 0,
    0.2, 0.2, 0, 0.4, 0,
    0, 0,   0,   0,   0.6,
    0, 0.4, 0,   0,   0),
  nrow = 5,
  byrow = TRUE,
  dimnames = list(letters[1:5], letters[1:5])
)

# These reference values were verified against keyplayer::diffusion().
expected_t2 <- c(a = 1.44, b = 1.28, c = 1.36, d = 0.84, e = 0.72)
expected_t5 <- c(a = 2.25408, b = 1.71776, c = 2.00448, d = 1.19328, e = 1.05024)

expect_equal(snafun::v_diffusion(P, T = 2), expected_t2)
expect_equal(snafun::v_diffusion(P), expected_t5)
expect_equal(unname(snafun::v_diffusion(P, vids = c(1, 3), T = 2)), c(1.44, 1.36))
expect_equal(unname(snafun::v_diffusion(P, vids = c("a", "c"), T = 2)), c(1.44, 1.36))
expect_equal(sum(snafun::v_diffusion(P, T = 2, rescaled = TRUE)), 1)


g_i <- snafun::to_igraph(P)
g_n <- snafun::to_network(P)
edge_index <- which(P > 0, arr.ind = TRUE)
edgelist <- data.frame(
  from = rownames(P)[edge_index[, 1]],
  to = colnames(P)[edge_index[, 2]],
  weight = P[edge_index]
)

expect_equal(snafun::v_diffusion(g_i, T = 2), expected_t2)
expect_equal(snafun::v_diffusion(g_n, T = 2), expected_t2)
expect_equal(snafun::v_diffusion(edgelist, T = 2), expected_t2)


bipartite_edges <- data.frame(
  from = c("a", "b"),
  to = c("x", "y"),
  weight = c(0.5, 0.5)
)

expect_error(
  snafun::v_diffusion(matrix(1, nrow = 2, ncol = 3), T = 1),
  "square adjacency or probability matrix"
)
expect_error(
  snafun::v_diffusion(bipartite_edges, T = 1),
  "only defined for one-mode networks"
)
expect_error(
  snafun::v_diffusion(P, T = 0),
  "positive integer"
)
expect_error(
  snafun::v_diffusion(P, vids = "z", T = 2),
  "not present in the graph"
)


if (requireNamespace("keyplayer", quietly = TRUE)) {
  expect_equal(
    unname(snafun::v_diffusion(P, T = 2)),
    unname(as.vector(keyplayer::diffusion(P, T = 2)))
  )
  expect_equal(
    unname(snafun::v_diffusion(P, T = 5)),
    unname(as.vector(keyplayer::diffusion(P, T = 5)))
  )
  expect_equal(
    unname(snafun::v_diffusion(P, vids = c(1, 3), T = 2)),
    unname(keyplayer::diffusion(P, node = c(1, 3), T = 2))
  )
}

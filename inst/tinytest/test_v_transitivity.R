report_side_effects()


expected_local_transitivity <- c(A = 1, B = 1, C = 1 / 3, D = NaN, E = NaN)

base_matrix <- matrix(
  0,
  nrow = 5,
  ncol = 5,
  dimnames = list(LETTERS[1:5], LETTERS[1:5])
)
base_matrix["A", "B"] <- 1
base_matrix["B", "A"] <- 1
base_matrix["A", "C"] <- 1
base_matrix["C", "A"] <- 1
base_matrix["B", "C"] <- 1
base_matrix["C", "B"] <- 1
base_matrix["C", "D"] <- 1
base_matrix["D", "C"] <- 1

weighted_loopy <- base_matrix
weighted_loopy["A", "A"] <- 9
weighted_loopy["B", "C"] <- 4
weighted_loopy["C", "B"] <- 4

directed_version <- matrix(0, nrow = 5, ncol = 5, dimnames = dimnames(base_matrix))
directed_version["A", "B"] <- 1
directed_version["C", "A"] <- 1
directed_version["B", "C"] <- 1
directed_version["C", "D"] <- 1

graphs <- list(
  igraph = snafun::to_igraph(weighted_loopy),
  network = snafun::to_network(weighted_loopy),
  matrix = weighted_loopy,
  edgelist = snafun::to_edgelist(snafun::to_igraph(weighted_loopy))
)

for (graph in graphs) {
  actual <- snafun::v_transitivity(graph)
  expect_equal(unname(actual[1:3]), unname(expected_local_transitivity[1:3]), tolerance = 1e-12)
  expect_true(all(is.nan(actual[4:5])))
  
  actual_zero <- snafun::v_transitivity(graph, isolates = "zero")
  expect_equal(unname(actual_zero), c(1, 1, 1 / 3, 0, 0), tolerance = 1e-12)
}


# Character vids should work when names are present.
expect_equal(
  snafun::v_transitivity(graphs$igraph, vids = c("A", "C")),
  c(A = 1, C = 1 / 3),
  tolerance = 1e-12
)


# Directed graphs are weakly symmetrized before the local coefficient is computed.
expected_directed <- snafun::v_transitivity(base_matrix, isolates = "zero")
for (graph in list(
  snafun::to_igraph(directed_version),
  snafun::to_network(directed_version),
  directed_version,
  snafun::to_edgelist(snafun::to_igraph(directed_version))
)) {
  expect_equal(
    unname(snafun::v_transitivity(graph, isolates = "zero")),
    unname(expected_directed),
    tolerance = 1e-12
  )
}


# Bipartite input should be rejected.
bipartite_matrix <- matrix(c(1, 0, 1, 0, 1, 0), nrow = 2, byrow = TRUE)
expect_error(
  snafun::v_transitivity(bipartite_matrix),
  "one-mode networks"
)


# Small random regression sweep across the supported formats.
set.seed(20260421)
for (directed in c(FALSE, TRUE)) {
  for (allow_loops in c(FALSE, TRUE)) {
    for (weighted in c(FALSE, TRUE)) {
      for (sim in seq_len(25)) {
        n <- sample(4:8, size = 1)
        mat <- matrix(stats::rbinom(n * n, size = 1, prob = stats::runif(1, 0.1, 0.5)), nrow = n)
        if (!directed) {
          mat[lower.tri(mat)] <- t(mat)[lower.tri(mat)]
        }
        if (!allow_loops) {
          diag(mat) <- 0
        }
        if (weighted) {
          nonzero <- which(mat != 0)
          if (length(nonzero) > 0) {
            mat[nonzero] <- sample(2:7, size = length(nonzero), replace = TRUE)
          }
        }
        dimnames(mat) <- list(paste0("v", seq_len(n)), paste0("v", seq_len(n)))
        
        reference <- snafun::v_transitivity(mat, isolates = "zero")
        graphs <- list(
          snafun::to_igraph(mat),
          snafun::to_network(mat),
          mat,
          snafun::to_edgelist(snafun::to_igraph(mat))
        )
        
        for (graph in graphs) {
          expect_equal(
            unname(snafun::v_transitivity(graph, isolates = "zero")),
            unname(reference),
            tolerance = 1e-12
          )
        }
      }
    }
  }
}

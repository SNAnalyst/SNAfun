# Tests for the fast internal predicates used by to_igraph.matrix()
# (performance optimisation, 2026-09-16). They must give EXACTLY the same result
# as the base expressions they replace:
#   matrix_is_binary(x)          == all(x %in% c(0, 1))
#   matrix_is_symmetric_fast(x)  == isSymmetric(x)   (for matrices without NA and
#                                                     with symmetric dimnames)

report_side_effects(FALSE)

matrix_is_binary <- snafun:::matrix_is_binary
matrix_is_symmetric_fast <- snafun:::matrix_is_symmetric_fast

set.seed(20260916)

# --- matrix_is_binary vs all(x %in% c(0, 1)) --------------------------------
bin_sym   <- matrix(c(0, 1, 1, 0), 2)
bin_asym  <- matrix(c(0, 1, 0, 0), 2)
weighted  <- matrix(c(0, 2.5, 2.5, 0), 2)
with_na   <- matrix(c(0, 1, NA, 0), 2)
big_bin   <- (snafun::to_matrix(igraph::sample_gnp(30, 0.2, directed = TRUE)) > 0) * 1
big_wt    <- big_bin; big_wt[big_wt == 1] <- stats::runif(sum(big_wt == 1), 1, 5)

for (nm in c("bin_sym", "bin_asym", "weighted", "with_na", "big_bin", "big_wt")) {
  m <- get(nm)
  expect_equal(matrix_is_binary(m), all(m %in% c(0, 1)),
               info = paste0("matrix_is_binary == all(%in%) for ", nm))
}

# --- matrix_is_symmetric_fast vs isSymmetric --------------------------------
for (nm in c("bin_sym", "bin_asym", "weighted", "big_bin", "big_wt")) {
  m <- get(nm)
  expect_equal(matrix_is_symmetric_fast(m), isSymmetric(m),
               info = paste0("matrix_is_symmetric_fast == isSymmetric for ", nm))
}

# symmetric weighted matrix -> both TRUE
sw <- matrix(c(0, 3, 1, 3, 0, 2, 1, 2, 0), 3)
expect_true(matrix_is_symmetric_fast(sw), info = "symmetric weighted -> TRUE")
expect_equal(matrix_is_symmetric_fast(sw), isSymmetric(sw), info = "matches isSymmetric (weighted symmetric)")

# rectangular matrix -> FALSE
rect <- matrix(1, nrow = 2, ncol = 3)
expect_false(matrix_is_symmetric_fast(rect), info = "rectangular -> not symmetric")

# NA present -> defers to isSymmetric(unname())
na_sym <- matrix(c(0, NA, NA, 0), 2)
expect_equal(matrix_is_symmetric_fast(na_sym), isSymmetric(unname(na_sym)),
             info = "NA case defers to isSymmetric")

# named symmetric adjacency matrix (rownames == colnames): both TRUE
g <- igraph::sample_gnp(12, 0.3, directed = FALSE)
igraph::V(g)$name <- paste0("v", 1:12)
mn <- snafun::to_matrix(g)
expect_true(matrix_is_symmetric_fast(mn), info = "named undirected adjacency is symmetric")
expect_equal(matrix_is_symmetric_fast(mn), isSymmetric(mn),
             info = "named case matches isSymmetric")

# Coverage test for fix_cug_input(), the CUG-input helper that turns either a
# square adjacency matrix or an (m x 2) edge-list matrix (carrying an "n"
# attribute for the full vertex count) into an igraph object.

report_side_effects(FALSE)

fix_cug_input <- snafun:::fix_cug_input

set.seed(20260914)

# Path 1: a square adjacency matrix -> to_igraph, vertex count preserved.
g <- igraph::sample_gnp(8, 0.3, directed = TRUE)
m <- snafun::to_matrix(g)
r1 <- fix_cug_input(m, directed = TRUE)
expect_true(inherits(r1, "igraph"), info = "square matrix -> igraph")
expect_equal(igraph::vcount(r1), 8L, info = "square matrix -> vertex count preserved")

# Path 2: an (m x 2) edge-list matrix with attr 'n' -> graph with n vertices,
# isolates added up to n.
el <- matrix(c(1, 2, 2, 3, 3, 4), ncol = 2, byrow = TRUE)
attr(el, "n") <- 6L
r2 <- fix_cug_input(el, directed = TRUE)
expect_true(inherits(r2, "igraph"), info = "edge-list matrix -> igraph")
expect_equal(igraph::vcount(r2), 6L,
             info = "edge-list matrix -> isolates added up to attr('n')")
expect_equal(igraph::ecount(r2), 3L,
             info = "edge-list matrix -> the three edges are present")

# Undirected request is honoured.
r3 <- fix_cug_input(el, directed = FALSE)
expect_false(igraph::is_directed(r3), info = "directed = FALSE yields an undirected graph")

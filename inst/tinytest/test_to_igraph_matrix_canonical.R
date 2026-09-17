# Regression tests for to_igraph.matrix() giving the CANONICAL igraph
# construction (2026-09-17).
#
# to_igraph.matrix() used to call simplify(remove.multiple = TRUE) unconditionally
# "because loops would otherwise occur twice". In current igraph
# (graph_from_adjacency_matrix() does not double loops) that simplify() removed
# nothing but shifted igraph::transitivity(type = "global") to a NON-canonical
# value for directed graphs with mutual dyads -- so g_transitivity(to_igraph(m))
# disagreed with g_transitivity(m) and with graph_from_adjacency_matrix(). The
# simplify() is now applied only when genuine multiple edges are present (which a
# matrix can never encode), so the output is canonical. These tests lock that in.

if (!requireNamespace("igraph", quietly = TRUE)) {
  exit_file("igraph not available")
}

report_side_effects(FALSE)
set.seed(20260917)

tr <- function(x) igraph::transitivity(x, type = "global")

# ---- directed graphs with mutual dyads: to_igraph(m) is canonical ----------
for (i in seq_len(15)) {
  n <- sample(40:150, 1)
  g <- igraph::sample_gnp(n, stats::runif(1, 0.08, 0.25), directed = TRUE)
  m <- snafun::to_matrix(g)
  canon <- igraph::graph_from_adjacency_matrix(m, mode = "directed")

  # to_igraph(m) has the same directed global transitivity as the canonical
  # graph_from_adjacency_matrix() construction ...
  expect_equal(tr(snafun::to_igraph(m)), tr(canon), tolerance = 1e-10,
               info = paste0("to_igraph.matrix canonical transitivity, directed #", i))
  # ... and g_transitivity is now stable across the conversion (this is the bug
  # that motivated the fix):
  expect_equal(snafun::g_transitivity(snafun::to_igraph(m)),
               snafun::g_transitivity(m), tolerance = 1e-10,
               info = paste0("g_transitivity(to_igraph(m)) == g_transitivity(m) #", i))
  # structure is preserved exactly (no edges added or removed)
  expect_equal(igraph::vcount(snafun::to_igraph(m)), igraph::vcount(canon),
               info = paste0("vcount preserved #", i))
  expect_equal(igraph::ecount(snafun::to_igraph(m)), igraph::ecount(canon),
               info = paste0("ecount preserved #", i))
}

# ---- undirected transitivity is (and stays) class-invariant ----------------
for (i in seq_len(8)) {
  g <- igraph::sample_gnp(sample(30:120, 1), stats::runif(1, 0.1, 0.3), directed = FALSE)
  m <- snafun::to_matrix(g)
  expect_equal(tr(snafun::to_igraph(m)),
               tr(igraph::graph_from_adjacency_matrix(m, mode = "undirected")),
               tolerance = 1e-10,
               info = paste0("to_igraph.matrix canonical transitivity, undirected #", i))
}

# ---- loops are preserved (not dropped, not doubled) ------------------------
Md <- matrix(c(1, 1, 0,
               0, 1, 1,
               1, 0, 0), nrow = 3, byrow = TRUE)   # two self-loops on the diagonal
gd <- snafun::to_igraph(Md)
expect_equal(sum(igraph::which_loop(gd)), 2L, info = "directed self-loops preserved (count)")
expect_equal(igraph::ecount(gd), sum(Md != 0), info = "directed with loops: edge count exact")
expect_false(any(igraph::which_multiple(gd)), info = "no spurious multiples (directed loops)")

Mu <- matrix(c(1, 1, 0,
               1, 0, 1,
               0, 1, 0), nrow = 3, byrow = TRUE)   # symmetric, one loop on vertex 1
gu <- snafun::to_igraph(Mu)
expect_equal(sum(igraph::which_loop(gu)), 1L, info = "undirected self-loop preserved (not doubled)")
expect_false(any(igraph::which_multiple(gu)), info = "no spurious multiples (undirected loop)")

# ---- weights are preserved -------------------------------------------------
Mw <- matrix(c(0, 3, 0,
               0, 0, 4,
               5, 0, 0), nrow = 3, byrow = TRUE)
gw <- snafun::to_igraph(Mw)
expect_true(igraph::is_weighted(gw), info = "weighted matrix -> weighted igraph")
expect_equal(sort(igraph::E(gw)$weight), c(3, 4, 5), info = "edge weights preserved")
expect_equal(igraph::ecount(gw), 3L, info = "weighted: one edge per non-zero cell")

# ---- directedness inferred from symmetry -----------------------------------
expect_true(igraph::is_directed(snafun::to_igraph(matrix(c(0,1,0, 0,0,1, 0,0,0), 3, byrow = TRUE))),
            info = "asymmetric matrix -> directed")
expect_false(igraph::is_directed(snafun::to_igraph(matrix(c(0,1,1, 1,0,1, 1,1,0), 3, byrow = TRUE))),
             info = "symmetric matrix -> undirected")

# ---- bipartite (incidence) input still works -------------------------------
inc <- matrix(c(1, 0, 1,
                0, 1, 1), nrow = 2, byrow = TRUE)
gb <- snafun::to_igraph(inc, bipartite = TRUE)
expect_true(igraph::is_bipartite(gb), info = "incidence matrix -> bipartite")
expect_equal(igraph::ecount(gb), sum(inc != 0), info = "bipartite: edge count exact")

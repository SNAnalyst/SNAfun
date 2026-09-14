# Regression tests for g_transitivity().
#
# CONTEXT (2026-09-14): the April 2026 rewrite replaced the trusted igraph/sna
# implementations with a hand-rolled matrix backend that
#   (a) computed transitivity from a DENSE `x %*% x` product, so it hung / ran
#       out of memory on large networks (e.g. the enwiki dataset), and
#   (b) was only "tested" against a verbatim copy of its own formula, so it
#       could never catch a conceptual error.
# The backend has been removed and the behaviour restored / hardened:
#   * g_transitivity.igraph  -> igraph::transitivity(x, type = "global")
#   * g_transitivity.network -> sna::gtrans(..., measure = "weak")
#   * g_transitivity.matrix / .data.frame -> build a (sparse) igraph object and
#     delegate to igraph::transitivity(), so they equal the igraph result AND
#     scale to very large networks.
#
# The tests below validate against igraph and sna as INDEPENDENT sources of
# truth, over a battery of generated networks (directed/undirected,
# weighted/unweighted), across all four input types (igraph, network, matrix,
# data.frame), and at both small and very large sizes. They are meant to stay
# in place as a regression guard for any future change to g_transitivity().

report_side_effects()

set.seed(20260914)


# ---------------------------------------------------------------------------
# Helper: check every input format for one generated network against the
# correct external reference.
#   - igraph / matrix / data.frame  -> igraph::transitivity(type = "global")
#     (this measure ignores edge weights and edge direction)
#   - network                       -> sna::gtrans(measure = "weak")
#   For undirected, unweighted graphs sna and igraph must additionally agree.
# ---------------------------------------------------------------------------

check_formats <- function(g, directed, weighted, label) {
  ref_ig <- igraph::transitivity(g, type = "global")

  # igraph object ----------------------------------------------------------
  expect_equal(snafun::g_transitivity(g), ref_ig,
               info = paste0(label, ": igraph input vs igraph::transitivity"))

  # matrix -----------------------------------------------------------------
  m <- snafun::to_matrix(g)
  expect_equal(snafun::g_transitivity(m), ref_ig,
               info = paste0(label, ": matrix input vs igraph::transitivity"))

  # data.frame (edge list) -------------------------------------------------
  el <- snafun::to_edgelist(g)
  expect_equal(snafun::g_transitivity(el), ref_ig,
               info = paste0(label, ": edge-list input vs igraph::transitivity"))

  # network object ---------------------------------------------------------
  net <- snafun::to_network(g)
  mode <- if (directed) "digraph" else "graph"
  ref_sna <- sna::gtrans(net, mode = mode, measure = "weak", use.adjacency = TRUE)
  expect_equal(snafun::g_transitivity(net), ref_sna,
               info = paste0(label, ": network input vs sna::gtrans(weak)"))

  # Independent cross-check between the two ecosystems, where the measures
  # coincide (undirected, unweighted graphs).
  if (!directed && !weighted) {
    expect_equal(snafun::g_transitivity(net), ref_ig, tolerance = 1e-6,
                 info = paste0(label, ": sna weak == igraph global (undirected)"))
  }

  # All four formats must agree with each other for the igraph-based measure.
  expect_equal(snafun::g_transitivity(m), snafun::g_transitivity(g),
               info = paste0(label, ": matrix == igraph"))
  expect_equal(snafun::g_transitivity(el), snafun::g_transitivity(g),
               info = paste0(label, ": edge-list == igraph"))
}


# Helper: attach random positive weights to an igraph object.
add_weights <- function(g) {
  igraph::E(g)$weight <- stats::runif(igraph::ecount(g), min = 1, max = 10)
  g
}


# ---------------------------------------------------------------------------
# Battery of SMALL and MEDIUM networks, all four kinds, both input sizes.
# Erdos-Renyi graphs give a mix of open and closed triads; the small-world
# graph guarantees a high, clearly non-trivial transitivity.
# ---------------------------------------------------------------------------

sizes <- c(small = 25L, medium = 300L)

for (sz_name in names(sizes)) {
  n <- sizes[[sz_name]]

  # undirected, unweighted
  g <- igraph::sample_gnp(n, p = 6 / n, directed = FALSE)
  check_formats(g, directed = FALSE, weighted = FALSE,
                label = paste0("gnp undirected unweighted (", sz_name, ")"))

  # directed, unweighted
  g <- igraph::sample_gnp(n, p = 6 / n, directed = TRUE)
  check_formats(g, directed = TRUE, weighted = FALSE,
                label = paste0("gnp directed unweighted (", sz_name, ")"))

  # undirected, weighted (weights must be ignored -> same as unweighted skeleton)
  g <- add_weights(igraph::sample_gnp(n, p = 6 / n, directed = FALSE))
  check_formats(g, directed = FALSE, weighted = TRUE,
                label = paste0("gnp undirected weighted (", sz_name, ")"))

  # directed, weighted
  g <- add_weights(igraph::sample_gnp(n, p = 6 / n, directed = TRUE))
  check_formats(g, directed = TRUE, weighted = TRUE,
                label = paste0("gnp directed weighted (", sz_name, ")"))

  # small-world (undirected, high clustering) as an extra, higher-transitivity case
  g <- igraph::sample_smallworld(dim = 1, size = n, nei = 3, p = 0.05)
  check_formats(g, directed = FALSE, weighted = FALSE,
                label = paste0("smallworld undirected (", sz_name, ")"))
}


# ---------------------------------------------------------------------------
# Explicit, hand-checkable values and edge cases.
# ---------------------------------------------------------------------------

# A fully connected triangle has global transitivity exactly 1, in every format.
tri <- igraph::make_full_graph(3)
expect_equal(snafun::g_transitivity(tri), 1, info = "triangle igraph == 1")
expect_equal(snafun::g_transitivity(snafun::to_matrix(tri)), 1, info = "triangle matrix == 1")
expect_equal(snafun::g_transitivity(snafun::to_edgelist(tri)), 1, info = "triangle edge-list == 1")
expect_equal(snafun::g_transitivity(snafun::to_network(tri)), 1, info = "triangle network == 1")

# A single edge has no length-2 paths -> NaN (must match igraph, not error / 0).
edge <- igraph::graph_from_literal(A - B)
expect_true(is.nan(snafun::g_transitivity(edge)), info = "single edge igraph -> NaN")
expect_true(is.nan(snafun::g_transitivity(snafun::to_matrix(edge))), info = "single edge matrix -> NaN")

# The default method must raise an informative error for unsupported input.
expect_error(snafun::g_transitivity(42L), info = "default method errors on integer")
expect_error(snafun::g_transitivity("not a graph"), info = "default method errors on character")

# A rectangular (two-mode / bipartite) matrix is not a one-mode adjacency.
rect <- matrix(c(1, 0, 1, 1, 0, 1), nrow = 2)
expect_error(snafun::g_transitivity(rect), info = "rectangular matrix is rejected")


# ---------------------------------------------------------------------------
# LARGE-NETWORK REGRESSION.
#
# This is the crucial guard. The April 2026 backend built a dense adjacency and
# computed `x %*% x`, which is O(n^2) in memory: for n = 50,000 that alone is
# ~20 GB, and it simply hung on enwiki-scale data. The restored implementation
# stays sparse (igraph), so it must return the correct value quickly.
#
# We exercise the two input types that are sparse by construction (igraph object
# and edge-list data.frame). The matrix and network representations are dense by
# construction (an n x n adjacency), so they are intentionally NOT used at this
# scale -- the point of the fix is precisely to avoid densification.
# ---------------------------------------------------------------------------

for (n_big in c(50000L, 200000L)) {
  gL <- igraph::sample_smallworld(dim = 1, size = n_big, nei = 3, p = 0.05)
  ref_big <- igraph::transitivity(gL, type = "global")

  # igraph input: must match and must be fast (a hang would never return).
  timing_ig <- system.time(v_ig <- snafun::g_transitivity(gL))[["elapsed"]]
  expect_equal(v_ig, ref_big,
               info = paste0("large igraph n=", n_big, " matches igraph::transitivity"))
  expect_true(timing_ig < 30,
              info = paste0("large igraph n=", n_big, " completes quickly (", round(timing_ig, 2), "s)"))

  # edge-list (data.frame) input: also sparse, must match.
  elL <- snafun::to_edgelist(gL)
  timing_el <- system.time(v_el <- snafun::g_transitivity(elL))[["elapsed"]]
  expect_equal(v_el, ref_big,
               info = paste0("large edge-list n=", n_big, " matches igraph::transitivity"))
  expect_true(timing_el < 30,
              info = paste0("large edge-list n=", n_big, " completes quickly (", round(timing_el, 2), "s)"))
}

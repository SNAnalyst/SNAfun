# Coverage tests for the core measures that previously had no direct tests:
# vertex centralities (v_*), graph-level measures (g_*), distances (d_*), and
# the graph predicates (is_*). Validated against igraph as independent ground
# truth, complemented by cross-representation checks (igraph vs network give the
# same answer) and sanity checks for measures without a simple igraph twin.
#
# Contract confirmed on 2026-09-14: the v_* functions accept igraph and network
# objects (not bare matrices).

report_side_effects(FALSE)

num <- function(x) unname(as.numeric(x))
set.seed(20260914)

# A connected undirected graph (so closeness/distance are well behaved).
gu <- igraph::simplify(igraph::sample_smallworld(1, 20, 2, 0.1))
igraph::V(gu)$name <- paste0("v", seq_len(igraph::vcount(gu)))
gu_n <- snafun::to_network(gu)

# A weighted version.
guw <- gu
igraph::E(guw)$weight <- stats::runif(igraph::ecount(guw), 1, 5)
guw_n <- snafun::to_network(guw)

# A directed graph.
gd <- igraph::sample_gnp(20, 0.2, directed = TRUE)
igraph::V(gd)$name <- paste0("v", seq_len(igraph::vcount(gd)))
gd_n <- snafun::to_network(gd)


# ---------------------------------------------------------------------------
# 1. Vertex centralities vs igraph (undirected, unweighted).
# ---------------------------------------------------------------------------

expect_equal(num(snafun::v_degree(gu)),      num(igraph::degree(gu)),
             info = "v_degree == igraph::degree")
expect_equal(num(snafun::v_betweenness(gu)), num(igraph::betweenness(gu)),
             info = "v_betweenness == igraph::betweenness")
expect_equal(num(snafun::v_closeness(gu)),   num(igraph::closeness(gu)),
             info = "v_closeness == igraph::closeness")
expect_equal(num(suppressWarnings(snafun::v_eigenvector(gu))),
             num(suppressWarnings(igraph::eigen_centrality(gu)$vector)),
             info = "v_eigenvector == igraph::eigen_centrality")
# Regression: v_eigenvector must not trigger igraph's deprecated `scale` warning
# (it used to pass scale = FALSE to eigen_centrality()).
eig_warn <- character(0)
withCallingHandlers(
  snafun::v_eigenvector(gu),
  warning = function(w) { eig_warn <<- c(eig_warn, conditionMessage(w)); invokeRestart("muffleWarning") }
)
expect_false(any(grepl("scale", eig_warn, ignore.case = TRUE)),
             info = "v_eigenvector does not emit the deprecated eigen_centrality 'scale' warning")
expect_equal(num(snafun::v_pagerank(gu)),    num(igraph::page_rank(gu)$vector),
             info = "v_pagerank == igraph::page_rank")
expect_equal(num(snafun::v_harmonic(gu)),    num(igraph::harmonic_centrality(gu)),
             info = "v_harmonic == igraph::harmonic_centrality")


# ---------------------------------------------------------------------------
# 2. Directed graph: degree modes and reciprocity vs igraph.
# ---------------------------------------------------------------------------

for (md in c("out", "in", "all")) {
  expect_equal(num(snafun::v_degree(gd, mode = md)), num(igraph::degree(gd, mode = md)),
               info = paste0("v_degree(mode=", md, ") == igraph::degree"))
}
expect_equal(num(snafun::v_pagerank(gd)), num(igraph::page_rank(gd)$vector),
             info = "directed v_pagerank == igraph::page_rank")


# ---------------------------------------------------------------------------
# 3. Cross-representation: igraph and network inputs must agree (also weighted).
#    Covers v_stress / v_geokpath / v_shapley, which have no simple igraph twin.
# ---------------------------------------------------------------------------

v_funcs <- c("v_degree", "v_betweenness", "v_closeness", "v_eigenvector",
             "v_pagerank", "v_harmonic", "v_stress", "v_geokpath", "v_shapley")
for (fn in v_funcs) {
  f <- get(fn, envir = asNamespace("snafun"))
  expect_equal(num(suppressWarnings(f(gu))), num(suppressWarnings(f(gu_n))),
               info = paste0(fn, ": igraph input == network input (unweighted)"))
  expect_equal(num(suppressWarnings(f(guw))), num(suppressWarnings(f(guw_n))),
               info = paste0(fn, ": igraph input == network input (weighted)"))
}


# ---------------------------------------------------------------------------
# 4. Sanity for the measures without a simple igraph reference.
# ---------------------------------------------------------------------------

nv <- igraph::vcount(gu)
for (fn in c("v_stress", "v_geokpath", "v_shapley")) {
  f <- get(fn, envir = asNamespace("snafun"))
  val <- num(suppressWarnings(f(gu)))
  expect_equal(length(val), nv, info = paste0(fn, ": one value per vertex"))
  expect_true(all(is.finite(val)), info = paste0(fn, ": all values finite"))
  expect_true(all(val >= 0), info = paste0(fn, ": all values non-negative"))
}


# ---------------------------------------------------------------------------
# 4b. v_eccentricity vs igraph; v_geokpath_w cross-representation + sanity.
# ---------------------------------------------------------------------------

expect_equal(num(snafun::v_eccentricity(gu)), num(igraph::eccentricity(gu)),
             info = "v_eccentricity == igraph::eccentricity")
expect_equal(num(snafun::v_eccentricity(gu)), num(snafun::v_eccentricity(gu_n)),
             info = "v_eccentricity: igraph == network")

gkw_i <- num(suppressWarnings(snafun::v_geokpath_w(gu)))
gkw_n <- num(suppressWarnings(snafun::v_geokpath_w(gu_n)))
expect_equal(gkw_i, gkw_n, info = "v_geokpath_w: igraph == network")
expect_equal(length(gkw_i), nv, info = "v_geokpath_w: one value per vertex")
expect_true(all(is.finite(gkw_i)) && all(gkw_i >= 0),
            info = "v_geokpath_w: finite and non-negative")


# ---------------------------------------------------------------------------
# 5. vids subsetting returns the corresponding subset.
# ---------------------------------------------------------------------------

full_deg <- snafun::v_degree(gu)
sub_deg  <- snafun::v_degree(gu, vids = c(1, 3, 5))
expect_equal(num(sub_deg), num(full_deg[c(1, 3, 5)]),
             info = "v_degree(vids = ...) returns the requested subset")


# ---------------------------------------------------------------------------
# 6. Graph-level measures vs igraph.
# ---------------------------------------------------------------------------

expect_equal(snafun::g_diameter(gu),      igraph::diameter(gu),
             info = "g_diameter == igraph::diameter")
expect_equal(snafun::g_radius(gu),        igraph::radius(gu),
             info = "g_radius == igraph::radius")
expect_equal(snafun::g_mean_distance(gu), igraph::mean_distance(gu),
             info = "g_mean_distance == igraph::mean_distance")
expect_equal(snafun::g_reciprocity(gd),   igraph::reciprocity(gd),
             info = "g_reciprocity == igraph::reciprocity")

# g_compactness and g_centralize: cross-representation + finiteness.
expect_equal(snafun::g_compactness(gu), snafun::g_compactness(gu_n),
             info = "g_compactness: igraph == network")
expect_true(is.finite(snafun::g_compactness(gu)) &&
            snafun::g_compactness(gu) >= 0 && snafun::g_compactness(gu) <= 1,
            info = "g_compactness in [0, 1]")
expect_equal(snafun::g_centralize(gu, measure = "degree")$centralization,
             snafun::g_centralize(gu_n, measure = "degree")$centralization,
             info = "g_centralize: igraph == network")


# ---------------------------------------------------------------------------
# 7. Distances.
# ---------------------------------------------------------------------------

expect_equal(unname(snafun::d_distance(gu)), unname(igraph::distances(gu)),
             info = "d_distance == igraph::distances (undirected)")
expect_equal(unname(snafun::d_distance(gd, mode = "out")),
             unname(igraph::distances(gd, mode = "out")),
             info = "d_distance == igraph::distances (directed, out)")

se_i <- snafun::d_structural_equivalence(gu)
se_n <- snafun::d_structural_equivalence(gu_n)
expect_equal(dim(as.matrix(se_i)), c(nv, nv),
             info = "d_structural_equivalence returns an n x n object")
expect_equal(unname(as.matrix(se_i)), unname(as.matrix(se_n)),
             info = "d_structural_equivalence: igraph == network")


# ---------------------------------------------------------------------------
# 8. Predicates and small helpers.
# ---------------------------------------------------------------------------

expect_true(snafun::is_connected(gu),  info = "is_connected TRUE for a connected graph")
expect_equal(snafun::is_connected(gu), igraph::is_connected(gu),
             info = "is_connected matches igraph")
disc <- igraph::disjoint_union(igraph::make_ring(4), igraph::make_ring(3))
expect_false(snafun::is_connected(disc), info = "is_connected FALSE for a disconnected graph")

expect_true(snafun::is_igraph(gu),   info = "is_igraph TRUE for igraph")
expect_false(snafun::is_igraph(gu_n), info = "is_igraph FALSE for network")
expect_true(snafun::is_network(gu_n), info = "is_network TRUE for network")
expect_false(snafun::is_network(gu),  info = "is_network FALSE for igraph")
expect_false(snafun::is_signed(gu),   info = "is_signed FALSE for an unsigned graph")

expect_false(snafun::has_loops(gu),   info = "has_loops FALSE for a loopless graph")
gl <- igraph::add_edges(gu, c(1, 1))
expect_true(snafun::has_loops(gl),    info = "has_loops TRUE after adding a self-loop")


# ---------------------------------------------------------------------------
# 9. make_mixingmatrix and g_summary run and return sensible shapes.
# ---------------------------------------------------------------------------

gattr <- gu
igraph::V(gattr)$grp <- rep(c("a", "b"), length.out = nv)
mm <- snafun::make_mixingmatrix(gattr, attrname = "grp")
mm_m <- as.matrix(mm)
expect_true(is.matrix(mm) || is.table(mm) || is.data.frame(mm),
            info = "make_mixingmatrix returns a matrix-like object")
expect_true(all(dim(mm_m) >= 2),
            info = "mixing matrix has a row/column per attribute level (>= 2 here)")
expect_true(all(mm_m >= 0) && sum(mm_m) > 0,
            info = "mixing matrix has non-negative entries and counts something")

su <- snafun::g_summary(gu)
expect_false(is.null(su), info = "g_summary returns a non-null result")


# ---------------------------------------------------------------------------
# 10. Contract: v_* reject a bare matrix input.
# ---------------------------------------------------------------------------

expect_error(snafun::v_degree(snafun::to_matrix(gu)),
             info = "v_degree rejects a matrix (igraph/network only)")

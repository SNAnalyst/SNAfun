# Tests for the group-A change (2026-09-16): measure methods that used to route
# network objects through dense sna functions now go through igraph via the
# sparse helper network_structure_to_igraph(). This fixes hangs on large graphs
# (e.g. sna::triad.census(enwiki_n) and sna::geodist() on enwiki_n) while
# keeping results identical.
#
# Covered here: count_triads (was sna::triad.census, hung) and g_mean_distance
# (was sna::geodist). g_transitivity already routes via igraph (0.2026.6);
# count_dyads is intentionally left on sna::dyad.census (it scales fine).

if (!requireNamespace("network", quietly = TRUE) ||
    !requireNamespace("sna", quietly = TRUE)) {
  exit_file("network / sna not available")
}

report_side_effects(FALSE)

set.seed(20260916)

df_eq <- function(a, b) isTRUE(all.equal(a, b, check.attributes = FALSE))


# ---------------------------------------------------------------------------
# 1. count_triads: igraph == network == matrix, across graph kinds.
# ---------------------------------------------------------------------------

triad_cases <- list(
  "directed"           = igraph::sample_gnp(40, 0.15, directed = TRUE),
  "directed dense"     = igraph::sample_gnp(30, 0.35, directed = TRUE),
  "undirected"         = igraph::sample_gnp(40, 0.2, directed = FALSE),
  "directed +isolates" = igraph::add_vertices(igraph::sample_gnp(30, 0.15, directed = TRUE), 6),
  "directed +loops"    = igraph::add_edges(igraph::sample_gnp(25, 0.15, directed = TRUE), c(1, 1, 2, 2))
)
for (nm in names(triad_cases)) {
  g <- triad_cases[[nm]]
  ref <- snafun::count_triads(g, echo = FALSE)                       # igraph
  net <- snafun::count_triads(snafun::to_network(g), echo = FALSE)   # network -> igraph now
  expect_true(df_eq(net, ref),
              info = paste0("count_triads network == igraph (", nm, ")"))
  mat <- snafun::count_triads(snafun::to_matrix(g), echo = FALSE)    # matrix -> igraph
  expect_true(df_eq(mat, ref),
              info = paste0("count_triads matrix == igraph (", nm, ")"))
}


# ---------------------------------------------------------------------------
# 2. g_mean_distance: igraph == network (connected AND disconnected).
# ---------------------------------------------------------------------------

# connected undirected
gc <- igraph::simplify(igraph::sample_smallworld(1, 50, 3, 0.1))
expect_equal(snafun::g_mean_distance(snafun::to_network(gc)),
             snafun::g_mean_distance(gc), tolerance = 1e-8,
             info = "g_mean_distance network == igraph (connected)")

# directed connected-ish
gd <- igraph::sample_gnp(60, 0.1, directed = TRUE)
expect_equal(snafun::g_mean_distance(snafun::to_network(gd)),
             snafun::g_mean_distance(gd), tolerance = 1e-8,
             info = "g_mean_distance network == igraph (directed)")

# disconnected: both must now agree (finite, averaged over reachable pairs),
# instead of the old network path returning Inf.
gdisc <- igraph::disjoint_union(igraph::make_ring(8), igraph::make_full_graph(5))
mn_ig <- snafun::g_mean_distance(gdisc)
mn_net <- snafun::g_mean_distance(snafun::to_network(gdisc))
expect_true(is.finite(mn_net),
            info = "g_mean_distance network is finite for a disconnected graph")
expect_equal(mn_net, mn_ig, tolerance = 1e-8,
             info = "g_mean_distance network == igraph (disconnected)")


# ---------------------------------------------------------------------------
# 3. network_structure_to_igraph() helper: structure preserved.
# ---------------------------------------------------------------------------

nsi <- snafun:::network_structure_to_igraph
for (nm in c("directed", "undirected", "directed +isolates")) {
  g <- triad_cases[[nm]]
  gs <- nsi(snafun::to_network(g))
  expect_equal(igraph::vcount(gs), igraph::vcount(g),
               info = paste0("helper preserves vertex count incl. isolates (", nm, ")"))
  expect_equal(igraph::is_directed(gs), igraph::is_directed(g),
               info = paste0("helper preserves directedness (", nm, ")"))
  expect_equal(sort(unname(igraph::degree(gs))), sort(unname(igraph::degree(g))),
               info = paste0("helper preserves degree sequence (", nm, ")"))
}


# ---------------------------------------------------------------------------
# 4. Scalability: the network paths must NOT hang on a moderately large graph
#    (the old sna::triad.census / sna::geodist dense route would be far slower).
# ---------------------------------------------------------------------------

gbig <- igraph::sample_gnp(1500, 6 / 1500, directed = TRUE)
netbig <- snafun::to_network(gbig)

t_tri <- system.time(v_tri <- snafun::count_triads(netbig, echo = FALSE))[["elapsed"]]
expect_true(df_eq(v_tri, snafun::count_triads(gbig, echo = FALSE)),
            info = "large count_triads network == igraph")
expect_true(t_tri < 20,
            info = paste0("large count_triads network is fast (", round(t_tri, 2), "s)"))

t_md <- system.time(v_md <- snafun::g_mean_distance(netbig))[["elapsed"]]
expect_equal(v_md, snafun::g_mean_distance(gbig), tolerance = 1e-8,
             info = "large g_mean_distance network == igraph")
expect_true(t_md < 20,
            info = paste0("large g_mean_distance network is fast (", round(t_md, 2), "s)"))

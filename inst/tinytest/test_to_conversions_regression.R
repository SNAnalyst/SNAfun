# Extensive regression tests for the conversion functions to_matrix(),
# to_igraph(), to_network() and to_edgelist().
#
# CONTEXT (2026-09-14): these functions were heavily rewritten in April 2026.
# While setting up the g_transitivity() tests we already hit two real problems
# in this area (an inconsistent directed matrix -> igraph conversion, and a
# failure of the edge-list -> igraph path on large networks). This file adds a
# broad, deliberately over-complete battery so that any behavioural change in
# the to_* functions is caught.
#
# STRATEGY: everything is validated against igraph / network / sna as
# independent ground truth, using round-trip invariants. The core invariant is
# that a graph, converted to another representation and back, must preserve:
#   * directedness,
#   * the full vertex set (including isolates),
#   * the edge set and the edge weights.
# For named graphs this is captured exactly by comparing the (name-sorted)
# adjacency matrices.

report_side_effects()

set.seed(20260914)


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

# Give every vertex a stable, unique name so identity survives reordering.
name_vs <- function(g) {
  igraph::V(g)$name <- paste0("v", seq_len(igraph::vcount(g)))
  g
}

# Attach distinct integer weights (distinct so asymmetry is actually tested).
add_wt <- function(g) {
  igraph::E(g)$weight <- seq_len(igraph::ecount(g))
  g
}

# Name-sorted adjacency matrix; captures edges + weights + isolates at once.
canon_adj <- function(g) {
  m <- snafun::to_matrix(g)
  if (is.null(rownames(m))) {
    stop("canon_adj() requires named vertices")
  }
  o <- order(rownames(m))
  m[o, o, drop = FALSE]
}

# Assert that a round-tripped graph 'rt' matches the original 'orig'.
expect_rt <- function(orig, rt, label) {
  expect_equal(snafun::is_directed(rt), snafun::is_directed(orig),
               info = paste0(label, ": directedness preserved"))
  expect_equal(snafun::count_vertices(rt), snafun::count_vertices(orig),
               info = paste0(label, ": vertex count preserved (isolates!)"))
  expect_equal(snafun::count_edges(rt), snafun::count_edges(orig),
               info = paste0(label, ": edge count preserved"))
  expect_equal(canon_adj(rt), canon_adj(orig),
               info = paste0(label, ": adjacency (edges + weights) preserved"))
}


# ---------------------------------------------------------------------------
# Build a broad battery of NAMED igraph graphs.
# ---------------------------------------------------------------------------

graphs <- list()
add_graph <- function(g, label) {
  graphs[[length(graphs) + 1]] <<- list(g = name_vs(g), label = label)
}

for (n in c(7L, 40L, 200L)) {
  p <- 5 / n
  add_graph(igraph::sample_gnp(n, p, directed = FALSE),           paste0("gnp undirected unweighted n=", n))
  add_graph(igraph::sample_gnp(n, p, directed = TRUE),            paste0("gnp directed unweighted n=", n))
  add_graph(add_wt(igraph::sample_gnp(n, p, directed = FALSE)),   paste0("gnp undirected weighted n=", n))
  add_graph(add_wt(igraph::sample_gnp(n, p, directed = TRUE)),    paste0("gnp directed weighted n=", n))
}

# Structural special cases.
add_graph(igraph::make_ring(12, directed = FALSE),               "ring undirected n=12")
add_graph(igraph::make_star(10, mode = "out"),                   "star directed out n=10")
add_graph(igraph::make_star(10, mode = "undirected"),            "star undirected n=10")
add_graph(igraph::make_full_graph(8, directed = FALSE),          "full undirected n=8")
add_graph(igraph::make_tree(15, children = 2, mode = "out"),     "tree directed n=15")
add_graph(igraph::sample_smallworld(1, 60, nei = 3, p = 0.05),   "smallworld undirected n=60")

# Graph with isolates (the classic edge-list roundtrip trap).
gi <- igraph::sample_gnp(20, 0.2, directed = FALSE)
gi <- igraph::add_vertices(gi, 5)                                 # 5 isolated vertices
add_graph(gi,                                                     "undirected with 5 isolates")

gid <- igraph::sample_gnp(20, 0.15, directed = TRUE)
gid <- igraph::add_vertices(gid, 4)
add_graph(gid,                                                    "directed with 4 isolates")

# Disconnected graph (two components).
gc <- igraph::disjoint_union(
  igraph::make_ring(6, directed = FALSE),
  igraph::make_full_graph(4, directed = FALSE)
)
add_graph(gc,                                                     "disconnected undirected")


# ---------------------------------------------------------------------------
# MAIN BATTERY: every graph, every round-trip path back to igraph.
# ---------------------------------------------------------------------------

for (item in graphs) {
  g   <- item$g
  lab <- item$label

  # 1. via edge list
  expect_rt(g, snafun::to_igraph(snafun::to_edgelist(g)),
            paste0(lab, " | igraph->edgelist->igraph"))

  # 2. via matrix
  expect_rt(g, snafun::to_igraph(snafun::to_matrix(g)),
            paste0(lab, " | igraph->matrix->igraph"))

  # 3. via network
  expect_rt(g, snafun::to_igraph(snafun::to_network(g)),
            paste0(lab, " | igraph->network->igraph"))

  # 4. chain: network -> edgelist -> igraph
  expect_rt(g, snafun::to_igraph(snafun::to_edgelist(snafun::to_network(g))),
            paste0(lab, " | network->edgelist->igraph"))

  # 5. chain: matrix -> network -> igraph
  expect_rt(g, snafun::to_igraph(snafun::to_network(snafun::to_matrix(g))),
            paste0(lab, " | matrix->network->igraph"))
}


# ---------------------------------------------------------------------------
# CROSS-CHECKS against igraph's own conversions (independent ground truth).
# ---------------------------------------------------------------------------

for (item in graphs) {
  g   <- item$g
  lab <- item$label
  weighted <- snafun::is_weighted(g)

  ig_adj <- as.matrix(igraph::as_adjacency_matrix(
    g, type = "both", sparse = FALSE,
    attr = if (weighted) "weight" else NULL
  ))
  sn_adj <- snafun::to_matrix(g)
  # Align on names before comparing.
  o1 <- order(rownames(ig_adj)); o2 <- order(rownames(sn_adj))
  expect_equal(unname(sn_adj[o2, o2, drop = FALSE]),
               unname(ig_adj[o1, o1, drop = FALSE]),
               info = paste0(lab, ": to_matrix == igraph::as_adjacency_matrix"))
}


# ---------------------------------------------------------------------------
# to_edgelist: consistency with igraph and the hidden metadata contract.
# ---------------------------------------------------------------------------

for (item in graphs) {
  g   <- item$g
  lab <- item$label

  el <- snafun::to_edgelist(g)
  expect_true(is.data.frame(el), info = paste0(lab, ": edgelist is a data.frame"))
  expect_equal(nrow(el), snafun::count_edges(g),
               info = paste0(lab, ": edgelist has one row per edge"))
  expect_equal(as.character(colnames(el)[1:2]), c("from", "to"),
               info = paste0(lab, ": edgelist first two columns are from/to"))
  # Hidden metadata must round-trip the directedness and full vertex set.
  expect_equal(isTRUE(attr(el, "snafun_directed")), snafun::is_directed(g),
               info = paste0(lab, ": edgelist stores directedness"))
  vmeta <- attr(el, "snafun_vertices")
  expect_equal(nrow(vmeta), snafun::count_vertices(g),
               info = paste0(lab, ": edgelist stores all vertices (incl. isolates)"))
}


# ---------------------------------------------------------------------------
# UNNAMED graphs: identity is positional, so check structure is preserved.
# ---------------------------------------------------------------------------

for (n in c(15L, 80L)) {
  for (dir in c(FALSE, TRUE)) {
    g0 <- igraph::sample_gnp(n, 5 / n, directed = dir)  # no names
    lab <- paste0("unnamed n=", n, if (dir) " directed" else " undirected")

    # matrix keeps vertex order, so a positional comparison is valid.
    rt_m <- snafun::to_igraph(snafun::to_matrix(g0))
    expect_equal(unname(snafun::to_matrix(rt_m)), unname(snafun::to_matrix(g0)),
                 info = paste0(lab, ": unnamed matrix roundtrip preserves adjacency"))
    expect_equal(snafun::is_directed(rt_m), dir,
                 info = paste0(lab, ": unnamed matrix roundtrip preserves directedness"))

    # edge list roundtrip: structure (counts + directedness) must survive.
    rt_e <- snafun::to_igraph(snafun::to_edgelist(g0))
    expect_equal(snafun::count_vertices(rt_e), snafun::count_vertices(g0),
                 info = paste0(lab, ": unnamed edgelist roundtrip vertex count"))
    expect_equal(snafun::count_edges(rt_e), snafun::count_edges(g0),
                 info = paste0(lab, ": unnamed edgelist roundtrip edge count"))
    expect_equal(snafun::is_directed(rt_e), dir,
                 info = paste0(lab, ": unnamed edgelist roundtrip directedness"))
  }
}


# ---------------------------------------------------------------------------
# BIPARTITE / two-mode conversions.
# ---------------------------------------------------------------------------

# Build a bipartite igraph with a known incidence matrix.
inc <- matrix(c(1, 0, 1,
                0, 1, 1,
                1, 1, 0,
                0, 0, 1,
                1, 0, 0), nrow = 5, byrow = TRUE)
rownames(inc) <- paste0("a", 1:5)
colnames(inc) <- paste0("b", 1:3)
gb <- igraph::graph_from_biadjacency_matrix(inc)

# to_matrix on a bipartite igraph must return the incidence matrix.
mb <- snafun::to_matrix(gb)
expect_equal(dim(mb), c(5L, 3L), info = "bipartite: to_matrix returns incidence matrix shape")
expect_equal(unname(mb[order(rownames(mb)), order(colnames(mb))]),
             unname(inc[order(rownames(inc)), order(colnames(inc))]),
             info = "bipartite: to_matrix returns the correct incidence matrix")

# Rectangular matrix -> network (bipartite) -> igraph must round-trip the incidence.
net_b <- snafun::to_network(inc, bipartite = TRUE)
expect_true(network::is.bipartite(net_b), info = "bipartite: to_network makes a bipartite network")
mb2 <- snafun::to_matrix(net_b)
expect_equal(unname(mb2[order(rownames(mb2)), , drop = FALSE][, order(colnames(mb2)), drop = FALSE]),
             unname(inc[order(rownames(inc)), order(colnames(inc))]),
             info = "bipartite: matrix->network->matrix preserves incidence")

# Rectangular matrix to a one-mode network must be refused.
expect_error(snafun::to_network(inc, bipartite = FALSE),
             info = "bipartite: rectangular matrix rejected for one-mode network")


# ---------------------------------------------------------------------------
# WEIGHT-SPECIFIC checks: weights must survive every representation.
# ---------------------------------------------------------------------------

gw <- name_vs(add_wt(igraph::sample_gnp(30, 0.2, directed = TRUE)))
expect_true(snafun::is_weighted(snafun::to_igraph(snafun::to_matrix(gw))),
            info = "weights: matrix roundtrip stays weighted")
expect_true(snafun::is_weighted(snafun::to_igraph(snafun::to_edgelist(gw))),
            info = "weights: edgelist roundtrip stays weighted")
expect_equal(sum(igraph::E(snafun::to_igraph(snafun::to_matrix(gw)))$weight),
             sum(igraph::E(gw)$weight),
             info = "weights: total weight preserved through matrix")


# ---------------------------------------------------------------------------
# LARGE-SCALE round trips (the gap that caused real failures).
# igraph <-> edge list is sparse and must work at scale; matrix/network are
# dense by construction and are exercised at a moderate size only.
# ---------------------------------------------------------------------------

for (n_big in c(5000L, 50000L)) {
  gL <- igraph::sample_smallworld(1, n_big, nei = 3, p = 0.05)

  # edge list roundtrip must not error and must preserve structure.
  t_el <- system.time({
    elL <- snafun::to_edgelist(gL)
    gL2 <- snafun::to_igraph(elL)
  })[["elapsed"]]
  expect_equal(snafun::count_vertices(gL2), snafun::count_vertices(gL),
               info = paste0("large n=", n_big, ": edgelist roundtrip vertex count"))
  expect_equal(snafun::count_edges(gL2), snafun::count_edges(gL),
               info = paste0("large n=", n_big, ": edgelist roundtrip edge count"))
  expect_equal(igraph::transitivity(gL2, "global"), igraph::transitivity(gL, "global"),
               info = paste0("large n=", n_big, ": edgelist roundtrip transitivity"))
  expect_true(t_el < 60,
              info = paste0("large n=", n_big, ": edgelist roundtrip is fast (", round(t_el, 2), "s)"))
}

# Moderate size for the dense representations.
gM <- name_vs(igraph::sample_gnp(400, 0.02, directed = TRUE))
expect_rt(gM, snafun::to_igraph(snafun::to_matrix(gM)),   "moderate n=400 | matrix roundtrip")
expect_rt(gM, snafun::to_igraph(snafun::to_network(gM)),  "moderate n=400 | network roundtrip")


# ---------------------------------------------------------------------------
# HUGE but SPARSE, crossing the 1e5 vertex-id boundary.
#
# REGRESSION: for graphs without vertex names, igraph returns numeric (double)
# vertex ids in the edge list. Ids >= 1e5 then stringified in scientific
# notation ("1e+05") on the way back to a graph, no longer matched the integer
# vertex table, and the edge-list -> igraph roundtrip failed with
# "Some vertices ... are missing in 'vertices'". This hit enwiki-scale data.
# We build empty graphs with a handful of edges so the test stays fast while
# still exercising a vertex id of 100,000 / 200,000.
# ---------------------------------------------------------------------------

for (n_v in c(100000L, 200000L)) {
  gsp <- igraph::make_empty_graph(n = n_v, directed = TRUE)
  gsp <- igraph::add_edges(gsp, c(1L, 2L, 3L, 4L, n_v - 1L, n_v, n_v, 1L))

  gsp2 <- snafun::to_igraph(snafun::to_edgelist(gsp))
  expect_equal(snafun::count_vertices(gsp2), n_v,
               info = paste0("huge sparse n=", n_v, ": all vertices survive edgelist roundtrip"))
  expect_equal(snafun::count_edges(gsp2), 4L,
               info = paste0("huge sparse n=", n_v, ": edges survive edgelist roundtrip"))
  expect_equal(snafun::is_directed(gsp2), TRUE,
               info = paste0("huge sparse n=", n_v, ": directedness survives"))
}

# Extensive tests for the group-A functions that were rerouted through igraph
# (2026-09-16): count_triads() and g_mean_distance(). For a large battery of
# network types they must return IDENTICAL results whether the input is an
# igraph, a network, or a matrix, and those results must match BOTH igraph and
# sna reference computations. A final section checks that the network path is
# speed-comparable to the igraph path (no dense-sna penalty).

if (!requireNamespace("network", quietly = TRUE) ||
    !requireNamespace("sna", quietly = TRUE)) {
  exit_file("network / sna not available")
}

report_side_effects(FALSE)
set.seed(20260916)

TRIAD_LABELS <- c("003", "012", "102", "021D", "021U", "021C", "111D", "111U",
                  "030T", "030C", "201", "120D", "120U", "120C", "210", "300")

wt <- function(g) { igraph::E(g)$weight <- stats::runif(igraph::ecount(g), 1, 9); g }

# sna reference triad census as a length-16 vector in the standard MAN order.
sna_triad_ref <- function(g) {
  net <- snafun::to_network(g)
  if (snafun::is_directed(g)) {
    unname(as.numeric(sna::triad.census(net, mode = "digraph")))
  } else {
    tc <- unname(as.numeric(sna::triad.census(net, mode = "graph")))  # 003,102,201,300
    v <- stats::setNames(numeric(16), TRIAD_LABELS)
    v[c("003", "102", "201", "300")] <- tc
    unname(v)
  }
}

# sna reference mean distance (averaged over reachable ordered pairs).
sna_mean_dist <- function(g) {
  net <- snafun::to_network(g)
  d <- sna::geodist(net, count.paths = FALSE, ignore.eval = TRUE)$gdist
  diag(d) <- NA
  mean(d[is.finite(d)])
}

ct_vec <- function(x) unname(as.numeric(snafun::count_triads(x, echo = FALSE)))


# ---------------------------------------------------------------------------
# Battery of one-mode graphs.
# ---------------------------------------------------------------------------

graphs <- list(
  "dir gnp small"      = igraph::sample_gnp(30, 0.15, directed = TRUE),
  "dir gnp dense"      = igraph::sample_gnp(25, 0.40, directed = TRUE),
  "dir gnp sparse"     = igraph::sample_gnp(80, 0.03, directed = TRUE),
  "undir gnp"          = igraph::sample_gnp(35, 0.2, directed = FALSE),
  "undir smallworld"   = igraph::simplify(igraph::sample_smallworld(1, 60, 3, 0.1)),
  "dir with isolates"  = igraph::add_vertices(igraph::sample_gnp(25, 0.15, directed = TRUE), 8),
  "undir with isolates"= igraph::add_vertices(igraph::sample_gnp(25, 0.2, directed = FALSE), 8),
  "ring directed"      = igraph::make_ring(15, directed = TRUE),
  "star out"           = igraph::make_star(12, mode = "out"),
  "full directed"      = igraph::make_full_graph(8, directed = TRUE),
  "full undirected"    = igraph::make_full_graph(8, directed = FALSE),
  "disconnected"       = igraph::disjoint_union(igraph::make_ring(8), igraph::make_full_graph(5)),
  "dir weighted"       = wt(igraph::sample_gnp(30, 0.2, directed = TRUE)),
  "undir weighted"     = wt(igraph::sample_gnp(30, 0.2, directed = FALSE)),
  "dir with loops"     = igraph::add_edges(igraph::sample_gnp(20, 0.15, directed = TRUE), c(1,1,3,3,5,5))
)

for (nm in names(graphs)) {
  g <- graphs[[nm]]
  has_loops <- any(igraph::which_loop(g))

  ## ---- count_triads --------------------------------------------------------
  ref_ig <- ct_vec(g)
  # input-class invariance
  expect_equal(ct_vec(snafun::to_network(g)), ref_ig,
               info = paste0("count_triads network == igraph (", nm, ")"))
  expect_equal(ct_vec(snafun::to_matrix(g)), ref_ig,
               info = paste0("count_triads matrix == igraph (", nm, ")"))
  # vs igraph reference
  expect_equal(ref_ig, unname(as.numeric(igraph::triad_census(g))),
               info = paste0("count_triads == igraph::triad_census (", nm, ")"))
  # vs sna reference (loops handled differently by the two engines, so skip them)
  if (!has_loops) {
    expect_equal(ref_ig, sna_triad_ref(g),
                 info = paste0("count_triads == sna::triad.census (", nm, ")"))
  }
  # total number of triads is choose(n, 3)
  expect_equal(sum(ref_ig), choose(igraph::vcount(g), 3),
               info = paste0("count_triads sums to choose(n,3) (", nm, ")"))

  ## ---- g_mean_distance -----------------------------------------------------
  md_ig <- snafun::g_mean_distance(g)
  expect_equal(snafun::g_mean_distance(snafun::to_network(g)), md_ig, tolerance = 1e-8,
               info = paste0("g_mean_distance network == igraph (", nm, ")"))
  expect_equal(md_ig,
               igraph::mean_distance(g, weights = NA, directed = TRUE, unconnected = TRUE),
               tolerance = 1e-8,
               info = paste0("g_mean_distance == igraph::mean_distance (unweighted) (", nm, ")"))
  if (!has_loops) {
    expect_equal(md_ig, sna_mean_dist(g), tolerance = 1e-6,
                 info = paste0("g_mean_distance == sna::geodist mean (", nm, ")"))
  }
}


# ---------------------------------------------------------------------------
# Bipartite / two-mode.
# ---------------------------------------------------------------------------

inc <- matrix(c(1,0,1, 0,1,1, 1,1,0, 0,0,1, 1,0,1), nrow = 5, byrow = TRUE)
gb <- igraph::graph_from_biadjacency_matrix(inc)

# count_triads is not defined for bipartite graphs -> must error for every class.
expect_error(snafun::count_triads(gb, echo = FALSE),
             info = "count_triads errors on bipartite igraph")
expect_error(snafun::count_triads(snafun::to_network(gb), echo = FALSE),
             info = "count_triads errors on bipartite network")

# g_mean_distance works on bipartite and must agree across classes.
expect_equal(snafun::g_mean_distance(snafun::to_network(gb)),
             snafun::g_mean_distance(gb), tolerance = 1e-8,
             info = "g_mean_distance network == igraph (bipartite)")


# ---------------------------------------------------------------------------
# Speed: the network path (convert + igraph) must be comparable to the igraph
# path and not dramatically slower than sna -- i.e. no dense-sna penalty.
# ---------------------------------------------------------------------------

set.seed(1)
gbig <- igraph::sample_gnp(1000, 6 / 1000, directed = TRUE)
netbig <- snafun::to_network(gbig)

t_ct_ig  <- system.time(a <- snafun::count_triads(gbig, echo = FALSE))[["elapsed"]]
t_ct_net <- system.time(b <- snafun::count_triads(netbig, echo = FALSE))[["elapsed"]]
t_ct_sna <- system.time(cc <- sna::triad.census(netbig, mode = "digraph"))[["elapsed"]]
expect_equal(ct_vec(netbig), ct_vec(gbig),
             info = "large count_triads network == igraph")
expect_true(t_ct_net < 5 * t_ct_ig + 3,
            info = paste0("count_triads network comparable to igraph (net=", round(t_ct_net, 2),
                          "s, ig=", round(t_ct_ig, 2), "s)"))
expect_true(t_ct_net < t_ct_sna + 3,
            info = paste0("count_triads network not slower than sna (net=", round(t_ct_net, 2),
                          "s, sna=", round(t_ct_sna, 2), "s)"))

t_md_ig  <- system.time(snafun::g_mean_distance(gbig))[["elapsed"]]
t_md_net <- system.time(snafun::g_mean_distance(netbig))[["elapsed"]]
expect_true(t_md_net < 5 * t_md_ig + 3,
            info = paste0("g_mean_distance network comparable to igraph (net=", round(t_md_net, 2),
                          "s, ig=", round(t_md_ig, 2), "s)"))

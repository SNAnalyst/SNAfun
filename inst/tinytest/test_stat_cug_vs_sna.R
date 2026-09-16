# Equivalence tests for stat_cug() against sna::cug.test().
#
# stat_cug() must produce the same conditional-uniform-graph test as the
# canonical sna::cug.test() (called with an equivalent snafun FUN via
# fix_cug_input()), for
#   * several statistics (not only transitivity),
#   * undirected and directed graphs,
#   * cmode = "edges" and cmode = "dyad.census",
#   * and input given as igraph / network / matrix / edgelist.
#
# Because the two implementations draw independent random graphs, the null
# distributions cannot be bit-identical; instead we require:
#   * the OBSERVED statistic to match exactly (deterministic), and
#   * the null distributions to be the same distribution -> sample means agree
#     within Monte Carlo error (5 standard errors) and spreads are comparable.
#
# It also checks the fix A performance change directly: convert_cug_matrix_to_graph()
# must build the same graph as snafun::to_igraph() for a binary matrix.

if (!requireNamespace("sna", quietly = TRUE) ||
    !requireNamespace("network", quietly = TRUE)) {
  exit_file("sna / network not available")
}

report_side_effects(FALSE)

convert <- snafun:::convert_cug_matrix_to_graph

# --- statistics under test (all return a numeric scalar) --------------------
f_trans   <- function(g) snafun::g_transitivity(g)
f_recip   <- function(g) snafun::g_reciprocity(g)
f_cent    <- function(g) snafun::g_centralize(g, measure = "degree")$centralization
f_meanbet <- function(g) mean(snafun::v_betweenness(g))

# sna::cug.test evaluates FUN on its own representation; wrap it through
# fix_cug_input() so the same statistic is computed on an equivalent graph.
sna_wrap <- function(FUN, directed) {
  function(m) as.numeric(FUN(snafun::fix_cug_input(m, directed = directed)))
}

# --- comparison helper ------------------------------------------------------
compare_cug <- function(x, FUN, directed, cmode, reps, label) {
  mode <- if (directed) "digraph" else "graph"
  set.seed(101)
  sc <- snafun::stat_cug(x, FUN = FUN, mode = mode, cmode = cmode,
                         reps = reps, graph = "igraph")
  # sna::cug.test() does not accept igraph objects, so hand it the adjacency
  # matrix of the same graph (the observed graph is identical either way).
  x_sna <- snafun::to_matrix(x)
  set.seed(202)
  sn <- sna::cug.test(x_sna, mode = mode, FUN = sna_wrap(FUN, directed),
                      cmode = cmode, reps = reps)

  # observed statistic is deterministic and must match exactly
  expect_equal(as.numeric(sc$obs.stat), as.numeric(sn$obs.stat), tolerance = 1e-8,
               info = paste0(label, ": observed statistic matches sna::cug.test"))

  # null distributions must be the same distribution
  rep_sc <- sc$rep.stat[is.finite(sc$rep.stat)]
  rep_sn <- sn$rep.stat[is.finite(sn$rep.stat)]
  sd_pool <- stats::sd(c(rep_sc, rep_sn))
  if (is.finite(sd_pool) && sd_pool > 1e-9) {
    se <- sd_pool * sqrt(2 / reps)
    expect_true(abs(mean(rep_sc) - mean(rep_sn)) < 5 * se,
                info = paste0(label, ": null mean agrees within Monte Carlo error"))
    ratio <- stats::sd(rep_sc) / stats::sd(rep_sn)
    expect_true(ratio > 0.6 && ratio < 1.7,
                info = paste0(label, ": null spread comparable"))
  } else {
    # degenerate (constant) statistic under this conditioning: means must match
    expect_equal(mean(rep_sc), mean(rep_sn), tolerance = 1e-6,
                 info = paste0(label, ": constant null matches"))
  }
}


# --- build test graphs ------------------------------------------------------
set.seed(1)
gu <- igraph::simplify(igraph::sample_smallworld(1, 35, 3, 0.1))   # undirected, connected
igraph::V(gu)$name <- paste0("v", seq_len(igraph::vcount(gu)))
gd <- igraph::sample_gnp(30, 0.2, directed = TRUE)                 # directed
igraph::V(gd)$name <- paste0("v", seq_len(igraph::vcount(gd)))

REPS <- 300


# --- 1. UNDIRECTED, cmode = "edges" ----------------------------------------
compare_cug(gu, f_trans,   directed = FALSE, cmode = "edges", reps = REPS, label = "undir/edges/transitivity")
compare_cug(gu, f_cent,    directed = FALSE, cmode = "edges", reps = REPS, label = "undir/edges/centralization")
compare_cug(gu, f_meanbet, directed = FALSE, cmode = "edges", reps = REPS, label = "undir/edges/mean-betweenness")


# --- 2. DIRECTED, cmode = "edges" ------------------------------------------
compare_cug(gd, f_trans, directed = TRUE, cmode = "edges", reps = REPS, label = "dir/edges/transitivity")
compare_cug(gd, f_recip, directed = TRUE, cmode = "edges", reps = REPS, label = "dir/edges/reciprocity")
compare_cug(gd, f_cent,  directed = TRUE, cmode = "edges", reps = REPS, label = "dir/edges/centralization")


# --- 3. DIRECTED, cmode = "dyad.census" ------------------------------------
compare_cug(gd, f_trans, directed = TRUE, cmode = "dyad.census", reps = REPS, label = "dir/dyad.census/transitivity")
compare_cug(gd, f_cent,  directed = TRUE, cmode = "dyad.census", reps = REPS, label = "dir/dyad.census/centralization")


# --- 4. Input-type invariance ----------------------------------------------
# The observed statistic and, with a fixed seed, the full null distribution must
# be identical whether x is an igraph, network, matrix, or edgelist of gu.
inputs <- list(
  igraph   = gu,
  network  = snafun::to_network(gu),
  matrix   = snafun::to_matrix(gu),
  edgelist = snafun::to_edgelist(gu)
)
ref <- NULL
for (ty in names(inputs)) {
  set.seed(999)
  r <- snafun::stat_cug(inputs[[ty]], FUN = f_trans, mode = "graph",
                        cmode = "edges", reps = 100, graph = "igraph")
  if (is.null(ref)) {
    ref <- r
  } else {
    expect_equal(r$obs.stat, ref$obs.stat, tolerance = 1e-8,
                 info = paste0("input ", ty, ": observed statistic matches igraph input"))
    expect_equal(r$rep.stat, ref$rep.stat, tolerance = 1e-8,
                 info = paste0("input ", ty, ": full null distribution matches igraph input"))
  }
}


# --- 5. fix A: the fast igraph construction equals to_igraph() for a binary matrix
set.seed(7)
mu <- snafun::to_matrix(gu)                        # symmetric 0/1
gd_m <- snafun::to_matrix(gd)                      # asymmetric 0/1
same_graph <- function(a, b) {
  (igraph::is_directed(a) == igraph::is_directed(b)) &&
    (igraph::vcount(a) == igraph::vcount(b)) &&
    (igraph::ecount(a) == igraph::ecount(b)) &&
    isTRUE(all.equal(sort(unname(igraph::degree(a))),
                     sort(unname(igraph::degree(b)))))
}
expect_true(same_graph(convert(mu, "igraph", directed = FALSE), snafun::to_igraph(mu)),
            info = "fast igraph build == to_igraph for an undirected binary matrix")
expect_true(same_graph(convert(gd_m, "igraph", directed = TRUE), snafun::to_igraph(gd_m)),
            info = "fast igraph build == to_igraph for a directed binary matrix")
# and transitivity agrees on both
expect_equal(snafun::g_transitivity(convert(mu, "igraph", directed = FALSE)),
             snafun::g_transitivity(snafun::to_igraph(mu)), tolerance = 1e-8,
             info = "fast build gives same transitivity (undirected)")
expect_equal(snafun::g_transitivity(convert(gd_m, "igraph", directed = TRUE)),
             snafun::g_transitivity(snafun::to_igraph(gd_m)), tolerance = 1e-8,
             info = "fast build gives same transitivity (directed)")


# --- 6. Performance regression: the igraph path must beat the network path ---
gperf <- igraph::simplify(igraph::sample_smallworld(1, 200, 3, 0.05))
t_ig <- system.time(
  snafun::stat_cug(gperf, FUN = snafun::g_transitivity, mode = "graph",
                   cmode = "edges", reps = 60, graph = "igraph")
)[["elapsed"]]
t_net <- system.time(
  snafun::stat_cug(gperf, FUN = snafun::g_transitivity, mode = "graph",
                   cmode = "edges", reps = 60, graph = "network")
)[["elapsed"]]
expect_true(t_ig < t_net,
            info = paste0("igraph path faster than network path (", round(t_ig, 2),
                          "s vs ", round(t_net, 2), "s)"))

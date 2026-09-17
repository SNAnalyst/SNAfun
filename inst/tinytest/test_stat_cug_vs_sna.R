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

# sna::cug.test evaluates FUN on its own representation (an sna edge list). We
# rebuild each drawn graph the SAME canonical way stat_cug() now does for its
# igraph path -- make_empty_graph() + add_edges() with NO simplify() for directed
# graphs -- so the two implementations use the same graph construction and their
# (construction-sensitive) directed transitivity values are comparable. For
# undirected graphs the sna edge list lists every tie in both directions, so we
# collapse that duplication with simplify(remove.multiple = TRUE); undirected
# global transitivity is not construction-sensitive, so this is safe.
# (We deliberately do NOT use snafun::fix_cug_input() here: it simplifies
# directed graphs, which shifts igraph's directed global transitivity to a
# non-canonical value -- exactly the discrepancy the 2026-09-17 reroute fixed.)
canon_build_from_sna_edgelist <- function(el, directed) {
  if (is.null(dim(el))) el <- el[[1]]
  if (nrow(el) == ncol(el) && is.null(attr(el, "n"))) {
    # square sociomatrix: graph_from_adjacency_matrix() is the canonical build.
    return(igraph::graph_from_adjacency_matrix(
      el, mode = if (directed) "directed" else "undirected"))
  }
  n <- attr(el, "n")
  ee <- el[, 1:2, drop = FALSE]
  g <- igraph::make_empty_graph(n = n, directed = directed)
  if (nrow(ee) > 0) {
    g <- igraph::add_edges(g, as.vector(t(ee)))
  }
  if (!directed) {
    g <- igraph::simplify(g, remove.multiple = TRUE, remove.loops = FALSE)
  }
  g
}
sna_wrap <- function(FUN, directed) {
  function(m) as.numeric(FUN(canon_build_from_sna_edgelist(m, directed = directed)))
}

# --- comparison helper ------------------------------------------------------
compare_cug <- function(x, FUN, directed, cmode, reps, label) {
  mode <- if (directed) "digraph" else "graph"
  set.seed(101)
  sc <- snafun::stat_cug(x, FUN = FUN, mode = mode, cmode = cmode,
                         reps = reps, graph_class = "igraph")
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
                        cmode = "edges", reps = 100, graph_class = "igraph")
  if (is.null(ref)) {
    ref <- r
  } else {
    expect_equal(r$obs.stat, ref$obs.stat, tolerance = 1e-8,
                 info = paste0("input ", ty, ": observed statistic matches igraph input"))
    expect_equal(r$rep.stat, ref$rep.stat, tolerance = 1e-8,
                 info = paste0("input ", ty, ": full null distribution matches igraph input"))
  }
}


# --- 5. the fast igraph construction is the CANONICAL one -------------------
# The stat_cug igraph build must give the same transitivity as the canonical
# igraph construction graph_from_adjacency_matrix() for both undirected AND
# directed graphs (guaranteed by the 2026-09-17 change: no simplify()).
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
canon_u <- igraph::graph_from_adjacency_matrix(mu, mode = "undirected")
canon_d <- igraph::graph_from_adjacency_matrix(gd_m, mode = "directed")
expect_true(same_graph(convert(mu, "igraph", directed = FALSE), canon_u),
            info = "fast igraph build == graph_from_adjacency (undirected)")
expect_true(same_graph(convert(gd_m, "igraph", directed = TRUE), canon_d),
            info = "fast igraph build == graph_from_adjacency (directed)")
# transitivity matches the canonical construction on both
expect_equal(igraph::transitivity(convert(mu, "igraph", directed = FALSE), type = "global"),
             igraph::transitivity(canon_u, type = "global"), tolerance = 1e-8,
             info = "fast build gives canonical transitivity (undirected)")
expect_equal(igraph::transitivity(convert(gd_m, "igraph", directed = TRUE), type = "global"),
             igraph::transitivity(canon_d, type = "global"), tolerance = 1e-8,
             info = "fast build gives canonical transitivity (directed)")


# --- 6. Performance regression: the igraph path must beat the network path ---
gperf <- igraph::simplify(igraph::sample_smallworld(1, 200, 3, 0.05))
t_ig <- system.time(
  snafun::stat_cug(gperf, FUN = snafun::g_transitivity, mode = "graph",
                   cmode = "edges", reps = 60, graph_class = "igraph")
)[["elapsed"]]
t_net <- system.time(
  snafun::stat_cug(gperf, FUN = snafun::g_transitivity, mode = "graph",
                   cmode = "edges", reps = 60, graph_class = "network")
)[["elapsed"]]
expect_true(t_ig < t_net,
            info = paste0("igraph path faster than network path (", round(t_ig, 2),
                          "s vs ", round(t_net, 2), "s)"))


# --- 7. igraph-native replicate sampler conditions correctly ----------------
# draw_cug_replicate_igraph() must honour the conditioning: correct vertex count,
# correct directedness, and (for cmode = "edges") the exact edge count, for every
# cmode. dyad.census draws via sna::rguman and must preserve the dyad census.
draw_ig <- snafun:::draw_cug_replicate_igraph
prep    <- snafun:::precompute_cug_target

# cmode = "edges", directed: exact arc count and directedness preserved
set.seed(3)
md <- snafun::to_matrix(gd); diag(md) <- 0
tgt_e_d <- prep(md, mode = "digraph", cmode = "edges", diag = FALSE)
for (i in 1:20) {
  g <- draw_ig(tgt_e_d, directed = TRUE)
  expect_equal(igraph::vcount(g), nrow(md), info = "edges/dir: vertex count")
  expect_true(igraph::is_directed(g), info = "edges/dir: directed")
  expect_equal(igraph::ecount(g), sum(md != 0), info = "edges/dir: exact arc count")
}

# cmode = "edges", undirected: exact (undirected) edge count and undirectedness
mu2 <- snafun::to_matrix(gu); diag(mu2) <- 0
tgt_e_u <- prep(mu2, mode = "graph", cmode = "edges", diag = FALSE)
m_u <- sum(mu2[upper.tri(mu2)] != 0)
for (i in 1:20) {
  g <- draw_ig(tgt_e_u, directed = FALSE)
  expect_equal(igraph::vcount(g), nrow(mu2), info = "edges/undir: vertex count")
  expect_false(igraph::is_directed(g), info = "edges/undir: undirected")
  expect_equal(igraph::ecount(g), m_u, info = "edges/undir: exact edge count")
}

# cmode = "size": correct n and directedness (edge count is random)
tgt_s <- prep(md, mode = "digraph", cmode = "size", diag = FALSE)
for (i in 1:10) {
  g <- draw_ig(tgt_s, directed = TRUE)
  expect_equal(igraph::vcount(g), nrow(md), info = "size: vertex count")
  expect_true(igraph::is_directed(g), info = "size: directed")
}

# cmode = "dyad.census": the dyad census of each replicate matches the target
tgt_dc <- prep(md, mode = "digraph", cmode = "dyad.census", diag = FALSE)
obs_dc <- suppressWarnings(sna::dyad.census(md))
for (i in 1:10) {
  g <- draw_ig(tgt_dc, directed = TRUE)
  expect_equal(igraph::vcount(g), nrow(md), info = "dyad.census: vertex count")
  dc <- suppressWarnings(sna::dyad.census(snafun::to_matrix(g)))
  expect_equal(as.numeric(dc), as.numeric(obs_dc),
               info = "dyad.census: dyad census preserved")
}

# The igraph sampler path gives the SAME null distribution as the sna dense-matrix
# path for a construction-INSENSITIVE statistic (density), for all three cmodes.
for (cm in c("size", "edges", "dyad.census")) {
  set.seed(77)
  r_ig  <- snafun::stat_cug(gd, FUN = snafun::g_density, mode = "digraph",
                            cmode = cm, reps = 300, graph_class = "igraph")
  set.seed(77)
  r_mat <- snafun::stat_cug(gd, FUN = snafun::g_density, mode = "digraph",
                            cmode = cm, reps = 300, graph_class = "matrix")
  # density is construction-insensitive, so observed must match exactly
  expect_equal(r_ig$obs.stat, r_mat$obs.stat, tolerance = 1e-10,
               info = paste0("sampler vs dense: observed density matches (", cm, ")"))
  # and the null means must agree within Monte Carlo error
  sdp <- stats::sd(c(r_ig$rep.stat, r_mat$rep.stat))
  if (is.finite(sdp) && sdp > 1e-9) {
    se <- sdp * sqrt(2 / 300)
    expect_true(abs(mean(r_ig$rep.stat) - mean(r_mat$rep.stat)) < 5 * se,
                info = paste0("sampler vs dense: null density mean agrees (", cm, ")"))
  }
}

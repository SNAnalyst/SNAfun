# Extensive tests for stat_qap_cor() (2026-09-18).
#
# Covers, across many network kinds:
#   * agreement of the observed statistic with sna::gcor(),
#   * agreement of the permutation p-values with sna::qaptest(),
#   * correctness of the (partial) correlation against an independent reference,
#   * pearson and spearman, one and several control matrices,
#   * invariance across input classes (igraph / network / matrix / edgelist),
#   * determinism and RNG-seed restoration,
#   * edge cases (zero variance) and p-value bookkeeping,
#   * and that the precompute optimization (2026-09-18) is result-identical to a
#     from-scratch recomputation.

if (!requireNamespace("sna", quietly = TRUE) ||
    !requireNamespace("network", quietly = TRUE) ||
    !requireNamespace("igraph", quietly = TRUE)) {
  exit_file("sna / network / igraph not available")
}

report_side_effects(FALSE)
set.seed(20260918)

eqnum <- function(a, b, tol = 1e-8) {
  isTRUE(all.equal(unname(as.numeric(a)), unname(as.numeric(b)), tolerance = tol))
}

mk_dir <- function(n, p = 0.3) {
  m <- matrix(stats::rbinom(n * n, 1, p), n)
  diag(m) <- 0
  m
}
mk_undir <- function(n, p = 0.3) {
  m <- mk_dir(n, p)
  m[lower.tri(m)] <- t(m)[lower.tri(m)]
  m
}
mk_dir_w <- function(n, p = 0.3) {
  m <- mk_dir(n, p)
  m[m != 0] <- round(stats::runif(sum(m != 0), 1, 9), 2)
  m
}

# --- independent reference implementation of the observed QAP correlation ----
ref_qap_cor <- function(A, B, controls = list(), method = "pearson",
                        diagonal = FALSE) {
  sel <- matrix(TRUE, nrow(A), ncol(A))
  if (!diagonal) diag(sel) <- FALSE
  xv <- as.numeric(A[sel]); yv <- as.numeric(B[sel])
  if (method == "spearman") { xv <- rank(xv); yv <- rank(yv) }
  if (length(controls) == 0L) {
    if (stats::sd(xv) == 0 || stats::sd(yv) == 0) return(NA_real_)
    return(stats::cor(xv, yv))
  }
  Z <- vapply(controls, function(m) as.numeric(m[sel]), numeric(sum(sel)))
  Z <- matrix(Z, nrow = sum(sel))
  if (method == "spearman") Z <- matrix(apply(Z, 2, rank), nrow = sum(sel))
  xr <- stats::resid(stats::lm(xv ~ Z))
  yr <- stats::resid(stats::lm(yv ~ Z))
  if (stats::sd(xr) == 0 || stats::sd(yr) == 0) return(NA_real_)
  stats::cor(xr, yr)
}


# =============================================================================
# 1. Observed statistic == sna::gcor (where the conventions coincide)
# =============================================================================
for (rep_i in 1:6) {
  n <- sample(10:22, 1)
  Ad <- mk_dir(n); Bd <- mk_dir(n)
  Au <- mk_undir(n); Bu <- mk_undir(n)
  # directed: agrees for both diagonal settings
  for (dg in c(FALSE, TRUE)) {
    s <- snafun::stat_qap_cor(Ad, Bd, reps = 1, directed = "directed", diagonal = dg)$obs.stat
    g <- sna::gcor(Ad, Bd, mode = "digraph", diag = dg)
    expect_true(eqnum(s, g), info = paste0("obs == sna::gcor directed diag=", dg))
  }
  # undirected: agrees WITHOUT the diagonal (documented divergence with diagonal)
  s <- snafun::stat_qap_cor(Au, Bu, reps = 1, directed = "undirected", diagonal = FALSE)$obs.stat
  g <- sna::gcor(Au, Bu, mode = "graph", diag = FALSE)
  expect_true(eqnum(s, g), info = "obs == sna::gcor undirected diag=FALSE")
}

# weighted directed also matches gcor
Aw <- mk_dir_w(18); Bw <- mk_dir_w(18)
expect_true(eqnum(snafun::stat_qap_cor(Aw, Bw, reps = 1, directed = "directed")$obs.stat,
                  sna::gcor(Aw, Bw, mode = "digraph", diag = FALSE)),
            info = "weighted obs == sna::gcor")


# =============================================================================
# 2. Observed statistic == independent reference (partial, pearson & spearman)
# =============================================================================
for (rep_i in 1:5) {
  n <- sample(12:20, 1)
  A <- mk_dir(n); B <- mk_dir(n); C <- mk_dir(n); D <- mk_dir(n)
  for (mth in c("pearson", "spearman")) {
    # no controls
    expect_true(eqnum(snafun::stat_qap_cor(A, B, method = mth, reps = 1, directed = "directed")$obs.stat,
                      ref_qap_cor(A, B, method = mth)),
                info = paste0("obs == reference, no controls, ", mth))
    # one control
    expect_true(eqnum(snafun::stat_qap_cor(A, B, controls = C, method = mth, reps = 1, directed = "directed")$obs.stat,
                      ref_qap_cor(A, B, list(C), method = mth)),
                info = paste0("obs == reference, 1 control, ", mth))
    # two controls
    expect_true(eqnum(snafun::stat_qap_cor(A, B, controls = list(C, D), method = mth, reps = 1, directed = "directed")$obs.stat,
                      ref_qap_cor(A, B, list(C, D), method = mth)),
                info = paste0("obs == reference, 2 controls, ", mth))
  }
}

# spearman no-controls equals ordinary Spearman correlation of the off-diagonal cells
A <- mk_dir_w(16); B <- mk_dir_w(16)
sel <- matrix(TRUE, 16, 16); diag(sel) <- FALSE
expect_true(eqnum(snafun::stat_qap_cor(A, B, method = "spearman", reps = 1, directed = "directed")$obs.stat,
                  stats::cor(as.numeric(A[sel]), as.numeric(B[sel]), method = "spearman")),
            info = "spearman obs == cor(method='spearman')")


# =============================================================================
# 3. p-values agree with sna::qaptest (distribution), directed & undirected
# =============================================================================
check_vs_qaptest <- function(A, B, mode, label, reps = 2000) {
  sn <- snafun::stat_qap_cor(A, B, reps = reps,
                             directed = if (mode == "digraph") "directed" else "undirected")
  qt <- sna::qaptest(list(A, B), sna::gcor, g1 = 1, g2 = 2, reps = reps)
  # observed statistic is deterministic -> exact
  expect_true(eqnum(sn$obs.stat, qt$testval, tol = 1e-8),
              info = paste0(label, ": observed matches qaptest"))
  # null means agree within Monte Carlo error
  se <- stats::sd(c(sn$rep.stat, qt$dist)) * sqrt(2 / reps)
  expect_true(abs(mean(sn$rep.stat) - mean(qt$dist)) < 6 * se,
              info = paste0(label, ": null mean agrees with qaptest"))
  # tail probabilities agree within a few percent
  expect_true(abs((sn$p.greater + sn$p.equal) - qt$pgreq) < 0.06,
              info = paste0(label, ": Pr(>=obs) agrees with qaptest pgreq"))
  expect_true(abs((sn$p.less + sn$p.equal) - qt$pleeq) < 0.06,
              info = paste0(label, ": Pr(<=obs) agrees with qaptest pleeq"))
}
set.seed(11)
check_vs_qaptest(mk_dir(20), mk_dir(20), "digraph", "directed")
check_vs_qaptest(mk_undir(20), mk_undir(20), "graph", "undirected")
# correlated pair: small p on the correct side
Abase <- mk_dir(24)
Bcorr <- Abase; flip <- sample(which(row(Abase) != col(Abase)), 20)
Bcorr[flip] <- 1 - Bcorr[flip]        # perturb a few cells -> strong positive assoc
res_corr <- snafun::stat_qap_cor(Abase, Bcorr, reps = 999, directed = "directed",
                                 alternative = "greater")
expect_true(res_corr$obs.stat > 0.5, info = "constructed pair is strongly correlated")
expect_true(res_corr$p.value < 0.01, info = "strong association -> small greater-tail p")


# =============================================================================
# 4. Invariance across input classes (igraph / network / matrix / edgelist)
# =============================================================================
Ad <- mk_dir(18); Bd <- mk_dir(18)
Ai <- snafun::to_igraph(Ad); Bi <- snafun::to_igraph(Bd)
inputs_A <- list(matrix = Ad, igraph = Ai, network = snafun::to_network(Ad),
                 edgelist = snafun::to_edgelist(Ai))
inputs_B <- list(matrix = Bd, igraph = Bi, network = snafun::to_network(Bd),
                 edgelist = snafun::to_edgelist(Bi))
ref <- snafun::stat_qap_cor(Ad, Bd, reps = 50, directed = "directed", seed = 314)
for (ty in names(inputs_A)) {
  r <- snafun::stat_qap_cor(inputs_A[[ty]], inputs_B[[ty]], reps = 50,
                            directed = "directed", seed = 314)
  expect_true(eqnum(r$obs.stat, ref$obs.stat),
              info = paste0("input ", ty, ": observed matches matrix input"))
  expect_true(eqnum(r$rep.stat, ref$rep.stat),
              info = paste0("input ", ty, ": null distribution matches matrix input"))
}


# =============================================================================
# 5. Determinism and RNG-seed restoration
# =============================================================================
A <- mk_dir(15); B <- mk_dir(15)
r1 <- snafun::stat_qap_cor(A, B, reps = 200, directed = "directed", seed = 99)
r2 <- snafun::stat_qap_cor(A, B, reps = 200, directed = "directed", seed = 99)
expect_true(eqnum(r1$rep.stat, r2$rep.stat), info = "same seed -> identical null distribution")
expect_equal(r1$obs.stat, r2$obs.stat, info = "same seed -> identical observed")

# a seeded call must not disturb the caller's global RNG stream
set.seed(2024)
before <- .Random.seed
invisible(snafun::stat_qap_cor(A, B, reps = 50, directed = "directed", seed = 7))
after <- .Random.seed
expect_identical(before, after, info = "seeded call restores the global RNG state")


# =============================================================================
# 6. Precompute optimization is result-identical to full recomputation
# =============================================================================
# Independently recompute the whole permutation distribution from scratch (build
# nothing once) and check it equals what stat_qap_cor() returns for the same
# permutations. We drive both with the same seed so the permutations coincide.
A <- mk_dir(16); B <- mk_dir(16); C <- mk_dir(16); D <- mk_dir(16)
for (mth in c("pearson", "spearman")) {
  for (ctrl in list(list(), list(C), list(C, D))) {
    res <- snafun::stat_qap_cor(A, B, controls = if (length(ctrl)) ctrl else NULL,
                                method = mth, reps = 100, directed = "directed", seed = 55)
    # replay: same seed, same sample.int() draws, recompute each rep from scratch
    set.seed(55)
    manual_obs <- ref_qap_cor(A, B, ctrl, method = mth)
    manual_rep <- numeric(100)
    for (i in seq_len(100)) {
      pm <- sample.int(nrow(B))
      manual_rep[[i]] <- ref_qap_cor(A, B[pm, pm, drop = FALSE], ctrl, method = mth)
    }
    expect_true(eqnum(res$obs.stat, manual_obs, tol = 1e-8),
                info = paste0("opt obs == from-scratch (", mth, ", ", length(ctrl), " controls)"))
    expect_true(eqnum(res$rep.stat, manual_rep, tol = 1e-8),
                info = paste0("opt null == from-scratch (", mth, ", ", length(ctrl), " controls)"))
  }
}


# =============================================================================
# 7. p-value bookkeeping and alternatives
# =============================================================================
res <- snafun::stat_qap_cor(mk_dir(20), mk_dir(20), reps = 500, directed = "directed")
# strict-greater + strict-less + equal over valid reps sums to 1
expect_equal(res$p.greater + res$p.less + res$p.equal, 1, tolerance = 1e-8,
             info = "p.greater + p.less + p.equal == 1")
expect_true(res$p.value >= 0 && res$p.value <= 1, info = "two-sided p in [0,1]")
# alternatives are consistent with the tail probabilities
A <- mk_dir(20); B <- mk_dir(20)
rg <- snafun::stat_qap_cor(A, B, reps = 400, directed = "directed", alternative = "greater", seed = 3)
rl <- snafun::stat_qap_cor(A, B, reps = 400, directed = "directed", alternative = "less", seed = 3)
rt <- snafun::stat_qap_cor(A, B, reps = 400, directed = "directed", alternative = "two.sided", seed = 3)
expect_equal(rg$p.value, rg$p.greater + rg$p.equal, tolerance = 1e-8,
             info = "greater alternative p == Pr(>=obs)")
expect_equal(rl$p.value, rl$p.less + rl$p.equal, tolerance = 1e-8,
             info = "less alternative p == Pr(<=obs)")
expect_equal(rt$p.value,
             min(1, 2 * min(rg$p.greater + rg$p.equal, rl$p.less + rl$p.equal)),
             tolerance = 1e-8, info = "two-sided p == 2*min(tails)")


# =============================================================================
# 8. Edge cases: zero-variance input -> NA observed and NA p-values
# =============================================================================
empty <- matrix(0, 10, 10)
A <- mk_dir(10)
res_na <- snafun::stat_qap_cor(A, empty, reps = 20, directed = "directed")
expect_true(is.na(res_na$obs.stat), info = "zero-variance y -> NA observed")
expect_true(is.na(res_na$p.value), info = "zero-variance y -> NA p.value")


# =============================================================================
# 9. Efficiency: with controls, per-replication cost must not explode
# =============================================================================
# The precompute means the controls case is only modestly more expensive than
# the no-controls case (it used to re-residualize x every replication). We assert
# a generous bound to stay robust on loaded machines, but one the old per-rep
# re-residualization would have blown on a larger graph.
n <- 120
A <- mk_dir(n); B <- mk_dir(n); C <- mk_dir(n); D <- mk_dir(n)
t_no <- system.time(snafun::stat_qap_cor(A, B, reps = 300, directed = "directed"))[["elapsed"]]
t_ct <- system.time(snafun::stat_qap_cor(A, B, controls = list(C, D), reps = 300,
                                         directed = "directed"))[["elapsed"]]
expect_true(t_ct < 6 * t_no + 3,
            info = paste0("controls case not dramatically slower (no=", round(t_no, 2),
                          "s, controls=", round(t_ct, 2), "s)"))

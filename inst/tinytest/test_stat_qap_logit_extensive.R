# Extensive tests for stat_qap_logit() (2026-09-18).
#
# stat_qap_logit() is a snafun-style wrapper around sna::netlogit(). These tests
# verify that:
#   * the fit (coefficients, standard errors, deviance, confusion table) and the
#     permutation distribution match sna::netlogit() exactly for the same seed,
#   * observed z-values equal coef / se and exp.beta equals exp(coef),
#   * the classification measures (accuracy/sensitivity/specificity/precision)
#     use the correct confusion-table orientation (checked against a manual
#     predicted-vs-observed computation on a non-degenerate model),
#   * p.equal is computed correctly from netlogit's pgreq/pleeq
#     (p.equal = max(0, pgreq + pleeq - 1)), and p.greater/p.less map to
#     pgreq/pleeq,
#   * results are invariant across input classes (igraph/network/matrix/edgelist),
#   * runs are deterministic under a seed and the global RNG is restored,
#   * and the binary-response validation and single-predictor path behave.

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
mk_dir <- function(n, p = 0.3) { m <- matrix(stats::rbinom(n * n, 1, p), n); diag(m) <- 0; m }
mk_undir <- function(n, p = 0.3) { m <- mk_dir(n, p); m[lower.tri(m)] <- t(m)[lower.tri(m)]; m }


# =============================================================================
# 1. Fit and permutation distribution match sna::netlogit exactly (same seed)
# =============================================================================
for (mode_i in c("digraph", "graph")) {
  n <- 22
  mk <- if (mode_i == "digraph") mk_dir else mk_undir
  set.seed(100 + nchar(mode_i))
  Y <- mk(n); X1 <- mk(n); X2 <- mk(n)
  dir_arg <- if (mode_i == "digraph") "directed" else "undirected"

  sn <- snafun::stat_qap_logit(Y, x = list(X1, X2), reps = 200,
                               directed = dir_arg, nullhyp = "qapy",
                               test.statistic = "beta", seed = 21)
  set.seed(21)
  nl <- sna::netlogit(Y, list(X1, X2), mode = mode_i, nullhyp = "qapy",
                      test.statistic = "beta", reps = 200)

  expect_true(eqnum(sn$coefficients, nl$coefficients), info = paste0(mode_i, ": coef == netlogit"))
  expect_true(eqnum(sn$se, nl$se), info = paste0(mode_i, ": se == netlogit"))
  expect_true(eqnum(sn$deviance, nl$deviance), info = paste0(mode_i, ": deviance == netlogit"))
  expect_true(eqnum(sn$null.deviance, nl$null.deviance), info = paste0(mode_i, ": null deviance == netlogit"))
  expect_true(eqnum(as.numeric(unclass(sn$confusion.table)), as.numeric(unclass(nl$ctable))),
              info = paste0(mode_i, ": confusion table == netlogit"))
  # permutation distribution of betas matches
  expect_true(eqnum(sn$beta.dist, nl$dist), info = paste0(mode_i, ": beta permutation dist == netlogit"))
  # p-values map to netlogit's pgreq / pleeq / pgreqabs
  expect_true(eqnum(sn$p.beta$p.greater, nl$pgreq), info = paste0(mode_i, ": p.greater == pgreq"))
  expect_true(eqnum(sn$p.beta$p.less, nl$pleeq), info = paste0(mode_i, ": p.less == pleeq"))
  expect_true(eqnum(sn$p.beta$p.two.sided, nl$pgreqabs), info = paste0(mode_i, ": p.two.sided == pgreqabs"))
  # p.equal is the correct tie probability = max(0, pgreq + pleeq - 1)
  expect_true(eqnum(sn$p.beta$p.equal, pmax(0, as.numeric(nl$pgreq) + as.numeric(nl$pleeq) - 1)),
              info = paste0(mode_i, ": p.equal == max(0, pgreq+pleeq-1)"))
}


# =============================================================================
# 2. Observed z-values and exp(beta)
# =============================================================================
set.seed(5)
Y <- mk_dir(20); X1 <- mk_dir(20); X2 <- mk_dir(20)
res <- snafun::stat_qap_logit(Y, x = list(X1, X2), reps = 50, directed = "directed", seed = 3)
expect_true(eqnum(res$z.stat, res$coefficients / res$se), info = "z == coef / se")
expect_true(eqnum(res$exp.beta, exp(res$coefficients)), info = "exp.beta == exp(coef)")
expect_true(all(is.finite(unlist(res$pseudo.r2))), info = "pseudo-R2 finite")


# =============================================================================
# 3. Classification measures use the correct orientation (non-degenerate model)
# =============================================================================
set.seed(9)
n <- 26
X1 <- mk_dir(n)
Y <- X1
flip <- sample(which(row(Y) != col(Y)), 30)
Y[flip] <- 1 - Y[flip]                 # Y strongly tracks X1 -> model predicts some 1s
res <- snafun::stat_qap_logit(Y, x = X1, reps = 50, directed = "directed", nullhyp = "qapy", seed = 1)
cls <- snafun:::stat_qap_logit_classification(res)
sel <- row(Y) != col(Y)
yv <- as.numeric(Y[sel]); pv <- as.numeric(res$fitted.values > 0.5)
TP <- sum(pv == 1 & yv == 1); TN <- sum(pv == 0 & yv == 0)
FP <- sum(pv == 1 & yv == 0); FN <- sum(pv == 0 & yv == 1)
expect_true(eqnum(cls$accuracy, (TP + TN) / length(yv)), info = "accuracy oriented correctly")
expect_true(eqnum(cls$sensitivity, TP / (TP + FN)), info = "sensitivity oriented correctly")
expect_true(eqnum(cls$specificity, TN / (TN + FP)), info = "specificity oriented correctly")
expect_true(eqnum(cls$precision, TP / (TP + FP)), info = "precision oriented correctly")
# and the model actually predicts some ties here (so the test is meaningful)
expect_true(sum(pv) > 0, info = "non-degenerate model predicts some ties")


# =============================================================================
# 4. Invariance across input classes
# =============================================================================
set.seed(4)
Yd <- mk_dir(18); Xd <- mk_dir(18)
Yi <- snafun::to_igraph(Yd); Xi <- snafun::to_igraph(Xd)
inputs_y <- list(matrix = Yd, igraph = Yi, network = snafun::to_network(Yd),
                 edgelist = snafun::to_edgelist(Yi))
inputs_x <- list(matrix = Xd, igraph = Xi, network = snafun::to_network(Xd),
                 edgelist = snafun::to_edgelist(Xi))
ref <- snafun::stat_qap_logit(Yd, x = Xd, reps = 40, directed = "directed", seed = 202)
for (ty in names(inputs_y)) {
  r <- snafun::stat_qap_logit(inputs_y[[ty]], x = inputs_x[[ty]], reps = 40,
                              directed = "directed", seed = 202)
  expect_true(eqnum(r$coefficients, ref$coefficients), info = paste0("input ", ty, ": coef matches"))
  expect_true(eqnum(r$se, ref$se), info = paste0("input ", ty, ": se matches"))
  expect_true(eqnum(r$beta.dist, ref$beta.dist), info = paste0("input ", ty, ": perm dist matches"))
}


# =============================================================================
# 5. Determinism and RNG-seed restoration
# =============================================================================
Y <- mk_dir(16); X <- mk_dir(16)
r1 <- snafun::stat_qap_logit(Y, x = X, reps = 100, directed = "directed", seed = 77)
r2 <- snafun::stat_qap_logit(Y, x = X, reps = 100, directed = "directed", seed = 77)
expect_true(eqnum(r1$beta.dist, r2$beta.dist), info = "same seed -> identical permutation distribution")
expect_true(eqnum(r1$coefficients, r2$coefficients), info = "same seed -> identical coefficients")

set.seed(2024)
before <- .Random.seed
invisible(snafun::stat_qap_logit(Y, x = X, reps = 30, directed = "directed", seed = 7))
after <- .Random.seed
expect_identical(before, after, info = "seeded call restores the global RNG state")


# =============================================================================
# 6. Edge cases and argument validation
# =============================================================================
# non-binary dependent network -> error
Y_val <- mk_dir(12); Y_val[Y_val == 1] <- 2
expect_error(
  snafun::stat_qap_logit(Y_val, x = mk_dir(12), reps = 10, directed = "directed"),
  "binary"
)
# a non-binary value only on the diagonal is fine when diagonal = FALSE
Y_ok <- mk_dir(12); diag(Y_ok) <- 5
expect_silent_fit <- tryCatch({
  snafun::stat_qap_logit(Y_ok, x = mk_dir(12), reps = 10, directed = "directed", diagonal = FALSE)
  TRUE
}, error = function(e) FALSE)
expect_true(expect_silent_fit, info = "non-binary diagonal ignored when diagonal = FALSE")

# single predictor path returns one slope + intercept
res1 <- snafun::stat_qap_logit(mk_dir(15), x = mk_dir(15), reps = 30,
                               directed = "directed", seed = 8)
expect_equal(length(res1$coefficients), 2L, info = "single predictor -> intercept + 1 slope")
expect_equal(names(res1$coefficients)[[1]], "(intercept)", info = "first coefficient is the intercept")


# =============================================================================
# 7. p-value bookkeeping
# =============================================================================
res <- snafun::stat_qap_logit(mk_dir(20), x = list(mk_dir(20), mk_dir(20)),
                              reps = 200, directed = "directed",
                              test.statistic = "beta", seed = 6)
# all reported probabilities lie in [0, 1]
for (comp in c("p.greater", "p.less", "p.equal", "p.two.sided")) {
  vals <- res$p.beta[[comp]]
  expect_true(all(vals >= -1e-9 & vals <= 1 + 1e-9), info = paste0(comp, " in [0,1]"))
}

report_side_effects()



compare_cug_to_sna_reference <- function(mat, mode, cmode, diag, reps) {
  set.seed(1401)
  ours <- snafun::stat_cug(
    x = mat,
    FUN = sna::gden,
    mode = mode,
    cmode = cmode,
    diag = diag,
    reps = reps,
    graph = "matrix"
  )
  set.seed(1401)
  theirs <- sna::cug.test(
    dat = mat,
    FUN = sna::gden,
    mode = mode,
    cmode = cmode,
    diag = diag,
    reps = reps
  )

  expect_equal(ours$obs.stat, theirs$obs.stat, tolerance = 1e-12)
  expect_equal(ours$rep.stat, theirs$rep.stat, tolerance = 1e-12)
  expect_equal(ours$plteobs, theirs$plteobs, tolerance = 1e-12)
  expect_equal(ours$pgteobs, theirs$pgteobs, tolerance = 1e-12)
}



set.seed(20260418)
for (directed in c(FALSE, TRUE)) {
  mode <- if (directed) "digraph" else "graph"
  for (simulation in seq_len(8)) {
      n <- sample(4:7, size = 1)
      density <- stats::runif(1, min = 0.1, max = 0.6)

      if (directed) {
        mat <- matrix(
          stats::rbinom(n * n, size = 1, prob = density),
          nrow = n
        )
      } else {
        upper <- matrix(0, nrow = n, ncol = n)
        upper[upper.tri(upper)] <- stats::rbinom(
          n = n * (n - 1) / 2,
          size = 1,
          prob = density
        )
        mat <- upper + t(upper)
      }

      # The audit below is specifically about parity with sna::cug.test() for
      # common structural CUG use. Loop-permitted undirected cases are covered
      # separately in the focused regression tests, because sna::cug.test()
      # evaluates those through its own internal edgelist representation.
      diag(mat) <- 0

      # Keep the graphs one-mode and binary, but vary sparsity and isolates.
      if (simulation %% 3 == 0) {
        isolate <- sample.int(n, size = 1)
        mat[isolate, ] <- 0
        mat[, isolate] <- 0
      }

      for (current_cmode in c("size", "edges", "dyad.census")) {
        compare_cug_to_sna_reference(
          mat = mat,
          mode = mode,
          cmode = current_cmode,
          diag = FALSE,
          reps = 15
        )
      }
    }
}

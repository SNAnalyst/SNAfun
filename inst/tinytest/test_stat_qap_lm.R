report_side_effects()



qap_lm_make_array <- function(...) {
  matrices <- list(...)
  out <- array(
    data = 0,
    dim = c(length(matrices), nrow(matrices[[1]]), ncol(matrices[[1]]))
  )
  for (index in seq_along(matrices)) {
    out[index, , ] <- matrices[[index]]
  }
  out
}



expect_qap_lm_matches_netlm <- function(y_matrix,
                                        predictors,
                                        directed,
                                        diagonal = FALSE,
                                        reps = 15,
                                        seed = 123,
                                        nullhyp = "qapspp") {
  predictor_array <- do.call(qap_lm_make_array, predictors)
  mode <- if (identical(directed, "directed")) "digraph" else "graph"

  ours_beta <- snafun::stat_qap_lm(
    y = y_matrix,
    x = predictors,
    reps = reps,
    seed = seed,
    directed = directed,
    diagonal = diagonal,
    test.statistic = "beta",
    nullhyp = nullhyp
  )
  set.seed(seed)
  theirs_beta <- sna::netlm(
    y = y_matrix,
    x = predictor_array,
    mode = mode,
    diag = diagonal,
    nullhyp = nullhyp,
    test.statistic = "beta",
    reps = reps
  )

  expect_equal(unname(ours_beta$coefficients), as.numeric(theirs_beta$coefficients), tolerance = 1e-12)
  expect_equal(unname(ours_beta$beta.dist), unname(theirs_beta$dist), tolerance = 1e-12)
  expect_equal(ours_beta$nullhyp, theirs_beta$nullhyp)

  ours_t <- snafun::stat_qap_lm(
    y = y_matrix,
    x = predictors,
    reps = reps,
    seed = seed,
    directed = directed,
    diagonal = diagonal,
    test.statistic = "t-value",
    nullhyp = nullhyp
  )
  expect_equal(length(ours_t$t.stat), length(ours_t$coefficients))
  expect_equal(ncol(ours_t$t.dist), length(ours_t$coefficients))
}



expect_qap_lm_format_parity <- function(y_matrix,
                                        predictors,
                                        directed,
                                        diagonal = FALSE,
                                        nullhyp = "qapspp") {
  matrix_fit <- snafun::stat_qap_lm(
    y = y_matrix,
    x = predictors,
    reps = 19,
    seed = 42,
    directed = directed,
    diagonal = diagonal,
    nullhyp = nullhyp
  )

  igraph_fit <- snafun::stat_qap_lm(
    y = snafun::to_igraph(y_matrix),
    x = lapply(predictors, snafun::to_igraph),
    reps = 19,
    seed = 42,
    directed = directed,
    diagonal = diagonal,
    nullhyp = nullhyp
  )

  network_fit <- snafun::stat_qap_lm(
    y = snafun::to_network(y_matrix),
    x = lapply(predictors, snafun::to_network),
    reps = 19,
    seed = 42,
    directed = directed,
    diagonal = diagonal,
    nullhyp = nullhyp
  )

  edgelist_fit <- snafun::stat_qap_lm(
    y = snafun::to_edgelist(y_matrix),
    x = lapply(predictors, snafun::to_edgelist),
    reps = 19,
    seed = 42,
    directed = directed,
    diagonal = diagonal,
    nullhyp = nullhyp
  )

  expect_equal(igraph_fit$coefficients, matrix_fit$coefficients, tolerance = 1e-12)
  expect_equal(network_fit$coefficients, matrix_fit$coefficients, tolerance = 1e-12)
  expect_equal(edgelist_fit$coefficients, matrix_fit$coefficients, tolerance = 1e-12)

  expect_equal(igraph_fit$t.stat, matrix_fit$t.stat, tolerance = 1e-12)
  expect_equal(network_fit$t.stat, matrix_fit$t.stat, tolerance = 1e-12)
  expect_equal(edgelist_fit$t.stat, matrix_fit$t.stat, tolerance = 1e-12)

  expect_equal(igraph_fit$beta.dist, matrix_fit$beta.dist, tolerance = 1e-12)
  expect_equal(network_fit$beta.dist, matrix_fit$beta.dist, tolerance = 1e-12)
  expect_equal(edgelist_fit$beta.dist, matrix_fit$beta.dist, tolerance = 1e-12)

  expect_equal(igraph_fit$t.dist, matrix_fit$t.dist, tolerance = 1e-12)
  expect_equal(network_fit$t.dist, matrix_fit$t.dist, tolerance = 1e-12)
  expect_equal(edgelist_fit$t.dist, matrix_fit$t.dist, tolerance = 1e-12)
}



directed_y <- matrix(
  c(0, 1, 0, 2,
    0, 0, 1, 0,
    1, 0, 0, 1,
    0, 2, 0, 0),
  nrow = 4,
  byrow = TRUE
)
rownames(directed_y) <- LETTERS[1:4]
colnames(directed_y) <- LETTERS[1:4]

directed_x1 <- matrix(
  c(0, 2, 1, 0,
    1, 0, 0, 1,
    0, 1, 0, 1,
    2, 0, 1, 0),
  nrow = 4,
  byrow = TRUE
)
rownames(directed_x1) <- LETTERS[1:4]
colnames(directed_x1) <- LETTERS[1:4]

directed_x2 <- matrix(
  c(0, 0, 3, 1,
    2, 0, 1, 0,
    0, 1, 0, 2,
    1, 0, 0, 0),
  nrow = 4,
  byrow = TRUE
)
rownames(directed_x2) <- LETTERS[1:4]
colnames(directed_x2) <- LETTERS[1:4]

undirected_y <- matrix(
  c(0, 1, 0, 2,
    1, 0, 1, 0,
    0, 1, 0, 1,
    2, 0, 1, 0),
  nrow = 4,
  byrow = TRUE
)
rownames(undirected_y) <- LETTERS[1:4]
colnames(undirected_y) <- LETTERS[1:4]

undirected_x1 <- matrix(
  c(0, 2, 1, 0,
    2, 0, 0, 1,
    1, 0, 0, 2,
    0, 1, 2, 0),
  nrow = 4,
  byrow = TRUE
)
rownames(undirected_x1) <- LETTERS[1:4]
colnames(undirected_x1) <- LETTERS[1:4]

undirected_x2 <- matrix(
  c(0, 0, 1, 3,
    0, 0, 2, 1,
    1, 2, 0, 0,
    3, 1, 0, 0),
  nrow = 4,
  byrow = TRUE
)
rownames(undirected_x2) <- LETTERS[1:4]
colnames(undirected_x2) <- LETTERS[1:4]



# Both supported null models should match sna::netlm() exactly.
expect_qap_lm_matches_netlm(
  y_matrix = directed_y,
  predictors = list(directed_x1, directed_x2),
  directed = "directed",
  nullhyp = "qapspp"
)
expect_qap_lm_matches_netlm(
  y_matrix = directed_y,
  predictors = list(directed_x1, directed_x2),
  directed = "directed",
  nullhyp = "qapy"
)
expect_qap_lm_matches_netlm(
  y_matrix = undirected_y,
  predictors = list(undirected_x1, undirected_x2),
  directed = "undirected",
  diagonal = TRUE,
  nullhyp = "qapspp"
)



# All supported input classes should agree exactly under a fixed seed.
expect_qap_lm_format_parity(
  y_matrix = directed_y,
  predictors = list(directed_x1, directed_x2),
  directed = "directed",
  nullhyp = "qapspp"
)
expect_qap_lm_format_parity(
  y_matrix = directed_y,
  predictors = list(directed_x1, directed_x2),
  directed = "directed",
  nullhyp = "qapy"
)
expect_qap_lm_format_parity(
  y_matrix = undirected_y,
  predictors = list(undirected_x1, undirected_x2),
  directed = "undirected",
  diagonal = TRUE,
  nullhyp = "qapspp"
)



# The default argument is qapspp, even though the underlying routine may fall back.
default_fit <- snafun::stat_qap_lm(
  y = directed_y,
  x = list(directed_x1, directed_x2),
  reps = 5,
  seed = 1,
  directed = "directed"
)
expect_equal(default_fit$requested.nullhyp, "qapspp")
expect_equal(default_fit$nullhyp, "qapspp")

# A one-predictor qapspp request resolves to qapy, with or without an intercept:
# with a single predictor there is nothing but the intercept to residualize
# against, so qapspp reduces to permuting the predictor. requested.nullhyp still
# records what was asked for.
single_predictor_fit <- snafun::stat_qap_lm(
  y = directed_y,
  x = directed_x1,
  reps = 5,
  seed = 1,
  directed = "directed"
)
expect_equal(single_predictor_fit$requested.nullhyp, "qapspp")
expect_equal(single_predictor_fit$nullhyp, "qapy")

single_predictor_no_intercept_fit <- snafun::stat_qap_lm(
  y = directed_y,
  x = directed_x1,
  intercept = FALSE,
  reps = 5,
  seed = 1,
  directed = "directed"
)
expect_equal(single_predictor_no_intercept_fit$requested.nullhyp, "qapspp")
expect_equal(single_predictor_no_intercept_fit$nullhyp, "qapy")



# The resolution is deterministic: it happens in stat_qap_lm() before sna::netlm
# is called, so it no longer depends on netlm's singularity detection, on the RNG
# stream, or on the platform. This tiny graph used to reach qapy only through a
# singular-matrix fallback that fired for roughly 60% of seeds, which made the
# suite flaky across R versions and operating systems; no seed is set here on
# purpose, to show the outcome no longer depends on one.
fallback_y <- snafun::create_manual_graph(A -- B, B -- C, C -- D)
fallback_x <- snafun::create_manual_graph(A -- B, B -- D, C -- D)
fallback_fit <- snafun::stat_qap_lm(
  y = fallback_y,
  x = fallback_x,
  reps = 19
)
expect_equal(fallback_fit$requested.nullhyp, "qapspp")
expect_equal(fallback_fit$nullhyp, "qapy")



# Tail probabilities should be well-formed for any available coefficient distribution.
for (component in c("p.beta", "p.t")) {
  p_set <- default_fit[[component]]
  if (all(is.na(p_set$p.greater))) {
    next
  }
  expect_true(all(p_set$p.greater >= 0 & p_set$p.greater <= 1, na.rm = TRUE))
  expect_true(all(p_set$p.less >= 0 & p_set$p.less <= 1, na.rm = TRUE))
  expect_true(all(p_set$p.equal >= 0 & p_set$p.equal <= 1, na.rm = TRUE))
  expect_true(all(p_set$p.two.sided >= 0 & p_set$p.two.sided <= 1, na.rm = TRUE))
  expect_equal(
    unname(p_set$p.greater + p_set$p.less + p_set$p.equal),
    rep(1, length(p_set$p.equal)),
    tolerance = 1e-12
  )
}



# Summary, print, and plot methods should work.
summary_out <- summary(default_fit)
expect_true(inherits(summary_out, "summary.stat_qap_lm"))
expect_equal(nrow(summary_out$coefficient.table), length(default_fit$coefficients))
expect_equal(summary_out$requested.nullhyp, default_fit$requested.nullhyp)
expect_false(any(grepl("^Pr\\(beta", colnames(summary_out$coefficient.table))))
expect_true(any(grepl("^Pr\\(t", colnames(summary_out$coefficient.table))))

print_text <- capture.output(print(default_fit))
expect_true(any(grepl("Quadratic Assignment Procedure Linear Model", print_text, fixed = TRUE)))
expect_true(any(grepl("Pr(two-sided)", print_text, fixed = TRUE)))

summary_text <- capture.output(print(summary_out))
expect_true(any(grepl("Summary of QAP Linear Model", summary_text, fixed = TRUE)))
expect_true(any(grepl("Permutation p-values are shown for t-values only.", summary_text, fixed = TRUE)))


# --- OLS goodness-of-fit block (R^2, adj R^2, F, residual SE) ----------------
# The summary must carry the same goodness-of-fit statistics that
# summary(sna::netlm(...)) reports.
expect_true(all(c("r.squared", "adj.r.squared", "sigma", "fstatistic",
                  "fstatistic.p.value", "residual.quantiles") %in% names(summary_out)),
            info = "summary carries OLS goodness-of-fit fields")

# self-consistency with the stored fit
gof_from_fit <- function(f) {
  fv <- as.numeric(f$fitted.values)
  rs <- as.numeric(f$residuals)
  di <- if (isTRUE(f$intercept)) 1L else 0L
  rdf <- f$df.residual
  qn <- rdf + f$rank
  mss <- if (isTRUE(f$intercept)) sum((fv - mean(fv))^2) else sum(fv^2)
  rss <- sum(rs^2)
  r2 <- mss / (mss + rss)
  list(r2 = r2, adj = 1 - (1 - r2) * ((qn - di) / rdf), sigma = sqrt(rss / rdf))
}
g_self <- gof_from_fit(default_fit)
expect_equal(summary_out$r.squared, g_self$r2, tolerance = 1e-10,
             info = "summary R^2 self-consistent with the fit")
expect_equal(summary_out$adj.r.squared, g_self$adj, tolerance = 1e-10,
             info = "summary adjusted R^2 self-consistent with the fit")
expect_equal(summary_out$sigma, g_self$sigma, tolerance = 1e-10,
             info = "summary residual std error self-consistent with the fit")

# the printed summary shows the goodness-of-fit block
expect_true(any(grepl("Multiple R-squared", summary_text, fixed = TRUE)),
            info = "printed summary shows Multiple R-squared")
expect_true(any(grepl("Adjusted R-squared", summary_text, fixed = TRUE)),
            info = "printed summary shows Adjusted R-squared")
expect_true(any(grepl("F-statistic", summary_text, fixed = TRUE)),
            info = "printed summary shows F-statistic")
expect_true(any(grepl("Residual standard error", summary_text, fixed = TRUE)),
            info = "printed summary shows residual standard error")

# and it matches sna::netlm's goodness-of-fit exactly on a fresh model
if (requireNamespace("sna", quietly = TRUE)) {
  set.seed(123)
  gof_n <- 18
  gof_mk <- function(n) { m <- matrix(stats::rbinom(n * n, 1, 0.3), n); diag(m) <- 0; m }
  gof_Y <- gof_mk(gof_n); gof_X1 <- gof_mk(gof_n); gof_X2 <- gof_mk(gof_n)
  gof_sn <- summary(snafun::stat_qap_lm(gof_Y, x = list(gof_X1, gof_X2),
                                        reps = 20, directed = "directed", seed = 9))
  set.seed(9)
  gof_nl <- sna::netlm(gof_Y, list(gof_X1, gof_X2), mode = "digraph", reps = 20)
  mss <- sum((stats::fitted(gof_nl) - mean(stats::fitted(gof_nl)))^2)
  rss <- sum(stats::resid(gof_nl)^2)
  qn <- NROW(gof_nl$qr$qr)
  rdf <- qn - gof_nl$rank
  r2 <- mss / (mss + rss)
  ar2 <- 1 - (1 - r2) * ((qn - gof_nl$intercept) / rdf)
  fval <- (mss / (gof_nl$rank - gof_nl$intercept)) / (rss / rdf)
  expect_equal(gof_sn$r.squared, r2, tolerance = 1e-10, info = "R^2 == sna::netlm")
  expect_equal(gof_sn$adj.r.squared, ar2, tolerance = 1e-10, info = "adjusted R^2 == sna::netlm")
  expect_equal(gof_sn$sigma, sqrt(rss / rdf), tolerance = 1e-10, info = "residual SE == sna::netlm")
  expect_equal(unname(gof_sn$fstatistic[["value"]]), fval, tolerance = 1e-10, info = "F-statistic == sna::netlm")
}

tmp_plot <- tempfile(fileext = ".png")
grDevices::png(filename = tmp_plot)
plot(default_fit, term = "directed_x1", statistic = "t-value")
grDevices::dev.off()
expect_true(file.exists(tmp_plot))
unlink(tmp_plot)

expect_equal(
  default_fit$predictor.names,
  c("directed_x1", "directed_x2")
)



# Invalid inputs should fail clearly.
expect_error(
  snafun::stat_qap_lm(
    y = directed_y,
    x = list(),
    reps = 5
  ),
  "predictor network or a non-empty list"
)

bad_predictor <- directed_x1
rownames(bad_predictor) <- c("A", "B", "C", "Z")
colnames(bad_predictor) <- c("A", "B", "C", "Z")
expect_error(
  snafun::stat_qap_lm(
    y = directed_y,
    x = bad_predictor,
    reps = 5,
    directed = "directed"
  ),
  "same vertex set"
)

expect_error(
  snafun::stat_qap_lm(
    y = directed_y,
    x = directed_x1,
    reps = 5,
    directed = "weird"
  ),
  "one of 'auto', 'directed', or 'undirected'"
)

expect_error(
  snafun::stat_qap_lm(
    y = directed_y,
    x = directed_x1,
    reps = 5,
    test.statistic = "wrong"
  ),
  "either 't-value' or 'beta'"
)

expect_error(
  snafun::stat_qap_lm(
    y = directed_y,
    x = directed_x1,
    reps = 5,
    nullhyp = "wrong"
  ),
  "either 'qapspp' or 'qapy'"
)

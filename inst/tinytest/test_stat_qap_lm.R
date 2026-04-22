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

single_predictor_fit <- snafun::stat_qap_lm(
  y = directed_y,
  x = directed_x1,
  reps = 5,
  seed = 1,
  directed = "directed"
)
expect_equal(single_predictor_fit$requested.nullhyp, "qapspp")
expect_equal(single_predictor_fit$nullhyp, "qapspp")

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



# Simple one-predictor qapspp cases should now fit cleanly.
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

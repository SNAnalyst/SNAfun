qap_logit_reference <- function(y_matrix,
                                predictors,
                                intercept = TRUE,
                                diagonal = FALSE,
                                directed = TRUE,
                                reps = 29,
                                test.statistic = "z-value",
                                nullhyp = "qapspp",
                                seed = 123) {
  set.seed(seed)
  sna::netlogit(
    y = y_matrix,
    x = predictors,
    intercept = intercept,
    mode = if (directed) "digraph" else "graph",
    diag = diagonal,
    reps = reps,
    test.statistic = test.statistic,
    nullhyp = nullhyp
  )
}


expect_qap_logit_matches_netlogit <- function(y_matrix,
                                              predictors,
                                              intercept = TRUE,
                                              diagonal = FALSE,
                                              directed = TRUE,
                                              reps = 29,
                                              test.statistic = "z-value",
                                              nullhyp = "qapspp",
                                              seed = 123) {
  ours <- snafun::stat_qap_logit(
    y = y_matrix,
    x = predictors,
    intercept = intercept,
    diagonal = diagonal,
    directed = if (directed) "directed" else "undirected",
    reps = reps,
    test.statistic = test.statistic,
    nullhyp = nullhyp,
    seed = seed
  )

  reference <- qap_logit_reference(
    y_matrix = y_matrix,
    predictors = predictors,
    intercept = intercept,
    diagonal = diagonal,
    directed = directed,
    reps = reps,
    test.statistic = test.statistic,
    nullhyp = nullhyp,
    seed = seed
  )

expect_equal(unname(ours$coefficients), unname(as.numeric(reference$coefficients)))
expect_equal(unname(ours$se), unname(as.numeric(reference$se)))
expect_equal(
  unname(ours$z.stat),
  unname(as.numeric(reference$coefficients) / as.numeric(reference$se))
)
  expect_equal(ours$deviance, reference$deviance)
  expect_equal(ours$null.deviance, reference$null.deviance)
  expect_equal(ours$aic, reference$aic)
  expect_equal(ours$bic, reference$bic)
  expect_equal(unname(ours$confusion.table), unname(reference$ctable))

  current_p <- if (identical(test.statistic, "beta")) ours$p.beta else ours$p.z
  expect_equal(unname(current_p$p.less), unname(as.numeric(reference$pleeq)))
  expect_equal(unname(current_p$p.greater), unname(as.numeric(reference$pgreq)))
  expect_equal(unname(current_p$p.two.sided), unname(as.numeric(reference$pgreqabs)))
}


expect_qap_logit_format_parity <- function(y_matrix,
                                           predictors,
                                           diagonal = FALSE,
                                           directed = TRUE,
                                           reps = 19,
                                           test.statistic = "z-value",
                                           nullhyp = "qapspp") {
  y_igraph <- snafun::to_igraph(y_matrix, directed = directed)
  y_network <- snafun::to_network(y_matrix, directed = directed)
  y_edgelist <- snafun::to_edgelist(y_matrix)

  predictor_igraph <- lapply(
    predictors,
    function(one_predictor) snafun::to_igraph(one_predictor, directed = directed)
  )
  predictor_network <- lapply(
    predictors,
    function(one_predictor) snafun::to_network(one_predictor, directed = directed)
  )
  predictor_edgelist <- lapply(
    predictors,
    function(one_predictor) snafun::to_edgelist(one_predictor)
  )

  matrix_fit <- snafun::stat_qap_logit(
    y = y_matrix,
    x = predictors,
    diagonal = diagonal,
    directed = if (directed) "directed" else "undirected",
    reps = reps,
    test.statistic = test.statistic,
    nullhyp = nullhyp,
    seed = 77
  )
  igraph_fit <- snafun::stat_qap_logit(
    y = y_igraph,
    x = predictor_igraph,
    diagonal = diagonal,
    directed = "auto",
    reps = reps,
    test.statistic = test.statistic,
    nullhyp = nullhyp,
    seed = 77
  )
  network_fit <- snafun::stat_qap_logit(
    y = y_network,
    x = predictor_network,
    diagonal = diagonal,
    directed = "auto",
    reps = reps,
    test.statistic = test.statistic,
    nullhyp = nullhyp,
    seed = 77
  )
  edgelist_fit <- snafun::stat_qap_logit(
    y = y_edgelist,
    x = predictor_edgelist,
    diagonal = diagonal,
    directed = if (directed) "directed" else "undirected",
    reps = reps,
    test.statistic = test.statistic,
    nullhyp = nullhyp,
    seed = 77
  )

  fit_list <- list(igraph_fit, network_fit, edgelist_fit)
  for (one_fit in fit_list) {
    expect_equal(one_fit$coefficients, matrix_fit$coefficients)
    expect_equal(one_fit$z.stat, matrix_fit$z.stat)
    expect_equal(one_fit$deviance, matrix_fit$deviance)
    expect_equal(one_fit$null.deviance, matrix_fit$null.deviance)
    expect_equal(one_fit$aic, matrix_fit$aic)
    expect_equal(one_fit$bic, matrix_fit$bic)
  }
}


y_directed <- matrix(
  c(
    0, 1, 0, 1,
    0, 0, 1, 0,
    1, 0, 0, 1,
    0, 1, 0, 0
  ),
  nrow = 4,
  byrow = TRUE
)
x_directed_1 <- matrix(
  c(
    0, 1, 1, 0,
    1, 0, 0, 1,
    0, 1, 0, 1,
    1, 0, 0, 0
  ),
  nrow = 4,
  byrow = TRUE
)
x_directed_2 <- matrix(
  c(
    0, 0, 1, 0,
    1, 0, 1, 0,
    0, 0, 0, 1,
    1, 1, 0, 0
  ),
  nrow = 4,
  byrow = TRUE
)

expect_qap_logit_matches_netlogit(
  y_matrix = y_directed,
  predictors = list(x_directed_1, x_directed_2),
  directed = TRUE,
  diagonal = FALSE,
  reps = 29,
  test.statistic = "z-value",
  nullhyp = "qapspp"
)

expect_qap_logit_matches_netlogit(
  y_matrix = y_directed,
  predictors = list(x_directed_1, x_directed_2),
  directed = TRUE,
  diagonal = FALSE,
  reps = 29,
  test.statistic = "beta",
  nullhyp = "qapy"
)

y_undirected <- matrix(
  c(
    0, 1, 0, 0, 1,
    1, 0, 0, 1, 0,
    0, 0, 0, 1, 1,
    0, 1, 1, 0, 0,
    1, 0, 1, 0, 0
  ),
  nrow = 5,
  byrow = TRUE
)
x_undirected_1 <- matrix(
  c(
    0, 0, 1, 1, 1,
    0, 0, 1, 1, 0,
    1, 1, 0, 1, 0,
    1, 1, 1, 0, 1,
    1, 0, 0, 1, 0
  ),
  nrow = 5,
  byrow = TRUE
)
x_undirected_2 <- matrix(
  c(
    0, 0, 1, 0, 0,
    0, 0, 0, 0, 1,
    1, 0, 0, 1, 0,
    0, 0, 1, 0, 0,
    0, 1, 0, 0, 0
  ),
  nrow = 5,
  byrow = TRUE
)

expect_qap_logit_matches_netlogit(
  y_matrix = y_undirected,
  predictors = list(x_undirected_1, x_undirected_2),
  directed = FALSE,
  diagonal = FALSE,
  reps = 29,
  test.statistic = "z-value",
  nullhyp = "qapspp"
)

expect_qap_logit_format_parity(
  y_matrix = y_directed,
  predictors = list(x_directed_1, x_directed_2),
  directed = TRUE,
  diagonal = FALSE,
  reps = 19,
  test.statistic = "z-value",
  nullhyp = "qapspp"
)

expect_qap_logit_format_parity(
  y_matrix = y_undirected,
  predictors = list(x_undirected_1, x_undirected_2),
  directed = FALSE,
  diagonal = FALSE,
  reps = 19,
  test.statistic = "beta",
  nullhyp = "qapy"
)

named_fit <- snafun::stat_qap_logit(
  y = y_directed,
  x = list(x_directed_1, x_directed_2),
  reps = 9,
  seed = 12
)
expect_equal(names(named_fit$coefficients), c("(intercept)", "x_directed_1", "x_directed_2"))

response_with_weighted_diag <- y_directed
diag(response_with_weighted_diag) <- 2
diag_ok <- snafun::stat_qap_logit(
  y = response_with_weighted_diag,
  x = x_directed_1,
  reps = 9,
  diagonal = FALSE,
  seed = 1
)
expect_true(inherits(diag_ok, "stat_qap_logit"))

expect_error(
  snafun::stat_qap_logit(
    y = response_with_weighted_diag,
    x = x_directed_1,
    reps = 9,
    diagonal = TRUE
  ),
  "binary"
)

expect_error(
  snafun::stat_qap_logit(
    y = y_directed + 0.5,
    x = x_directed_1,
    reps = 9
  ),
  "binary"
)

summary_out <- summary(named_fit)
expect_true(inherits(summary_out, "summary.stat_qap_logit"))
expect_true("beta" %in% colnames(summary_out$coefficient.table))
expect_true("exp.beta" %in% colnames(summary_out$coefficient.table))
expect_true("std.error" %in% colnames(summary_out$coefficient.table))
expect_true("z.value" %in% colnames(summary_out$coefficient.table))
expect_equal(sum(summary_out$confusion.table), named_fit$n)
expect_true(all(c("accuracy", "sensitivity", "specificity", "precision") %in% names(summary_out$classification)))
expect_true(all(c("Null deviance (df)", "Residual deviance (df)", "LR chi-square", "LR df", "LR p-value") %in% names(summary_out$fit.table)))
expect_equal(unname(summary_out$fit.df), c(named_fit$df.null, named_fit$df.residual))
expect_equal(
  summary_out$classification[["sensitivity"]],
  1 - (summary_out$confusion.table[1, 2] / sum(summary_out$confusion.table[, 2]))
)
expect_equal(
  summary_out$classification[["specificity"]],
  1 - (summary_out$confusion.table[2, 1] / sum(summary_out$confusion.table[, 1]))
)

print_capture <- capture.output(print(named_fit))
expect_true(any(grepl("Quadratic Assignment Procedure Logistic Model", print_capture)))
expect_true(any(grepl("x_directed_1", print_capture)))
expect_true(any(grepl("Classification", print_capture)))
expect_true(any(grepl("Null deviance \\(df\\)", print_capture)))

summary_capture <- capture.output(print(summary_out))
expect_true(any(grepl("Summary of QAP Logistic Model", summary_capture)))
expect_true(any(grepl("Confusion Table", summary_capture)))
expect_true(any(grepl("LR chi-square", summary_capture)))
expect_true(any(grepl("Residual deviance \\(df\\)", summary_capture)))

plot_file <- tempfile(fileext = ".pdf")
grDevices::pdf(plot_file)
plot(named_fit, term = "x_directed_1", statistic = "z-value")
grDevices::dev.off()
expect_true(file.exists(plot_file))
unlink(plot_file)

seed_a <- snafun::stat_qap_logit(
  y = y_directed,
  x = list(x_directed_1, x_directed_2),
  reps = 19,
  seed = 222
)
seed_b <- snafun::stat_qap_logit(
  y = y_directed,
  x = list(x_directed_1, x_directed_2),
  reps = 19,
  seed = 222
)
expect_equal(seed_a$z.dist, seed_b$z.dist)

expect_error(
  snafun::stat_qap_logit(
    y = y_directed,
    x = x_directed_1,
    reps = 0
  ),
  "positive integer"
)

expect_error(
  snafun::stat_qap_logit(
    y = y_directed,
    x = x_directed_1,
    test.statistic = "t-value"
  ),
  "either 'z-value' or 'beta'"
)

expect_error(
  snafun::stat_qap_logit(
    y = y_directed,
    x = x_directed_1,
    nullhyp = "not-a-null"
  ),
  "should be one of"
)

report_side_effects()



qap_reference_vector <- function(mat, diagonal = FALSE, directed = TRUE) {
  selector <- matrix(TRUE, nrow = nrow(mat), ncol = ncol(mat))
  if (!diagonal) {
    diag(selector) <- FALSE
  }

  as.numeric(mat[selector])
}



qap_reference_partial <- function(x, y, controls, diagonal = FALSE,
                                  directed = TRUE, method = "pearson") {
  x_vec <- qap_reference_vector(x, diagonal = diagonal, directed = directed)
  y_vec <- qap_reference_vector(y, diagonal = diagonal, directed = directed)
  z_mat <- do.call(
    cbind,
    lapply(
      controls,
      function(one_control) {
        qap_reference_vector(
          one_control,
          diagonal = diagonal,
          directed = directed
        )
      }
    )
  )
  if (is.null(dim(z_mat))) {
    z_mat <- matrix(z_mat, ncol = 1L)
  }

  if (method == "spearman") {
    x_vec <- rank(x_vec, ties.method = "average")
    y_vec <- rank(y_vec, ties.method = "average")
    z_mat <- apply(z_mat, 2, rank, ties.method = "average")
    if (is.null(dim(z_mat))) {
      z_mat <- matrix(z_mat, ncol = 1L)
    }
  }

  design <- cbind("(Intercept)" = 1, z_mat)
  x_resid <- stats::lm.fit(x = design, y = x_vec)$residuals
  y_resid <- stats::lm.fit(x = design, y = y_vec)$residuals
  stats::cor(x_resid, y_resid)
}



expect_qap_format_parity <- function(x_matrix, y_matrix, directed, diagonal = FALSE,
                                     controls = NULL, method = "pearson") {
  x_igraph <- snafun::to_igraph(x_matrix)
  x_network <- snafun::to_network(x_matrix)
  y_igraph <- snafun::to_igraph(y_matrix)
  y_network <- snafun::to_network(y_matrix)
  y_edgelist <- snafun::to_edgelist(y_matrix)

  if (!is.null(controls)) {
    controls_igraph <- lapply(controls, snafun::to_igraph)
    controls_network <- lapply(controls, snafun::to_network)
    controls_edgelist <- lapply(controls, snafun::to_edgelist)
  } else {
    controls_igraph <- NULL
    controls_network <- NULL
    controls_edgelist <- NULL
  }

  base_result <- snafun::stat_qap_cor(
    x = x_matrix,
    y = y_matrix,
    controls = controls,
    method = method,
    reps = 29,
    seed = 42,
    diagonal = diagonal,
    directed = directed
  )

  igraph_result <- snafun::stat_qap_cor(
    x = x_igraph,
    y = y_igraph,
    controls = controls_igraph,
    method = method,
    reps = 29,
    seed = 42,
    diagonal = diagonal,
    directed = directed
  )

  network_result <- snafun::stat_qap_cor(
    x = x_network,
    y = y_network,
    controls = controls_network,
    method = method,
    reps = 29,
    seed = 42,
    diagonal = diagonal,
    directed = directed
  )

  edgelist_result <- snafun::stat_qap_cor(
    x = snafun::to_edgelist(x_matrix),
    y = y_edgelist,
    controls = controls_edgelist,
    method = method,
    reps = 29,
    seed = 42,
    diagonal = diagonal,
    directed = directed
  )

  expect_equal(igraph_result$obs.stat, base_result$obs.stat, tolerance = 1e-12)
  expect_equal(network_result$obs.stat, base_result$obs.stat, tolerance = 1e-12)
  expect_equal(edgelist_result$obs.stat, base_result$obs.stat, tolerance = 1e-12)

  expect_equal(igraph_result$rep.stat, base_result$rep.stat, tolerance = 1e-12)
  expect_equal(network_result$rep.stat, base_result$rep.stat, tolerance = 1e-12)
  expect_equal(edgelist_result$rep.stat, base_result$rep.stat, tolerance = 1e-12)
}



directed_x <- matrix(
  c(0, 2, 0, 1,
    1, 0, 3, 0,
    0, 1, 0, 4,
    2, 0, 1, 0),
  nrow = 4,
  byrow = TRUE
)
rownames(directed_x) <- LETTERS[1:4]
colnames(directed_x) <- LETTERS[1:4]

directed_y <- matrix(
  c(0, 1, 4, 0,
    2, 0, 1, 2,
    0, 3, 0, 1,
    1, 0, 2, 0),
  nrow = 4,
  byrow = TRUE
)
rownames(directed_y) <- LETTERS[1:4]
colnames(directed_y) <- LETTERS[1:4]

undirected_x <- matrix(
  c(0, 2, 1, 0,
    2, 0, 3, 1,
    1, 3, 0, 2,
    0, 1, 2, 0),
  nrow = 4,
  byrow = TRUE
)
rownames(undirected_x) <- LETTERS[1:4]
colnames(undirected_x) <- LETTERS[1:4]

undirected_y <- matrix(
  c(0, 1, 2, 2,
    1, 0, 4, 0,
    2, 4, 0, 3,
    2, 0, 3, 0),
  nrow = 4,
  byrow = TRUE
)
rownames(undirected_y) <- LETTERS[1:4]
colnames(undirected_y) <- LETTERS[1:4]

control_z <- matrix(
  c(0, 4, 1, 0,
    4, 0, 2, 1,
    1, 2, 0, 3,
    0, 1, 3, 0),
  nrow = 4,
  byrow = TRUE
)
rownames(control_z) <- LETTERS[1:4]
colnames(control_z) <- LETTERS[1:4]



# The observed statistic should match direct matrix correlation under the chosen
# directed/diagonal convention.
observed_directed <- snafun::stat_qap_cor(
  x = directed_x,
  y = directed_y,
  reps = 19,
  seed = 1,
  directed = "directed"
)
expect_equal(
  observed_directed$obs.stat,
  stats::cor(c(directed_x[row(directed_x) != col(directed_x)]),
             c(directed_y[row(directed_y) != col(directed_y)])),
  tolerance = 1e-12
)

observed_directed_diag <- snafun::stat_qap_cor(
  x = directed_x,
  y = directed_y,
  reps = 19,
  seed = 1,
  diagonal = TRUE,
  directed = "directed"
)
expect_equal(
  observed_directed_diag$obs.stat,
  stats::cor(c(directed_x), c(directed_y)),
  tolerance = 1e-12
)

observed_undirected <- snafun::stat_qap_cor(
  x = undirected_x,
  y = undirected_y,
  reps = 19,
  seed = 1,
  directed = "undirected"
)
expect_equal(
  observed_undirected$obs.stat,
  stats::cor(c(undirected_x[row(undirected_x) != col(undirected_x)]),
             c(undirected_y[row(undirected_y) != col(undirected_y)])),
  tolerance = 1e-12
)

observed_undirected_diag <- snafun::stat_qap_cor(
  x = undirected_x,
  y = undirected_y,
  reps = 19,
  seed = 1,
  diagonal = TRUE,
  directed = "undirected"
)
expect_equal(
  observed_undirected_diag$obs.stat,
  stats::cor(c(undirected_x), c(undirected_y)),
  tolerance = 1e-12
)



# Partial correlation should match an explicit residualization reference.
partial_result <- snafun::stat_qap_cor(
  x = undirected_x,
  y = undirected_y,
  controls = control_z,
  reps = 19,
  seed = 1,
  directed = "undirected"
)
expect_equal(
  partial_result$obs.stat,
  qap_reference_partial(
    x = undirected_x,
    y = undirected_y,
    controls = list(control_z),
    diagonal = FALSE,
    directed = FALSE,
    method = "pearson"
  ),
  tolerance = 1e-12
)

partial_spearman <- snafun::stat_qap_cor(
  x = undirected_x,
  y = undirected_y,
  controls = list(control_z, undirected_x * 2 + 1),
  method = "spearman",
  reps = 19,
  seed = 1,
  directed = "undirected"
)
expect_equal(
  partial_spearman$obs.stat,
  qap_reference_partial(
    x = undirected_x,
    y = undirected_y,
    controls = list(control_z, undirected_x * 2 + 1),
    diagonal = FALSE,
    directed = FALSE,
    method = "spearman"
  ),
  tolerance = 1e-12
)



# Different graph representations should agree exactly under a fixed seed.
expect_qap_format_parity(
  x_matrix = directed_x,
  y_matrix = directed_y,
  directed = "directed"
)
expect_qap_format_parity(
  x_matrix = undirected_x,
  y_matrix = undirected_y,
  directed = "undirected",
  controls = list(control_z)
)



# Weighted inputs should keep their weights, and seed handling should be reproducible.
weighted_igraph_x <- snafun::to_igraph(directed_x)
weighted_igraph_y <- snafun::to_igraph(directed_y)
seed_a <- snafun::stat_qap_cor(
  x = weighted_igraph_x,
  y = weighted_igraph_y,
  reps = 25,
  seed = 99,
  directed = "directed"
)
seed_b <- snafun::stat_qap_cor(
  x = weighted_igraph_x,
  y = weighted_igraph_y,
  reps = 25,
  seed = 99,
  directed = "directed"
)
expect_equal(seed_a$obs.stat, seed_b$obs.stat, tolerance = 1e-12)
expect_equal(seed_a$rep.stat, seed_b$rep.stat, tolerance = 1e-12)

expect_true(all(seed_a$rep.stat <= 1))
expect_true(all(seed_a$rep.stat >= -1))
expect_true(seed_a$p.value >= 0 && seed_a$p.value <= 1)
expect_true(seed_a$p.greater >= 0 && seed_a$p.greater <= 1)
expect_true(seed_a$p.less >= 0 && seed_a$p.less <= 1)
expect_true(seed_a$p.equal >= 0 && seed_a$p.equal <= 1)
expect_equal(seed_a$p.greater + seed_a$p.less + seed_a$p.equal, 1, tolerance = 1e-12)



# Auto detection should infer directedness from the supplied matrix.
auto_directed <- snafun::stat_qap_cor(
  x = directed_x,
  y = directed_y,
  reps = 15,
  seed = 5
)
expect_equal(auto_directed$mode, "digraph")

auto_undirected <- snafun::stat_qap_cor(
  x = undirected_x,
  y = undirected_y,
  reps = 15,
  seed = 5
)
expect_equal(auto_undirected$mode, "graph")



# Summary, print, and plot methods should work on the returned object.
summary_out <- summary(seed_a)
expect_true(inherits(summary_out, "summary.stat_qap_cor"))
expect_true(length(summary_out$null.summary) >= 6)
expect_equal(summary_out$p.equal, seed_a$p.equal, tolerance = 1e-12)

print_text <- capture.output(print(seed_a))
expect_true(any(grepl("Quadratic Assignment Procedure Correlation Test", print_text, fixed = TRUE)))
expect_true(any(grepl("Pr(X=Obs):", print_text, fixed = TRUE)))
expect_false(any(grepl("Reported p-value:", print_text, fixed = TRUE)))

summary_text <- capture.output(print(summary_out))
expect_true(any(grepl("Summary of QAP Correlation Test", summary_text, fixed = TRUE)))
expect_true(any(grepl("Pr(X=Obs):", summary_text, fixed = TRUE)))
expect_false(any(grepl("Reported p-value:", summary_text, fixed = TRUE)))



# Regression: the undirected diagonal case should match the student's full
# matrix correlation rather than the triangle-only convention from sna::gcor().
student_x <- snafun::create_manual_graph(A -- B, B -- C, C -- D)
student_y <- snafun::create_manual_graph(A -- B, B -- D, C -- D)
student_x_mat <- snafun::to_matrix(student_x)[c("A", "B", "C", "D"), c("A", "B", "C", "D")]
student_y_mat <- snafun::to_matrix(student_y)[c("A", "B", "C", "D"), c("A", "B", "C", "D")]
student_res <- snafun::stat_qap_cor(student_x, student_y, reps = 19, seed = 1, diagonal = TRUE)
expect_equal(student_res$obs.stat, stats::cor(c(student_x_mat), c(student_y_mat)), tolerance = 1e-12)
expect_equal(student_res$p.greater + student_res$p.less + student_res$p.equal, 1, tolerance = 1e-12)

tmp_plot <- tempfile(fileext = ".png")
grDevices::png(filename = tmp_plot)
plot(seed_a)
grDevices::dev.off()
expect_true(file.exists(tmp_plot))
unlink(tmp_plot)



# Invalid inputs should fail clearly.
expect_error(
  snafun::stat_qap_cor(
    x = matrix(1:6, nrow = 2),
    y = matrix(1:6, nrow = 2),
    reps = 5
  ),
  "one-mode square matrices"
)

bad_names <- directed_y
rownames(bad_names) <- c("A", "B", "C", "Z")
colnames(bad_names) <- c("A", "B", "C", "Z")
expect_error(
  snafun::stat_qap_cor(
    x = directed_x,
    y = bad_names,
    reps = 5,
    directed = "directed"
  ),
  "same vertex set"
)

undirected_bad <- directed_x
expect_error(
  snafun::stat_qap_cor(
    x = undirected_bad,
    y = directed_y,
    reps = 5,
    directed = "undirected"
  ),
  "symmetric matrices"
)

na_matrix <- directed_x
na_matrix[1, 2] <- NA_real_
expect_error(
  snafun::stat_qap_cor(
    x = na_matrix,
    y = directed_y,
    reps = 5,
    directed = "directed"
  ),
  "Missing values"
)

bipartite_matrix <- matrix(1:6, nrow = 2)
expect_error(
  snafun::stat_qap_cor(
    x = bipartite_matrix,
    y = bipartite_matrix,
    reps = 5,
    directed = "directed"
  ),
  "one-mode square matrices"
)

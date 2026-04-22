report_side_effects()



random_qap_matrix <- function(n, directed, weighted, loops) {
  if (directed) {
    mat <- matrix(
      stats::rbinom(n = n * n, size = 1, prob = 0.35),
      nrow = n,
      ncol = n
    )
    if (weighted) {
      mat[mat != 0] <- stats::runif(sum(mat != 0), min = -2, max = 5)
    }
    if (!loops) {
      diag(mat) <- 0
    } else {
      diag(mat) <- sample(c(0, 1, 2), size = n, replace = TRUE)
    }

    # A bare symmetric matrix is semantically ambiguous: it can represent either
    # an undirected graph or a directed graph with reciprocal ties everywhere.
    # The audit wants to compare genuinely directed inputs across all formats, so
    # we force at least one asymmetric dyad whenever needed.
    if (isTRUE(all(mat == t(mat)))) {
      mat[1, 2] <- 1
      mat[2, 1] <- 0
    }
    return(mat)
  }

  upper_part <- matrix(
    stats::rbinom(n = n * n, size = 1, prob = 0.35),
    nrow = n,
    ncol = n
  )
  upper_part[lower.tri(upper_part, diag = TRUE)] <- 0
  mat <- upper_part + t(upper_part)
  if (weighted) {
    weights <- matrix(0, nrow = n, ncol = n)
    weights[upper.tri(weights)] <- stats::runif(n = sum(upper.tri(weights)), min = 1, max = 6)
    mat <- weights + t(weights)
  }
  if (loops) {
    diag(mat) <- sample(c(0, 1, 2), size = n, replace = TRUE)
  }
  mat
}



for (directed_flag in c(TRUE, FALSE)) {
  for (weighted_flag in c(TRUE, FALSE)) {
    for (loops_flag in c(TRUE, FALSE)) {
      for (iteration in seq_len(20)) {
        set.seed(20260421 + iteration + 100 * directed_flag + 10 * weighted_flag + loops_flag)
        x_mat <- random_qap_matrix(
          n = sample(4:7, size = 1),
          directed = directed_flag,
          weighted = weighted_flag,
          loops = loops_flag
        )
        y_mat <- random_qap_matrix(
          n = nrow(x_mat),
          directed = directed_flag,
          weighted = weighted_flag,
          loops = loops_flag
        )

        labels <- paste0("v", seq_len(nrow(x_mat)))
        rownames(x_mat) <- labels
        colnames(x_mat) <- labels
        rownames(y_mat) <- labels
        colnames(y_mat) <- labels

        directed_mode <- if (directed_flag) "directed" else "undirected"

        matrix_result <- snafun::stat_qap_cor(
          x = x_mat,
          y = y_mat,
          reps = 17,
          seed = 11,
          diagonal = loops_flag,
          directed = directed_mode
        )

        igraph_result <- snafun::stat_qap_cor(
          x = snafun::to_igraph(x_mat),
          y = snafun::to_igraph(y_mat),
          reps = 17,
          seed = 11,
          diagonal = loops_flag,
          directed = directed_mode
        )

        network_result <- snafun::stat_qap_cor(
          x = snafun::to_network(x_mat),
          y = snafun::to_network(y_mat),
          reps = 17,
          seed = 11,
          diagonal = loops_flag,
          directed = directed_mode
        )

        edgelist_result <- snafun::stat_qap_cor(
          x = snafun::to_edgelist(x_mat),
          y = snafun::to_edgelist(y_mat),
          reps = 17,
          seed = 11,
          diagonal = loops_flag,
          directed = directed_mode
        )

        expect_equal(igraph_result$obs.stat, matrix_result$obs.stat, tolerance = 1e-12)
        expect_equal(network_result$obs.stat, matrix_result$obs.stat, tolerance = 1e-12)
        expect_equal(edgelist_result$obs.stat, matrix_result$obs.stat, tolerance = 1e-12)

        expect_equal(igraph_result$rep.stat, matrix_result$rep.stat, tolerance = 1e-12)
        expect_equal(network_result$rep.stat, matrix_result$rep.stat, tolerance = 1e-12)
        expect_equal(edgelist_result$rep.stat, matrix_result$rep.stat, tolerance = 1e-12)
      }
    }
  }
}

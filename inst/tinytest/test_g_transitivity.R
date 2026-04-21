report_side_effects()



expected_weak_transitivity <- function(x) {
  if (!is.matrix(x)) {
    x <- as.matrix(x)
  }
  
  if (nrow(x) != ncol(x)) {
    stop("expected_weak_transitivity() only supports one-mode matrices")
  }
  
  x <- matrix(
    data = x,
    nrow = nrow(x),
    ncol = ncol(x),
    dimnames = dimnames(x)
  )
  storage.mode(x) <- "numeric"
  x[is.na(x)] <- 0
  x[x != 0] <- 1
  
  if (nrow(x) > 0) {
    diag(x) <- 0
  }
  
  if (nrow(x) == 0) {
    return(NaN)
  }
  
  two_paths <- x %*% x
  diag(two_paths) <- 0
  
  denominator <- sum(two_paths)
  if (denominator == 0) {
    return(NaN)
  }
  
  numerator <- sum(two_paths * x)
  numerator / denominator
}



expect_transitivity_across_formats <- function(mat, directed, weighted = FALSE) {
  expected <- expected_weak_transitivity(mat)
  binary_mat <- mat
  binary_mat[is.na(binary_mat)] <- 0
  binary_mat[binary_mat != 0] <- 1
  
  ig <- igraph::graph_from_adjacency_matrix(
    adjmatrix = binary_mat,
    mode = if (directed) "directed" else "undirected",
    weighted = NULL,
    diag = TRUE
  )
  
  if (weighted) {
    edge_pairs <- which(mat != 0, arr.ind = TRUE)
    if (!directed) {
      edge_pairs <- edge_pairs[edge_pairs[, 1] <= edge_pairs[, 2], , drop = FALSE]
    }
    if (nrow(edge_pairs) > 0) {
      edge_ids <- igraph::get_edge_ids(
        graph = ig,
        vp = as.vector(t(edge_pairs[, c(1, 2), drop = FALSE])),
        directed = directed,
        error = FALSE
      )
      igraph::edge_attr(ig, "weight", index = edge_ids) <- mat[edge_pairs]
    }
  }
  
  nw <- network::as.network.matrix(
    x = mat,
    directed = directed,
    matrix.type = "adjacency",
    ignore.eval = FALSE,
    names.eval = "weight"
  )
  edgelist <- snafun::to_edgelist(ig)
  
  actual_values <- list(
    matrix = snafun::g_transitivity(mat),
    igraph = snafun::g_transitivity(ig),
    network = snafun::g_transitivity(nw),
    data_frame = snafun::g_transitivity(edgelist)
  )
  
  for (value in actual_values) {
    if (is.nan(expected)) {
      expect_true(is.nan(value))
    } else {
      expect_equal(value, expected, tolerance = 1e-12)
    }
  }
}



# Directed 3-cycle is the main historical regression for the igraph backend.
directed_cycle <- matrix(
  c(0, 1, 0,
    0, 0, 1,
    1, 0, 0),
  nrow = 3,
  byrow = TRUE
)
expect_transitivity_across_formats(directed_cycle, directed = TRUE)



# A transitive directed triad should score 1.
directed_transitive <- matrix(
  c(0, 1, 1,
    0, 0, 1,
    0, 0, 0),
  nrow = 3,
  byrow = TRUE
)
expect_transitivity_across_formats(directed_transitive, directed = TRUE)



# If there are no valid 2-paths, transitivity is undefined and should be NaN.
single_edge <- matrix(
  c(0, 1, 0,
    0, 0, 0,
    0, 0, 0),
  nrow = 3,
  byrow = TRUE
)
expect_transitivity_across_formats(single_edge, directed = TRUE)



# Undirected examples.
triangle <- matrix(
  c(0, 1, 1,
    1, 0, 1,
    1, 1, 0),
  nrow = 3,
  byrow = TRUE
)
expect_transitivity_across_formats(triangle, directed = FALSE)

open_triplet <- matrix(
  c(0, 1, 0,
    1, 0, 1,
    0, 1, 0),
  nrow = 3,
  byrow = TRUE
)
expect_transitivity_across_formats(open_triplet, directed = FALSE)



# Weights should be discarded and loops ignored.
weighted_loopy <- matrix(
  c(9, 2, 7, 0,
    0, 5, 3, 0,
    0, 0, 4, 1,
    0, 0, 0, 8),
  nrow = 4,
  byrow = TRUE
)
expect_transitivity_across_formats(weighted_loopy, directed = TRUE, weighted = TRUE)



# Isolates should not affect the result.
with_isolate <- matrix(
  c(0, 1, 1, 0,
    0, 0, 1, 0,
    0, 0, 0, 0,
    0, 0, 0, 0),
  nrow = 4,
  byrow = TRUE
)
expect_transitivity_across_formats(with_isolate, directed = TRUE)



# Empty graph stays NaN.
expect_true(is.nan(snafun::g_transitivity(matrix(0, 0, 0))))



# Bipartite inputs are not supported.
bipartite_matrix <- matrix(c(1, 0, 0, 1, 1, 0), nrow = 2, byrow = TRUE)
expect_error(
  snafun::g_transitivity(bipartite_matrix),
  "only defined for one-mode networks"
)

# Random regression sweep across the requested variants.
set.seed(20260418)
for (directed in c(FALSE, TRUE)) {
  for (weighted in c(FALSE, TRUE)) {
    for (allow_loops in c(FALSE, TRUE)) {
      for (sim in seq_len(40)) {
        n <- sample(3:8, size = 1)
        density <- stats::runif(1, min = 0.05, max = 0.7)
        
        if (directed) {
          mat <- matrix(stats::rbinom(n * n, size = 1, prob = density), nrow = n)
        } else {
          upper <- matrix(0, nrow = n, ncol = n)
          upper[upper.tri(upper)] <- stats::rbinom(
            n = n * (n - 1) / 2,
            size = 1,
            prob = density
          )
          mat <- upper + t(upper)
        }
        
        if (!allow_loops) {
          diag(mat) <- 0
        } else {
          diag(mat) <- stats::rbinom(n, size = 1, prob = density)
        }
        
        # Force an isolate in some simulations to keep that branch covered.
        if (sim %% 5 == 0) {
          isolate <- sample.int(n, size = 1)
          mat[isolate, ] <- 0
          mat[, isolate] <- 0
        }
        
        if (weighted) {
          weights <- matrix(
            sample(c(0, 1, 2, 5), size = n * n, replace = TRUE,
                   prob = c(0.3, 0.3, 0.2, 0.2)),
            nrow = n
          )
          if (!directed) {
            weights[lower.tri(weights)] <- t(weights)[lower.tri(weights)]
          }
          mat[mat != 0] <- weights[mat != 0]
          if (!allow_loops) {
            diag(mat) <- 0
          }
        }
        
        expect_transitivity_across_formats(
          mat = mat,
          directed = directed,
          weighted = weighted
        )
      }
    }
  }
}

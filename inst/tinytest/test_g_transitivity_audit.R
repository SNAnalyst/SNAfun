report_side_effects()



audit_expected_weak_transitivity <- function(x) {
  if (!is.matrix(x)) {
    x <- as.matrix(x)
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



make_transitivity_audit_formats <- function(mat, directed, use_names) {
  binary_mat <- mat
  binary_mat[binary_mat != 0] <- 1
  
  ig <- igraph::graph_from_adjacency_matrix(
    adjmatrix = binary_mat,
    mode = if (directed) "directed" else "undirected",
    weighted = NULL,
    diag = TRUE
  )
  
  if (use_names) {
    igraph::vertex_attr(ig, "name") <- paste0("v", seq_len(nrow(mat)))
    rownames(mat) <- paste0("v", seq_len(nrow(mat)))
    colnames(mat) <- paste0("v", seq_len(ncol(mat)))
  }
  
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
  
  nw <- network::as.network.matrix(
    x = mat,
    directed = directed,
    matrix.type = "adjacency",
    ignore.eval = FALSE,
    names.eval = "weight"
  )
  if (use_names) {
    network::set.vertex.attribute(nw, "vertex.names", paste0("v", seq_len(nrow(mat))))
  }
  
  edgelist <- snafun::to_edgelist(ig)
  
  list(
    matrix = mat,
    igraph = ig,
    network = nw,
    data_frame = edgelist
  )
}



expect_audit_case <- function(mat, directed, use_names) {
  expected <- audit_expected_weak_transitivity(mat)
  formats <- make_transitivity_audit_formats(mat, directed = directed, use_names = use_names)
  
  values <- c(
    matrix = snafun::g_transitivity(formats$matrix),
    igraph = snafun::g_transitivity(formats$igraph),
    network = snafun::g_transitivity(formats$network),
    data_frame = snafun::g_transitivity(formats$data_frame)
  )
  
  if (is.nan(expected)) {
    expect_true(all(is.nan(values)))
  } else {
    expect_equal(as.numeric(values), rep(expected, length(values)), tolerance = 1e-12)
  }
}



set.seed(20260419)
for (directed in c(FALSE, TRUE)) {
  for (weighted in c(FALSE, TRUE)) {
    for (allow_loops in c(FALSE, TRUE)) {
      for (use_names in c(FALSE, TRUE)) {
        for (sim in seq_len(120)) {
          n <- sample(3:10, size = 1)
          density <- stats::runif(1, min = 0.01, max = 0.75)
          
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
          
          if (!allow_loops) {
            diag(mat) <- 0
          } else {
            diag(mat) <- stats::rbinom(n, size = 1, prob = density)
          }
          
          if (weighted) {
            weight_values <- sample(
              x = c(0, 1, 2, 3, 7),
              size = n * n,
              replace = TRUE,
              prob = c(0.25, 0.25, 0.2, 0.2, 0.1)
            )
            weights <- matrix(weight_values, nrow = n)
            if (!directed) {
              weights[lower.tri(weights)] <- t(weights)[lower.tri(weights)]
            }
            mat[mat != 0] <- weights[mat != 0]
            if (!allow_loops) {
              diag(mat) <- 0
            }
          }
          
          # Force structurally sparse cases regularly, because those used to be
          # the main source of wrong answers in one of the old backends.
          if (sim %% 6 == 0) {
            isolate <- sample.int(n, size = 1)
            mat[isolate, ] <- 0
            mat[, isolate] <- 0
          }
          
          if (sim %% 11 == 0 && n >= 3) {
            mat[,] <- 0
            if (directed) {
              mat[1, 2] <- 1
              mat[2, 3] <- 1
              if (weighted) {
                mat[1, 2] <- 5
                mat[2, 3] <- 2
              }
            } else {
              mat[1, 2] <- mat[2, 1] <- 1
              mat[2, 3] <- mat[3, 2] <- 1
              if (weighted) {
                mat[1, 2] <- mat[2, 1] <- 5
                mat[2, 3] <- mat[3, 2] <- 2
              }
            }
          }
          
          expect_audit_case(
            mat = mat,
            directed = directed,
            use_names = use_names
          )
        }
      }
    }
  }
}

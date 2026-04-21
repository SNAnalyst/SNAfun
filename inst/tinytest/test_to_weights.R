report_side_effects()


drop_to_edgelist_metadata <- function(x) {
  attr(x, "snafun_vertices") <- NULL
  attr(x, "snafun_bipartite") <- NULL
  attr(x, "snafun_directed") <- NULL
  x
}


sort_visible_edgelist <- function(x) {
  x <- drop_to_edgelist_metadata(x)
  rownames(x) <- NULL
  if ("from" %in% colnames(x)) {
    x$from <- as.character(x$from)
  }
  if ("to" %in% colnames(x)) {
    x$to <- as.character(x$to)
  }
  if ("weight" %in% colnames(x)) {
    x <- x[order(as.character(x$from), as.character(x$to), x$weight), , drop = FALSE]
  } else {
    x <- x[order(as.character(x$from), as.character(x$to)), , drop = FALSE]
  }
  rownames(x) <- NULL
  x
}


canonicalize_visible_edgelist <- function(x, directed, bipartite) {
  x <- sort_visible_edgelist(x)
  if (!directed && !bipartite && nrow(x) > 0) {
    swap_rows <- as.character(x$from) > as.character(x$to)
    if (any(swap_rows)) {
      tmp <- x$from[swap_rows]
      x$from[swap_rows] <- x$to[swap_rows]
      x$to[swap_rows] <- tmp
    }
    x <- sort_visible_edgelist(x)
  }
  x
}


normalize_matrix_order <- function(x) {
  if (!is.null(rownames(x))) {
    x <- x[order(rownames(x)), , drop = FALSE]
  }
  if (!is.null(colnames(x))) {
    x <- x[, order(colnames(x)), drop = FALSE]
  }
  x
}


expect_same_matrix <- function(actual, expected) {
  expect_identical(
    normalize_matrix_order(actual),
    normalize_matrix_order(expected)
  )
}


expected_visible_edgelist <- function(mat, bipartite = FALSE) {
  nr <- nrow(mat)
  nc <- ncol(mat)
  rn <- rownames(mat)
  cn <- colnames(mat)
  if (is.null(rn)) rn <- as.character(seq_len(nr))
  if (is.null(cn)) cn <- as.character(seq_len(nc))
  
  if (bipartite || nr != nc) {
    idx <- which(mat != 0, arr.ind = TRUE)
  } else if (isSymmetric(mat)) {
    idx <- which(upper.tri(mat, diag = TRUE) & mat != 0, arr.ind = TRUE)
  } else {
    idx <- which(mat != 0, arr.ind = TRUE)
  }
  
  if (nrow(idx) == 0) {
    return(data.frame(from = character(0), to = character(0), stringsAsFactors = FALSE))
  }
  
  weights <- mat[idx]
  out <- data.frame(
    from = rn[idx[, 1]],
    to = cn[idx[, 2]],
    stringsAsFactors = FALSE
  )
  if (!all(weights == 1)) {
    out$weight <- as.numeric(weights)
  }
  sort_visible_edgelist(out)
}


expect_matrix_and_weight_semantics <- function(x,
                                               expected_matrix,
                                               expected_directed,
                                               expected_bipartite,
                                               expected_weighted) {
  actual_matrix <- snafun::to_matrix(x)
  expect_same_matrix(actual_matrix, expected_matrix)
  
  igraph_version <- snafun::to_igraph(x)
  expect_same_matrix(snafun::to_matrix(igraph_version), expected_matrix)
  expect_identical(snafun::is_directed(igraph_version), expected_directed)
  expect_identical(snafun::is_bipartite(igraph_version), expected_bipartite)
  expect_identical(snafun::is_weighted(igraph_version), expected_weighted)
  
  network_version <- if (expected_bipartite) {
    snafun::to_network(x, bipartite = TRUE)
  } else {
    snafun::to_network(x)
  }
  expect_same_matrix(snafun::to_matrix(network_version), expected_matrix)
  expect_identical(snafun::is_directed(network_version), expected_directed)
  expect_identical(snafun::is_bipartite(network_version), expected_bipartite)
  expect_identical(snafun::is_weighted(network_version), expected_weighted)
}


expect_graph_source_outputs <- function(x,
                                        expected_matrix,
                                        expected_directed,
                                        expected_bipartite,
                                        expected_weighted) {
  expect_matrix_and_weight_semantics(
    x = x,
    expected_matrix = expected_matrix,
    expected_directed = expected_directed,
    expected_bipartite = expected_bipartite,
    expected_weighted = expected_weighted
  )
  
  expected_edgelist <- expected_visible_edgelist(
    mat = expected_matrix,
    bipartite = expected_bipartite
  )
  actual_edgelist <- canonicalize_visible_edgelist(
    snafun::to_edgelist(x),
    directed = expected_directed,
    bipartite = expected_bipartite
  )
  expected_edgelist <- canonicalize_visible_edgelist(
    expected_edgelist,
    directed = expected_directed,
    bipartite = expected_bipartite
  )
  expect_identical(actual_edgelist, expected_edgelist)
}


expect_plain_edgelist_outputs <- function(x,
                                          expected_matrix,
                                          expected_directed,
                                          expected_bipartite,
                                          expected_weighted,
                                          directed_override = NULL,
                                          bipartite_override = NULL) {
  matrix_args <- list(x = x)
  if (!is.null(directed_override)) {
    matrix_args$directed <- directed_override
  }
  actual_matrix <- do.call(snafun::to_matrix, matrix_args)
  expect_same_matrix(actual_matrix, expected_matrix)
  
  igraph_args <- list(x = x)
  if (!is.null(bipartite_override)) {
    igraph_args$bipartite <- bipartite_override
  }
  if (!is.null(directed_override)) {
    igraph_args$directed <- directed_override
  }
  igraph_version <- do.call(snafun::to_igraph, igraph_args)
  expect_same_matrix(snafun::to_matrix(igraph_version), expected_matrix)
  expect_identical(snafun::is_directed(igraph_version), expected_directed)
  expect_identical(snafun::is_bipartite(igraph_version), expected_bipartite)
  expect_identical(snafun::is_weighted(igraph_version), expected_weighted)
  
  network_args <- list(x = x)
  if (!is.null(bipartite_override)) {
    network_args$bipartite <- bipartite_override
  }
  if (!is.null(directed_override)) {
    network_args$directed <- directed_override
  }
  network_version <- do.call(snafun::to_network, network_args)
  expect_same_matrix(snafun::to_matrix(network_version), expected_matrix)
  expect_identical(snafun::is_directed(network_version), expected_directed)
  expect_identical(snafun::is_bipartite(network_version), expected_bipartite)
  expect_identical(snafun::is_weighted(network_version), expected_weighted)
}


# deterministic one-mode weighted cases ----------------------------------------
undirected_weighted <- matrix(
  0,
  nrow = 5,
  ncol = 5,
  dimnames = list(LETTERS[1:5], LETTERS[1:5])
)
undirected_weighted["A", "B"] <- 2
undirected_weighted["B", "A"] <- 2
undirected_weighted["A", "D"] <- 4.5
undirected_weighted["D", "A"] <- 4.5
undirected_weighted["C", "D"] <- 7
undirected_weighted["D", "C"] <- 7

directed_weighted <- matrix(
  0,
  nrow = 5,
  ncol = 5,
  dimnames = list(LETTERS[1:5], LETTERS[1:5])
)
directed_weighted["A", "B"] <- 2
directed_weighted["B", "A"] <- 5.5
directed_weighted["C", "D"] <- 7
directed_weighted["D", "D"] <- 3.25

bipartite_weighted <- matrix(
  c(1.5, 0, 0, 5,
    0, 3.75, 0, 0,
    0, 0, 0, 0),
  nrow = 3,
  byrow = TRUE,
  dimnames = list(c("p1", "p2", "p3"), c("e1", "e2", "e3", "e4"))
)

deterministic_cases <- list(
  list(matrix = undirected_weighted, directed = FALSE, bipartite = FALSE),
  list(matrix = directed_weighted, directed = TRUE, bipartite = FALSE),
  list(matrix = bipartite_weighted, directed = FALSE, bipartite = TRUE),
  list(matrix = (undirected_weighted > 0) * 1, directed = FALSE, bipartite = FALSE),
  list(matrix = (directed_weighted > 0) * 1, directed = TRUE, bipartite = FALSE),
  list(matrix = (bipartite_weighted > 0) * 1, directed = FALSE, bipartite = TRUE)
)

for (case in deterministic_cases) {
  mat <- case$matrix
  weighted <- snafun::is_weighted(mat)
  graph_i <- if (case$bipartite) snafun::to_igraph(mat, bipartite = TRUE) else snafun::to_igraph(mat)
  graph_n <- if (case$bipartite) snafun::to_network(mat, bipartite = TRUE) else snafun::to_network(mat)
  graph_e <- snafun::to_edgelist(graph_i)
  
  expect_graph_source_outputs(
    x = mat,
    expected_matrix = mat,
    expected_directed = case$directed,
    expected_bipartite = case$bipartite,
    expected_weighted = weighted
  )
  expect_graph_source_outputs(
    x = graph_i,
    expected_matrix = mat,
    expected_directed = case$directed,
    expected_bipartite = case$bipartite,
    expected_weighted = weighted
  )
  expect_graph_source_outputs(
    x = graph_n,
    expected_matrix = mat,
    expected_directed = case$directed,
    expected_bipartite = case$bipartite,
    expected_weighted = weighted
  )
  expect_matrix_and_weight_semantics(
    x = graph_e,
    expected_matrix = mat,
    expected_directed = case$directed,
    expected_bipartite = case$bipartite,
    expected_weighted = weighted
  )
}


# plain external weighted edgelists -------------------------------------------
plain_directed_weighted <- data.frame(
  from = c("A", "B", "C", "D"),
  to = c("B", "A", "D", "D"),
  weight = c(2, 5.5, 7, 3.25),
  label = c("x", "y", "z", "loop"),
  stringsAsFactors = FALSE
)
expect_plain_edgelist_outputs(
  x = plain_directed_weighted,
  expected_matrix = directed_weighted[c("A", "B", "C", "D"), c("A", "B", "C", "D"), drop = FALSE],
  expected_directed = TRUE,
  expected_bipartite = FALSE,
  expected_weighted = TRUE
)

plain_undirected_weighted <- data.frame(
  from = c("A", "A", "C"),
  to = c("B", "D", "D"),
  weight = c(2, 4.5, 7),
  label = c("ab", "ad", "cd"),
  stringsAsFactors = FALSE
)
expect_plain_edgelist_outputs(
  x = plain_undirected_weighted,
  expected_matrix = undirected_weighted[c("A", "B", "C", "D"), c("A", "B", "C", "D"), drop = FALSE],
  expected_directed = FALSE,
  expected_bipartite = FALSE,
  expected_weighted = TRUE,
  directed_override = FALSE
)

plain_bipartite_weighted <- data.frame(
  from = c("p1", "p1", "p2"),
  to = c("e1", "e4", "e2"),
  weight = c(1.5, 5, 3.75),
  label = c("a", "b", "c"),
  stringsAsFactors = FALSE
)
expect_plain_edgelist_outputs(
  x = plain_bipartite_weighted,
  expected_matrix = bipartite_weighted[1:2, c("e1", "e2", "e4"), drop = FALSE],
  expected_directed = FALSE,
  expected_bipartite = TRUE,
  expected_weighted = TRUE
)


# randomized regression tests for weighted and unweighted cases ----------------
set.seed(20260420)
for (i in seq_len(12)) {
  # directed weighted one-mode
  mat_directed <- matrix(0, nrow = 6, ncol = 6, dimnames = list(LETTERS[1:6], LETTERS[1:6]))
  keep <- matrix(stats::runif(36) < 0.2, nrow = 6)
  weights <- round(stats::runif(sum(keep), min = 0.25, max = 9.5), 2)
  mat_directed[keep] <- weights
  diag(mat_directed) <- round(diag(mat_directed), 2)
  
  # undirected weighted one-mode
  mat_undirected <- matrix(0, nrow = 6, ncol = 6, dimnames = list(LETTERS[1:6], LETTERS[1:6]))
  upper_keep <- which(upper.tri(mat_undirected) & stats::runif(36) < 0.2, arr.ind = TRUE)
  if (nrow(upper_keep) > 0) {
    upper_weights <- round(stats::runif(nrow(upper_keep), min = 0.25, max = 9.5), 2)
    mat_undirected[upper_keep] <- upper_weights
    mat_undirected[t(upper_keep)] <- upper_weights
  }
  
  # bipartite weighted
  mat_bip <- matrix(
    0,
    nrow = 3,
    ncol = 4,
    dimnames = list(paste0("p", 1:3), paste0("e", 1:4))
  )
  bip_keep <- matrix(stats::runif(12) < 0.3, nrow = 3)
  mat_bip[bip_keep] <- round(stats::runif(sum(bip_keep), min = 0.25, max = 9.5), 2)
  
  simulation_cases <- list(
    list(matrix = mat_directed, bipartite = FALSE),
    list(matrix = mat_undirected, bipartite = FALSE),
    list(matrix = mat_bip, bipartite = TRUE),
    list(matrix = (mat_directed > 0) * 1, bipartite = FALSE),
    list(matrix = (mat_undirected > 0) * 1, bipartite = FALSE),
    list(matrix = (mat_bip > 0) * 1, bipartite = TRUE)
  )
  
  for (case in simulation_cases) {
    mat <- case$matrix
    weighted <- snafun::is_weighted(mat)
    graph_i <- if (case$bipartite) snafun::to_igraph(mat, bipartite = TRUE) else snafun::to_igraph(mat)
    graph_n <- if (case$bipartite) snafun::to_network(mat, bipartite = TRUE) else snafun::to_network(mat)
    graph_e <- snafun::to_edgelist(graph_i)
    
    expect_graph_source_outputs(
      x = mat,
      expected_matrix = mat,
      expected_directed = snafun::is_directed(mat),
      expected_bipartite = case$bipartite,
      expected_weighted = weighted
    )
    expect_graph_source_outputs(
      x = graph_i,
      expected_matrix = mat,
      expected_directed = snafun::is_directed(mat),
      expected_bipartite = case$bipartite,
      expected_weighted = weighted
    )
    expect_graph_source_outputs(
      x = graph_n,
      expected_matrix = mat,
      expected_directed = snafun::is_directed(mat),
      expected_bipartite = case$bipartite,
      expected_weighted = weighted
    )
    expect_matrix_and_weight_semantics(
      x = graph_e,
      expected_matrix = mat,
      expected_directed = snafun::is_directed(mat),
      expected_bipartite = case$bipartite,
      expected_weighted = weighted
    )
  }
}

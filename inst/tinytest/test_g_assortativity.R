report_side_effects()


make_assortativity_graph <- function() {
  mat <- matrix(
    0,
    nrow = 5,
    ncol = 5,
    dimnames = list(LETTERS[1:5], LETTERS[1:5])
  )
  mat["A", "B"] <- 1
  mat["B", "A"] <- 1
  mat["B", "C"] <- 1
  mat["C", "B"] <- 1
  mat["C", "D"] <- 1
  mat["D", "C"] <- 1
  mat["D", "E"] <- 1
  mat["E", "D"] <- 1
  
  ig <- snafun::to_igraph(mat)
  igraph::V(ig)$group <- c("X", "X", "Y", "Y", "Y")
  igraph::V(ig)$score <- c(1, 2, 2, 4, 5)
  
  list(
    matrix = mat,
    igraph = ig,
    network = snafun::to_network(ig),
    edgelist = snafun::to_edgelist(ig),
    vertices = data.frame(
      name = LETTERS[1:5],
      group = c("X", "X", "Y", "Y", "Y"),
      score = c(1, 2, 2, 4, 5),
      stringsAsFactors = FALSE
    )
  )
}


assort_graphs <- make_assortativity_graph()


# Nominal assortativity via stored or supplied vertex information.
expected_nominal <- igraph::assortativity_nominal(
  graph = assort_graphs$igraph,
  types = as.integer(factor(igraph::V(assort_graphs$igraph)$group)),
  directed = FALSE,
  normalized = TRUE
)

expect_equal(
  snafun::g_assortativity(assort_graphs$igraph, attrname = "group", directed = FALSE),
  expected_nominal,
  tolerance = 1e-12
)

expect_equal(
  snafun::g_assortativity(assort_graphs$network, attrname = "group", directed = FALSE),
  expected_nominal,
  tolerance = 1e-12
)

expect_equal(
  snafun::g_assortativity(assort_graphs$matrix, values = assort_graphs$vertices$group, directed = FALSE),
  expected_nominal,
  tolerance = 1e-12
)

expect_equal(
  snafun::g_assortativity(assort_graphs$edgelist, attrname = "group", directed = FALSE),
  expected_nominal,
  tolerance = 1e-12
)

plain_edgelist <- data.frame(
  from = assort_graphs$edgelist$from,
  to = assort_graphs$edgelist$to,
  stringsAsFactors = FALSE
)

expect_equal(
  snafun::g_assortativity(
    plain_edgelist,
    attrname = "group",
    vertices = assort_graphs$vertices,
    directed = FALSE
  ),
  expected_nominal,
  tolerance = 1e-12
)


# Numeric assortativity, including named vectors.
expected_numeric <- igraph::assortativity(
  graph = assort_graphs$igraph,
  values = igraph::V(assort_graphs$igraph)$score,
  directed = FALSE,
  normalized = TRUE
)

expect_equal(
  snafun::g_assortativity(assort_graphs$igraph, attrname = "score", directed = FALSE),
  expected_numeric,
  tolerance = 1e-12
)

expect_equal(
  snafun::g_assortativity(assort_graphs$network, attrname = "score", directed = FALSE),
  expected_numeric,
  tolerance = 1e-12
)

named_scores <- assort_graphs$vertices$score
names(named_scores) <- rev(assort_graphs$vertices$name)
named_scores <- named_scores[rev(seq_along(named_scores))]

expect_equal(
  snafun::g_assortativity(assort_graphs$matrix, values = named_scores, directed = FALSE),
  expected_numeric,
  tolerance = 1e-12
)

expect_equal(
  snafun::g_assortativity(
    plain_edgelist,
    values = assort_graphs$vertices$score,
    vertices = assort_graphs$vertices,
    directed = FALSE
  ),
  expected_numeric,
  tolerance = 1e-12
)


# Directed numeric example.
directed_mat <- matrix(
  c(0, 1, 0, 0,
    0, 0, 1, 0,
    1, 0, 0, 1,
    0, 0, 0, 0),
  nrow = 4,
  byrow = TRUE,
  dimnames = list(letters[1:4], letters[1:4])
)
directed_values <- c(a = 1, b = 1, c = 3, d = 5)
expected_directed <- igraph::assortativity(
  graph = snafun::to_igraph(directed_mat),
  values = directed_values,
  directed = TRUE,
  normalized = TRUE
)

for (graph in list(
  snafun::to_igraph(directed_mat),
  snafun::to_network(directed_mat),
  directed_mat,
  snafun::to_edgelist(snafun::to_igraph(directed_mat))
)) {
  expect_equal(
    snafun::g_assortativity(graph, values = directed_values, directed = TRUE),
    expected_directed,
    tolerance = 1e-12
  )
}


# Errors and edge cases.
expect_error(
  snafun::g_assortativity(assort_graphs$igraph, attrname = "group", values = assort_graphs$vertices$group),
  "exactly one"
)

expect_error(
  snafun::g_assortativity(assort_graphs$igraph),
  "exactly one"
)

expect_error(
  snafun::g_assortativity(assort_graphs$matrix, attrname = "group"),
  "do not store vertex attributes"
)

expect_error(
  snafun::g_assortativity(assort_graphs$igraph, attrname = "missing_attribute"),
  "does not occur"
)

expect_error(
  snafun::g_assortativity(assort_graphs$igraph, values = 1:3),
  "length equal"
)

expect_error(
  snafun::g_assortativity(assort_graphs$igraph, values = c(1, 2, NA, 4, 5)),
  "missing vertex values"
)

expect_error(
  snafun::g_assortativity(matrix(c(1, 0, 0, 1), nrow = 1), values = c("A", "B", "C", "D", "E")),
  "one-mode networks"
)


# Random parity sweep between formats.
set.seed(20260421)
for (sim in seq_len(30)) {
  n <- sample(5:9, size = 1)
  mat <- matrix(stats::rbinom(n * n, size = 1, prob = stats::runif(1, 0.1, 0.4)), nrow = n)
  diag(mat) <- 0
  mat[lower.tri(mat)] <- t(mat)[lower.tri(mat)]
  dimnames(mat) <- list(paste0("v", seq_len(n)), paste0("v", seq_len(n)))
  
  group_values <- sample(c("A", "B", "C"), size = n, replace = TRUE)
  score_values <- sample(1:5, size = n, replace = TRUE)
  names(score_values) <- rev(rownames(mat))
  score_values <- score_values[rev(seq_along(score_values))]
  
  reference_nominal <- snafun::g_assortativity(mat, values = group_values, directed = FALSE)
  reference_numeric <- snafun::g_assortativity(mat, values = score_values, directed = FALSE)
  
  ig <- snafun::to_igraph(mat)
  igraph::V(ig)$group <- group_values
  igraph::V(ig)$score <- unname(score_values[igraph::V(ig)$name])
  nw <- snafun::to_network(ig)
  el <- snafun::to_edgelist(ig)
  
  expect_equal(snafun::g_assortativity(ig, attrname = "group", directed = FALSE), reference_nominal, tolerance = 1e-12)
  expect_equal(snafun::g_assortativity(nw, attrname = "group", directed = FALSE), reference_nominal, tolerance = 1e-12)
  expect_equal(snafun::g_assortativity(el, attrname = "group", directed = FALSE), reference_nominal, tolerance = 1e-12)
  
  expect_equal(snafun::g_assortativity(ig, attrname = "score", directed = FALSE), reference_numeric, tolerance = 1e-12)
  expect_equal(snafun::g_assortativity(nw, attrname = "score", directed = FALSE), reference_numeric, tolerance = 1e-12)
  expect_equal(snafun::g_assortativity(el, attrname = "score", directed = FALSE), reference_numeric, tolerance = 1e-12)
}

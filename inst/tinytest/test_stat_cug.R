report_side_effects()



expect_cug_matches_sna <- function(mat, mode, cmode, diag = FALSE, reps = 50) {
  set.seed(20260418)
  ours <- snafun::stat_cug(
    x = mat,
    FUN = sna::gden,
    mode = mode,
    cmode = cmode,
    diag = diag,
    reps = reps,
    graph_class = "matrix"
  )
  set.seed(20260418)
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



expect_cug_input_parity <- function(mat, mode = "digraph", cmode = "edges",
                                    reps = 40, diag = FALSE) {
  ig <- snafun::to_igraph(mat)
  nw <- snafun::to_network(mat)
  el <- snafun::to_edgelist(mat)

  set.seed(99)
  res_matrix <- snafun::stat_cug(
    x = mat,
    FUN = sna::gden,
    mode = mode,
    cmode = cmode,
    diag = diag,
    reps = reps,
    graph_class = "matrix"
  )
  set.seed(99)
  res_igraph <- snafun::stat_cug(
    x = ig,
    FUN = sna::gden,
    mode = mode,
    cmode = cmode,
    diag = diag,
    reps = reps,
    graph_class = "matrix"
  )
  set.seed(99)
  res_network <- snafun::stat_cug(
    x = nw,
    FUN = sna::gden,
    mode = mode,
    cmode = cmode,
    diag = diag,
    reps = reps,
    graph_class = "matrix"
  )
  set.seed(99)
  res_edgelist <- snafun::stat_cug(
    x = el,
    FUN = sna::gden,
    mode = mode,
    cmode = cmode,
    diag = diag,
    reps = reps,
    graph_class = "matrix"
  )

  expect_equal(res_igraph$obs.stat, res_matrix$obs.stat, tolerance = 1e-12)
  expect_equal(res_network$obs.stat, res_matrix$obs.stat, tolerance = 1e-12)
  expect_equal(res_edgelist$obs.stat, res_matrix$obs.stat, tolerance = 1e-12)

  expect_equal(res_igraph$rep.stat, res_matrix$rep.stat, tolerance = 1e-12)
  expect_equal(res_network$rep.stat, res_matrix$rep.stat, tolerance = 1e-12)
  expect_equal(res_edgelist$rep.stat, res_matrix$rep.stat, tolerance = 1e-12)
}



# Directed reference matrix
directed_mat <- matrix(
  c(0, 1, 1, 0,
    0, 0, 1, 1,
    1, 0, 0, 0,
    0, 1, 0, 0),
  nrow = 4,
  byrow = TRUE
)


# Undirected reference matrix
undirected_mat <- matrix(
  c(0, 1, 1, 0, 0,
    1, 0, 0, 1, 0,
    1, 0, 0, 1, 1,
    0, 1, 1, 0, 0,
    0, 0, 1, 0, 0),
  nrow = 5,
  byrow = TRUE
)


# Loopy directed matrix
loopy_mat <- matrix(
  c(1, 1, 0, 0,
    0, 1, 1, 0,
    1, 0, 0, 0,
    0, 0, 1, 1),
  nrow = 4,
  byrow = TRUE
)


# Exact parity with sna::cug.test for its native use case.
for (current_cmode in c("size", "edges", "dyad.census")) {
  expect_cug_matches_sna(
    mat = directed_mat,
    mode = "digraph",
    cmode = current_cmode,
    diag = FALSE,
    reps = 35
  )
  expect_cug_matches_sna(
    mat = undirected_mat,
    mode = "graph",
    cmode = current_cmode,
    diag = FALSE,
    reps = 35
  )
}


# Loops/diagonal handling should also match the sna reference when loops are allowed.
expect_cug_matches_sna(
  mat = loopy_mat,
  mode = "digraph",
  cmode = "edges",
  diag = TRUE,
  reps = 35
)


# All supported input classes should yield identical results after conversion.
expect_cug_input_parity(
  mat = directed_mat,
  mode = "digraph",
  cmode = "edges",
  reps = 30
)
expect_cug_input_parity(
  mat = undirected_mat,
  mode = "graph",
  cmode = "dyad.census",
  reps = 30
)


# Auto graph selection should find a workable class for igraph functions even on network input.
set.seed(123)
auto_network <- snafun::stat_cug(
  x = snafun::to_network(undirected_mat),
  FUN = igraph::transitivity,
  mode = "graph",
  cmode = "edges",
  reps = 15,
  graph_class = "auto"
)
expect_equal(auto_network$graph_class, "igraph")

set.seed(123)
explicit_igraph <- snafun::stat_cug(
  x = snafun::to_igraph(undirected_mat),
  FUN = igraph::transitivity,
  mode = "graph",
  cmode = "edges",
  reps = 15,
  graph_class = "igraph"
)
expect_equal(auto_network$obs.stat, explicit_igraph$obs.stat, tolerance = 1e-12)
expect_equal(auto_network$rep.stat, explicit_igraph$rep.stat, tolerance = 1e-12)


# A custom function should receive mode/diag/directed automatically when those formals exist.
custom_cug_stat <- function(x, mode = NULL, diag = NULL, directed = NULL) {
  if (is.null(mode) || is.null(diag) || is.null(directed)) {
    stop("stat_cug() did not inject the expected arguments")
  }
  if (directed && mode != "digraph") {
    stop("directed=TRUE should imply mode='digraph'")
  }
  if (!directed && mode != "graph") {
    stop("directed=FALSE should imply mode='graph'")
  }

  mat <- snafun::to_matrix(x)
  if (!diag) {
    diag(mat) <- 0
  }
  sum(mat != 0)
}

set.seed(7)
custom_result <- snafun::stat_cug(
  x = snafun::to_edgelist(loopy_mat),
  FUN = custom_cug_stat,
  mode = "digraph",
  cmode = "edges",
  diag = TRUE,
  reps = 12,
  graph_class = "edgelist"
)
expect_equal(custom_result$graph_class, "edgelist")
expect_equal(custom_result$obs.stat, sum(loopy_mat != 0), tolerance = 1e-12)


# graph_class = "same" should keep the edgelist representation for edgelist input.
set.seed(8)
same_result <- snafun::stat_cug(
  x = snafun::to_edgelist(undirected_mat),
  FUN = custom_cug_stat,
  mode = "graph",
  cmode = "edges",
  reps = 10,
  graph_class = "same"
)
expect_equal(same_result$graph_class, "edgelist")


# Weighted inputs are binarized, so the result should match the unweighted structure.
weighted_directed <- directed_mat
weighted_directed[weighted_directed != 0] <- c(2, 5, 3, 9, 4, 7)

set.seed(202)
expect_warning(
  weighted_result <- snafun::stat_cug(
    x = weighted_directed,
    FUN = sna::gden,
    mode = "digraph",
    cmode = "edges",
    reps = 20,
    graph_class = "matrix"
  ),
  "Weighted input detected"
)
set.seed(202)
binary_result <- snafun::stat_cug(
  x = directed_mat,
  FUN = sna::gden,
  mode = "digraph",
  cmode = "edges",
  reps = 20,
  graph_class = "matrix"
)
expect_equal(weighted_result$obs.stat, binary_result$obs.stat, tolerance = 1e-12)
expect_equal(weighted_result$rep.stat, binary_result$rep.stat, tolerance = 1e-12)


# Function labels and FUN.args should be preserved in the result and output.
fun_args_result <- snafun::stat_cug(
  x = snafun::to_network(undirected_mat),
  FUN = sna::gtrans,
  mode = "graph",
  cmode = "edges",
  reps = 12,
  FUN.args = list(diag = TRUE, mode = "graph", measure = "rank")
)
expect_equal(fun_args_result$fun, "sna::gtrans")
expect_true(is.list(fun_args_result$fun.args))
expect_equal(fun_args_result$fun.args$measure, "rank")


# Plot and print methods should be usable without errors.
tmp_pdf <- tempfile(fileext = ".pdf")
grDevices::pdf(tmp_pdf)
plot_value <- plot(binary_result)
grDevices::dev.off()
expect_inherits(plot_value, "stat_cug")

print_value <- print(binary_result)
expect_inherits(print_value, "stat_cug")
print_capture <- capture.output(print(fun_args_result))
expect_true(any(grepl("Statistic Function: sna::gtrans", print_capture)))
expect_true(any(grepl("Statistic Arguments:", print_capture)))
expect_true(any(grepl("measure = ", print_capture, fixed = TRUE)))

summary_value <- summary(binary_result)
expect_inherits(summary_value, "summary.stat_cug")
expect_equal(summary_value$obs.stat, binary_result$obs.stat, tolerance = 1e-12)
expect_equal(summary_value$graph_class, binary_result$graph_class)
expect_equal(summary_value$valid.reps, binary_result$valid.reps)
expect_true("Mean" %in% names(summary_value$null.summary))

summary_fun_args <- summary(fun_args_result)
expect_equal(summary_fun_args$fun, "sna::gtrans")
expect_true(is.list(summary_fun_args$fun.args))
summary_print_value <- print(summary_fun_args)
expect_inherits(summary_print_value, "summary.stat_cug")
summary_capture <- capture.output(print(summary_fun_args))
expect_true(any(grepl("Statistic Function: sna::gtrans", summary_capture)))
expect_true(any(grepl("Statistic Arguments:", summary_capture)))

summary_df <- as.data.frame(binary_result)
expect_true(is.data.frame(summary_df))
expect_equal(nrow(summary_df), 1)
expect_equal(summary_df$obs_stat, binary_result$obs.stat, tolerance = 1e-12)
expect_equal(summary_df$p_greater_equal_obs, binary_result$pgteobs, tolerance = 1e-12)
expect_equal(summary_df$graph_class, binary_result$graph_class)
expect_equal(summary_df$fun, binary_result$fun)

replicate_df <- as.data.frame(binary_result, replicates = TRUE)
expect_true(is.data.frame(replicate_df))
expect_equal(nrow(replicate_df), binary_result$reps + 1L)
expect_equal(replicate_df$type[[1]], "observed")
expect_equal(replicate_df$statistic[[1]], binary_result$obs.stat, tolerance = 1e-12)
expect_equal(
  replicate_df$statistic[replicate_df$type == "replicate"],
  binary_result$rep.stat,
  tolerance = 1e-12
)
expect_equal(replicate_df$fun[[1]], binary_result$fun)


# Error handling
expect_error(
  snafun::stat_cug(matrix(1, nrow = 2, ncol = 3), FUN = sna::gden),
  "only supports one-mode graphs"
)
expect_error(
  snafun::stat_cug(directed_mat, FUN = sna::gden, ignore.eval = FALSE),
  "only 'ignore.eval = TRUE' is implemented"
)
expect_error(
  snafun::stat_cug(
    directed_mat,
    FUN = function(x) c(1, 2),
    cmode = "edges",
    reps = 5,
    graph_class = "matrix"
  ),
  "single numeric statistic"
)
expect_error(
  as.data.frame(binary_result, replicates = NA),
  "should be either TRUE or FALSE"
)


# ---------------------------------------------------------------------------
# graph -> graph_class rename (2026-09-17)
# ---------------------------------------------------------------------------

# The deprecated 'graph' argument still works but warns, and produces the same
# result as the new 'graph_class' argument. (In tinytest expect_warning() returns
# the expectation, not the call's value, so we grab the value separately with
# suppressWarnings().)
expect_warning(
  snafun::stat_cug(directed_mat, FUN = sna::gden, cmode = "edges", reps = 20,
                   graph = "matrix"),
  "deprecated"
)
set.seed(555)
dep_result <- suppressWarnings(
  snafun::stat_cug(directed_mat, FUN = sna::gden, cmode = "edges", reps = 20,
                   graph = "matrix")
)
set.seed(555)
new_result <- snafun::stat_cug(directed_mat, FUN = sna::gden, cmode = "edges",
                               reps = 20, graph_class = "matrix")
expect_equal(dep_result$graph_class, "matrix")
expect_equal(dep_result$obs.stat, new_result$obs.stat, tolerance = 1e-12)
expect_equal(dep_result$rep.stat, new_result$rep.stat, tolerance = 1e-12)

# The result object's element is now named 'graph_class', not 'graph'. (Note:
# result$graph would still return a value through R's partial matching of $, so
# we check the actual element names exactly.)
expect_false("graph" %in% names(new_result))
expect_true("graph_class" %in% names(new_result))

# 'auto' now PREFERS igraph: for a statistic that works on an igraph object,
# the chosen evaluation class is 'igraph', even for network/matrix/edgelist input.
for (inp in list(directed_mat,
                 snafun::to_network(directed_mat),
                 snafun::to_igraph(directed_mat),
                 snafun::to_edgelist(directed_mat))) {
  set.seed(11)
  auto_res <- snafun::stat_cug(inp, FUN = snafun::g_transitivity, mode = "digraph",
                               cmode = "edges", reps = 10, graph_class = "auto")
  expect_equal(auto_res$graph_class, "igraph",
               info = "auto prefers igraph for an igraph-compatible statistic")
}

# 'auto' still falls back when the statistic does NOT work on igraph (raw sna
# function needs a matrix/network), so it must not error.
set.seed(12)
auto_sna <- snafun::stat_cug(directed_mat, FUN = sna::gtrans, mode = "digraph",
                             cmode = "edges", reps = 10, graph_class = "auto")
expect_true(auto_sna$graph_class %in% c("matrix", "network"),
            info = "auto falls back to a matrix/network class for raw sna functions")

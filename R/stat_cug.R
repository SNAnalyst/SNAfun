#' Conditional Uniform Graph test
#'
#' Run a conditional uniform graph (CUG) test on a single network statistic.
#'
#' This function generalizes \code{\link[sna]{cug.test}} to the graph classes
#' supported throughout \pkg{snafun}. The observed network may be supplied as an
#' \code{igraph} object, a \code{network} object, a square adjacency matrix, or
#' an edgelist in a \code{data.frame}. The statistic itself may come from
#' \pkg{igraph}, \pkg{sna}, \pkg{snafun}, or from a user-defined function.
#'
#' The null distributions are structural, binary CUG null models. In other
#' words, the current implementation conditions on the binary tie pattern rather
#' than on edge values. Weighted inputs are therefore binarized before the
#' observed statistic and the replicate statistics are computed. This mirrors
#' the most common use of CUG tests in network analysis and avoids the awkward
#' \code{snafun::fix_cug_input()} workaround that was previously needed when
#' using \code{sna::cug.test()} with non-\pkg{sna} functions.
#'
#' Three conditioning schemes are available:
#'
#' \itemize{
#'   \item \code{cmode = "size"} draws binary Bernoulli graphs with the same
#'   number of vertices and tie probability 0.5.
#'   \item \code{cmode = "edges"} draws graphs with the same number of vertices
#'   and the same number of edges.
#'   \item \code{cmode = "dyad.census"} draws directed binary graphs with the
#'   same dyad census. For undirected observed graphs, this implies a dyad
#'   census with zero asymmetric dyads, so the replicates are undirected in
#'   substance even though they are generated through a directed dyad-census
#'   engine, just as in \code{sna::cug.test()}.
#' }
#'
#' By default, \code{graph = "auto"} tries to evaluate \code{FUN} on a graph in
#' the same class as the original input. If that fails, it tries the other
#' supported graph classes until it finds one that yields a numeric scalar. This
#' makes it convenient to use \code{igraph} functions on \code{network} input,
#' \pkg{sna} functions on \code{igraph} input, or small custom wrapper
#' functions. If you want full control, set \code{graph} explicitly.
#'
#' If the statistic function has formal arguments named \code{mode},
#' \code{diag}, or \code{directed}, \code{snafun::stat_cug()} supplies them
#' automatically unless you already set them explicitly in \code{FUN.args}.
#'
#' @param x A one-mode graph supplied as an \code{igraph} object, a
#'   \code{network} object, a square matrix, or an edgelist \code{data.frame}.
#' @param FUN A function, or the name of a function, that calculates a single
#'   numeric graph statistic.
#' @param mode Character scalar indicating whether the null model should be
#'   directed or undirected. The default \code{"auto"} infers this from
#'   \code{x}. If \code{"graph"} is chosen for a directed input, the observed
#'   graph is weakly symmetrized before the statistic is computed.
#' @param cmode Character scalar giving the conditioning scheme:
#'   \code{"size"}, \code{"edges"}, or \code{"dyad.census"}.
#' @param diag Logical scalar indicating whether loops on the diagonal are
#'   allowed in the observed graph and the null model.
#' @param reps Number of replicate graphs to generate.
#' @param graph Character scalar indicating the graph class on which
#'   \code{FUN} should be evaluated. One of \code{"auto"}, \code{"same"},
#'   \code{"igraph"}, \code{"network"}, \code{"matrix"}, or
#'   \code{"edgelist"}.
#' @param ignore.eval Logical scalar. Currently only \code{TRUE} is supported.
#'   This means edge weights are ignored and every non-zero entry is treated as
#'   a tie.
#' @param FUN.args Optional list of additional arguments passed to \code{FUN}.
#'
#' @return An object of class \code{stat_cug}. It contains the observed
#'   statistic, the replicate statistics, the conditioning choices, the chosen
#'   graph class for the statistic evaluation, and upper- and lower-tail
#'   empirical p-values.
#' @family statistics functions
#' @export
#'
#' @examples
#' g <- snafun::create_random_graph(
#'   n_vertices = 12,
#'   strategy = "gnm",
#'   m = 20,
#'   directed = FALSE,
#'   graph = "igraph"
#' )
#'
#' # A snafun statistic
#' stat_cug(g, FUN = snafun::g_transitivity, cmode = "edges", reps = 99)
#'
#' # An igraph statistic on network input
#' g_n <- snafun::to_network(g)
#' stat_cug(
#'   g_n,
#'   FUN = igraph::transitivity,
#'   cmode = "edges",
#'   reps = 99,
#'   graph = "igraph"
#' )
#'
#' # A custom function on edgelists
#' count_visible_edges <- function(x) nrow(x)
#' stat_cug(
#'   snafun::to_edgelist(g),
#'   FUN = count_visible_edges,
#'   cmode = "edges",
#'   reps = 49,
#'   graph = "edgelist"
#' )
stat_cug <- function(x,
                     FUN,
                     mode = c("auto", "digraph", "graph"),
                     cmode = c("size", "edges", "dyad.census"),
                     diag = FALSE,
                     reps = 1000,
                     graph = c("auto", "same", "igraph", "network", "matrix", "edgelist"),
                     ignore.eval = TRUE,
                     FUN.args = list()) {
  mode <- mode[[1]]
  cmode <- cmode[[1]]
  graph <- graph[[1]]

  if (!inherits(x, "igraph") &&
      !inherits(x, "network") &&
      !is.matrix(x) &&
      !is.data.frame(x)) {
    stop(
      "'x' should be an 'igraph' object, a 'network' object, a square matrix, or an edgelist data.frame"
    )
  }
  if (!is.logical(diag) || length(diag) != 1L || is.na(diag)) {
    stop("'diag' should be either TRUE or FALSE")
  }
  if (!is.logical(ignore.eval) || length(ignore.eval) != 1L || is.na(ignore.eval)) {
    stop("'ignore.eval' should be either TRUE or FALSE")
  }
  if (!isTRUE(ignore.eval)) {
    stop(
      "Currently only 'ignore.eval = TRUE' is implemented in 'stat_cug()'.\n",
      "This means weighted ties are binarized before the CUG test is run."
    )
  }
  if (!is.list(FUN.args)) {
    stop("'FUN.args' should be a list")
  }
  if (length(reps) != 1L || is.na(reps) || reps < 1 || reps != as.integer(reps)) {
    stop("'reps' should be a single positive integer")
  }
  reps <- as.integer(reps)

  observed <- prepare_cug_observed_matrix(
    x = x,
    mode = mode,
    diag = diag,
    ignore.eval = ignore.eval
  )
  observed_matrix <- observed$matrix
  mode <- observed$mode
  directed <- identical(mode, "digraph")

  graph <- resolve_cug_graph_type(
    graph = graph,
    x = x,
    observed_matrix = observed_matrix,
    FUN = FUN,
    FUN.args = FUN.args,
    mode = mode,
    diag = diag,
    directed = directed
  )

  observed_graph <- convert_cug_matrix_to_graph(
    x = observed_matrix,
    graph = graph
  )
  obs_stat <- evaluate_cug_statistic(
    x = observed_graph,
    FUN = FUN,
    FUN.args = FUN.args,
    mode = mode,
    diag = diag,
    directed = directed,
    graph_label = graph
  )

  replicate_stats <- numeric(reps)
  for (rep_index in seq_len(reps)) {
    replicate_matrix <- draw_cug_replicate_matrix(
      observed_matrix = observed_matrix,
      mode = mode,
      cmode = cmode,
      diag = diag
    )
    replicate_graph <- convert_cug_matrix_to_graph(
      x = replicate_matrix,
      graph = graph
    )
    replicate_stats[[rep_index]] <- evaluate_cug_statistic(
      x = replicate_graph,
      FUN = FUN,
      FUN.args = FUN.args,
      mode = mode,
      diag = diag,
      directed = directed,
      graph_label = graph
    )
  }

  valid_replicates <- !(is.na(replicate_stats) | is.nan(replicate_stats))
  if (is.na(obs_stat) || is.nan(obs_stat) || !any(valid_replicates)) {
    p_less_equal <- NA_real_
    p_greater_equal <- NA_real_
  } else {
    p_less_equal <- mean(replicate_stats[valid_replicates] <= obs_stat)
    p_greater_equal <- mean(replicate_stats[valid_replicates] >= obs_stat)
  }

  result <- list(
    obs.stat = obs_stat,
    rep.stat = replicate_stats,
    mode = mode,
    diag = diag,
    cmode = cmode,
    plteobs = p_less_equal,
    pgteobs = p_greater_equal,
    reps = reps,
    valid.reps = sum(valid_replicates),
    graph = graph,
    ignore.eval = ignore.eval,
    call = match.call()
  )
  class(result) <- "stat_cug"
  result
}


#' Summarize a CUG test result
#'
#' Summarize the empirical null distribution from a \code{stat_cug()} result.
#'
#' In contrast to the compact \code{print()} method, \code{summary()} reports
#' a set of descriptive statistics for the valid replicate values. This is
#' useful when you want to inspect not only the tail probabilities but also the
#' location and spread of the simulated null distribution.
#'
#' @param object Object returned by \code{\link{stat_cug}}.
#' @param na.rm Logical scalar, should missing replicate statistics be removed
#'   before summarizing? The default is \code{TRUE}.
#' @param ... Ignored.
#'
#' @return An object of class \code{summary.stat_cug}.
#' @export
summary.stat_cug <- function(object, na.rm = TRUE, ...) {
  replicate_values <- object$rep.stat
  if (isTRUE(na.rm)) {
    replicate_values <- replicate_values[!(is.na(replicate_values) | is.nan(replicate_values))]
  }

  null_summary <- if (length(replicate_values) == 0L) {
    c(
      Min. = NA_real_,
      `1st Qu.` = NA_real_,
      Median = NA_real_,
      Mean = NA_real_,
      `3rd Qu.` = NA_real_,
      Max. = NA_real_
    )
  } else {
    summary(replicate_values)
  }

  null_sd <- if (length(replicate_values) <= 1L) {
    NA_real_
  } else {
    stats::sd(replicate_values)
  }

  out <- list(
    obs.stat = object$obs.stat,
    rep.stat = object$rep.stat,
    mode = object$mode,
    diag = object$diag,
    cmode = object$cmode,
    plteobs = object$plteobs,
    pgteobs = object$pgteobs,
    reps = object$reps,
    valid.reps = object$valid.reps,
    graph = object$graph,
    ignore.eval = object$ignore.eval,
    call = object$call,
    null.summary = null_summary,
    null.sd = null_sd
  )
  class(out) <- "summary.stat_cug"
  out
}


#' Print a summary of a CUG test result
#'
#' Print the output created by \code{\link{summary.stat_cug}}.
#'
#' @param x Object returned by \code{\link{summary.stat_cug}}.
#' @param digits Number of significant digits to print.
#' @param ... Ignored.
#'
#' @return Invisibly returns \code{x}.
#' @export
print.summary.stat_cug <- function(x, digits = 4, ...) {
  cat("\nSummary of Conditional Uniform Graph Test\n\n")
  cat("Conditioning Method:", x$cmode, "\n")
  cat("Graph Type:", x$mode, "\n")
  cat("Statistic Graph Class:", x$graph, "\n")
  cat("Diagonal Used:", x$diag, "\n")
  cat("Replications:", x$reps, "\n")
  cat("Valid replicate statistics:", x$valid.reps, "\n\n")
  cat("Observed Value:", format(signif(x$obs.stat, digits = digits)), "\n")
  cat("Pr(X>=Obs):", format(signif(x$pgteobs, digits = digits)), "\n")
  cat("Pr(X<=Obs):", format(signif(x$plteobs, digits = digits)), "\n\n")
  cat("Null distribution summary:\n")
  print(signif(c(x$null.summary, `Std. Dev.` = x$null.sd), digits = digits))
  invisible(x)
}


#' Print a CUG test result
#'
#' Print a compact summary of a \code{stat_cug} result.
#'
#' The layout deliberately resembles the \pkg{sna} print method for
#' \code{cug.test}, while adding the graph class used to evaluate the statistic
#' and the number of valid replicate statistics.
#'
#' @param x Object returned by \code{\link{stat_cug}}.
#' @param digits Number of significant digits to print.
#' @param ... Ignored.
#'
#' @return Invisibly returns \code{x}.
#' @export
print.stat_cug <- function(x, digits = 4, ...) {
  cat("\nUnivariate Conditional Uniform Graph Test\n\n")
  cat("Conditioning Method:", x$cmode, "\n")
  cat("Graph Type:", x$mode, "\n")
  cat("Statistic Graph Class:", x$graph, "\n")
  cat("Diagonal Used:", x$diag, "\n")
  cat("Replications:", x$reps, "\n")
  cat("Valid replicate statistics:", x$valid.reps, "\n\n")
  cat("Observed Value:", format(signif(x$obs.stat, digits = digits)), "\n")
  cat("Pr(X>=Obs):", format(signif(x$pgteobs, digits = digits)), "\n")
  cat("Pr(X<=Obs):", format(signif(x$plteobs, digits = digits)), "\n\n")
  invisible(x)
}


#' Convert a CUG test result to a data frame
#'
#' Convert a \code{stat_cug} object to a regular \code{data.frame}.
#'
#' By default a one-row summary table is returned, which is convenient for
#' reporting or combining multiple CUG analyses. If \code{replicates = TRUE},
#' the replicate statistics are also returned in long format, one row per
#' simulated graph.
#'
#' @param x Object returned by \code{\link{stat_cug}}.
#' @param row.names Optional row names passed through to the returned data
#'   frame.
#' @param optional Ignored, included only for compatibility with the generic.
#' @param replicates Logical scalar. If \code{FALSE} (the default), return a
#'   single summary row. If \code{TRUE}, return one row per replicate and flag
#'   the observed statistic separately.
#' @param ... Ignored.
#'
#' @return A \code{data.frame}.
#' @export
as.data.frame.stat_cug <- function(x, row.names = NULL, optional = FALSE,
                                   replicates = FALSE, ...) {
  if (!is.logical(replicates) || length(replicates) != 1L || is.na(replicates)) {
    stop("'replicates' should be either TRUE or FALSE")
  }

  if (!replicates) {
    out <- data.frame(
      obs_stat = x$obs.stat,
      p_greater_equal_obs = x$pgteobs,
      p_less_equal_obs = x$plteobs,
      mode = x$mode,
      diag = x$diag,
      cmode = x$cmode,
      reps = x$reps,
      valid_reps = x$valid.reps,
      graph = x$graph,
      ignore_eval = x$ignore.eval,
      stringsAsFactors = FALSE
    )
    if (!is.null(row.names)) {
      rownames(out) <- row.names
    }
    return(out)
  }

  out <- data.frame(
    type = c("observed", rep("replicate", length(x$rep.stat))),
    index = c(0L, seq_along(x$rep.stat)),
    statistic = c(x$obs.stat, x$rep.stat),
    valid = c(!(is.na(x$obs.stat) | is.nan(x$obs.stat)),
              !(is.na(x$rep.stat) | is.nan(x$rep.stat))),
    mode = x$mode,
    diag = x$diag,
    cmode = x$cmode,
    reps = x$reps,
    graph = x$graph,
    stringsAsFactors = FALSE
  )
  if (!is.null(row.names)) {
    rownames(out) <- row.names
  }
  out
}


#' Plot a CUG test result
#'
#' Plot the empirical null distribution from \code{stat_cug()} and mark the
#' observed statistic with a vertical reference line.
#'
#' @param x Object returned by \code{\link{stat_cug}}.
#' @param main Plot title. By default this follows the wording used in
#'   \pkg{sna}.
#' @param sub Plot subtitle. By default this reports the conditioning method and
#'   the number of replications, again following \pkg{sna}.
#' @param breaks Histogram break specification passed to
#'   \code{\link[graphics]{hist}}.
#' @param col Fill color for the histogram bars.
#' @param border Border color for the histogram bars.
#' @param observed_col Color of the vertical line for the observed statistic.
#' @param observed_lwd Line width for the observed statistic.
#' @param xlab X-axis label.
#' @param prob Logical scalar, should a density histogram be drawn? The default
#'   matches \pkg{sna} and is \code{TRUE}.
#' @param ... Additional graphical arguments passed to
#'   \code{\link[graphics]{hist}}.
#'
#' @return Invisibly returns \code{x}.
#' @export
plot.stat_cug <- function(x,
                          main = "Univariate CUG Test",
                          sub = paste("Conditioning:", x$cmode, "Reps:", x$reps),
                          breaks = "Sturges",
                          col = "grey85",
                          border = "white",
                          observed_col = "firebrick",
                          observed_lwd = 2,
                          xlab = "CUG Replicates",
                          prob = TRUE,
                          ...) {
  valid_replicates <- x$rep.stat[!(is.na(x$rep.stat) | is.nan(x$rep.stat))]
  if (length(valid_replicates) == 0L) {
    stop("There are no valid replicate statistics to plot")
  }

  graphics::hist(
    valid_replicates,
    breaks = breaks,
    col = col,
    border = border,
    prob = prob,
    main = main,
    sub = sub,
    xlab = xlab,
    ...
  )
  if (!(is.na(x$obs.stat) | is.nan(x$obs.stat))) {
    graphics::abline(v = x$obs.stat, col = observed_col, lwd = observed_lwd)
  }
  invisible(x)
}


#' Prepare the observed graph for a CUG test
#'
#' Convert the supported graph classes to a square binary adjacency matrix and
#' enforce the requested directed/undirected interpretation.
#'
#' @param x Observed graph.
#' @param mode Requested CUG mode.
#' @param diag Logical scalar, should loops be retained?
#' @param ignore.eval Logical scalar, currently required to be \code{TRUE}.
#'
#' @return A list containing the prepared adjacency matrix and the resolved
#'   \code{mode}.
#' @keywords internal
#' @noRd
prepare_cug_observed_matrix <- function(x, mode, diag, ignore.eval) {
  if (!ignore.eval) {
    stop("Internal error: only 'ignore.eval = TRUE' is supported")
  }

  observed_matrix <- snafun::to_matrix(x)
  if (!is.matrix(observed_matrix) || nrow(observed_matrix) != ncol(observed_matrix)) {
    stop("'stat_cug()' currently only supports one-mode graphs")
  }

  if (identical(mode, "auto")) {
    mode <- if (snafun::is_directed(observed_matrix)) "digraph" else "graph"
  } else if (!mode %in% c("digraph", "graph")) {
    stop("'mode' should be one of 'auto', 'digraph', or 'graph'")
  }

  if (!identical(nrow(observed_matrix), ncol(observed_matrix))) {
    stop("'stat_cug()' is only defined for one-mode graphs, not for bipartite networks")
  }

  if (snafun::is_weighted(x)) {
    warning(
      "Weighted input detected. 'stat_cug()' currently ignores edge values and binarizes all non-zero ties."
    )
  }

  observed_matrix[is.na(observed_matrix)] <- 0
  observed_matrix[observed_matrix != 0] <- 1

  # When the user explicitly requests an undirected CUG, we weakly symmetrize
  # the observed graph first so the observed statistic is compared to an
  # undirected null model on the same footing.
  if (identical(mode, "graph") && !isSymmetric(observed_matrix)) {
    observed_matrix <- snafun::to_symmetric_matrix(observed_matrix, rule = "weak")
  }

  if (!diag) {
    diag(observed_matrix) <- 0
  }

  list(matrix = observed_matrix, mode = mode)
}


#' Resolve the graph class used for the test statistic
#'
#' Try the requested graph class, or detect a workable class automatically by
#' evaluating the observed statistic on converted versions of the observed
#' graph.
#'
#' @param graph Requested graph class.
#' @param x Original observed graph.
#' @param observed_matrix Prepared observed adjacency matrix.
#' @param FUN,FUN.args,mode,diag,directed Arguments forwarded to the statistic.
#'
#' @return Character scalar naming the graph class used for \code{FUN}.
#' @keywords internal
#' @noRd
resolve_cug_graph_type <- function(graph, x, observed_matrix, FUN, FUN.args,
                                   mode, diag, directed) {
  allowed_graphs <- c("same", "igraph", "network", "matrix", "edgelist", "auto")
  if (!graph %in% allowed_graphs) {
    stop(
      "'graph' should be one of 'auto', 'same', 'igraph', 'network', 'matrix', or 'edgelist'"
    )
  }

  original_graph <- infer_cug_original_graph_class(x)
  if (identical(graph, "same")) {
    return(original_graph)
  }
  if (!identical(graph, "auto")) {
    return(graph)
  }

  candidates <- unique(c(
    original_graph,
    "igraph",
    "network",
    "matrix",
    "edgelist"
  ))

  for (candidate in candidates) {
    candidate_graph <- convert_cug_matrix_to_graph(
      x = observed_matrix,
      graph = candidate
    )
    candidate_result <- try(
      evaluate_cug_statistic(
        x = candidate_graph,
        FUN = FUN,
        FUN.args = FUN.args,
        mode = mode,
        diag = diag,
        directed = directed,
        graph_label = candidate
      ),
      silent = TRUE
    )
    if (!inherits(candidate_result, "try-error")) {
      return(candidate)
    }
  }

  stop(
    "Could not find a supported graph representation on which 'FUN' returns a numeric scalar.\n",
    "Try setting the 'graph' argument explicitly."
  )
}


#' Infer the original graph class for a CUG statistic
#'
#' Map the supported input classes to the graph-class labels used in
#' \code{stat_cug()}.
#'
#' @param x Observed graph.
#'
#' @return Character scalar.
#' @keywords internal
#' @noRd
infer_cug_original_graph_class <- function(x) {
  if (inherits(x, "igraph")) {
    return("igraph")
  }
  if (inherits(x, "network")) {
    return("network")
  }
  if (is.matrix(x)) {
    return("matrix")
  }
  if (is.data.frame(x)) {
    return("edgelist")
  }
  stop("Unsupported graph class in 'infer_cug_original_graph_class()'")
}


#' Convert a simulated CUG matrix to the requested graph class
#'
#' @param x Square binary adjacency matrix.
#' @param graph Requested graph class.
#'
#' @return Object in the requested graph representation.
#' @keywords internal
#' @noRd
convert_cug_matrix_to_graph <- function(x, graph) {
  if (identical(graph, "matrix")) {
    return(x)
  }
  if (identical(graph, "igraph")) {
    return(snafun::to_igraph(x))
  }
  if (identical(graph, "network")) {
    return(snafun::to_network(x))
  }
  if (identical(graph, "edgelist")) {
    return(snafun::to_edgelist(x))
  }
  stop("Unsupported 'graph' value in 'convert_cug_matrix_to_graph()'")
}


#' Evaluate the CUG statistic on one graph
#'
#' Evaluate \code{FUN} and make sure the return value is a numeric scalar.
#'
#' @param x Graph object in the representation chosen for the statistic.
#' @param FUN,FUN.args,mode,diag,directed Statistic specification.
#' @param graph_label Character scalar used in error messages.
#'
#' @return Numeric scalar.
#' @keywords internal
#' @noRd
evaluate_cug_statistic <- function(x, FUN, FUN.args, mode, diag, directed,
                                   graph_label) {
  fun <- match.fun(FUN)
  evaluation_arguments <- c(list(x), FUN.args)
  fun_formals <- tryCatch(names(formals(fun)), error = function(e) character(0))

  if (!("mode" %in% names(FUN.args)) && "mode" %in% fun_formals) {
    evaluation_arguments$mode <- mode
  }
  if (!("diag" %in% names(FUN.args)) && "diag" %in% fun_formals) {
    evaluation_arguments$diag <- diag
  }
  if (!("directed" %in% names(FUN.args)) && "directed" %in% fun_formals) {
    evaluation_arguments$directed <- directed
  }

  result <- do.call(fun, evaluation_arguments)
  if (!is.numeric(result) || length(result) != 1L) {
    stop(
      "'FUN' should return a single numeric statistic when evaluated on a ",
      graph_label,
      " graph"
    )
  }
  as.numeric(result[[1]])
}


#' Draw one replicate graph for a CUG test
#'
#' Generate a binary adjacency matrix from one of the supported CUG null models.
#'
#' @param observed_matrix Prepared binary adjacency matrix of the observed
#'   graph.
#' @param mode Character scalar, \code{"digraph"} or \code{"graph"}.
#' @param cmode Conditioning scheme.
#' @param diag Logical scalar indicating whether loops are allowed.
#'
#' @return Square binary adjacency matrix.
#' @keywords internal
#' @noRd
draw_cug_replicate_matrix <- function(observed_matrix, mode, cmode, diag) {
  n_vertices <- nrow(observed_matrix)

  if (identical(cmode, "size")) {
    return(
      sna::rgraph(
        n = n_vertices,
        m = 1,
        mode = mode,
        diag = diag,
        tprob = 0.5
      )
    )
  }

  if (identical(cmode, "edges")) {
    n_edges <- count_cug_edges_from_matrix(
      x = observed_matrix,
      mode = mode,
      diag = diag
    )
    return(
      sna::rgnm(
        n = 1,
        nv = n_vertices,
        m = n_edges,
        mode = mode,
        diag = diag
      )
    )
  }

  if (identical(cmode, "dyad.census")) {
    dyad_counts <- suppressWarnings(
      sna::dyad.census(observed_matrix)
    )
    return(
      sna::rguman(
        n = 1,
        nv = n_vertices,
        mut = dyad_counts[[1]],
        asym = dyad_counts[[2]],
        null = dyad_counts[[3]],
        method = "exact"
      )
    )
  }

  stop("'cmode' should be one of 'size', 'edges', or 'dyad.census'")
}


#' Count edges in a binary adjacency matrix for a CUG edge-conditioned null
#'
#' Count edges in the way expected by \code{sna::rgnm()} for the chosen graph
#' mode.
#'
#' @param x Binary adjacency matrix.
#' @param mode Character scalar, \code{"digraph"} or \code{"graph"}.
#' @param diag Logical scalar indicating whether loops count as admissible ties.
#'
#' @return Integer edge count.
#' @keywords internal
#' @noRd
count_cug_edges_from_matrix <- function(x, mode, diag) {
  if (identical(mode, "digraph")) {
    return(sum(x != 0))
  }

  sum(x[upper.tri(x, diag = diag)] != 0)
}

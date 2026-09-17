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
#' The \code{graph_class} argument controls the graph class on which \code{FUN}
#' is evaluated for the observed graph and for every replicate. \strong{It is
#' recommended to set this explicitly.} The best choice depends on \code{FUN}:
#'
#' \itemize{
#'   \item For \pkg{igraph} statistics and \pkg{snafun} statistics (which accept
#'   any class), use \code{graph_class = "igraph"}. This is by far the fastest
#'   option: the replicates are drawn and built directly as (sparse)
#'   \code{igraph} objects, avoiding the dense \eqn{n \times n} adjacency matrix.
#'   It is also the most reproducible, because it fixes a single, canonical graph
#'   construction (this matters for construction-sensitive statistics such as the
#'   global transitivity of a directed graph, whose value can otherwise differ
#'   slightly between graph classes).
#'   \item For \pkg{sna} statistics (e.g. \code{sna::gtrans}, \code{sna::gden}),
#'   use \code{graph_class = "matrix"} (or \code{"network"}), because those
#'   functions require a matrix or a \code{network} object.
#' }
#'
#' By default, \code{graph_class = "auto"} first tries \code{igraph} (the fast,
#' canonical path) and falls back to the original input class and then the
#' remaining classes until it finds one on which \code{FUN} returns a numeric
#' scalar. This is convenient, but note two things: (1) with \code{"auto"} you do
#' not control which class is used, so for a construction-sensitive statistic the
#' result may depend on that automatic choice; and (2) an explicitly chosen class
#' that \code{FUN} cannot handle raises an error, whereas \code{"auto"} would
#' have searched for a working class. In short: set \code{graph_class} to the
#' class your \code{FUN} expects; leave it at \code{"auto"} only when you are
#' unsure.
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
#' @param graph_class Character scalar indicating the graph class on which
#'   \code{FUN} should be evaluated. One of \code{"auto"} (try \code{igraph}
#'   first, then fall back), \code{"same"} (the class of \code{x}),
#'   \code{"igraph"}, \code{"network"}, \code{"matrix"}, or \code{"edgelist"}.
#'   Setting this explicitly is recommended; see Details for guidance (use
#'   \code{"igraph"} for \pkg{igraph}/\pkg{snafun} statistics and \code{"matrix"}
#'   for \pkg{sna} statistics).
#' @param ignore.eval Logical scalar. Currently only \code{TRUE} is supported.
#'   This means edge weights are ignored and every non-zero entry is treated as
#'   a tie.
#' @param FUN.args Optional list of additional arguments passed to \code{FUN}.
#' @param graph \strong{Deprecated.} The former name of \code{graph_class}. It
#'   still works but issues a deprecation warning and will be removed in a future
#'   release; please use \code{graph_class} instead. (The name was changed
#'   because \code{graph} was easily confused with \code{mode = "graph"}, which
#'   means "undirected".)
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
#' # The same statistic on network input
#' g_n <- snafun::to_network(g)
#' stat_cug(
#'   g_n,
#'   FUN = snafun::g_transitivity,
#'   cmode = "edges",
#'   reps = 99
#' )
#'
#' # A custom function on edgelists
#' count_visible_edges <- function(x) nrow(x)
#' stat_cug(
#'   snafun::to_edgelist(g),
#'   FUN = count_visible_edges,
#'   cmode = "edges",
#'   reps = 49,
#'   graph_class = "edgelist"
#' )
stat_cug <- function(x,
                     FUN,
                     mode = c("auto", "digraph", "graph"),
                     cmode = c("size", "edges", "dyad.census"),
                     diag = FALSE,
                     reps = 1000,
                     graph_class = c("auto", "same", "igraph", "network", "matrix", "edgelist"),
                     ignore.eval = TRUE,
                     FUN.args = list(),
                     graph = NULL) {
  call <- match.call(expand.dots = FALSE)
  mode <- mode[[1]]
  cmode <- cmode[[1]]

  # The argument that selects the class on which FUN is evaluated used to be
  # called 'graph'. It was renamed to 'graph_class' (2026-09-17) because 'graph'
  # collided confusingly with mode = "graph" (which means "undirected"). 'graph'
  # is kept as a DEPRECATED alias so existing scripts keep working.
  if (!is.null(graph)) {
    .Deprecated(msg = paste0(
      "The 'graph' argument of 'stat_cug()' has been renamed to 'graph_class' ",
      "and is deprecated; please use 'graph_class' instead."
    ))
    if (missing(graph_class)) {
      graph_class <- graph
    } else {
      warning(
        "Both the deprecated 'graph' and the new 'graph_class' were supplied to ",
        "'stat_cug()'; using 'graph_class' and ignoring 'graph'."
      )
    }
  }
  graph_class <- graph_class[[1]]

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

  graph_class <- resolve_cug_graph_type(
    graph = graph_class,
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
    graph = graph_class,
    directed = directed
  )
  obs_stat <- evaluate_cug_statistic(
    x = observed_graph,
    FUN = FUN,
    FUN.args = FUN.args,
    mode = mode,
    diag = diag,
    directed = directed,
    graph_label = graph_class
  )

  # Precompute everything that is constant across replicates (performance, B):
  #  * cug_target: the conditioning parameters (edge count / dyad census / tie
  #    probability). Previously these were recomputed from the observed matrix on
  #    every replicate (e.g. sna::dyad.census() per rep).
  #  * fun_spec: which of mode/diag/directed to inject into FUN, previously
  #    re-derived by reflection on every replicate.
  cug_target <- precompute_cug_target(observed_matrix, mode = mode,
                                      cmode = cmode, diag = diag)
  fun_spec <- precompute_cug_fun_spec(FUN, FUN.args = FUN.args, mode = mode,
                                      diag = diag, directed = directed)

  # For the igraph representation we draw each replicate with igraph's own random
  # graph constructors (sample_gnm / sample_gnp; see draw_cug_replicate_igraph()),
  # which is 2.5-3.8x faster than drawing an sna matrix/edge list and rebuilding
  # the graph, and produces the canonical igraph construction that the observed
  # graph now also uses. (cmode = "dyad.census" has no igraph-native sampler and
  # still draws via sna::rguman.) Other representations go through the dense
  # matrix, which they require anyway.
  build_igraph_replicates <- identical(graph_class, "igraph")

  replicate_stats <- numeric(reps)
  for (rep_index in seq_len(reps)) {
    if (build_igraph_replicates) {
      replicate_graph <- draw_cug_replicate_igraph(cug_target, directed = directed)
    } else {
      replicate_graph <- convert_cug_matrix_to_graph(
        x = draw_cug_replicate_matrix(cug_target),
        graph = graph_class,
        directed = directed
      )
    }
    replicate_stats[[rep_index]] <- evaluate_cug_statistic_fast(
      replicate_graph, fun_spec = fun_spec, graph_label = graph_class
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
    graph_class = graph_class,
    ignore.eval = ignore.eval,
    fun = stat_cug_fun_label(FUN = FUN, call = call),
    fun.args = FUN.args,
    call = call
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
    graph_class = object$graph_class,
    ignore.eval = object$ignore.eval,
    fun = object$fun,
    fun.args = object$fun.args,
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
  cat("Statistic Graph Class:", x$graph_class, "\n")
  cat("Statistic Function:", x$fun, "\n")
  cat("Statistic Arguments:", stat_cug_format_fun_args(x$fun.args, digits = digits), "\n")
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
  cat("Statistic Graph Class:", x$graph_class, "\n")
  cat("Statistic Function:", x$fun, "\n")
  cat("Statistic Arguments:", stat_cug_format_fun_args(x$fun.args, digits = digits), "\n")
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
    graph_class = x$graph_class,
    fun = x$fun,
    fun_args = stat_cug_format_fun_args(x$fun.args),
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
    graph_class = x$graph_class,
    fun = x$fun,
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

  observed <- x$obs.stat
  have_observed <- !(is.na(observed) | is.nan(observed))

  dots <- list(...)
  hist_args <- list(
    x = valid_replicates,
    breaks = breaks,
    col = col,
    border = border,
    prob = prob,
    main = main,
    sub = sub,
    xlab = xlab
  )
  # In a CUG test the observed statistic often lies far in the tail of the null
  # distribution (e.g. an observed transitivity of 0.65 against replicates near
  # 0.05). If the x-axis only spans the replicate range, the observed reference
  # line is drawn outside the plot region and is therefore invisible. We widen
  # xlim to include the observed value (as sna::plot.cug.test() effectively
  # does), unless the caller supplied an explicit xlim.
  if (have_observed && !("xlim" %in% names(dots))) {
    rng <- range(c(valid_replicates, observed), na.rm = TRUE)
    pad <- diff(rng) * 0.04
    if (!is.finite(pad) || pad == 0) {
      pad <- if (rng[[2]] != 0) abs(rng[[2]]) * 0.04 else 1
    }
    hist_args$xlim <- c(rng[[1]] - pad, rng[[2]] + pad)
  }

  do.call(graphics::hist, c(hist_args, dots))
  if (have_observed) {
    graphics::abline(v = observed, col = observed_col, lwd = observed_lwd)
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

  # 'auto' prefers igraph (2026-09-17). Building and evaluating replicates as
  # igraph objects uses the fast sparse edge-list path (see the replicate loop in
  # stat_cug()), is typically several times faster than the dense matrix/network
  # path, and gives a canonical, reproducible construction of the graph. We only
  # fall back to the original class and the remaining classes when FUN does not
  # work on an igraph object (e.g. raw sna functions such as sna::gtrans, which
  # need a matrix or network). Users who need a specific class should set
  # 'graph_class' explicitly (recommended); see the function documentation.
  candidates <- unique(c(
    "igraph",
    original_graph,
    "network",
    "matrix",
    "edgelist"
  ))

  for (candidate in candidates) {
    candidate_graph <- convert_cug_matrix_to_graph(
      x = observed_matrix,
      graph = candidate,
      directed = directed
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
convert_cug_matrix_to_graph <- function(x, graph, directed = FALSE) {
  if (identical(graph, "matrix")) {
    return(x)
  }
  if (identical(graph, "igraph")) {
    # Fast path (stat_cug performance fix, 2026-09-14). The CUG matrices are
    # already binary and their directedness is fixed by the test's `mode`, so we
    # build the igraph directly from the edge coordinates instead of scanning the
    # full dense n x n matrix (as graph_from_adjacency_matrix() would). Building
    # from the (sparse) edge coordinates touches only the edges and preserves
    # isolates via make_empty_graph(n).
    #
    # NOTE (2026-09-17): we deliberately do NOT call simplify() here. For a
    # DIRECTED graph with mutual dyads, simplify(remove.multiple = TRUE) shifts
    # igraph::transitivity(type = "global") to a non-canonical value, even though
    # there are no genuine multiple edges (verified: for one fixed graph
    # add_edges + simplify gave 0.1707 while add_edges alone,
    # graph_from_adjacency_matrix() and igraph::sample_gnm() all gave 0.1717).
    # make_empty() + add_edges() WITHOUT simplify() is therefore the canonical
    # igraph construction and matches the replicate sampler (sample_gnm/
    # sample_gnp), so the observed graph and every replicate are constructed
    # identically. For undirected input we take only the upper triangle, so no
    # edge is ever added twice and no de-duplication is needed.
    directed <- isTRUE(directed)
    if (directed) {
      idx <- which(x != 0, arr.ind = TRUE)              # every arc, incl. loops
    } else {
      idx <- which(x != 0 & upper.tri(x, diag = TRUE), arr.ind = TRUE)
    }
    g <- igraph::make_empty_graph(n = nrow(x), directed = directed)
    if (nrow(idx) > 0) {
      g <- igraph::add_edges(g, as.vector(t(idx)))
    }
    return(g)
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


#' Precompute the CUG conditioning target once
#'
#' Compute the conditioning parameters (constant across replicates) a single
#' time, so they are not recomputed from the observed matrix inside the replicate
#' loop. For \code{cmode = "edges"} this is the edge count; for
#' \code{cmode = "dyad.census"} the mutual/asymmetric/null dyad counts; for
#' \code{cmode = "size"} the tie probability.
#'
#' @param observed_matrix Prepared binary adjacency matrix of the observed graph.
#' @param mode Character scalar, \code{"digraph"} or \code{"graph"}.
#' @param cmode Conditioning scheme.
#' @param diag Logical scalar indicating whether loops are allowed.
#'
#' @return A list describing the null model to draw from.
#' @keywords internal
#' @noRd
precompute_cug_target <- function(observed_matrix, mode, cmode, diag) {
  n_vertices <- nrow(observed_matrix)
  if (identical(cmode, "size")) {
    return(list(cmode = "size", n_vertices = n_vertices, mode = mode,
                diag = diag, tprob = 0.5))
  }
  if (identical(cmode, "edges")) {
    return(list(cmode = "edges", n_vertices = n_vertices, mode = mode, diag = diag,
                m = count_cug_edges_from_matrix(observed_matrix, mode = mode, diag = diag)))
  }
  if (identical(cmode, "dyad.census")) {
    dyad_counts <- suppressWarnings(sna::dyad.census(observed_matrix))
    return(list(cmode = "dyad.census", n_vertices = n_vertices,
                mut = dyad_counts[[1]], asym = dyad_counts[[2]], null = dyad_counts[[3]]))
  }
  stop("'cmode' should be one of 'size', 'edges', or 'dyad.census'")
}


#' Draw one replicate graph for a CUG test as a dense adjacency matrix
#'
#' Used for the non-igraph output representations. Takes the precomputed target
#' from \code{\link{precompute_cug_target}} so nothing is recomputed per rep.
#'
#' @param target Precomputed CUG target.
#'
#' @return Square binary adjacency matrix.
#' @keywords internal
#' @noRd
draw_cug_replicate_matrix <- function(target) {
  if (identical(target$cmode, "size")) {
    return(sna::rgraph(target$n_vertices, 1, mode = target$mode,
                       diag = target$diag, tprob = target$tprob))
  }
  if (identical(target$cmode, "edges")) {
    return(sna::rgnm(1, target$n_vertices, target$m, mode = target$mode,
                     diag = target$diag))
  }
  # dyad.census
  sna::rguman(1, target$n_vertices, mut = target$mut, asym = target$asym,
              null = target$null, method = "exact")
}


#' Draw one replicate graph for a CUG test as an (sna) edge list
#'
#' The fast path for the igraph representation: draws the replicate directly as
#' an edge list (return.as.edgelist = TRUE), so no dense n x n adjacency matrix
#' is ever allocated or scanned.
#'
#' @param target Precomputed CUG target.
#'
#' @return A two-or-three column edge-list matrix (columns: sender, receiver,
#'   value); for undirected null models each tie may appear in both directions.
#' @keywords internal
#' @noRd
draw_cug_replicate_edges <- function(target) {
  el <- if (identical(target$cmode, "size")) {
    sna::rgraph(target$n_vertices, 1, mode = target$mode, diag = target$diag,
                tprob = target$tprob, return.as.edgelist = TRUE)
  } else if (identical(target$cmode, "edges")) {
    sna::rgnm(1, target$n_vertices, target$m, mode = target$mode,
              diag = target$diag, return.as.edgelist = TRUE)
  } else {
    sna::rguman(1, target$n_vertices, mut = target$mut, asym = target$asym,
                null = target$null, method = "exact", return.as.edgelist = TRUE)
  }
  # sna::rguman() (and n = 1 draws in general) may wrap the edge list in a list.
  if (is.list(el) && !is.data.frame(el)) {
    el <- el[[1]]
  }
  el
}


#' Draw one CUG replicate directly as an igraph object
#'
#' The fast path for \code{graph_class = "igraph"} (2026-09-17). For
#' \code{cmode = "size"} and \code{cmode = "edges"} the replicate is drawn with
#' igraph's own C-level constructors (\code{\link[igraph]{sample_gnp}} /
#' \code{\link[igraph]{sample_gnm}}), which is 2.5-3.8x faster than drawing an
#' sna matrix/edge list and rebuilding the graph, and which produces the
#' canonical igraph construction that the observed graph also uses (see the note
#' in \code{\link{convert_cug_matrix_to_graph}}). For \code{cmode =
#' "dyad.census"} igraph has no native sampler that conditions on the dyad
#' census, so we fall back to \code{sna::rguman()} (as an edge list) and build
#' the graph canonically via \code{\link{build_cug_igraph_from_edges}}.
#'
#' @param target Precomputed CUG target from \code{\link{precompute_cug_target}}.
#' @param directed Logical scalar.
#'
#' @return An \code{igraph} object.
#' @keywords internal
#' @noRd
draw_cug_replicate_igraph <- function(target, directed) {
  directed <- isTRUE(directed)
  if (identical(target$cmode, "size")) {
    return(igraph::sample_gnp(target$n_vertices, target$tprob,
                              directed = directed, loops = isTRUE(target$diag)))
  }
  if (identical(target$cmode, "edges")) {
    return(igraph::sample_gnm(target$n_vertices, target$m,
                              directed = directed, loops = isTRUE(target$diag)))
  }
  # dyad.census: no igraph-native sampler -> draw via sna and build canonically.
  build_cug_igraph_from_edges(draw_cug_replicate_edges(target),
                              n_vertices = target$n_vertices, directed = directed)
}


#' Build a CUG replicate igraph directly from an edge list
#'
#' Used for the \code{cmode = "dyad.census"} igraph path. Builds the graph from
#' the (sparse) edge list with \code{make_empty_graph()} + \code{add_edges()},
#' preserving isolates via \code{n_vertices}.
#'
#' NOTE (2026-09-17): this does NOT call \code{simplify()}. For a directed graph
#' with mutual dyads, \code{simplify(remove.multiple = TRUE)} shifts
#' \code{igraph::transitivity(type = "global")} to a non-canonical value even
#' though there are no genuine multiple edges. Plain \code{add_edges()} matches
#' \code{graph_from_adjacency_matrix()} / \code{sample_gnm()} (the canonical
#' construction), keeping this path consistent with the observed graph and with
#' the sample_gnm/sample_gnp replicates. sna::rguman() draws a directed graph in
#' which mutual dyads are two distinct arcs (not multiple edges), so no
#' de-duplication is needed.
#'
#' @param edges Edge-list matrix (first two columns are the endpoints).
#' @param n_vertices Total number of vertices (to preserve isolates).
#' @param directed Logical scalar.
#'
#' @return An \code{igraph} object.
#' @keywords internal
#' @noRd
build_cug_igraph_from_edges <- function(edges, n_vertices, directed) {
  g <- igraph::make_empty_graph(n = n_vertices, directed = isTRUE(directed))
  if (!is.null(edges) && nrow(edges) > 0) {
    g <- igraph::add_edges(g, as.vector(t(edges[, 1:2, drop = FALSE])))
  }
  g
}


#' Precompute the FUN evaluation spec for a CUG test
#'
#' Resolve, once, which of \code{mode}/\code{diag}/\code{directed} should be
#' injected into \code{FUN} (based on its formals and the user's \code{FUN.args}),
#' instead of doing this reflection on every replicate.
#'
#' @param FUN,FUN.args,mode,diag,directed Statistic specification.
#'
#' @return A list with the resolved function and the extra argument list.
#' @keywords internal
#' @noRd
precompute_cug_fun_spec <- function(FUN, FUN.args, mode, diag, directed) {
  fun <- match.fun(FUN)
  fun_formals <- tryCatch(names(formals(fun)), error = function(e) character(0))
  extra <- FUN.args
  if (!("mode" %in% names(FUN.args)) && "mode" %in% fun_formals) {
    extra$mode <- mode
  }
  if (!("diag" %in% names(FUN.args)) && "diag" %in% fun_formals) {
    extra$diag <- diag
  }
  if (!("directed" %in% names(FUN.args)) && "directed" %in% fun_formals) {
    extra$directed <- directed
  }
  list(fun = fun, extra = extra)
}


#' Evaluate the CUG statistic using a precomputed FUN spec
#'
#' @param x Graph object in the representation chosen for the statistic.
#' @param fun_spec Output of \code{\link{precompute_cug_fun_spec}}.
#' @param graph_label Character scalar used in error messages.
#'
#' @return Numeric scalar.
#' @keywords internal
#' @noRd
evaluate_cug_statistic_fast <- function(x, fun_spec, graph_label) {
  result <- do.call(fun_spec$fun, c(list(x), fun_spec$extra))
  if (!is.numeric(result) || length(result) != 1L) {
    stop(
      "'FUN' should return a single numeric statistic when evaluated on a ",
      graph_label,
      " graph"
    )
  }
  as.numeric(result[[1]])
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


#' Create a stable user-facing label for a CUG statistic function
#'
#' @keywords internal
#' @noRd
stat_cug_fun_label <- function(FUN, call) {
  if (!is.null(call[["FUN"]])) {
    return(paste(deparse(call[["FUN"]], width.cutoff = 500L), collapse = ""))
  }

  fun <- match.fun(FUN)
  if (!is.null(attr(fun, "srcref"))) {
    return("<function>")
  }
  fun_name <- tryCatch(deparse(substitute(FUN), width.cutoff = 500L), error = function(e) NULL)
  if (!is.null(fun_name) && length(fun_name) > 0L) {
    return(paste(fun_name, collapse = ""))
  }
  "<function>"
}


#' Format CUG function arguments for printing
#'
#' @keywords internal
#' @noRd
stat_cug_format_fun_args <- function(x, digits = 4) {
  if (!is.list(x) || length(x) == 0L) {
    return("<none>")
  }

  formatted <- vapply(
    names(x),
    function(one_name) {
      value <- x[[one_name]]
      value_text <- paste(
        utils::capture.output(dput(signif_if_numeric(value, digits = digits))),
        collapse = ""
      )
      paste0(one_name, " = ", value_text)
    },
    character(1)
  )
  paste(formatted, collapse = ", ")
}


#' Apply signif to numeric objects while leaving others untouched
#'
#' @keywords internal
#' @noRd
signif_if_numeric <- function(x, digits = 4) {
  if (is.numeric(x)) {
    return(signif(x, digits = digits))
  }
  x
}

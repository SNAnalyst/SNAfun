#' Quadratic Assignment Procedure linear model
#'
#' Fit a simple QAP-based linear model for one dependent network and one or more
#' predictor networks.
#'
#' \code{stat_qap_lm()} extends the familiar network-regression idea from
#' \code{\link[sna]{netlm}} to the graph classes supported throughout
#' \pkg{snafun}. The dependent network \code{y} and all predictor networks in
#' \code{x} may be supplied as \code{igraph} objects, \code{network} objects,
#' square adjacency matrices, or edgelist \code{data.frame}s. The function then
#' converts all inputs to aligned one-mode matrices, vectorizes them according to
#' the requested directed/undirected and diagonal conventions, and fits an
#' ordinary least-squares model on those dyadic values.
#'
#' The observed model is complemented by a QAP null distribution obtained by
#' either simultaneously permuting the rows and columns of \code{y} while
#' holding the predictors fixed (\code{nullhyp = "qapy"}), or by permuting the
#' residualized predictor associated with each coefficient in turn
#' (\code{nullhyp = "qapspp"}). The latter is the default and mirrors the
#' semi-partialling strategy from \code{\link[sna]{netlm}}. For each
#' coefficient, \code{stat_qap_lm()} stores the permutation distribution for the
#' requested \code{test.statistic}. This keeps the function close to the speed
#' of \code{\link[sna]{netlm}} while still reporting both observed coefficients
#' and observed t-values. In the special case of a one-predictor model without
#' an intercept, \pkg{sna} automatically falls back from \code{"qapspp"} to
#' \code{"qapy"}; \code{stat_qap_lm()} follows that behavior and records both
#' the requested and the actually used null model. In addition, some very small
#' or perfectly determined one-predictor examples can make the semi-partialling
#' step numerically singular under \code{"qapspp"}; in those cases
#' \code{stat_qap_lm()} also falls back to \code{"qapy"} with a warning.
#'
#' As in \code{\link{stat_qap_cor}}, the vectorization convention follows the
#' adjacency matrix that students actually inspect: for directed analyses, all
#' dyads are used, optionally including the diagonal; for undirected analyses,
#' the full symmetric matrix is vectorized as-is rather than collapsing to a
#' single triangle.
#'
#' @param y Dependent network or network-like object.
#' @param x Predictor network, or a list of predictor networks. Each predictor
#'   must be supplied in one of the same formats as \code{y}.
#' @param intercept Logical scalar; should an intercept be included?
#' @param reps Number of QAP permutations.
#' @param diagonal Logical scalar; should diagonal entries be included?
#' @param directed How should the input be interpreted? One of \code{"auto"},
#'   \code{"directed"}, or \code{"undirected"}. The default infers this from
#'   \code{y}.
#' @param test.statistic Which statistic should be emphasized in the printed
#'   output and plots? One of \code{"t-value"} or \code{"beta"}. The
#'   \code{"beta"} option focuses on the raw regression coefficient itself,
#'   whereas \code{"t-value"} focuses on that coefficient relative to its
#'   standard error. In other words, \code{"beta"} is easier to read as an
#'   effect size, while \code{"t-value"} is closer to the familiar
#'   regression-style question of how large the coefficient is compared to its
#'   estimation uncertainty. This default matches the default
#'   \code{test.statistic} used by \code{\link[sna]{netlm}}.
#' @param nullhyp Which QAP null model should be used? One of \code{"qapspp"}
#'   or \code{"qapy"}. The default \code{"qapspp"} permutes each predictor
#'   after residualizing it on the other predictors; \code{"qapy"} permutes the
#'   dependent network. In a one-predictor model without an intercept,
#'   \code{\link[sna]{netlm}} automatically uses \code{"qapy"}. If
#'   \code{"qapspp"} becomes numerically singular for a simple one-predictor
#'   model, \code{stat_qap_lm()} also falls back to \code{"qapy"}.
#' @param tol Numerical tolerance passed to the underlying QR decomposition.
#' @param seed Optional integer seed for reproducible permutations.
#'
#' @return An object of class \code{stat_qap_lm}. It contains the fitted
#'   coefficients, t-values, residuals, fitted values, the replicate
#'   distribution for the selected \code{test.statistic}, and empirical tail
#'   probabilities for each coefficient.
#' @family statistics functions
#' @export
#'
#' @examples
#' y <- snafun::create_manual_graph(A -- B, B -- C, C -- D)
#' x1 <- snafun::create_manual_graph(A -- B, B -- D, C -- D)
#' x2 <- snafun::add_vertices(
#'   snafun::create_manual_graph(A -- C, B -- C),
#'   vertices = "D"
#' )
#'
#' stat_qap_lm(y, x = x1, reps = 99)
#' stat_qap_lm(y, x = list(x1, x2), reps = 49, test.statistic = "beta")
#' stat_qap_lm(y, x = list(x1, x2), reps = 49, nullhyp = "qapy")
#'
#' y_m <- snafun::to_matrix(y)
#' x1_n <- snafun::to_network(x1)
#' stat_qap_lm(y_m, x = x1_n, reps = 49, directed = "undirected")
stat_qap_lm <- function(y,
                        x,
                        intercept = TRUE,
                        reps = 1000,
                        diagonal = FALSE,
                        directed = c("auto", "directed", "undirected"),
                        test.statistic = c("t-value", "beta"),
                        nullhyp = c("qapspp", "qapy"),
                        tol = 1e-7,
                        seed = NULL) {
  call <- match.call(expand.dots = FALSE)
  directed <- directed[[1]]
  test.statistic <- test.statistic[[1]]
  nullhyp <- nullhyp[[1]]
  requested_nullhyp <- nullhyp

  if (!is.logical(intercept) || length(intercept) != 1L || is.na(intercept)) {
    stop("'intercept' should be either TRUE or FALSE")
  }
  if (!is.logical(diagonal) || length(diagonal) != 1L || is.na(diagonal)) {
    stop("'diagonal' should be either TRUE or FALSE")
  }
  if (!directed %in% c("auto", "directed", "undirected")) {
    stop("'directed' should be one of 'auto', 'directed', or 'undirected'")
  }
  if (!test.statistic %in% c("t-value", "beta")) {
    stop("'test.statistic' should be either 't-value' or 'beta'")
  }
  if (!nullhyp %in% c("qapspp", "qapy")) {
    stop("'nullhyp' should be either 'qapspp' or 'qapy'")
  }
  if (length(reps) != 1L || is.na(reps) || reps < 1 || reps != as.integer(reps)) {
    stop("'reps' should be a single positive integer")
  }
  reps <- as.integer(reps)
  if (length(tol) != 1L || is.na(tol) || tol <= 0) {
    stop("'tol' should be a single positive number")
  }
  if (!is.null(seed) && (length(seed) != 1L || is.na(seed))) {
    stop("'seed' should be NULL or a single non-missing integer value")
  }

  seed_state <- stat_qap_cor_set_seed(seed = seed)
  if (is.function(seed_state$restore)) {
    on.exit(seed_state$restore(), add = TRUE)
  }

  y_matrix_raw <- stat_qap_cor_prepare_matrix(x = y, directed_override = NULL)
  directed_flag <- stat_qap_cor_resolve_direction(
    x = y_matrix_raw,
    directed = directed
  )
  y_matrix <- stat_qap_cor_prepare_matrix(x = y, directed_override = directed_flag)
  predictor_matrices <- stat_qap_lm_prepare_predictors(
    x = x,
    directed_flag = directed_flag,
    reference = y_matrix
  )

  predictor_names <- stat_qap_lm_predictor_names(
    x = x,
    n_predictors = length(predictor_matrices),
    x_call = call[["x"]]
  )
  mode <- if (directed_flag) "digraph" else "graph"
  if (identical(nullhyp, "qapspp") &&
      length(predictor_matrices) == 1L &&
      !isTRUE(intercept)) {
    nullhyp <- "qapy"
  }
  fit <- stat_qap_lm_run_netlm(
    y = y_matrix,
    x = predictor_matrices,
    intercept = intercept,
    reps = reps,
    diagonal = diagonal,
    mode = mode,
    nullhyp = nullhyp,
    test.statistic = test.statistic,
    tol = tol,
    seed = seed
  )
  coefficient_names <- c(
    if (isTRUE(intercept)) "(intercept)",
    predictor_names
  )

  coefficients <- stats::setNames(as.numeric(fit$coefficients), coefficient_names)
  t_stat <- if (identical(test.statistic, "t-value")) {
    if (isTRUE(attr(fit, "stat_qap_lm_fallback_beta_for_t"))) {
      stats::setNames(stat_qap_lm_extract_tstat(fit = fit), coefficient_names)
    } else {
      stats::setNames(as.numeric(fit$tstat), coefficient_names)
    }
  } else {
    stats::setNames(stat_qap_lm_extract_tstat(fit = fit), coefficient_names)
  }
  fitted_values <- fit$fitted.values
  residuals <- fit$residuals
  empty_dist <- matrix(
    numeric(0),
    nrow = 0L,
    ncol = length(coefficient_names),
    dimnames = list(NULL, coefficient_names)
  )
  replicate_beta <- if (identical(test.statistic, "beta")) {
    dist <- fit$dist
    colnames(dist) <- coefficient_names
    dist
  } else {
    empty_dist
  }
  replicate_t <- if (identical(test.statistic, "t-value")) {
    dist <- fit$dist
    colnames(dist) <- coefficient_names
    dist
  } else {
    empty_dist
  }

  p_beta <- stat_qap_lm_empirical_p_matrix(
    observed = coefficients,
    replicate = replicate_beta
  )
  p_t <- stat_qap_lm_empirical_p_matrix(
    observed = t_stat,
    replicate = replicate_t
  )

  result <- list(
    coefficients = coefficients,
    t.stat = t_stat,
    fitted.values = fitted_values,
    residuals = residuals,
    rank = fit$rank,
    df.residual = fit$df.residual,
    beta.dist = replicate_beta,
    t.dist = replicate_t,
    p.beta = p_beta,
    p.t = p_t,
    reps = reps,
    valid.reps.beta = colSums(!(is.na(replicate_beta) | is.nan(replicate_beta))),
    valid.reps.t = colSums(!(is.na(replicate_t) | is.nan(replicate_t))),
    intercept = intercept,
    diagonal = diagonal,
    mode = mode,
    test.statistic = test.statistic,
    nullhyp = fit$nullhyp,
    requested.nullhyp = requested_nullhyp,
    predictor.names = predictor_names,
    seed = seed,
    call = call
  )
  class(result) <- "stat_qap_lm"
  result
}


#' Summarize a QAP linear model result
#'
#' Summarize the coefficient-level results from \code{\link{stat_qap_lm}}.
#'
#' @param object Object returned by \code{\link{stat_qap_lm}}.
#' @param ... Ignored.
#'
#' @return An object of class \code{summary.stat_qap_lm}.
#' @export
summary.stat_qap_lm <- function(object, ...) {
  coefficient_table <- cbind(
    beta = object$coefficients,
    t.value = object$t.stat
  )

  beta_available <- any(!is.na(object$p.beta$p.greater))
  t_available <- any(!is.na(object$p.t$p.greater))

  if (isTRUE(beta_available)) {
    coefficient_table <- cbind(
      coefficient_table,
      `Pr(beta>Obs)` = object$p.beta$p.greater,
      `Pr(beta<Obs)` = object$p.beta$p.less,
      `Pr(beta=Obs)` = object$p.beta$p.equal,
      `Pr(beta two-sided)` = object$p.beta$p.two.sided
    )
  }

  if (isTRUE(t_available)) {
    coefficient_table <- cbind(
      coefficient_table,
      `Pr(t>Obs)` = object$p.t$p.greater,
      `Pr(t<Obs)` = object$p.t$p.less,
      `Pr(t=Obs)` = object$p.t$p.equal,
      `Pr(t two-sided)` = object$p.t$p.two.sided
    )
  }

  out <- list(
    coefficients = object$coefficients,
    t.stat = object$t.stat,
    coefficient.table = coefficient_table,
    reps = object$reps,
    valid.reps.beta = object$valid.reps.beta,
    valid.reps.t = object$valid.reps.t,
    intercept = object$intercept,
    diagonal = object$diagonal,
    mode = object$mode,
    test.statistic = object$test.statistic,
    nullhyp = object$nullhyp,
    requested.nullhyp = object$requested.nullhyp,
    beta.p.available = beta_available,
    t.p.available = t_available,
    predictor.names = object$predictor.names,
    rank = object$rank,
    df.residual = object$df.residual,
    call = object$call
  )
  class(out) <- "summary.stat_qap_lm"
  out
}


#' Print a QAP linear model result
#'
#' Print a compact coefficient summary for a \code{stat_qap_lm} object.
#'
#' @param x Object returned by \code{\link{stat_qap_lm}}.
#' @param digits Number of significant digits to print.
#' @param ... Ignored.
#'
#' @return Invisibly returns \code{x}.
#' @export
print.stat_qap_lm <- function(x, digits = 4, ...) {
  cat("\nQuadratic Assignment Procedure Linear Model\n\n")
  cat("Graph Type:", x$mode, "\n")
  cat("Diagonal Used:", x$diagonal, "\n")
  cat("Intercept:", x$intercept, "\n")
  cat("Permutations:", x$reps, "\n")
  cat("Rank:", x$rank, "\n")
  cat("Residual df:", x$df.residual, "\n")
  cat("Highlighted statistic:", x$test.statistic, "\n")
  cat("Null hypothesis:", x$nullhyp, "\n")
  if (!is.null(x$requested.nullhyp) && !identical(x$requested.nullhyp, x$nullhyp)) {
    cat("Requested null hypothesis:", x$requested.nullhyp, "\n")
  }
  cat("\n")

  coefficient_table <- cbind(
    beta = x$coefficients,
    t.value = x$t.stat,
    `Pr(>Obs)` = if (x$test.statistic == "beta") x$p.beta$p.greater else x$p.t$p.greater,
    `Pr(<Obs)` = if (x$test.statistic == "beta") x$p.beta$p.less else x$p.t$p.less,
    `Pr(=Obs)` = if (x$test.statistic == "beta") x$p.beta$p.equal else x$p.t$p.equal,
    `Pr(two-sided)` = if (x$test.statistic == "beta") x$p.beta$p.two.sided else x$p.t$p.two.sided
  )
  print(signif(coefficient_table, digits = digits))
  invisible(x)
}


#' Print a summarized QAP linear model
#'
#' Print the object returned by \code{\link{summary.stat_qap_lm}}.
#'
#' @param x Object returned by \code{\link{summary.stat_qap_lm}}.
#' @param digits Number of significant digits to print.
#' @param ... Ignored.
#'
#' @return Invisibly returns \code{x}.
#' @export
print.summary.stat_qap_lm <- function(x, digits = 4, ...) {
  cat("\nSummary of QAP Linear Model\n\n")
  cat("Graph Type:", x$mode, "\n")
  cat("Diagonal Used:", x$diagonal, "\n")
  cat("Intercept:", x$intercept, "\n")
  cat("Permutations:", x$reps, "\n")
  cat("Rank:", x$rank, "\n")
  cat("Residual df:", x$df.residual, "\n")
  cat("Highlighted statistic:", x$test.statistic, "\n")
  cat("Null hypothesis:", x$nullhyp, "\n")
  if (!is.null(x$requested.nullhyp) && !identical(x$requested.nullhyp, x$nullhyp)) {
    cat("Requested null hypothesis:", x$requested.nullhyp, "\n")
  }
  if (!isTRUE(x$beta.p.available) && isTRUE(x$t.p.available)) {
    cat("Permutation p-values are shown for t-values only.\n")
  }
  if (isTRUE(x$beta.p.available) && !isTRUE(x$t.p.available)) {
    cat("Permutation p-values are shown for betas only.\n")
  }
  cat("\n")
  print(signif(x$coefficient.table, digits = digits))
  invisible(x)
}


#' Plot a QAP linear model result
#'
#' Plot the permutation distribution for a selected coefficient from
#' \code{\link{stat_qap_lm}}.
#'
#' @param x Object returned by \code{\link{stat_qap_lm}}.
#' @param term Coefficient to plot. Either a numeric index or a coefficient
#'   name.
#' @param statistic Which replicate distribution should be plotted? One of
#'   \code{"beta"} or \code{"t-value"}.
#' @param main Plot title.
#' @param sub Plot subtitle.
#' @param breaks Histogram break specification.
#' @param col Fill color for the histogram bars.
#' @param border Border color for the histogram bars.
#' @param observed_col Color of the observed-value reference line.
#' @param observed_lwd Line width of the observed-value reference line.
#' @param xlab X-axis label.
#' @param prob Logical scalar; should a density histogram be drawn?
#' @param ... Additional graphical arguments passed to
#'   \code{\link[graphics]{hist}}.
#'
#' @return Invisibly returns \code{x}.
#' @export
plot.stat_qap_lm <- function(x,
                             term = 1,
                             statistic = c("beta", "t-value"),
                             main = NULL,
                             sub = NULL,
                             breaks = "Sturges",
                             col = "grey85",
                             border = "white",
                             observed_col = "firebrick",
                             observed_lwd = 2,
                             xlab = NULL,
                             prob = TRUE,
                             ...) {
  statistic <- statistic[[1]]
  if (!statistic %in% c("beta", "t-value")) {
    stop("'statistic' should be either 'beta' or 't-value'")
  }

  term_index <- stat_qap_lm_resolve_term_index(x = x, term = term)
  replicate_values <- if (statistic == "beta") x$beta.dist[, term_index] else x$t.dist[, term_index]
  replicate_values <- replicate_values[!(is.na(replicate_values) | is.nan(replicate_values))]
  if (length(replicate_values) == 0L) {
    stop("There are no valid replicate statistics to plot for this coefficient")
  }

  observed_value <- if (statistic == "beta") x$coefficients[[term_index]] else x$t.stat[[term_index]]
  term_name <- names(x$coefficients)[[term_index]]
  if (is.null(main)) {
    main <- paste("QAP LM", statistic, "for", term_name)
  }
  if (is.null(sub)) {
    sub <- paste("Permutations:", x$reps)
  }
  if (is.null(xlab)) {
    xlab <- paste(statistic, "replicates")
  }

  graphics::hist(
    replicate_values,
    breaks = breaks,
    col = col,
    border = border,
    prob = prob,
    main = main,
    sub = sub,
    xlab = xlab,
    ...
  )
  if (!(is.na(observed_value) | is.nan(observed_value))) {
    graphics::abline(v = observed_value, col = observed_col, lwd = observed_lwd)
  }
  invisible(x)
}


#' Prepare predictor inputs for QAP linear models
#'
#' @keywords internal
#' @noRd
stat_qap_lm_prepare_predictors <- function(x, directed_flag, reference) {
  if (is.null(x)) {
    stop("'x' should contain at least one predictor network")
  }

  predictor_list <- x
  if (inherits(predictor_list, "igraph") ||
      inherits(predictor_list, "network") ||
      is.matrix(predictor_list) ||
      is.data.frame(predictor_list)) {
    predictor_list <- list(predictor_list)
  }
  if (!is.list(predictor_list) || length(predictor_list) == 0L) {
    stop("'x' should be a predictor network or a non-empty list of predictor networks")
  }

  lapply(
    predictor_list,
    function(one_predictor) {
      stat_qap_cor_prepare_matrix(
        x = one_predictor,
        directed_override = directed_flag,
        reference = reference
      )
    }
  )
}


#' Determine coefficient names for QAP linear models
#'
#' @keywords internal
#' @noRd
stat_qap_lm_predictor_names <- function(x, n_predictors, x_call = NULL) {
  out <- rep("", n_predictors)

  if (is.list(x) && !is.null(names(x))) {
    out[seq_len(min(length(names(x)), n_predictors))] <- names(x)[seq_len(min(length(names(x)), n_predictors))]
  }

  if (!is.null(x_call)) {
    if (is.call(x_call) && identical(as.character(x_call[[1]]), "list")) {
      call_names <- vapply(
        as.list(x_call)[-1],
        function(one_call) {
          paste(deparse(one_call, width.cutoff = 500L), collapse = "")
        },
        character(1)
      )
      missing <- which(!nzchar(out))
      out[missing] <- call_names[seq_along(missing)]
    } else if (n_predictors == 1L && !nzchar(out[[1]])) {
      out[[1]] <- paste(deparse(x_call, width.cutoff = 500L), collapse = "")
    }
  }

  missing <- which(!nzchar(out))
  out[missing] <- paste0("x", missing)
  out
}


#' Run a single netlm fit with aligned matrices
#'
#' @keywords internal
#' @noRd
stat_qap_lm_run_netlm <- function(y,
                                  x,
                                  intercept,
                                  reps,
                                  diagonal,
                                  mode,
                                  nullhyp,
                                  test.statistic,
                                  tol,
                                  seed) {
  if (!is.null(seed)) {
    set.seed(as.integer(seed))
  }

  tryCatch(
    sna::netlm(
      y = y,
      x = x,
      intercept = intercept,
      mode = mode,
      diag = diagonal,
      nullhyp = nullhyp,
      test.statistic = test.statistic,
      tol = tol,
      reps = reps
    ),
    error = function(e) {
      if (identical(nullhyp, "qapspp") &&
          grepl("singular matrix", conditionMessage(e), fixed = TRUE)) {
        warning(
          "QAP SPP permutations failed due to a singular intermediate regression; ",
          "falling back to 'qapy' for this model.",
          call. = FALSE
        )
        return(sna::netlm(
          y = y,
          x = x,
          intercept = intercept,
          mode = mode,
          diag = diagonal,
          nullhyp = "qapy",
          test.statistic = test.statistic,
          tol = tol,
          reps = reps
        ))
      }
      if (identical(test.statistic, "t-value")) {
        warning(
          "T-value permutation distribution could not be computed by 'sna::netlm'; ",
          "falling back to a beta-based fit for the same model. ",
          "Observed t-values remain available, but permutation-based t-value p-values are returned as NA.",
          call. = FALSE
        )
        beta_fit <- tryCatch(
          sna::netlm(
            y = y,
            x = x,
            intercept = intercept,
            mode = mode,
            diag = diagonal,
            nullhyp = nullhyp,
            test.statistic = "beta",
            tol = tol,
            reps = reps
          ),
          error = function(e2) {
            if (identical(nullhyp, "qapspp") &&
                grepl("singular matrix", conditionMessage(e2), fixed = TRUE)) {
              return(sna::netlm(
                y = y,
                x = x,
                intercept = intercept,
                mode = mode,
                diag = diagonal,
                nullhyp = "qapy",
                test.statistic = "beta",
                tol = tol,
                reps = reps
              ))
            }
            stop(e2)
          }
        )
        beta_fit$dist <- matrix(
          numeric(0),
          nrow = 0L,
          ncol = length(beta_fit$coefficients)
        )
        attr(beta_fit, "stat_qap_lm_fallback_beta_for_t") <- TRUE
        return(beta_fit)
      }
      stop(e)
    }
  )
}


#' Extract observed t-values from a netlm fit
#'
#' @keywords internal
#' @noRd
stat_qap_lm_extract_tstat <- function(fit) {
  if (fit$df.residual <= 0 || fit$rank <= 0) {
    return(rep(NA_real_, length(fit$coefficients)))
  }

  resvar <- sum(fit$residuals^2) / (fit$n - fit$rank)
  se <- tryCatch(
    sqrt(diag(chol2inv(fit$qr$qr)) * resvar),
    error = function(e) {
      rep(NA_real_, length(fit$coefficients))
    }
  )
  as.numeric(fit$coefficients) / se
}


#' Compute empirical p-values for coefficient matrices
#'
#' @keywords internal
#' @noRd
stat_qap_lm_empirical_p_matrix <- function(observed, replicate) {
  tie_tolerance <- sqrt(.Machine$double.eps)

  p_greater <- p_less <- p_equal <- p_two_sided <- rep(NA_real_, length(observed))
  names(p_greater) <- names(p_less) <- names(p_equal) <- names(p_two_sided) <- names(observed)

  for (coef_index in seq_along(observed)) {
    obs <- observed[[coef_index]]
    rep_values <- replicate[, coef_index]
    valid <- !(is.na(rep_values) | is.nan(rep_values))
    if (is.na(obs) || is.nan(obs) || !any(valid)) {
      next
    }

    rep_values <- rep_values[valid]
    equal <- abs(rep_values - obs) <= tie_tolerance
    p_equal[[coef_index]] <- mean(equal)
    p_greater[[coef_index]] <- mean(rep_values > (obs + tie_tolerance))
    p_less[[coef_index]] <- mean(rep_values < (obs - tie_tolerance))
    p_two_sided[[coef_index]] <- min(
      1,
      2 * min(
        p_greater[[coef_index]] + p_equal[[coef_index]],
        p_less[[coef_index]] + p_equal[[coef_index]]
      )
    )
  }

  list(
    p.greater = p_greater,
    p.less = p_less,
    p.equal = p_equal,
    p.two.sided = p_two_sided
  )
}


#' Resolve a coefficient selection for plotting
#'
#' @keywords internal
#' @noRd
stat_qap_lm_resolve_term_index <- function(x, term) {
  if (is.numeric(term)) {
    if (length(term) != 1L || is.na(term) || term < 1 || term > length(x$coefficients)) {
      stop("'term' should select an existing coefficient")
    }
    return(as.integer(term))
  }

  if (is.character(term) && length(term) == 1L) {
    match_index <- match(term, names(x$coefficients))
    if (is.na(match_index)) {
      stop("'term' should match one of the coefficient names")
    }
    return(match_index)
  }

  stop("'term' should be either a single index or a single coefficient name")
}

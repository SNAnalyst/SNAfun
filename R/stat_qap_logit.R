#' Quadratic Assignment Procedure logistic model
#'
#' Fit a simple QAP-based logistic network model for one binary dependent
#' network and one or more predictor networks.
#'
#' \code{stat_qap_logit()} extends \code{\link[sna]{netlogit}} to the graph
#' classes used throughout \pkg{snafun}. The dependent network \code{y} and all
#' predictors in \code{x} may be supplied as \code{igraph} objects,
#' \code{network} objects, square adjacency matrices, or edgelist
#' \code{data.frame}s. All inputs are converted to aligned one-mode matrices,
#' after which \code{\link[sna]{netlogit}} is used to fit the logistic model and
#' generate a QAP null distribution.
#'
#' The dependent network must be binary on the cells that are actually analyzed.
#' If \code{diagonal = FALSE}, diagonal values are ignored and may therefore
#' differ from 0/1 without affecting the fit. Predictors may be binary or
#' valued.
#'
#' As in \code{\link{stat_qap_lm}}, \code{directed = "auto"} infers the graph
#' type from \code{y}. Predictor names are preserved from the original call
#' where possible, so \code{list(net1, net2)} is reported as \code{net1} and
#' \code{net2} rather than the backend defaults \code{x1} and \code{x2}.
#'
#' \code{test.statistic = "beta"} emphasizes raw logistic coefficients,
#' whereas \code{test.statistic = "z-value"} emphasizes those coefficients
#' relative to their estimated standard errors. In teaching terms,
#' \code{"beta"} is easiest to read as an effect size on the log-odds scale,
#' while \code{"z-value"} is closer to the classical regression question of
#' how large a coefficient is relative to its estimation uncertainty. This
#' default matches the default \code{test.statistic} used by
#' \code{\link[sna]{netlogit}}.
#'
#' The summary method reports four simple classification measures based on the
#' model's confusion table. \code{accuracy} is the overall proportion of dyads
#' that are classified correctly. \code{sensitivity} is the proportion of
#' observed ties that are predicted as ties. \code{specificity} is the
#' proportion of observed non-ties that are predicted as non-ties.
#' \code{precision} is the proportion of predicted ties that are actually ties.
#' These correspond closely to parts of the standard \code{netlogit} output:
#' \code{accuracy} matches "Total Fraction Correct", \code{precision} matches
#' "Fraction Predicted 1s Correct", \code{sensitivity} equals
#' \eqn{1 -} "False Negative Rate", and \code{specificity} equals
#' \eqn{1 -} "False Positive Rate".
#' These quantities are often useful in teaching because they separate
#' "how often was the model right overall?" from "how well did it recover the
#' ties?" and "how many predicted ties were true ties?".
#'
#' @param y Binary dependent network or network-like object.
#' @param x Predictor network, or a list of predictor networks. Each predictor
#'   must be supplied in one of the same formats as \code{y}.
#' @param intercept Logical scalar; should an intercept be included?
#' @param reps Number of QAP permutations.
#' @param diagonal Logical scalar; should diagonal entries be included?
#' @param directed How should the input be interpreted? One of \code{"auto"},
#'   \code{"directed"}, or \code{"undirected"}. The default infers this from
#'   \code{y}.
#' @param test.statistic Which statistic should be emphasized in printed output
#'   and plots? One of \code{"z-value"} or \code{"beta"}.
#' @param nullhyp Which permutation design should be used? One of
#'   \code{"qapspp"}, \code{"qapy"}, \code{"qap"}, \code{"qapx"},
#'   \code{"qapallx"}, \code{"cugtie"}, \code{"cugden"},
#'   \code{"cuguman"}, or \code{"classical"}. The default
#'   \code{"qapspp"} mirrors the semi-partialling strategy that is typically
#'   most useful for multiple-predictor QAP models.
#' @param tol Numerical tolerance passed to the underlying fit.
#' @param seed Optional integer seed for reproducible permutations.
#'
#' @return An object of class \code{stat_qap_logit}. It contains the fitted
#'   coefficients, exponentiated coefficients, z-values, fitted values, model
#'   deviance information, a confusion table, and the permutation distribution
#'   for the requested \code{test.statistic}.
#' @family statistics functions
#' @export
#'
#' @examples
#' y <- snafun::create_manual_graph(A -+ B, B -+ C, C -+ A)
#' x1 <- snafun::create_manual_graph(A -+ B, B -+ A, C -+ A)
#' x2 <- snafun::create_manual_graph(A -+ C, C -+ B)
#'
#' stat_qap_logit(y, x = x1, reps = 49)
#' stat_qap_logit(y, x = list(x1, x2), reps = 49, test.statistic = "beta")
#' stat_qap_logit(y, x = list(x1, x2), reps = 49, nullhyp = "qapy")
#'
#' y_u <- snafun::create_manual_graph(A -- B, B -- C, C -- D)
#' x_u <- snafun::create_manual_graph(A -- B, B -- D, C -- D)
#' stat_qap_logit(y_u, x = x_u, reps = 49, directed = "undirected")
stat_qap_logit <- function(y,
                           x,
                           intercept = TRUE,
                           reps = 1000,
                           diagonal = FALSE,
                           directed = c("auto", "directed", "undirected"),
                           test.statistic = c("z-value", "beta"),
                           nullhyp = c(
                             "qapspp",
                             "qapy",
                             "qap",
                             "qapx",
                             "qapallx",
                             "cugtie",
                             "cugden",
                             "cuguman",
                             "classical"
                           ),
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
  if (!test.statistic %in% c("z-value", "beta")) {
    stop("'test.statistic' should be either 'z-value' or 'beta'")
  }
  if (!nullhyp %in% c(
    "qapspp",
    "qapy",
    "qap",
    "qapx",
    "qapallx",
    "cugtie",
    "cugden",
    "cuguman",
    "classical"
  )) {
    stop(
      "'nullhyp' should be one of 'qapspp', 'qapy', 'qap', 'qapx', ",
      "'qapallx', 'cugtie', 'cugden', 'cuguman', or 'classical'"
    )
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
  stat_qap_logit_validate_response(
    y = y_matrix,
    diagonal = diagonal
  )
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

  fit <- stat_qap_logit_run_netlogit(
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
  std_error <- stats::setNames(as.numeric(fit$se), coefficient_names)
  z_stat <- stat_qap_logit_compute_z(
    coefficients = coefficients,
    std_error = std_error
  )
  exp_beta <- stats::setNames(exp(coefficients), coefficient_names)

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
  replicate_z <- if (identical(test.statistic, "z-value")) {
    dist <- fit$dist
    colnames(dist) <- coefficient_names
    dist
  } else {
    empty_dist
  }
  z_fallback <- isTRUE(attr(fit, "stat_qap_logit_fallback_beta_for_z"))

  p_beta <- stat_qap_logit_pvalues(
    fit = fit,
    coefficient_names = coefficient_names,
    available = identical(test.statistic, "beta")
  )
  p_z <- stat_qap_logit_pvalues(
    fit = fit,
    coefficient_names = coefficient_names,
    available = identical(test.statistic, "z-value") && !isTRUE(z_fallback)
  )

  result <- list(
    coefficients = coefficients,
    exp.beta = exp_beta,
    z.stat = z_stat,
    se = std_error,
    fitted.values = fit$fitted.values,
    residuals = fit$residuals,
    linear.predictors = fit$linear.predictors,
    n = fit$n,
    df.model = fit$df.model,
    df.residual = fit$df.residual,
    df.null = fit$df.null,
    deviance = fit$deviance,
    null.deviance = fit$null.deviance,
    aic = fit$aic,
    bic = fit$bic,
    confusion.table = fit$ctable,
    pseudo.r2 = stat_qap_logit_pseudo_r2(fit = fit),
    beta.dist = replicate_beta,
    z.dist = replicate_z,
    p.beta = p_beta,
    p.z = p_z,
    reps = reps,
    valid.reps.beta = colSums(!(is.na(replicate_beta) | is.nan(replicate_beta))),
    valid.reps.z = colSums(!(is.na(replicate_z) | is.nan(replicate_z))),
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
  class(result) <- "stat_qap_logit"
  result
}


#' Summarize a QAP logistic network model
#'
#' Summarize the coefficient-level results from \code{\link{stat_qap_logit}}.
#'
#' @param object Object returned by \code{\link{stat_qap_logit}}.
#' @param ... Ignored.
#'
#' @return An object of class \code{summary.stat_qap_logit}.
#' @export
summary.stat_qap_logit <- function(object, ...) {
  coefficient_table <- cbind(
    beta = object$coefficients,
    exp.beta = object$exp.beta,
    std.error = object$se,
    z.value = object$z.stat
  )

  beta_available <- any(!is.na(object$p.beta$p.greater))
  z_available <- any(!is.na(object$p.z$p.greater))

  if (isTRUE(beta_available)) {
    coefficient_table <- cbind(
      coefficient_table,
      `Pr(beta>Obs)` = object$p.beta$p.greater,
      `Pr(beta<Obs)` = object$p.beta$p.less,
      `Pr(beta=Obs)` = object$p.beta$p.equal,
      `Pr(beta two-sided)` = object$p.beta$p.two.sided
    )
  }
  if (isTRUE(z_available)) {
    coefficient_table <- cbind(
      coefficient_table,
      `Pr(z>Obs)` = object$p.z$p.greater,
      `Pr(z<Obs)` = object$p.z$p.less,
      `Pr(z=Obs)` = object$p.z$p.equal,
      `Pr(z two-sided)` = object$p.z$p.two.sided
    )
  }

  lr_test <- stat_qap_logit_lr_test(object = object)
  classification <- stat_qap_logit_classification(object = object)
  fit_table <- c(
    `Null deviance (df)` = object$null.deviance,
    `Residual deviance (df)` = object$deviance,
    `LR chi-square` = lr_test$statistic,
    `LR df` = lr_test$parameter,
    `LR p-value` = lr_test$p.value,
    AIC = object$aic,
    BIC = object$bic,
    `Pseudo-R2 (adj)` = object$pseudo.r2$adjusted,
    `Pseudo-R2 (McFadden)` = object$pseudo.r2$mcfadden
  )

  out <- list(
    coefficient.table = coefficient_table,
    coefficients = object$coefficients,
    exp.beta = object$exp.beta,
    se = object$se,
    z.stat = object$z.stat,
    reps = object$reps,
    valid.reps.beta = object$valid.reps.beta,
    valid.reps.z = object$valid.reps.z,
    intercept = object$intercept,
    diagonal = object$diagonal,
    mode = object$mode,
    test.statistic = object$test.statistic,
    nullhyp = object$nullhyp,
    requested.nullhyp = object$requested.nullhyp,
    beta.p.available = beta_available,
    z.p.available = z_available,
    predictor.names = object$predictor.names,
    n = object$n,
    df.model = object$df.model,
    df.residual = object$df.residual,
    df.null = object$df.null,
    deviance = object$deviance,
    null.deviance = object$null.deviance,
    aic = object$aic,
    bic = object$bic,
    pseudo.r2 = object$pseudo.r2,
    lr.test = lr_test,
    fit.table = fit_table,
    fit.df = c(
      `Null deviance (df)` = object$df.null,
      `Residual deviance (df)` = object$df.residual
    ),
    confusion.table = object$confusion.table,
    classification = classification,
    call = object$call
  )
  class(out) <- "summary.stat_qap_logit"
  out
}


#' Format logistic goodness-of-fit values with degrees of freedom
#'
#' @keywords internal
#' @noRd
stat_qap_logit_format_fit_table <- function(x, digits = 4) {
  fit_table <- x$fit.table
  null_dev <- fit_table[["Null deviance (df)"]]
  resid_dev <- fit_table[["Residual deviance (df)"]]
  fit_table[["Null deviance (df)"]] <- paste0(
    signif(null_dev, digits = digits),
    " (",
    x$fit.df[["Null deviance (df)"]],
    ")"
  )
  fit_table[["Residual deviance (df)"]] <- paste0(
    signif(resid_dev, digits = digits),
    " (",
    x$fit.df[["Residual deviance (df)"]],
    ")"
  )
  fit_table
}


#' Print a QAP logistic network model
#'
#' Print a compact coefficient summary for a \code{stat_qap_logit} object.
#'
#' @param x Object returned by \code{\link{stat_qap_logit}}.
#' @param digits Number of significant digits to print.
#' @param ... Ignored.
#'
#' @return Invisibly returns \code{x}.
#' @export
print.stat_qap_logit <- function(x, digits = 4, ...) {
  x_summary <- summary(x)
  cat("\nQuadratic Assignment Procedure Logistic Model\n\n")
  cat("Graph Type:", x$mode, "\n")
  cat("Diagonal Used:", x$diagonal, "\n")
  cat("Intercept:", x$intercept, "\n")
  cat("Permutations:", x$reps, "\n")
  cat("Observations:", x$n, "\n")
  cat("Residual df:", x$df.residual, "\n")
  cat("Highlighted statistic:", x$test.statistic, "\n")
  cat("Null hypothesis:", x$nullhyp, "\n")
  if (!is.null(x$requested.nullhyp) && !identical(x$requested.nullhyp, x$nullhyp)) {
    cat("Requested null hypothesis:", x$requested.nullhyp, "\n")
  }
  cat("\n")

  p_current <- if (identical(x$test.statistic, "beta")) x$p.beta else x$p.z
  coefficient_table <- cbind(
    beta = x$coefficients,
    exp.beta = x$exp.beta,
    z.value = x$z.stat,
    `Pr(>Obs)` = p_current$p.greater,
    `Pr(<Obs)` = p_current$p.less,
    `Pr(=Obs)` = p_current$p.equal,
    `Pr(two-sided)` = p_current$p.two.sided
  )
  print(signif(coefficient_table, digits = digits))

  cat("\nGoodness of Fit\n\n")
  print(stat_qap_logit_format_fit_table(x_summary, digits = digits))
  cat("\nConfusion Table\n\n")
  print(x$confusion.table)
  cat("\nClassification\n\n")
  print(signif(unlist(x_summary$classification), digits = digits))
  invisible(x)
}


#' Print a summarized QAP logistic network model
#'
#' Print the object returned by \code{\link{summary.stat_qap_logit}}.
#'
#' @param x Object returned by \code{\link{summary.stat_qap_logit}}.
#' @param digits Number of significant digits to print.
#' @param ... Ignored.
#'
#' @return Invisibly returns \code{x}.
#' @export
print.summary.stat_qap_logit <- function(x, digits = 4, ...) {
  cat("\nSummary of QAP Logistic Model\n\n")
  cat("Graph Type:", x$mode, "\n")
  cat("Diagonal Used:", x$diagonal, "\n")
  cat("Intercept:", x$intercept, "\n")
  cat("Permutations:", x$reps, "\n")
  cat("Observations:", x$n, "\n")
  cat("Residual df:", x$df.residual, "\n")
  cat("Highlighted statistic:", x$test.statistic, "\n")
  cat("Null hypothesis:", x$nullhyp, "\n")
  if (!is.null(x$requested.nullhyp) && !identical(x$requested.nullhyp, x$nullhyp)) {
    cat("Requested null hypothesis:", x$requested.nullhyp, "\n")
  }
  if (!isTRUE(x$beta.p.available) && isTRUE(x$z.p.available)) {
    cat("Permutation p-values are shown for z-values only.\n")
  }
  if (isTRUE(x$beta.p.available) && !isTRUE(x$z.p.available)) {
    cat("Permutation p-values are shown for betas only.\n")
  }
  cat("\n")
  print(signif(x$coefficient.table, digits = digits))

  cat("\nGoodness of Fit\n\n")
  print(stat_qap_logit_format_fit_table(x, digits = digits))
  cat("\nConfusion Table\n\n")
  print(x$confusion.table)
  cat("\nClassification\n\n")
  print(signif(unlist(x$classification), digits = digits))
  invisible(x)
}


#' Plot a QAP logistic network model result
#'
#' Plot the permutation distribution for a selected coefficient from
#' \code{\link{stat_qap_logit}}.
#'
#' @param x Object returned by \code{\link{stat_qap_logit}}.
#' @param term Coefficient to plot. Either a numeric index or a coefficient
#'   name.
#' @param statistic Which replicate distribution should be plotted? One of
#'   \code{"beta"} or \code{"z-value"}.
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
plot.stat_qap_logit <- function(x,
                                term = 1,
                                statistic = c("beta", "z-value"),
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
  if (!statistic %in% c("beta", "z-value")) {
    stop("'statistic' should be either 'beta' or 'z-value'")
  }

  term_index <- stat_qap_lm_resolve_term_index(x = x, term = term)
  replicate_values <- if (identical(statistic, "beta")) x$beta.dist[, term_index] else x$z.dist[, term_index]
  replicate_values <- replicate_values[!(is.na(replicate_values) | is.nan(replicate_values))]
  if (length(replicate_values) == 0L) {
    stop("There are no valid replicate statistics to plot for this coefficient")
  }

  observed_value <- if (identical(statistic, "beta")) x$coefficients[[term_index]] else x$z.stat[[term_index]]
  term_name <- names(x$coefficients)[[term_index]]
  if (is.null(main)) {
    main <- paste("QAP logit", statistic, "for", term_name)
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


#' Validate the binary response for QAP logistic models
#'
#' @keywords internal
#' @noRd
stat_qap_logit_validate_response <- function(y, diagonal) {
  values <- as.vector(y)
  if (!isTRUE(diagonal)) {
    values <- as.vector(y[row(y) != col(y)])
  }
  values <- values[!(is.na(values) | is.nan(values))]

  # Logistic network regression is defined for a binary dependent network.
  if (!all(values %in% c(0, 1))) {
    stop(
      "The dependent network 'y' should be binary (0/1) on the cells included in the model"
    )
  }
  invisible(NULL)
}


#' Run a single netlogit fit with aligned matrices
#'
#' @keywords internal
#' @noRd
stat_qap_logit_run_netlogit <- function(y,
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

  fit_call <- function(nullhyp_value, statistic_value) {
    sna::netlogit(
      y = y,
      x = x,
      intercept = intercept,
      mode = mode,
      diag = diagonal,
      nullhyp = nullhyp_value,
      test.statistic = statistic_value,
      tol = tol,
      reps = reps
    )
  }

  tryCatch(
    fit_call(nullhyp, test.statistic),
    error = function(e) {
      if (identical(nullhyp, "qapspp") &&
          grepl("singular matrix", conditionMessage(e), fixed = TRUE)) {
        warning(
          "QAP SPP permutations failed due to a singular intermediate regression; ",
          "falling back to 'qapy' for this model.",
          call. = FALSE
        )
        fit <- fit_call("qapy", test.statistic)
        fit$nullhyp <- "qapy"
        return(fit)
      }
      if (identical(test.statistic, "z-value")) {
        warning(
          "Z-value permutation distribution could not be computed by 'sna::netlogit'; ",
          "falling back to a beta-based fit for the same model. ",
          "Observed z-values remain available, but permutation-based z-value p-values are returned as NA.",
          call. = FALSE
        )
        fallback_nullhyp <- nullhyp
        beta_fit <- tryCatch(
          fit_call(fallback_nullhyp, "beta"),
          error = function(e2) {
            if (identical(fallback_nullhyp, "qapspp") &&
                grepl("singular matrix", conditionMessage(e2), fixed = TRUE)) {
              fallback_nullhyp <<- "qapy"
              return(fit_call("qapy", "beta"))
            }
            stop(e2)
          }
        )
        beta_fit$dist <- matrix(
          numeric(0),
          nrow = 0L,
          ncol = length(beta_fit$coefficients)
        )
        beta_fit$nullhyp <- fallback_nullhyp
        attr(beta_fit, "stat_qap_logit_fallback_beta_for_z") <- TRUE
        return(beta_fit)
      }
      stop(e)
    }
  )
}


#' Convert netlogit p-values to a consistent snafun structure
#'
#' @keywords internal
#' @noRd
stat_qap_logit_pvalues <- function(fit, coefficient_names, available) {
  if (!isTRUE(available)) {
    p_greater <- p_less <- p_equal <- p_two_sided <- rep(NA_real_, length(coefficient_names))
    names(p_greater) <- names(p_less) <- names(p_equal) <- names(p_two_sided) <- coefficient_names
    return(list(
      p.greater = p_greater,
      p.less = p_less,
      p.equal = p_equal,
      p.two.sided = p_two_sided
    ))
  }

  p_less <- stats::setNames(as.numeric(fit$pleeq), coefficient_names)
  p_greater <- stats::setNames(as.numeric(fit$pgreq), coefficient_names)
  p_two_sided <- stats::setNames(as.numeric(fit$pgreqabs), coefficient_names)

  p_equal <- pmax(0, 1 - p_less - p_greater)
  names(p_equal) <- coefficient_names

  list(
    p.greater = p_greater,
    p.less = p_less,
    p.equal = p_equal,
    p.two.sided = p_two_sided
  )
}


#' Compute pseudo-R2 measures for a netlogit fit
#'
#' @keywords internal
#' @noRd
stat_qap_logit_pseudo_r2 <- function(fit) {
  adjusted <- (fit$null.deviance - fit$deviance) /
    (fit$null.deviance - fit$deviance + fit$df.null)
  mcfadden <- (fit$null.deviance - fit$deviance) / fit$null.deviance

  list(
    adjusted = adjusted,
    mcfadden = mcfadden
  )
}


#' Compute observed z-values from coefficients and standard errors
#'
#' @keywords internal
#' @noRd
stat_qap_logit_compute_z <- function(coefficients, std_error) {
  z <- rep(NA_real_, length(coefficients))
  names(z) <- names(coefficients)
  valid <- !(is.na(coefficients) | is.na(std_error) | std_error == 0)
  z[valid] <- coefficients[valid] / std_error[valid]
  z
}


#' Compute a likelihood-ratio improvement summary
#'
#' @keywords internal
#' @noRd
stat_qap_logit_lr_test <- function(object) {
  statistic <- object$null.deviance - object$deviance
  parameter <- object$df.null - object$df.residual
  p.value <- if (is.na(statistic) || is.na(parameter) || parameter < 0) {
    NA_real_
  } else {
    stats::pchisq(statistic, df = parameter, lower.tail = FALSE)
  }

  list(
    statistic = statistic,
    parameter = parameter,
    p.value = p.value
  )
}


#' Summarize binary classification performance
#'
#' @keywords internal
#' @noRd
stat_qap_logit_classification <- function(object) {
  confusion <- unclass(object$confusion.table)
  if (!identical(dim(confusion), c(2L, 2L))) {
    return(list(
      accuracy = NA_real_,
      sensitivity = NA_real_,
      specificity = NA_real_,
      precision = NA_real_
    ))
  }

  true_negative <- confusion[1, 1]
  false_negative <- confusion[1, 2]
  false_positive <- confusion[2, 1]
  true_positive <- confusion[2, 2]
  total <- sum(confusion)

  safe_divide <- function(num, den) {
    if (is.na(den) || den == 0) {
      return(NA_real_)
    }
    num / den
  }

  list(
    accuracy = safe_divide(true_negative + true_positive, total),
    sensitivity = safe_divide(true_positive, true_positive + false_negative),
    specificity = safe_divide(true_negative, true_negative + false_positive),
    precision = safe_divide(true_positive, true_positive + false_positive)
  )
}

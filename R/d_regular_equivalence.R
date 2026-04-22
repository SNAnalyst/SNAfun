#' Regular equivalence
#'
#' Calculate approximate regular equivalence between all pairs of vertices.
#'
#' \code{d_regular_equivalence()} is based on the iterative CATREGE logic used
#' in \code{\link[sna]{redist}}, but is implemented in \pkg{snafun} style so it
#' works directly for \code{igraph}, \code{network}, \code{matrix}, and
#' \code{data.frame} edgelist input. The output is an equivalence matrix rather
#' than a distance matrix: values run from 0 to 1, with 1 indicating the
#' strongest regular equivalence found by the iterative refinement process and 0
#' indicating the weakest equivalence in the final solution.
#'
#' The algorithm starts from a seed partition (by default, all vertices are in
#' the same role) and repeatedly refines that partition by comparing the types
#' of alters to which each vertex is tied. Two vertices remain in the same role
#' only if they have identical mixes of in- and out-neighbor roles, together
#' with identical tie types. The final role history is then converted to a
#' pairwise equivalence score.
#'
#' Optional edge weights can be incorporated. When weights are used, dyads are
#' typed by their actual weighted in/out pattern rather than merely by the
#' presence or absence of ties. This makes the method more sensitive, but it can
#' also create many distinct tie types when the weights are highly granular.
#'
#' For unweighted binary networks, the result is the normalized similarity
#' counterpart of \code{\link[sna]{redist}} with \code{method = "catrege"}.
#'
#' @param x graph of class \code{igraph}, \code{network}, \code{matrix}, or
#'   \code{data.frame}.
#' @param weights Either \code{NA} (the default), \code{NULL}, or a character
#'   string giving an edge attribute name. If \code{NULL}, the edge attribute
#'   \code{"weight"} is used when available. If \code{NA}, no weights are used.
#'   If a character string is supplied, that attribute or edgelist column is
#'   used as the tie value.
#' @param digits Number of decimals to use in the returned matrix.
#' @param mode Character scalar indicating whether the graph should be treated
#'   as \code{"digraph"} or \code{"graph"}. The default \code{"auto"} infers
#'   this from \code{x}.
#' @param diag Logical scalar, should diagonal entries be treated as meaningful
#'   ties?
#' @param seed.partition Optional vector giving a starting partition for the
#'   iterative refinement. By default all vertices start in the same role.
#'
#' @return A square matrix of regular-equivalence scores between 0 and 1.
#' @export
#'
#' @examples
#' g <- snafun::create_manual_graph(A -+ B, B -+ C, D -+ E, E -+ F)
#' d_regular_equivalence(g)
#' d_regular_equivalence(snafun::to_network(g))
#' d_regular_equivalence(snafun::to_matrix(g))
#'
#' g_w <- snafun::add_edge_attributes(g, "weight", c(1, 2, 1, 2))
#' d_regular_equivalence(g_w, weights = "weight")
#'
#' seed <- c(1, 1, 1, 2, 2, 2)
#' d_regular_equivalence(g, seed.partition = seed)
d_regular_equivalence <- function(x,
                                  weights = NA,
                                  digits = 3,
                                  mode = c("auto", "digraph", "graph"),
                                  diag = FALSE,
                                  seed.partition = NULL) {
  UseMethod("d_regular_equivalence")
}


#' @export
d_regular_equivalence.default <- function(x,
                                          weights = NA,
                                          digits = 3,
                                          mode = c("auto", "digraph", "graph"),
                                          diag = FALSE,
                                          seed.partition = NULL) {
  txt <- methods_error_message("x", "d_regular_equivalence")
  stop(txt)
}


#' @export
d_regular_equivalence.igraph <- function(x,
                                         weights = NA,
                                         digits = 3,
                                         mode = c("auto", "digraph", "graph"),
                                         diag = FALSE,
                                         seed.partition = NULL) {
  if (missing(weights)) {
    weights <- NA
  }
  mode <- snafun.match.arg(mode)
  adjacency <- d_regular_equivalence_prepare_igraph(
    x = x,
    weights = weights
  )
  d_regular_equivalence.matrix(
    x = adjacency,
    digits = digits,
    mode = if (identical(mode, "auto")) {
      if (snafun::is_directed(x)) "digraph" else "graph"
    } else {
      mode
    },
    diag = diag,
    seed.partition = seed.partition
  )
}


#' @export
d_regular_equivalence.network <- function(x,
                                          weights = NA,
                                          digits = 3,
                                          mode = c("auto", "digraph", "graph"),
                                          diag = FALSE,
                                          seed.partition = NULL) {
  if (missing(weights)) {
    weights <- NA
  }
  mode <- snafun.match.arg(mode)
  adjacency <- d_regular_equivalence_prepare_network(
    x = x,
    weights = weights
  )
  d_regular_equivalence.matrix(
    x = adjacency,
    digits = digits,
    mode = if (identical(mode, "auto")) {
      if (snafun::is_directed(x)) "digraph" else "graph"
    } else {
      mode
    },
    diag = diag,
    seed.partition = seed.partition
  )
}


#' @export
d_regular_equivalence.data.frame <- function(x,
                                             weights = NA,
                                             digits = 3,
                                             mode = c("auto", "digraph", "graph"),
                                             diag = FALSE,
                                             seed.partition = NULL) {
  if (missing(weights)) {
    weights <- NA
  }
  mode <- snafun.match.arg(mode)
  adjacency <- d_regular_equivalence_prepare_edgelist(
    x = x,
    weights = weights,
    mode = mode
  )
  d_regular_equivalence.matrix(
    x = adjacency,
    digits = digits,
    mode = mode,
    diag = diag,
    seed.partition = seed.partition
  )
}


#' @export
d_regular_equivalence.matrix <- function(x,
                                         weights = NA,
                                         digits = 3,
                                         mode = c("auto", "digraph", "graph"),
                                         diag = FALSE,
                                         seed.partition = NULL) {
  if (missing(weights)) {
    weights <- NA
  }
  mode <- snafun.match.arg(mode)
  if (!is.matrix(x) || nrow(x) != ncol(x)) {
    stop("Please input a square adjacency matrix for 'x'")
  }
  if (length(digits) != 1L || is.na(digits) || digits < 0) {
    stop("'digits' should be a single non-negative number")
  }
  if (!is.logical(diag) || length(diag) != 1L || is.na(diag)) {
    stop("'diag' should be either TRUE or FALSE")
  }

  adjacency <- unclass(as.matrix(x))
  if (!is.numeric(adjacency) && !is.logical(adjacency)) {
    stop("The adjacency matrix should contain numeric or logical values")
  }
  adjacency <- apply(adjacency, c(1, 2), as.numeric)
  dimnames(adjacency) <- dimnames(x)

  resolved_mode <- d_regular_equivalence_resolve_mode(
    x = adjacency,
    mode = mode
  )
  adjacency <- d_regular_equivalence_prepare_matrix_core(
    x = adjacency,
    mode = resolved_mode,
    diag = diag
  )
  d_regular_equivalence_core(
    x = adjacency,
    digits = digits,
    seed.partition = seed.partition
  )
}


#' Prepare an igraph for regular-equivalence calculations
#'
#' @keywords internal
#' @noRd
d_regular_equivalence_prepare_igraph <- function(x, weights) {
  attr_weight <- d_regular_equivalence_resolve_graph_weight(
    x = x,
    weights = weights,
    has_weight = snafun::is_weighted(x),
    has_attribute = snafun::has_edge_attribute
  )

  if (is.null(attr_weight)) {
    adjacency <- igraph::as_adjacency_matrix(
      graph = x,
      type = "both",
      attr = NULL,
      sparse = FALSE
    )
    return(as.matrix(adjacency))
  }

  adjacency <- igraph::as_adjacency_matrix(
    graph = x,
    type = "both",
    attr = attr_weight,
    sparse = FALSE
  )
  as.matrix(adjacency)
}


#' Prepare a network object for regular-equivalence calculations
#'
#' @keywords internal
#' @noRd
d_regular_equivalence_prepare_network <- function(x, weights) {
  attr_weight <- d_regular_equivalence_resolve_graph_weight(
    x = x,
    weights = weights,
    has_weight = snafun::is_weighted(x),
    has_attribute = snafun::has_edge_attribute
  )

  if (is.null(attr_weight)) {
    return(network::as.matrix.network(x, matrix.type = "adjacency"))
  }

  network::as.matrix.network(
    x,
    matrix.type = "adjacency",
    attrname = attr_weight
  )
}


#' Prepare an edgelist for regular-equivalence calculations
#'
#' @keywords internal
#' @noRd
d_regular_equivalence_prepare_edgelist <- function(x, weights, mode) {
  if (inherits(x, "tbl_df")) {
    x <- as.data.frame(x)
  }
  if (ncol(x) < 2) {
    stop("'x' should have at least two columns")
  }

  selected_weights <- d_regular_equivalence_resolve_edgelist_weight(
    x = x,
    weights = weights
  )

  if (is.na(selected_weights)) {
    matrix_data <- snafun::to_matrix(x[, 1:2, drop = FALSE])
  } else {
    weight_column <- match(selected_weights, colnames(x))
    x_weighted <- x[, c(1, 2, weight_column), drop = FALSE]
    matrix_data <- snafun::to_matrix(x_weighted)
  }

  if (!identical(mode, "auto")) {
    matrix_data <- d_regular_equivalence_prepare_matrix_core(
      x = matrix_data,
      mode = mode,
      diag = TRUE
    )
  }
  matrix_data
}


#' Resolve the weight attribute to use for graph objects
#'
#' @keywords internal
#' @noRd
d_regular_equivalence_resolve_graph_weight <- function(x,
                                                       weights,
                                                       has_weight,
                                                       has_attribute) {
  if (length(weights) == 0L) {
    if (isTRUE(has_weight)) {
      return("weight")
    }
    return(NULL)
  }
  if (is.na(weights)) {
    return(NULL)
  }
  if (is.null(weights)) {
    if (isTRUE(has_weight)) {
      return("weight")
    }
    return(NULL)
  }
  if (is.character(weights) && length(weights) == 1L) {
    if (has_attribute(x, weights)) {
      return(as.character(weights))
    }
    stop("The edge weights attribute you specified does not occur in 'x'")
  }
  stop("'weights' should be NA, NULL, or a single character string")
}


#' Resolve the weight column to use for edgelists
#'
#' @keywords internal
#' @noRd
d_regular_equivalence_resolve_edgelist_weight <- function(x, weights) {
  if (length(weights) == 0L) {
    if ("weight" %in% colnames(x)) {
      return("weight")
    }
    return(NA_character_)
  }
  if (is.na(weights)) {
    return(NA_character_)
  }
  if (is.null(weights)) {
    if ("weight" %in% colnames(x)) {
      return("weight")
    }
    return(NA_character_)
  }
  if (is.character(weights) && length(weights) == 1L) {
    if (weights %in% colnames(x)) {
      return(as.character(weights))
    }
    stop("The edge weights column you specified does not occur in 'x'")
  }
  stop("'weights' should be NA, NULL, or a single character string")
}


#' Resolve graph mode for regular-equivalence calculations
#'
#' @keywords internal
#' @noRd
d_regular_equivalence_resolve_mode <- function(x, mode) {
  if (!identical(mode, "auto")) {
    return(mode)
  }
  if (snafun::is_directed(x)) {
    "digraph"
  } else {
    "graph"
  }
}


#' Prepare an adjacency matrix for regular-equivalence calculations
#'
#' @keywords internal
#' @noRd
d_regular_equivalence_prepare_matrix_core <- function(x, mode, diag) {
  out <- x

  if (identical(mode, "graph")) {
    sym_rule <- if (all(out %in% c(0, 1))) "weak" else "max"
    out <- snafun::to_symmetric_matrix(out, rule = sym_rule)
  }

  if (!isTRUE(diag)) {
    diag(out) <- 0
  }
  out[is.na(out) | is.nan(out)] <- 0
  out
}


#' Compute normalized regular-equivalence scores
#'
#' @keywords internal
#' @noRd
d_regular_equivalence_core <- function(x, digits, seed.partition) {
  n <- nrow(x)
  vertex_names <- rownames(x)
  if (is.null(vertex_names)) {
    vertex_names <- colnames(x)
  }
  if (is.null(vertex_names)) {
    vertex_names <- as.character(seq_len(n))
  }

  if (is.null(seed.partition)) {
    current_partition <- rep(1, n)
  } else {
    if (length(seed.partition) != n) {
      stop("'seed.partition' should have one value per vertex")
    }
    if (any(is.na(seed.partition))) {
      stop("'seed.partition' should not contain missing values")
    }
    current_partition <- as.integer(as.factor(seed.partition))
  }

  tie_codes <- d_regular_equivalence_tie_codes(x)
  role_history <- matrix(numeric(0), nrow = 0L, ncol = n)
  changed <- TRUE

  while (isTRUE(changed)) {
    neighborhood <- d_regular_equivalence_neighborhood(
      tie_codes = tie_codes,
      partition = current_partition
    )
    role_history <- rbind(role_history, current_partition)

    changed <- FALSE
    next_partition <- seq_len(n)
    for (i in 2:n) {
      for (j in seq_len(i - 1L)) {
        if (current_partition[[i]] == current_partition[[j]]) {
          if (all(neighborhood[, i, ] == neighborhood[, j, ])) {
            next_partition[[i]] <- next_partition[[j]]
          } else {
            changed <- TRUE
          }
        }
      }
    }
    current_partition <- next_partition
  }

  equivalence <- d_regular_equivalence_from_history(
    role_history = role_history,
    vertex_names = vertex_names,
    weighted = !all(x %in% c(0, 1))
  )
  round(equivalence, digits = digits)
}


#' Encode dyads by their in/out tie pattern
#'
#' @keywords internal
#' @noRd
d_regular_equivalence_tie_codes <- function(x) {
  pattern_matrix <- matrix(
    paste(x, t(x), sep = " | "),
    nrow = nrow(x),
    ncol = ncol(x),
    dimnames = dimnames(x)
  )
  zero_pattern <- paste(0, 0, sep = " | ")
  patterns <- unique(as.vector(pattern_matrix))
  patterns <- c(zero_pattern, setdiff(patterns, zero_pattern))
  matrix(
    match(pattern_matrix, patterns) - 1L,
    nrow = nrow(x),
    ncol = ncol(x),
    dimnames = dimnames(x)
  )
}


#' Build neighborhood signatures for the current partition
#'
#' @keywords internal
#' @noRd
d_regular_equivalence_neighborhood <- function(tie_codes, partition) {
  n <- nrow(tie_codes)
  n_types <- max(tie_codes)
  neighborhood <- array(FALSE, dim = c(n_types, n, n))

  if (n_types == 0L) {
    return(neighborhood)
  }

  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      code <- tie_codes[i, j]
      if (code > 0L) {
        neighborhood[code, i, partition[[j]]] <- TRUE
      }
    }
  }
  neighborhood
}


#' Convert role history to normalized equivalence
#'
#' @keywords internal
#' @noRd
d_regular_equivalence_from_history <- function(role_history,
                                              vertex_names,
                                              weighted = FALSE) {
  n <- ncol(role_history)
  equivalence <- matrix(0, nrow = n, ncol = n)

  if (isTRUE(weighted)) {
    history_weights <- seq_len(nrow(role_history))
    history_total <- sum(history_weights)

    for (i in seq_len(n)) {
      for (j in seq_len(n)) {
        same_roles <- role_history[, i] == role_history[, j]
        equivalence[i, j] <- if (any(same_roles)) {
          sum(history_weights[same_roles]) / history_total
        } else {
          0
        }
      }
    }

    dimnames(equivalence) <- list(vertex_names, vertex_names)
    diag(equivalence) <- 1
    return(equivalence)
  }

  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      same_roles <- role_history[, i] == role_history[, j]
      equivalence[i, j] <- if (any(same_roles)) max(which(same_roles)) else 0
    }
  }

  if (max(equivalence) == min(equivalence)) {
    out <- matrix(1, nrow = n, ncol = n)
  } else {
    out <- (equivalence - min(equivalence)) / (max(equivalence) - min(equivalence))
  }

  dimnames(out) <- list(vertex_names, vertex_names)
  diag(out) <- 1
  out
}

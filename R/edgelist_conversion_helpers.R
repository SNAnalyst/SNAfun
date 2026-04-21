#' Resolve conversion semantics for data.frame edgelists
#'
#' These helpers centralize the unavoidable ambiguity of plain edgelists. A
#' visible two-column edgelist does not always tell us whether a one-mode graph
#' is directed: a single row `A, B` could mean either `A -> B` or an undirected
#' tie between `A` and `B`. snafun can only recover the original intent
#' automatically when hidden roundtrip metadata are still present, when the
#' sender/receiver sets are visibly disjoint (bipartite), or when every visible
#' row has an exact reciprocal counterpart. In every other case the safest
#' default remains "directed", but callers can now override that explicitly.
#'
#' @param x data.frame edgelist
#' @param directed optional logical override supplied by the caller
#' @param bipartite optional logical override supplied by the caller
#'
#' @return A list describing the resolved conversion semantics.
#' @keywords internal
#' @noRd
resolve_edgelist_conversion <- function(x, directed = NULL, bipartite = NULL) {
  stored_vertices <- extract_stored_edgelist_vertices(x)
  has_stored_bipartite <- !is.null(attr(x, "snafun_bipartite", exact = TRUE))
  has_stored_directed <- !is.null(attr(x, "snafun_directed", exact = TRUE))
  stored_bipartite <- extract_stored_edgelist_bipartite(x)
  stored_directed <- extract_stored_edgelist_directed(x)
  visible_bipartite <- is_visibly_bipartite_edgelist(x)
  exact_reciprocals <- edgelist_has_exact_reciprocals(x)
  
  if (is.null(bipartite)) {
    if (has_stored_bipartite) {
      bipartite <- stored_bipartite
    } else if (!is.null(directed)) {
      # When the caller explicitly forces directed/undirected semantics, that is
      # usually a stronger signal than the visible sender/receiver split of a
      # sparse edgelist. In that situation we keep the graph one-mode unless
      # bipartite was requested explicitly.
      bipartite <- FALSE
    } else {
      bipartite <- visible_bipartite
    }
  } else {
    bipartite <- isTRUE(bipartite)
  }
  
  if (is.null(directed)) {
    if (has_stored_directed) {
      directed <- stored_directed
    } else if (bipartite) {
      directed <- FALSE
    } else {
      directed <- !exact_reciprocals
    }
  } else {
    directed <- isTRUE(directed)
  }
  
  collapsed_x <- x
  # Reciprocal rows like A-B and B-A with identical attributes are a common
  # way of encoding an undirected edge list. If we inferred undirectedness from
  # that pattern, we collapse each reciprocal pair back to one visible row so
  # downstream graph constructors do not mistakenly create parallel edges.
  if (!directed && !bipartite && !has_stored_directed && exact_reciprocals) {
    collapsed_x <- collapse_exact_reciprocal_edgelist(x)
  }
  
  list(
    x = collapsed_x,
    vertices = stored_vertices,
    bipartite = bipartite,
    directed = directed,
    has_stored_directed = has_stored_directed,
    stored_bipartite = stored_bipartite,
    stored_directed = stored_directed,
    visible_bipartite = visible_bipartite,
    exact_reciprocals = exact_reciprocals
  )
}


#' Check whether an edgelist is visibly bipartite
#'
#' @param x data.frame edgelist
#'
#' @return logical scalar
#' @keywords internal
#' @noRd
is_visibly_bipartite_edgelist <- function(x) {
  if (ncol(x) < 2 || nrow(x) == 0) {
    return(FALSE)
  }
  length(intersect(as.character(x[[1]]), as.character(x[[2]]))) == 0
}


#' Check whether every visible row has an exact reciprocal partner
#'
#' @param x data.frame edgelist
#'
#' @return logical scalar
#' @keywords internal
#' @noRd
edgelist_has_exact_reciprocals <- function(x) {
  if (ncol(x) < 2 || nrow(x) == 0) {
    return(FALSE)
  }
  forward_keys <- serialize_edgelist_rows(x)
  reverse_keys <- serialize_edgelist_rows(reverse_edgelist_rows(x))
  identical(sort(forward_keys), sort(reverse_keys))
}


#' Reverse the endpoints of an edgelist
#'
#' @param x data.frame edgelist
#'
#' @return data.frame with the first two columns swapped
#' @keywords internal
#' @noRd
reverse_edgelist_rows <- function(x) {
  reversed <- x
  reversed[[1]] <- x[[2]]
  reversed[[2]] <- x[[1]]
  reversed
}


#' Serialize edgelist rows to stable comparison keys
#'
#' @param x data.frame edgelist
#'
#' @return character vector with one key per row
#' @keywords internal
#' @noRd
serialize_edgelist_rows <- function(x) {
  if (nrow(x) == 0) {
    return(character(0))
  }
  apply(
    as.data.frame(lapply(x, as.character), stringsAsFactors = FALSE),
    MARGIN = 1,
    FUN = paste,
    collapse = "\r"
  )
}


#' Collapse reciprocal duplicates in an undirected edgelist
#'
#' @param x data.frame edgelist
#'
#' @return data.frame with one retained row per reciprocal pair
#' @keywords internal
#' @noRd
collapse_exact_reciprocal_edgelist <- function(x) {
  if (nrow(x) == 0) {
    return(x)
  }
  
  canonical <- x
  first <- as.character(x[[1]])
  second <- as.character(x[[2]])
  swap_rows <- first > second
  canonical[[1]][swap_rows] <- x[[2]][swap_rows]
  canonical[[2]][swap_rows] <- x[[1]][swap_rows]
  canonical_keys <- serialize_edgelist_rows(canonical)
  loop_rows <- as.character(canonical[[1]]) == as.character(canonical[[2]])
  
  keep_indices <- integer(0)
  for (one_key in unique(canonical_keys)) {
    rows <- which(canonical_keys == one_key)
    if (length(rows) == 0) {
      next
    }
    if (all(loop_rows[rows])) {
      keep_indices <- c(keep_indices, rows)
    } else {
      keep_indices <- c(keep_indices, rows[seq_len(length(rows) / 2)])
    }
  }
  
  x[sort(keep_indices), , drop = FALSE]
}

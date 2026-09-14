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

  # PERFORMANCE: is_visibly_bipartite_edgelist() and, especially,
  # edgelist_has_exact_reciprocals() scan the whole edgelist (the latter
  # serializes every row twice). They are only needed when neither the caller
  # nor the stored roundtrip metadata already determine the interpretation.
  # We therefore compute them lazily and cache the result, so edgelists produced
  # by snafun::to_edgelist() (which always carry stored metadata) skip the scans
  # entirely. This keeps conversions of very large edgelists fast.
  visible_bipartite <- NULL
  get_visible_bipartite <- function() {
    if (is.null(visible_bipartite)) {
      visible_bipartite <<- is_visibly_bipartite_edgelist(x)
    }
    visible_bipartite
  }
  exact_reciprocals <- NULL
  get_exact_reciprocals <- function() {
    if (is.null(exact_reciprocals)) {
      exact_reciprocals <<- edgelist_has_exact_reciprocals(x)
    }
    exact_reciprocals
  }

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
      bipartite <- get_visible_bipartite()
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
      directed <- !get_exact_reciprocals()
    }
  } else {
    directed <- isTRUE(directed)
  }

  collapsed_x <- x
  # Reciprocal rows like A-B and B-A with identical attributes are a common
  # way of encoding an undirected edge list. If we inferred undirectedness from
  # that pattern, we collapse each reciprocal pair back to one visible row so
  # downstream graph constructors do not mistakenly create parallel edges.
  # The get_exact_reciprocals() call is last, so it is skipped (short-circuit)
  # whenever the cheaper conditions already rule collapsing out - in particular
  # for edgelists that carry stored directedness metadata.
  if (!directed && !bipartite && !has_stored_directed && get_exact_reciprocals()) {
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
    # These reflect whatever was actually computed above; they stay NULL when
    # the scan was not needed. No caller relies on them (they are informational).
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
  # Vectorized row serialization. The previous implementation used
  # apply(MARGIN = 1, paste), which loops over rows at the R level and became a
  # severe bottleneck on large edgelists (this scan runs twice per reciprocal
  # check). paste() with sep is fully vectorized and produces identical keys.
  do.call(paste, c(lapply(x, as.character), sep = "\r"))
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
  
  # Keep genuine self-loops in full, otherwise keep one row per reciprocal pair
  # (the first half of each canonical group). Done fully vectorized. Earlier
  # implementations (a which()-loop over unique keys, then an lapply over groups)
  # were O(number_of_keys * number_of_rows) / a per-group R loop and became a
  # bottleneck on large reciprocal edgelists.
  #
  # A group sharing one canonical key is homogeneous: either all its rows are
  # loops (from == to) or none are, because a loop's key "A<sep>A" cannot
  # coincide with a non-loop key. So this per-row rule reproduces the old
  # per-group rule exactly (which specific row of a reciprocal pair is kept does
  # not matter, only that one survives).
  n <- length(canonical_keys)
  ord <- order(canonical_keys)                  # stable: ties keep ascending index
  runs <- rle(canonical_keys[ord])
  within_rank <- integer(n)                      # 1-based position within its group
  group_size  <- integer(n)                      # size of each row's group
  within_rank[ord] <- sequence(runs$lengths)
  group_size[ord]  <- rep(runs$lengths, runs$lengths)

  keep <- loop_rows | (within_rank <= (group_size %/% 2L))
  x[keep, , drop = FALSE]
}

# Tests for the internal edge-list conversion helpers in
# R/edgelist_conversion_helpers.R and for the performance optimisation applied
# on 2026-09-14:
#   * serialize_edgelist_rows() vectorized (was apply(MARGIN = 1, paste)),
#   * resolve_edgelist_conversion() now computes the O(edges) reciprocal /
#     bipartite scans lazily (skipped entirely for snafun edgelists that carry
#     roundtrip metadata),
#   * collapse_exact_reciprocal_edgelist() no longer uses an O(keys * rows)
#     which()-loop.
# These are behaviour-preserving speedups; the tests lock in BOTH the behaviour
# and the "scans are skipped" property, plus a timing regression guard.

report_side_effects()

set.seed(20260914)

# Internal (non-exported) helpers are reached with the ::: convention already
# used elsewhere in this test suite.
serialize_edgelist_rows        <- snafun:::serialize_edgelist_rows
edgelist_has_exact_reciprocals <- snafun:::edgelist_has_exact_reciprocals
is_visibly_bipartite_edgelist  <- snafun:::is_visibly_bipartite_edgelist
collapse_exact_reciprocal_edgelist <- snafun:::collapse_exact_reciprocal_edgelist
resolve_edgelist_conversion    <- snafun:::resolve_edgelist_conversion


# ---------------------------------------------------------------------------
# serialize_edgelist_rows(): must equal an independent apply()-based reference.
# ---------------------------------------------------------------------------

reference_serialize <- function(x) {
  if (nrow(x) == 0) return(character(0))
  apply(as.data.frame(lapply(x, as.character), stringsAsFactors = FALSE),
        MARGIN = 1, FUN = paste, collapse = "\r")
}

df_two   <- data.frame(from = c("A", "B", "A"), to = c("B", "A", "C"),
                       stringsAsFactors = FALSE)
df_three <- data.frame(from = c("A", "B"), to = c("B", "A"),
                       weight = c(2, 2), stringsAsFactors = FALSE)
df_int   <- data.frame(from = c(1L, 2L, 100000L), to = c(2L, 1L, 1L))
df_mixed <- data.frame(from = c("x", "y"), to = c(1L, 2L),
                       col = factor(c("a", "b")), stringsAsFactors = FALSE)
df_empty <- data.frame(from = character(0), to = character(0))

for (nm in c("df_two", "df_three", "df_int", "df_mixed")) {
  d <- get(nm)
  expect_equal(serialize_edgelist_rows(d), reference_serialize(d),
               info = paste0("serialize_edgelist_rows == apply reference (", nm, ")"))
}
expect_equal(serialize_edgelist_rows(df_empty), character(0),
             info = "serialize_edgelist_rows handles the empty edgelist")


# ---------------------------------------------------------------------------
# edgelist_has_exact_reciprocals()
# ---------------------------------------------------------------------------

recip <- data.frame(from = c("A", "B", "A", "C"), to = c("B", "A", "C", "A"),
                    stringsAsFactors = FALSE)
expect_true(edgelist_has_exact_reciprocals(recip),
            info = "fully reciprocal edgelist is detected")

directed_only <- data.frame(from = c("A", "B", "C"), to = c("B", "C", "A"),
                            stringsAsFactors = FALSE)
expect_false(edgelist_has_exact_reciprocals(directed_only),
             info = "directed (non-reciprocal) edgelist is not seen as reciprocal")

# Reciprocity must also account for extra attribute columns.
recip_attr <- data.frame(from = c("A", "B"), to = c("B", "A"),
                         w = c(5, 5), stringsAsFactors = FALSE)
expect_true(edgelist_has_exact_reciprocals(recip_attr),
            info = "reciprocal with matching attributes detected")
recip_attr_bad <- data.frame(from = c("A", "B"), to = c("B", "A"),
                             w = c(5, 9), stringsAsFactors = FALSE)
expect_false(edgelist_has_exact_reciprocals(recip_attr_bad),
             info = "reciprocal endpoints but different attributes -> not reciprocal")

expect_false(edgelist_has_exact_reciprocals(df_empty),
             info = "empty edgelist is not reciprocal")


# ---------------------------------------------------------------------------
# is_visibly_bipartite_edgelist()
# ---------------------------------------------------------------------------

bip <- data.frame(from = c("a1", "a2", "a1"), to = c("b1", "b1", "b2"),
                  stringsAsFactors = FALSE)
expect_true(is_visibly_bipartite_edgelist(bip),
            info = "disjoint sender/receiver sets -> visibly bipartite")
expect_false(is_visibly_bipartite_edgelist(directed_only),
             info = "overlapping sender/receiver sets -> not bipartite")
expect_false(is_visibly_bipartite_edgelist(df_empty),
             info = "empty edgelist -> not bipartite")


# ---------------------------------------------------------------------------
# collapse_exact_reciprocal_edgelist()
# ---------------------------------------------------------------------------

collapsed <- collapse_exact_reciprocal_edgelist(recip)
expect_equal(nrow(collapsed), 2L,
             info = "reciprocal pairs collapse to one row each")
# Every retained undirected edge must still be present (as a canonical pair).
canon <- unname(apply(collapsed[, 1:2], 1, function(r) paste(sort(r), collapse = "-")))
expect_equal(sort(canon), c("A-B", "A-C"),
             info = "collapse keeps exactly the distinct undirected edges")

# Weights on a reciprocal pair must be preserved by the collapse.
collapsed_w <- collapse_exact_reciprocal_edgelist(recip_attr)
expect_equal(nrow(collapsed_w), 1L, info = "reciprocal weighted pair -> one row")
expect_equal(collapsed_w$w, 5, info = "collapse preserves the edge weight")

# Self-loops are kept in full (not halved).
loop_df <- data.frame(from = c("A", "A", "B"), to = c("A", "B", "A"),
                      stringsAsFactors = FALSE)
collapsed_loop <- collapse_exact_reciprocal_edgelist(loop_df)
expect_true(sum(collapsed_loop$from == collapsed_loop$to) == 1L,
            info = "collapse keeps the self-loop")


# ---------------------------------------------------------------------------
# resolve_edgelist_conversion(): decisions AND the lazy-scan optimisation.
# ---------------------------------------------------------------------------

# (a) A snafun edgelist carries stored metadata, so BOTH scans must be skipped:
#     the informational fields stay NULL. This is the core regression guard for
#     the optimisation.
g <- igraph::sample_gnp(30, 0.15, directed = TRUE)
el_meta <- snafun::to_edgelist(g)
res_meta <- resolve_edgelist_conversion(el_meta)
expect_true(is.null(res_meta$exact_reciprocals),
            info = "stored-metadata edgelist: reciprocal scan is skipped (lazy)")
expect_true(is.null(res_meta$visible_bipartite),
            info = "stored-metadata edgelist: bipartite scan is skipped (lazy)")
expect_equal(res_meta$directed, TRUE,
             info = "stored-metadata edgelist: directedness taken from metadata")

# undirected metadata roundtrip
gu <- igraph::sample_gnp(30, 0.15, directed = FALSE)
res_meta_u <- resolve_edgelist_conversion(snafun::to_edgelist(gu))
expect_equal(res_meta_u$directed, FALSE,
             info = "stored-metadata undirected edgelist: directed = FALSE")
expect_true(is.null(res_meta_u$exact_reciprocals),
            info = "stored-metadata undirected edgelist: scan still skipped")

# (b) Plain reciprocal edgelist (no metadata): inferred undirected + collapsed.
res_recip <- resolve_edgelist_conversion(recip)
expect_equal(res_recip$directed, FALSE,
             info = "plain reciprocal edgelist inferred as undirected")
expect_equal(nrow(res_recip$x), 2L,
             info = "plain reciprocal edgelist collapsed to canonical rows")
expect_true(isTRUE(res_recip$exact_reciprocals),
            info = "plain edgelist: reciprocal scan actually ran")

# (c) Plain non-reciprocal edgelist -> directed.
res_dir <- resolve_edgelist_conversion(directed_only)
expect_equal(res_dir$directed, TRUE,
             info = "plain non-reciprocal edgelist inferred as directed")

# (d) Plain disjoint edgelist -> bipartite.
res_bip <- resolve_edgelist_conversion(bip)
expect_equal(res_bip$bipartite, TRUE,
             info = "plain disjoint edgelist inferred as bipartite")

# (e) Explicit caller override wins over the visible pattern.
res_override <- resolve_edgelist_conversion(recip, directed = TRUE)
expect_equal(res_override$directed, TRUE,
             info = "explicit directed = TRUE overrides reciprocal inference")


# ---------------------------------------------------------------------------
# End-to-end: the decisions above must produce the right graphs.
# ---------------------------------------------------------------------------

expect_false(snafun::is_directed(snafun::to_igraph(recip)),
             info = "to_igraph on a reciprocal edgelist -> undirected graph")
expect_true(snafun::is_directed(snafun::to_igraph(directed_only)),
            info = "to_igraph on a non-reciprocal edgelist -> directed graph")
expect_true(snafun::is_bipartite(suppressWarnings(snafun::to_igraph(bip))),
            info = "to_igraph on a disjoint edgelist -> bipartite graph")


# ---------------------------------------------------------------------------
# TIMING REGRESSION.
#
# Guards against reintroducing the O(rows) apply()-scan or the O(keys * rows)
# collapse loop. Bounds are deliberately generous so the test is robust on slow
# CI machines while still failing hard for a quadratic/row-loop regression
# (which would take minutes, not seconds, at these sizes).
# ---------------------------------------------------------------------------

# (a) snafun roundtrip of a large edgelist: the scans must be SKIPPED entirely,
#     so this is near-instant regardless of size.
gL <- igraph::sample_smallworld(1, 100000, nei = 3, p = 0.05)
t_meta <- system.time({
  elL <- snafun::to_edgelist(gL)
  res_big_meta <- resolve_edgelist_conversion(elL)
})[["elapsed"]]
expect_true(is.null(res_big_meta$exact_reciprocals),
            info = "large stored-metadata edgelist: scan skipped")
expect_true(t_meta < 30,
            info = paste0("large metadata edgelist resolves fast (", round(t_meta, 2), "s)"))

# (b) PLAIN reciprocal edgelist (no metadata): here the vectorized reciprocal
#     scan AND the vectorized collapse actually run. Its cost is string-sort
#     bound, so we use a moderate size that still makes a quadratic/row-loop
#     regression (which would take minutes even here) fail loudly, while staying
#     robustly fast on slow CI machines.
gP <- igraph::sample_smallworld(1, 20000, nei = 3, p = 0.05)
e_big <- igraph::as_edgelist(gP)                 # undirected: each edge once
e_big <- rbind(e_big, e_big[, c(2, 1)])          # make it explicitly reciprocal
plain_big <- data.frame(from = as.integer(e_big[, 1]),
                        to   = as.integer(e_big[, 2]))
t_plain <- system.time(res_big_plain <- resolve_edgelist_conversion(plain_big))[["elapsed"]]
expect_equal(res_big_plain$directed, FALSE,
             info = "plain reciprocal edgelist inferred undirected")
expect_equal(nrow(res_big_plain$x), nrow(e_big) / 2,
             info = "plain reciprocal edgelist collapsed to half the rows")
expect_true(t_plain < 30,
            info = paste0("plain reciprocal edgelist resolves fast (", round(t_plain, 2), "s)"))

# Supplementary tests for count_unique_edges_in_interval() and
# count_edges_in_interval(), beyond test_count_edges_in_interval.R.
#
# CONTEXT (2026-09-14): the April 2026 rewrite fixed two real bugs in
# count_unique_edges_in_interval() - a nonsensical `df_sub[-not_unique, ]`
# (negating a logical vector) and an off-by-one that added a spurious final
# interval with an NA upper bound. It also made the directed/undirected
# de-duplication explicit. These tests pin down the interval count, the
# half-open boundaries, the default directedness, and the relationship to the
# non-unique counter, using a hand-built temporal network with known answers.

if (!requireNamespace("networkDynamic", quietly = TRUE) ||
    !requireNamespace("network", quietly = TRUE)) {
  exit_file("networkDynamic / network not available")
}

report_side_effects(FALSE)

# edge.spells columns: onset, terminus, tail, head.
# Interval scheme used below: start = 0, end = 10, number = 2  ->  [0,5), [5,10).
spells <- rbind(
  c(1, 1.5, 1, 2),   # 1 -> 2            (interval 1)
  c(2, 2.5, 1, 2),   # 1 -> 2 duplicate  (interval 1)
  c(3, 3.5, 2, 1),   # 2 -> 1 reciprocal (interval 1)
  c(4, 4.5, 3, 4),   # 3 -> 4            (interval 1)
  c(6, 6.5, 1, 3),   # 1 -> 3            (interval 2)
  c(7, 7.5, 1, 3)    # 1 -> 3 duplicate  (interval 2)
)
nd <- networkDynamic::networkDynamic(edge.spells = spells, verbose = FALSE)


# --- number of intervals equals `number` ----------------------------------
expect_equal(length(snafun::count_unique_edges_in_interval(nd, 0, 10, 2)), 2L,
             info = "output length equals the number of intervals (2)")
expect_equal(length(snafun::count_unique_edges_in_interval(nd, 0, 10, 5)), 5L,
             info = "output length equals the number of intervals (5)")


# --- unique counts, directed vs undirected --------------------------------
# Interval 1 directed unique: (1->2),(2->1),(3->4) = 3 ; interval 2: (1->3) = 1
expect_equal(as.integer(snafun::count_unique_edges_in_interval(nd, 0, 10, 2, directed = TRUE)),
             c(3L, 1L),
             info = "directed: reciprocal ties (1->2, 2->1) counted separately")
# Undirected collapses 1<->2 : interval 1 = {1,2},{3,4} = 2 ; interval 2 = 1
expect_equal(as.integer(snafun::count_unique_edges_in_interval(nd, 0, 10, 2, directed = FALSE)),
             c(2L, 1L),
             info = "undirected: reciprocal ties collapse to one")


# --- default `directed` is taken from the object (directed here) -----------
expect_true(network::is.directed(nd), info = "the test object is directed")
expect_equal(as.integer(snafun::count_unique_edges_in_interval(nd, 0, 10, 2)),
             c(3L, 1L),
             info = "default directedness follows the networkDynamic object")


# --- relationship to the non-unique counter -------------------------------
# Non-unique counts every starting edge: interval 1 = 4, interval 2 = 2.
expect_equal(as.integer(snafun::count_edges_in_interval(nd, 0, 10, 2)),
             c(4L, 2L),
             info = "non-unique counter counts every starting edge")
# Unique count never exceeds the non-unique count.
u <- as.integer(snafun::count_unique_edges_in_interval(nd, 0, 10, 2, directed = TRUE))
a <- as.integer(snafun::count_edges_in_interval(nd, 0, 10, 2))
expect_true(all(u <= a), info = "unique count never exceeds the raw count")


# --- half-open interval boundaries ----------------------------------------
# An edge whose onset is exactly on the internal breakpoint (5) belongs to the
# SECOND interval, not the first; an edge at onset == end (10) is excluded.
spells_b <- rbind(
  c(0, 0.5, 1, 2),    # onset == start -> interval 1
  c(5, 5.5, 1, 3),    # onset == breakpoint -> interval 2
  c(10, 10.5, 2, 4)   # onset == end -> excluded (half-open)
)
nd_b <- networkDynamic::networkDynamic(edge.spells = spells_b, verbose = FALSE)
expect_equal(as.integer(snafun::count_unique_edges_in_interval(nd_b, 0, 10, 2, directed = TRUE)),
             c(1L, 1L),
             info = "boundary onsets land in the right half-open interval; onset==end excluded")


# --- all-unique edges: unique count equals the raw count ------------------
spells_u <- rbind(
  c(1, 1.5, 1, 2),
  c(2, 2.5, 3, 4),
  c(6, 6.5, 1, 4)
)
nd_u <- networkDynamic::networkDynamic(edge.spells = spells_u, verbose = FALSE)
expect_equal(
  as.integer(snafun::count_unique_edges_in_interval(nd_u, 0, 10, 2, directed = TRUE)),
  as.integer(snafun::count_edges_in_interval(nd_u, 0, 10, 2)),
  info = "with no duplicate edges, unique and raw counts coincide"
)


# --- error handling --------------------------------------------------------
expect_error(snafun::count_unique_edges_in_interval(igraph::make_ring(5)),
             info = "non-networkDynamic input is rejected")
expect_error(snafun::count_edges_in_interval(matrix(0, 3, 3)),
             info = "non-networkDynamic input is rejected (raw counter)")

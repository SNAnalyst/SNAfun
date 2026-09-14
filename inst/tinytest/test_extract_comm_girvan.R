# Tests for extract_comm_girvan() (Girvan-Newman / edge-betweenness community
# detection), validated against igraph::cluster_edge_betweenness() directly.
#
# CONTEXT (2026-09-14): the April 2026 change forwards the `directed` argument to
# igraph::cluster_edge_betweenness(). Previously the argument existed in the
# snafun signature but was NOT passed on, so it was silently ignored. Because
# igraph's own default for `directed` is also TRUE, the DEFAULT behaviour is
# unchanged; only an explicit `directed = FALSE` now takes effect. The tests
# below lock in both the parity with igraph and the fact that `directed` is
# actually honoured.

report_side_effects(FALSE)

mem <- function(comm) as.integer(igraph::membership(comm))

set.seed(3)
gd <- igraph::sample_gnp(30, 0.12, directed = TRUE)     # directed
gu <- igraph::sample_gnp(30, 0.15, directed = FALSE)    # undirected


# --- parity with igraph on a DIRECTED graph -------------------------------
r_def <- suppressWarnings(snafun::extract_comm_girvan(gd))               # default directed = TRUE
i_def <- suppressWarnings(igraph::cluster_edge_betweenness(gd))          # igraph default
expect_equal(mem(r_def), mem(i_def),
             info = "directed default matches igraph::cluster_edge_betweenness default")

r_f <- suppressWarnings(snafun::extract_comm_girvan(gd, directed = FALSE))
i_f <- suppressWarnings(igraph::cluster_edge_betweenness(gd, directed = FALSE))
expect_equal(mem(r_f), mem(i_f),
             info = "directed = FALSE matches igraph::cluster_edge_betweenness(directed = FALSE)")

# Regression guard for the April fix: `directed` must actually change the result
# on a directed graph (it was silently ignored before).
expect_false(identical(mem(r_def), mem(r_f)),
             info = "directed = TRUE and directed = FALSE differ on a directed graph")


# --- parity on an UNDIRECTED graph ----------------------------------------
r_u <- suppressWarnings(snafun::extract_comm_girvan(gu))
i_u <- suppressWarnings(igraph::cluster_edge_betweenness(gu))
expect_equal(mem(r_u), mem(i_u),
             info = "undirected graph matches igraph")


# --- input types agree: igraph vs network vs matrix -----------------------
r_net <- suppressWarnings(snafun::extract_comm_girvan(snafun::to_network(gu)))
expect_equal(mem(r_net), mem(r_u),
             info = "network input gives the same communities as igraph input")

r_mat <- suppressWarnings(snafun::extract_comm_girvan(snafun::to_igraph(snafun::to_matrix(gu))))
expect_equal(mem(r_mat), mem(r_u),
             info = "matrix->igraph input gives the same communities as igraph input")


# --- return object shape ---------------------------------------------------
expect_true(inherits(r_u, "communities"),
            info = "returns an igraph 'communities' object")
expect_true(is.finite(igraph::modularity(r_u)),
            info = "modularity is available and finite")
expect_equal(length(mem(r_u)), igraph::vcount(gu),
             info = "one membership entry per vertex")


# --- default method errors on unsupported input ---------------------------
expect_error(snafun::extract_comm_girvan(42L),
             info = "default method errors on unsupported input")

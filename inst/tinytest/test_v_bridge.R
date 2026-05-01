report_side_effects()

data("judge_net", package = "snafun")


bridge_reference_matrix <- function(mat, type = c("all", "out", "in"), directed = TRUE) {
  type <- match.arg(type)
  if (!directed) {
    return(mat)
  }
  if (identical(type, "out")) {
    return(mat)
  }
  if (identical(type, "in")) {
    return(t(mat))
  }
  mat + t(mat)
}


bridge_reference_direct <- function(mat, membership, keep, absolute = FALSE) {
  out <- rep(NA_real_, nrow(mat))
  for (i in seq_len(nrow(mat))) {
    if (!keep[[i]]) {
      next
    }
    target_mask <- keep & membership != membership[[i]]
    values <- mat[i, target_mask, drop = TRUE]
    if (isTRUE(absolute)) {
      values <- abs(values)
    }
    out[[i]] <- sum(values)
  }
  out
}


bridge_reference_indirect <- function(mat, membership, keep) {
  out <- rep(NA_real_, nrow(mat))
  levels_kept <- sort(unique(membership[keep]))
  for (i in seq_len(nrow(mat))) {
    if (!keep[[i]]) {
      next
    }
    subtotal <- 0
    for (one_level in setdiff(levels_kept, membership[[i]])) {
      target_mask <- keep & membership == one_level
      one_step <- rowSums(mat[, target_mask, drop = FALSE])
      subtotal <- subtotal + sum(mat[i, ] * one_step)
    }
    out[[i]] <- subtotal
  }
  out
}


bridge_reference_sp_graph <- function(mat, directed = TRUE, weights = NULL) {
  mat2 <- mat
  if (length(weights) == 1L && is.na(weights)) {
    mat2[mat2 != 0] <- 1
  } else {
    pos <- mat2 > 0
    mat2[!pos] <- 0
    mat2[pos] <- 1 / mat2[pos]
  }
  diag(mat2) <- 0
  if (directed) {
    return(igraph::graph_from_adjacency_matrix(mat2, mode = "directed", weighted = TRUE, diag = FALSE))
  }
  mat_sym <- pmin(mat2, t(mat2))
  only_one_side <- (mat_sym == 0) & ((mat2 > 0) | (t(mat2) > 0))
  mat_sym[only_one_side] <- pmax(mat2, t(mat2))[only_one_side]
  diag(mat_sym) <- 0
  igraph::graph_from_adjacency_matrix(mat_sym, mode = "undirected", weighted = TRUE, diag = FALSE)
}


bridge_reference_closeness <- function(graph, membership, keep, mode = "out") {
  out <- rep(NA_real_, igraph::vcount(graph))
  for (i in seq_len(igraph::vcount(graph))) {
    if (!keep[[i]]) next
    targets <- which(keep & membership != membership[[i]])
    d <- igraph::distances(graph, v = i, to = targets, mode = mode, weights = NULL)
    d <- d[is.finite(d)]
    out[[i]] <- if (length(d) == 0L) NaN else 1 / mean(d)
  }
  out
}


bridge_reference_betweenness <- function(graph, membership, keep, directed = TRUE) {
  labels <- seq_len(igraph::vcount(graph))
  counts <- stats::setNames(rep(0, length(labels)), labels)
  for (i in labels) {
    if (!keep[[i]]) next
    targets <- which(keep & membership != membership[[i]])
    if (length(targets) == 0L) next
    sp <- igraph::all_shortest_paths(graph, from = i, to = targets, mode = if (directed) "out" else "all", weights = NULL)
    mids <- unlist(lapply(sp$vpaths, function(one_path) {
      ids <- as.integer(one_path)
      if (length(ids) <= 2L) integer(0) else ids[2:(length(ids) - 1L)]
    }))
    if (length(mids) > 0L) {
      tab <- table(factor(mids, levels = labels))
      counts <- counts + as.numeric(tab)
    }
  }
  if (!directed) counts <- counts / 2
  counts[!keep] <- NA_real_
  counts
}


# directed signed weighted example ---------------------------------------------
mat <- matrix(
  c(0,  2, -1,  3,
    0,  0,  4, -2,
    5,  0,  0,  0,
    1,  0,  0,  0),
  nrow = 4,
  byrow = TRUE
)
rownames(mat) <- colnames(mat) <- c("A", "B", "C", "D")
membership <- c(A = 1, B = 1, C = 2, D = 2)
keep_all <- rep(TRUE, 4)

g_i <- snafun::to_igraph(mat)
g_n <- snafun::to_network(mat)
g_e <- snafun::to_edgelist(g_i)

mat_out <- bridge_reference_matrix(mat, type = "out", directed = TRUE)
mat_in <- bridge_reference_matrix(mat, type = "in", directed = TRUE)
mat_all <- bridge_reference_matrix(mat, type = "all", directed = TRUE)

ref_strength_out <- bridge_reference_direct(mat_out, unname(membership), keep_all, absolute = TRUE)
ref_strength_in <- bridge_reference_direct(mat_in, unname(membership), keep_all, absolute = TRUE)
ref_strength_all <- bridge_reference_direct(mat_all, unname(membership), keep_all, absolute = TRUE)

ref_ei1_out <- bridge_reference_direct(mat_out, unname(membership), keep_all, absolute = FALSE)
ref_ei1_all <- bridge_reference_direct(mat_all, unname(membership), keep_all, absolute = FALSE)
ref_ei2_out <- ref_ei1_out + bridge_reference_indirect(mat_out, unname(membership), keep_all)

expect_equal(
  unname(snafun::v_bridge_strength(g_i, communities = membership, type = "out")),
  ref_strength_out
)
expect_equal(
  unname(snafun::v_bridge_strength(g_i, communities = membership, type = "in")),
  ref_strength_in
)
expect_equal(
  unname(snafun::v_bridge_strength(g_i, communities = membership, type = "all")),
  ref_strength_all
)

expect_equal(
  unname(snafun::v_bridge_expected_influence(g_i, communities = membership, type = "out")),
  ref_ei1_out
)
expect_equal(
  unname(snafun::v_bridge_expected_influence(g_i, communities = membership, type = "all")),
  ref_ei1_all
)
expect_equal(
  unname(snafun::v_bridge_expected_influence2(g_i, communities = membership, type = "out")),
  ref_ei2_out
)


# backend parity ----------------------------------------------------------------
expect_equal(
  snafun::v_bridge_strength(g_n, communities = membership, type = "out"),
  snafun::v_bridge_strength(g_i, communities = membership, type = "out")
)
expect_equal(
  snafun::v_bridge_strength(mat, communities = membership, type = "out"),
  snafun::v_bridge_strength(g_i, communities = membership, type = "out")
)
expect_equal(
  snafun::v_bridge_strength(g_e, communities = membership, type = "out"),
  snafun::v_bridge_strength(g_i, communities = membership, type = "out")
)

expect_equal(
  snafun::v_bridge_expected_influence(g_n, communities = membership, type = "out"),
  snafun::v_bridge_expected_influence(g_i, communities = membership, type = "out")
)
expect_equal(
  snafun::v_bridge_expected_influence2(mat, communities = membership, type = "out"),
  snafun::v_bridge_expected_influence2(g_i, communities = membership, type = "out")
)


# subsetted communities ---------------------------------------------------------
keep_subset <- unname(membership %in% c(1, 2))
expect_equal(
  snafun::v_bridge_strength(g_i, communities = membership, use_communities = c(1, 2)),
  snafun::v_bridge_strength(g_i, communities = membership)
)

membership3 <- c(A = 1, B = 1, C = 2, D = 3)
subset_scores <- snafun::v_bridge_expected_influence(
  g_i,
  communities = membership3,
  use_communities = c(1, 2),
  type = "out"
)
expect_true(is.na(subset_scores[["D"]]))
expect_false(any(is.na(subset_scores[c("A", "B", "C")])))


# list input -------------------------------------------------------------------
community_list <- list(c("A", "B"), c("C", "D"))
expect_equal(
  snafun::v_bridge_strength(g_i, communities = community_list, type = "out"),
  snafun::v_bridge_strength(g_i, communities = membership, type = "out")
)


# communities object input -----------------------------------------------------
judge_coms <- snafun::extract_comm_walktrap(judge_net)
judge_membership <- snafun::extract_comm_membership(judge_coms)
expect_equal(
  snafun::v_bridge_strength(judge_net, communities = judge_coms),
  snafun::v_bridge_strength(judge_net, communities = judge_membership)
)
expect_equal(
  snafun::v_bridge_expected_influence(judge_net, communities = judge_coms),
  snafun::v_bridge_expected_influence(judge_net, communities = judge_membership)
)


# undirected handling ----------------------------------------------------------
g_u <- snafun::create_manual_graph(A -- B, B -- C, C -- D)
g_u <- snafun::add_edge_attributes(g_u, attr_name = "weight", value = c(-2, 3, 4))
membership_u <- c(A = 1, B = 1, C = 2, D = 2)
expect_equal(
  snafun::v_bridge_strength(g_u, communities = membership_u, type = "out"),
  snafun::v_bridge_strength(g_u, communities = membership_u, type = "all")
)
expect_equal(
  snafun::v_bridge_expected_influence(g_u, communities = membership_u, type = "in"),
  snafun::v_bridge_expected_influence(g_u, communities = membership_u, type = "all")
)


# rescaling and error handling -------------------------------------------------
rescaled_scores <- snafun::v_bridge_strength(g_i, communities = membership, rescaled = TRUE)
expect_equal(sum(rescaled_scores), 1)

expect_error(
  snafun::v_bridge_strength(g_i, communities = membership, use_communities = 1),
  "at least two communities"
)
expect_error(
  snafun::v_bridge_strength(g_i, communities = c(1, 1, 2), type = "out"),
  "same number of vertices"
)
expect_error(
  snafun::v_bridge_strength(g_i, communities = c(X = 1, Y = 1, Z = 2, W = 2), type = "out"),
  "same vertices"
)


# shortest-path bridge centralities --------------------------------------------
mat_sp <- matrix(
  c(0, 1, -5, 0,
    0, 0,  0, 1,
    0, 0,  0, 1,
    0, 0,  0, 0),
  nrow = 4,
  byrow = TRUE
)
rownames(mat_sp) <- colnames(mat_sp) <- c("A", "B", "C", "D")
membership_sp <- c(A = 1, B = 1, C = 2, D = 2)
keep_sp <- rep(TRUE, 4)
g_sp <- snafun::to_igraph(mat_sp)

ref_graph_weighted <- bridge_reference_sp_graph(mat_sp, directed = TRUE, weights = NULL)
ref_graph_binary <- bridge_reference_sp_graph(mat_sp, directed = TRUE, weights = NA)

ref_close_weighted <- bridge_reference_closeness(ref_graph_weighted, unname(membership_sp), keep_sp, mode = "out")
ref_close_binary <- bridge_reference_closeness(ref_graph_binary, unname(membership_sp), keep_sp, mode = "out")
ref_bet_weighted <- bridge_reference_betweenness(ref_graph_weighted, unname(membership_sp), keep_sp, directed = TRUE)
ref_bet_binary <- bridge_reference_betweenness(ref_graph_binary, unname(membership_sp), keep_sp, directed = TRUE)

expect_equal(
  unname(snafun::v_bridge_closeness(g_sp, communities = membership_sp, mode = "out")),
  ref_close_weighted
)
expect_equal(
  unname(snafun::v_bridge_closeness(g_sp, communities = membership_sp, mode = "out", weights = NA)),
  ref_close_binary
)
expect_equal(
  unname(snafun::v_bridge_betweenness(g_sp, communities = membership_sp)),
  unname(ref_bet_weighted)
)
expect_equal(
  unname(snafun::v_bridge_betweenness(g_sp, communities = membership_sp, weights = NA)),
  unname(ref_bet_binary)
)

expect_equal(
  snafun::v_bridge_closeness(snafun::to_network(g_sp), communities = membership_sp, mode = "out"),
  snafun::v_bridge_closeness(g_sp, communities = membership_sp, mode = "out")
)
expect_equal(
  snafun::v_bridge_betweenness(snafun::to_edgelist(g_sp), communities = membership_sp),
  snafun::v_bridge_betweenness(g_sp, communities = membership_sp)
)

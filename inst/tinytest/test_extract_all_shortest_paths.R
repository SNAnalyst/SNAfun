report_side_effects()


canonical_path_table <- function(x) {
  if (nrow(x) == 0L) {
    return(x)
  }

  key <- vapply(
    x$vertices,
    FUN = function(one_path) paste(as.character(one_path), collapse = "|"),
    FUN.VALUE = character(1)
  )
  x <- x[order(key), , drop = FALSE]
  rownames(x) <- NULL
  x$path_id <- seq_len(nrow(x))
  x$from <- as.character(x$from)
  x$to <- as.character(x$to)
  x$vertices <- I(lapply(x$vertices, as.character))
  x
}


canonical_edge_table <- function(x) {
  if (nrow(x) == 0L) {
    return(x)
  }

  x$from <- as.character(x$from)
  x$to <- as.character(x$to)
  x <- x[order(x$path_id, x$step, x$from, x$to), , drop = FALSE]
  rownames(x) <- NULL
  x
}


# exact multiple-path example --------------------------------------------------
g <- snafun::create_manual_graph(A -+ B, A -+ C, B -+ D, C -+ D)
out <- snafun::extract_all_shortest_paths(g, from = "A", to = "D")

expect_inherits(out, "snafun_all_shortest_paths")
expect_equal(out$from, "A")
expect_equal(out$to, "D")
expect_equal(out$mode, "out")
expect_equal(nrow(out$paths), 2L)
expect_equal(nrow(out$edges), 4L)
expect_equal(unname(out$nrgeo[c("A", "B", "C", "D")]), c(1, 1, 1, 2))
expect_equal(
  sort(vapply(out$paths$vertices, paste, collapse = "|", FUN.VALUE = character(1))),
  c("A|B|D", "A|C|D")
)


# parity across graph classes --------------------------------------------------
g_n <- snafun::to_network(g)
g_m <- snafun::to_matrix(g)
rownames(g_m) <- colnames(g_m) <- c("A", "B", "C", "D")
g_e <- snafun::to_edgelist(g)

out_network <- snafun::extract_all_shortest_paths(g_n, from = "A", to = "D")
out_matrix <- snafun::extract_all_shortest_paths(g_m, from = "A", to = "D")
out_edgelist <- snafun::extract_all_shortest_paths(g_e, from = "A", to = "D")

expect_equal(canonical_path_table(out_network$paths), canonical_path_table(out$paths))
expect_equal(canonical_path_table(out_matrix$paths), canonical_path_table(out$paths))
expect_equal(canonical_path_table(out_edgelist$paths), canonical_path_table(out$paths))

expect_equal(canonical_edge_table(out_network$edges), canonical_edge_table(out$edges))
expect_equal(canonical_edge_table(out_matrix$edges), canonical_edge_table(out$edges))
expect_equal(canonical_edge_table(out_edgelist$edges), canonical_edge_table(out$edges))

expect_equal(unname(out_network$nrgeo), unname(out$nrgeo))
expect_equal(unname(out_matrix$nrgeo), unname(out$nrgeo))
expect_equal(unname(out_edgelist$nrgeo), unname(out$nrgeo))


# weighted versus unweighted ---------------------------------------------------
g_w <- snafun::add_edge_attributes(
  g,
  attr_name = "weight",
  value = c(1, 1, 10, 1)
)

out_weighted <- snafun::extract_all_shortest_paths(g_w, from = "A", to = "D")
out_unweighted <- snafun::extract_all_shortest_paths(g_w, from = "A", to = "D", weights = NA)

expect_equal(nrow(out_weighted$paths), 1L)
expect_equal(as.character(out_weighted$paths$vertices[[1]]), c("A", "C", "D"))
expect_equal(out_weighted$paths$distance[[1]], 2)
expect_equal(nrow(out_unweighted$paths), 2L)


# unreachable targets and unnamed vertices ------------------------------------
m_unreach <- matrix(0, 4, 4)
m_unreach[1, 2] <- 1
m_unreach[2, 3] <- 1

out_unreach <- snafun::extract_all_shortest_paths(m_unreach, from = 1, to = 4)

expect_equal(out_unreach$from, 1L)
expect_equal(out_unreach$to, 4L)
expect_equal(nrow(out_unreach$paths), 0L)
expect_equal(nrow(out_unreach$edges), 0L)
expect_equal(unname(out_unreach$nrgeo), c(1, 1, 1, 0))


# default target set includes all vertices ------------------------------------
out_all <- snafun::extract_all_shortest_paths(g, from = "A")
expect_true(any(vapply(out_all$paths$vertices, length, integer(1)) == 1L))
expect_true(any(vapply(
  out_all$paths$vertices,
  FUN = function(one_path) identical(as.character(one_path), "A"),
  FUN.VALUE = logical(1)
)))


# error handling ---------------------------------------------------------------
expect_error(
  snafun::extract_all_shortest_paths(g, from = c("A", "B"), to = "D"),
  "exactly one vertex"
)
expect_error(
  snafun::extract_all_shortest_paths(g, from = "Z", to = "D"),
  "Unknown vertex name"
)
expect_error(
  snafun::extract_all_shortest_paths(g, from = "A", to = "Z"),
  "Unknown vertex name"
)

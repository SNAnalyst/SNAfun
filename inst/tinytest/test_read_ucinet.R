report_side_effects()


normalize_named_matrix <- function(x) {
  if (!is.null(rownames(x))) {
    x <- x[order(rownames(x)), , drop = FALSE]
  }
  if (!is.null(colnames(x))) {
    x <- x[, order(colnames(x)), drop = FALSE]
  }
  x
}


canonicalize_edgelist <- function(x) {
  if (!is.data.frame(x)) {
    x <- snafun::to_edgelist(x)
  }
  x <- data.frame(x, check.names = FALSE, stringsAsFactors = FALSE)
  if ("from" %in% colnames(x)) {
    x$from <- as.character(x$from)
  }
  if ("to" %in% colnames(x)) {
    x$to <- as.character(x$to)
  }
  x <- x[order(
    if ("from" %in% colnames(x)) as.character(x$from) else seq_len(nrow(x)),
    if ("to" %in% colnames(x)) as.character(x$to) else seq_len(nrow(x))
  ), , drop = FALSE]
  rownames(x) <- NULL
  x
}


expect_same_named_matrix <- function(actual, expected) {
  expect_equal(
    normalize_named_matrix(actual),
    normalize_named_matrix(expected)
  )
}


write_ucinet_v6404 <- function(path,
                               mat,
                               row_labels = rownames(mat),
                               column_labels = colnames(mat),
                               title = "") {
  header_path <- if (grepl("\\.##h$", path)) path else paste0(path, ".##h")
  data_path <- sub("h$", "d", header_path)

  if (is.null(row_labels)) {
    row_labels <- as.character(seq_len(nrow(mat)))
  }
  if (is.null(column_labels)) {
    column_labels <- as.character(seq_len(ncol(mat)))
  }

  header_con <- file(header_path, "wb")
  writeBin(as.raw(0), header_con, size = 1)
  writeBin(charToRaw("V6404"), header_con, size = 1)
  writeBin(26L, header_con, size = 2)
  writeBin(4L, header_con, size = 2)
  writeBin(20L, header_con, size = 2)
  writeBin(1L, header_con, size = 2)
  writeBin(3L, header_con, size = 2)
  writeBin(8L, header_con, size = 1)
  writeBin(2L, header_con, size = 2)
  writeBin(as.integer(ncol(mat)), header_con, size = 4)
  writeBin(as.integer(nrow(mat)), header_con, size = 4)
  writeBin(nchar(title, type = "bytes"), header_con, size = 1)
  if (nzchar(title)) {
    writeBin(charToRaw(title), header_con, size = 1)
  }
  writeBin(1L, header_con, size = 1)
  writeBin(1L, header_con, size = 1)

  for (one_label in column_labels) {
    one_label <- as.character(one_label)
    writeBin(nchar(one_label, type = "bytes"), header_con, size = 2)
    if (nzchar(one_label)) {
      writeBin(charToRaw(one_label), header_con, size = 1)
    }
  }
  for (one_label in row_labels) {
    one_label <- as.character(one_label)
    writeBin(nchar(one_label, type = "bytes"), header_con, size = 2)
    if (nzchar(one_label)) {
      writeBin(charToRaw(one_label), header_con, size = 1)
    }
  }
  close(header_con)

  data_con <- file(data_path, "wb")
  # UCINET stores values row-wise in the .##d file.
  writeBin(as.numeric(t(mat)), data_con, size = 4, endian = "little")
  close(data_con)

  header_path
}


expect_import_across_graph_classes <- function(file,
                                               expected_matrix,
                                               expected_directed,
                                               expected_bipartite,
                                               expected_weighted) {
  imported_igraph <- snafun::read_ucinet(file)
  expect_true(inherits(imported_igraph, "igraph"))
  expect_same_named_matrix(snafun::to_matrix(imported_igraph), expected_matrix)
  expect_identical(snafun::is_directed(imported_igraph), expected_directed)
  expect_identical(snafun::is_bipartite(imported_igraph), expected_bipartite)
  expect_identical(snafun::is_weighted(imported_igraph), expected_weighted)

  imported_network <- snafun::read_ucinet(file, graph = "network")
  expect_true(inherits(imported_network, "network"))
  expect_same_named_matrix(snafun::to_matrix(imported_network), expected_matrix)

  imported_matrix <- snafun::read_ucinet(file, graph = "matrix")
  expect_true(is.matrix(imported_matrix))
  expect_same_named_matrix(imported_matrix, expected_matrix)

  imported_edgelist <- snafun::read_ucinet(file, graph = "edgelist")
  expect_true(is.data.frame(imported_edgelist))
  expect_same_named_matrix(snafun::to_matrix(imported_edgelist), expected_matrix)
}


# DL fullmatrix, one-mode, weighted, isolate preserved -------------------------
dl_fullmatrix_file <- tempfile(fileext = ".dl")
writeLines(
  c(
    "dl n = 4 format = fullmatrix",
    "labels:",
    "Alice, Bob, Carol, Dora",
    "data:",
    "0 2 0 0",
    "2 0 3 0",
    "0 3 0 0",
    "0 0 0 0"
  ),
  con = dl_fullmatrix_file
)

expected_dl_fullmatrix <- matrix(
  c(
    0, 2, 0, 0,
    2, 0, 3, 0,
    0, 3, 0, 0,
    0, 0, 0, 0
  ),
  nrow = 4,
  byrow = TRUE,
  dimnames = list(
    c("Alice", "Bob", "Carol", "Dora"),
    c("Alice", "Bob", "Carol", "Dora")
  )
)

expect_import_across_graph_classes(
  file = dl_fullmatrix_file,
  expected_matrix = expected_dl_fullmatrix,
  expected_directed = FALSE,
  expected_bipartite = FALSE,
  expected_weighted = TRUE
)

dl_fullmatrix_graph <- snafun::read_ucinet(dl_fullmatrix_file)
expect_identical(igraph::graph_attr(dl_fullmatrix_graph, "ucinet_source"), "dl")
expect_identical(igraph::graph_attr(dl_fullmatrix_graph, "ucinet_format"), "fullmatrix")
expect_true("Dora" %in% igraph::V(dl_fullmatrix_graph)$name)

dl_fullmatrix_edgelist <- snafun::read_ucinet(dl_fullmatrix_file, graph = "edgelist")
expect_true(!is.null(attr(dl_fullmatrix_edgelist, "snafun_vertices", exact = TRUE)))
expect_true("Dora" %in% attr(dl_fullmatrix_edgelist, "snafun_vertices", exact = TRUE)[[1]])


# DL fullmatrix, diagonal absent ----------------------------------------------
dl_diag_absent_file <- tempfile(fileext = ".dl")
writeLines(
  c(
    "dl n = 3 format = fullmatrix diagonal = absent",
    "labels:",
    "A, B, C",
    "data:",
    "1 0",
    "1 1",
    "0 1"
  ),
  con = dl_diag_absent_file
)

expected_diag_absent <- matrix(
  c(
    0, 1, 0,
    1, 0, 1,
    0, 1, 0
  ),
  nrow = 3,
  byrow = TRUE,
  dimnames = list(c("A", "B", "C"), c("A", "B", "C"))
)
expect_same_named_matrix(
  snafun::to_matrix(snafun::read_ucinet(dl_diag_absent_file)),
  expected_diag_absent
)


# DL rectangular fullmatrix becomes bipartite ---------------------------------
dl_bipartite_file <- tempfile(fileext = ".dl")
writeLines(
  c(
    "dl nr = 3 nc = 2 format = fullmatrix",
    "row labels:",
    "Alice, Bob, Carol",
    "column labels:",
    "Project1, Project2",
    "data:",
    "1 0",
    "2 3",
    "0 4"
  ),
  con = dl_bipartite_file
)

expected_dl_bipartite <- matrix(
  c(
    1, 0,
    2, 3,
    0, 4
  ),
  nrow = 3,
  byrow = TRUE,
  dimnames = list(
    c("Alice", "Bob", "Carol"),
    c("Project1", "Project2")
  )
)

expect_import_across_graph_classes(
  file = dl_bipartite_file,
  expected_matrix = expected_dl_bipartite,
  expected_directed = FALSE,
  expected_bipartite = TRUE,
  expected_weighted = TRUE
)


# DL edgelist with labels, weights, and isolate -------------------------------
dl_edgelist_file <- tempfile(fileext = ".dl")
writeLines(
  c(
    "dl n = 4 format = edgelist1",
    "labels:",
    "Anna, Ben, Cara, Dora",
    "data:",
    "1 2 5",
    "2 3 2"
  ),
  con = dl_edgelist_file
)

expected_dl_edgelist <- matrix(
  0,
  nrow = 4,
  ncol = 4,
  dimnames = list(
    c("Anna", "Ben", "Cara", "Dora"),
    c("Anna", "Ben", "Cara", "Dora")
  )
)
expected_dl_edgelist["Anna", "Ben"] <- 5
expected_dl_edgelist["Ben", "Cara"] <- 2

expect_import_across_graph_classes(
  file = dl_edgelist_file,
  expected_matrix = expected_dl_edgelist,
  expected_directed = TRUE,
  expected_bipartite = FALSE,
  expected_weighted = TRUE
)
expect_true("Dora" %in% igraph::V(snafun::read_ucinet(dl_edgelist_file))$name)


# DL edgelist forced to undirected should preserve weights --------------------
dl_edgelist_undirected_file <- tempfile(fileext = ".dl")
writeLines(
  c(
    "dl n = 3 format = edgelist1",
    "labels:",
    "A, B, C",
    "data:",
    "1 2 4.5"
  ),
  con = dl_edgelist_undirected_file
)

expected_dl_undirected <- matrix(
  0,
  nrow = 3,
  ncol = 3,
  dimnames = list(c("A", "B", "C"), c("A", "B", "C"))
)
expected_dl_undirected["A", "B"] <- 4.5
expected_dl_undirected["B", "A"] <- 4.5

forced_undirected <- snafun::read_ucinet(
  dl_edgelist_undirected_file,
  directed = "undirected"
)
expect_false(snafun::is_directed(forced_undirected))
expect_true(snafun::is_weighted(forced_undirected))
expect_same_named_matrix(snafun::to_matrix(forced_undirected), expected_dl_undirected)


# Binary UCINET, one-mode, directed, weighted ---------------------------------
binary_directed_matrix <- matrix(
  c(
    0, 1.5, 0, 0,
    0, 0, 0, 0,
    0, 0, 0, 0,
    0, 4, 0, 0
  ),
  nrow = 4,
  byrow = TRUE,
  dimnames = list(
    c("Alice", "Bob", "Cara", "Dora"),
    c("Alice", "Bob", "Cara", "Dora")
  )
)
binary_directed_header <- write_ucinet_v6404(
  tempfile(pattern = "ucinet_directed_"),
  binary_directed_matrix,
  title = "Binary directed test"
)

expect_import_across_graph_classes(
  file = binary_directed_header,
  expected_matrix = binary_directed_matrix,
  expected_directed = TRUE,
  expected_bipartite = FALSE,
  expected_weighted = TRUE
)

binary_directed_graph <- snafun::read_ucinet(binary_directed_header)
expect_identical(igraph::graph_attr(binary_directed_graph, "ucinet_source"), "binary")
expect_identical(igraph::graph_attr(binary_directed_graph, "title"), "Binary directed test")
expect_true("Cara" %in% igraph::V(binary_directed_graph)$name)


# Binary UCINET, rectangular, bipartite ---------------------------------------
binary_bipartite_matrix <- matrix(
  c(
    1, 0,
    0, 2,
    3, 4
  ),
  nrow = 3,
  byrow = TRUE,
  dimnames = list(
    c("R1", "R2", "R3"),
    c("C1", "C2")
  )
)
binary_bipartite_header <- write_ucinet_v6404(
  tempfile(pattern = "ucinet_bipartite_"),
  binary_bipartite_matrix,
  title = "Binary bipartite test"
)

binary_bipartite_graph <- snafun::read_ucinet(binary_bipartite_header)
expect_true(snafun::is_bipartite(binary_bipartite_graph))
expect_false(snafun::is_directed(binary_bipartite_graph))
expect_true(snafun::is_weighted(binary_bipartite_graph))
expect_same_named_matrix(
  snafun::to_matrix(binary_bipartite_graph),
  binary_bipartite_matrix
)


# Error handling ---------------------------------------------------------------
missing_binary_header <- tempfile(fileext = ".##h")
missing_binary_con <- file(missing_binary_header, "wb")
writeBin(as.raw(0), missing_binary_con, size = 1)
close(missing_binary_con)
expect_error(
  snafun::read_ucinet(missing_binary_header),
  "matching UCINET data file '.##d' was not found"
)

unsupported_dl_file <- tempfile(fileext = ".dl")
writeLines(
  c(
    "dl n = 3 format = nodelist1",
    "data:",
    "1 2"
  ),
  con = unsupported_dl_file
)
expect_error(
  snafun::read_ucinet(unsupported_dl_file),
  "Unsupported UCINET format"
)


unlink(c(
  dl_fullmatrix_file,
  dl_diag_absent_file,
  dl_bipartite_file,
  dl_edgelist_file,
  dl_edgelist_undirected_file,
  missing_binary_header,
  unsupported_dl_file,
  binary_directed_header,
  sub("h$", "d", binary_directed_header),
  binary_bipartite_header,
  sub("h$", "d", binary_bipartite_header)
))

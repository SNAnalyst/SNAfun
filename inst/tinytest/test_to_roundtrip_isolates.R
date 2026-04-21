report_side_effects()


drop_edgelist_metadata <- function(x) {
  attr(x, "snafun_vertices") <- NULL
  attr(x, "snafun_bipartite") <- NULL
  attr(x, "snafun_directed") <- NULL
  x
}


# unipartite, weighted, with isolates -----------------------------------------
mat_u <- matrix(0, nrow = 4, ncol = 4)
mat_u[1, 2] <- 2
rownames(mat_u) <- colnames(mat_u) <- LETTERS[1:4]

ig_u <- snafun::to_igraph(mat_u)
el_u <- snafun::to_edgelist(ig_u)

expect_true(is.data.frame(attr(el_u, "snafun_vertices", exact = TRUE)))
expect_false(isTRUE(attr(el_u, "snafun_bipartite", exact = TRUE)))

ig_u_roundtrip <- snafun::to_igraph(el_u)
nw_u_roundtrip <- snafun::to_network(el_u)
mat_u_roundtrip <- snafun::to_matrix(el_u)

expect_equal(snafun::count_vertices(ig_u_roundtrip), 4)
expect_equal(snafun::count_vertices(nw_u_roundtrip), 4)
expect_equal(snafun::extract_isolates(ig_u_roundtrip), c("C", "D"))
expect_equal(snafun::extract_isolates(nw_u_roundtrip), c("C", "D"))
expect_identical(mat_u_roundtrip, mat_u)
expect_identical(snafun::to_matrix(nw_u_roundtrip), mat_u)


# numeric edgelist should also keep the full vertex set -------------------------
el_u_numeric <- snafun::to_edgelist(ig_u, named = FALSE)
ig_u_numeric <- snafun::to_igraph(el_u_numeric)
mat_u_numeric <- snafun::to_matrix(el_u_numeric)

expect_equal(snafun::count_vertices(ig_u_numeric), 4)
expect_equal(snafun::extract_isolates(ig_u_numeric), c("3", "4"))
expect_identical(unname(mat_u_numeric), unname(mat_u))


# bipartite with isolates in both partitions -----------------------------------
mat_b <- matrix(c(1, 0,
                  0, 0,
                  0, 0), nrow = 3, byrow = TRUE)
rownames(mat_b) <- c("p1", "p2", "p3")
colnames(mat_b) <- c("e1", "e2")

ig_b <- snafun::to_igraph(mat_b, bipartite = TRUE)
el_b <- snafun::to_edgelist(ig_b)

expect_true(isTRUE(attr(el_b, "snafun_bipartite", exact = TRUE)))
expect_true("type" %in% colnames(attr(el_b, "snafun_vertices", exact = TRUE)))

ig_b_roundtrip <- snafun::to_igraph(el_b)
nw_b_roundtrip <- snafun::to_network(el_b)
mat_b_roundtrip <- snafun::to_matrix(el_b)

expect_true(snafun::is_bipartite(ig_b_roundtrip))
expect_true(snafun::is_bipartite(nw_b_roundtrip))
expect_equal(snafun::count_vertices(ig_b_roundtrip), 5)
expect_equal(snafun::count_vertices(nw_b_roundtrip), 5)
expect_equal(snafun::extract_isolates(ig_b_roundtrip), c("p2", "p3", "e2"))
expect_equal(snafun::extract_isolates(nw_b_roundtrip), c("p2", "p3", "e2"))
expect_identical(mat_b_roundtrip, mat_b)


# metadata are invisible to the ordinary edgelist columns ----------------------
expect_identical(
  drop_edgelist_metadata(el_u),
  data.frame(from = "A", to = "B", weight = 2)
)

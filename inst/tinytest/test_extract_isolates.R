
report_side_effects()


#---- IGRAPH
mat <- matrix(0, nrow = 4, ncol = 4)
# edges, incl one self-loop
mat[1, 3] <- mat[4,4] <- 1
ig <- igraph::graph_from_adjacency_matrix(mat)

expect_error(extract_isolates(mat))


expect_equivalent(extract_isolates(ig), c(2, 4))
expect_true(is.integer(extract_isolates(ig)))
expect_true(is.integer(extract_isolates(ig, names = FALSE)))

# 4 has a loop to itself
expect_equivalent(extract_isolates(ig, loops = TRUE), 2)

# add names
igraph::V(ig)$name <-  LETTERS[1:4]
expect_equivalent(extract_isolates(ig), c("B", "D"))
expect_true(is.integer(extract_isolates(ig, names = FALSE)))
expect_equivalent(extract_isolates(ig, loops = TRUE), "B")
expect_equivalent(extract_isolates(ig, names = FALSE, loops = TRUE), 2)


# bipartite
big <- igraph::make_bipartite_graph(c(rep(0, 6), rep(1, 8)), 
                                  edges = c(1, 10, 2, 11, 3, 12, 4, 10, 
                                            5, 7, 5, 8, 5, 12))
expect_equivalent(extract_isolates(big), c(6, 9, 13, 14))

igraph::V(big)$name <- c(paste("person", LETTERS[1:6]), 
                       paste("event", letters[1:8]))
expect_equivalent(extract_isolates(big), c("person F", "event c", "event g", "event h" ))
expect_equivalent(extract_isolates(big, names = FALSE), c(6, 9, 13, 14))





#---- NETWORK
nw <- network::as.network(mat, loops = TRUE)
expect_equivalent(extract_isolates(nw), c(2, 4))
expect_true(is.integer(extract_isolates(nw)))
expect_true(is.integer(extract_isolates(nw, names = FALSE)))

# 4 has a loop to itself
expect_equivalent(extract_isolates(nw, loops = TRUE), 2)

# add names
network::set.vertex.attribute(nw, "vertex.names", value = LETTERS[1:4])
expect_equivalent(extract_isolates(nw), c("B", "D"))
expect_true(is.integer(extract_isolates(nw, names = FALSE)))
expect_equivalent(extract_isolates(nw, loops = TRUE), "B")
expect_equivalent(extract_isolates(nw, names = FALSE, loops = TRUE), 2)

# bipartite
bmat <- igraph::as_adjacency_matrix(big, sparse = FALSE)[1:6, 7:14]
bnw <- network::as.network.matrix(bmat, bipartite = TRUE)
expect_equivalent(extract_isolates(bnw, names = FALSE), c(6, 9, 13, 14))
expect_equivalent(extract_isolates(bnw), c("person F", "event c", "event g", "event h" ))
network::delete.vertex.attribute(bnw, "vertex.names")
expect_equivalent(extract_isolates(bnw, names = FALSE), c(6, 9, 13, 14))
expect_equivalent(extract_isolates(bnw, names = TRUE), c(6, 9, 13, 14))





data(soccer98, package = "snafun")
expect_equal(snafun::extract_isolates(soccer98), "SAR")
g <- snafun::remove_isolates(soccer98)
expect_length(snafun::extract_isolates(g), 0)

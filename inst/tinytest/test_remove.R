
report_side_effects()


# it is possible to only have few tests for remove_vertices, since this 
# function is used in remove_isolates. Hence, all tests for remove_isolates 
# also test remove_vertices.




mat <- structure(c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 
                   0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 
                   0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                   0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 
                   0, 1, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 1, 1, 0, 0, 0, 0, 1, 0), 
                 dim = c(10L, 10L))



### IGRAPH ----
ig <- igraph::graph_from_adjacency_matrix(mat)
expect_equal(length(extract_isolates(ig)), 4)
expect_equal(length(extract_isolates(ig, loops = TRUE)), 2)
expect_true(igraph::vcount(ig) == 10)
expect_true(igraph::ecount(ig) == 15)
expect_true(igraph::vcount(remove_isolates(ig)) == 6)
expect_true(igraph::vcount(remove_isolates(ig, loops = TRUE)) == 8)
expect_true(igraph::ecount(remove_isolates(ig)) == 13) # vanwege de self-loops
expect_true(igraph::ecount(remove_isolates(ig, loops = TRUE)) == 15)
# nothing should happen if the network does not have any isolates
ig_no_isolates <- remove_isolates(ig)
expect_identical(ig_no_isolates, remove_isolates(ig_no_isolates))

expect_error(remove_isolates(mat), "class")
expect_error(remove_isolates(mat), "igraph")
expect_error(remove_isolates(mat), "network")


expect_true(igraph::vcount(remove_vertices(ig, c(1, 2, 4))) == 7)
expect_true(igraph::ecount(remove_vertices(ig, c(1, 2, 4))) == 7)  # self-loop counts too
expect_true(igraph::ecount(remove_vertices(ig, c(1, 2, 8))) == 8)
expect_error(suppressWarnings(remove_vertices(ig, c("A", "B", "C"))), "Invalid")


# named vertices
igraph::V(ig)$name <- LETTERS[1:10]
expect_true(igraph::vcount(remove_isolates(ig)) == 6)
expect_true(igraph::vcount(remove_isolates(ig, loops = TRUE)) == 8)
expect_true(igraph::ecount(remove_isolates(ig)) == 13) # vanwege de self-loops
expect_true(igraph::ecount(remove_isolates(ig, loops = TRUE)) == 15)
expect_equal(igraph::V(remove_isolates(ig))$name, c("B", "C", "D", "G", "I", "J"))
expect_equal(igraph::V(remove_isolates(ig, loops = TRUE))$name, c("A", "B", "C", "D", "G", "H", "I", "J"))
ig_no_isolates <- remove_isolates(ig)
expect_identical(ig_no_isolates, remove_isolates(ig_no_isolates))



# bipartite
big <- igraph::make_bipartite_graph(c(rep(0, 6), rep(1, 8)), 
                                  edges = c(1, 10, 2, 11, 3, 12, 4, 10, 
                                            5, 7, 5, 8, 5, 12))

expect_equal(extract_isolates(big), c(6, 9, 13, 14))
expect_equal(extract_isolates(big, loops = TRUE), c(6, 9, 13, 14))
expect_true(igraph::vcount(big) == 14)
expect_true(igraph::ecount(big) == 7)
expect_true(igraph::vcount(remove_isolates(big)) == 10)
expect_true(igraph::ecount(remove_isolates(big)) == 7)
big_no_isolates <- remove_isolates(big)
expect_identical(big_no_isolates, remove_isolates(big_no_isolates))

igraph::V(big)$name <- c(paste("person", LETTERS[1:6]), 
                       paste("event", letters[1:8]))
expect_true(igraph::vcount(big) == 14)
expect_true(igraph::ecount(big) == 7)
expect_true(igraph::vcount(remove_isolates(big)) == 10)
expect_true(igraph::ecount(remove_isolates(big)) == 7)
expect_equal(igraph::V(remove_isolates(big))$name, 
                       c("person A", "person B", "person C", "person D", "person E",
                         "event a", "event b", "event d", "event e", "event f" ))
big_no_isolates <- remove_isolates(big)
expect_identical(big_no_isolates, remove_isolates(big_no_isolates))




### NETWORK ----
mat <- structure(c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 
                   0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 
                   0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                   0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 
                   0, 1, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 1, 1, 0, 0, 0, 0, 1, 0), 
                 dim = c(10L, 10L))


nw <- network::as.network.matrix(mat, loops = TRUE)
expect_equal(length(extract_isolates(nw)), 4)
expect_equal(length(extract_isolates(nw, loops = TRUE)), 2)
expect_true(network::network.size(nw) == 10)
expect_true(network::network.edgecount(nw) == 15)
expect_true(network::network.size(remove_isolates(nw)) == 6)
expect_true(network::network.size(remove_isolates(nw, loops = TRUE)) == 8)
expect_true(network::network.edgecount(remove_isolates(nw)) == 13) # vanwege de self-loops
expect_true(network::network.edgecount(remove_isolates(nw, loops = TRUE)) == 15)
# nothing should happen if the network does not have any isolates
nw_no_isolates <- remove_isolates(nw)
expect_identical(nw_no_isolates, remove_isolates(nw_no_isolates))

expect_true(network::network.size(remove_vertices(nw, c(1, 2, 4))) == 7)
expect_true(network::network.edgecount(remove_vertices(nw, c(1, 2, 4))) == 7)  # self-loop counts too
expect_true(network::network.edgecount(remove_vertices(nw, c(1, 2, 8))) == 8)
expect_error(remove_vertices(nw, c("A", "B", "C")))

# named vertices
network::set.vertex.attribute(nw, "vertex.names", LETTERS[1:10])
expect_true(network::network.size(remove_isolates(nw)) == 6)
expect_true(network::network.size(remove_isolates(nw, loops = TRUE)) == 8)
expect_true(network::network.edgecount(remove_isolates(nw)) == 13) # vanwege de self-loops
expect_true(network::network.edgecount(remove_isolates(nw, loops = TRUE)) == 15)
expect_equal(network::get.vertex.attribute(remove_isolates(nw), "vertex.names"), 
                       c("B", "C", "D", "G", "I", "J"))
expect_equal(network::get.vertex.attribute(remove_isolates(nw, loops = TRUE), "vertex.names"), 
                       c("A", "B", "C", "D", "G", "H", "I", "J"))
nw_no_isolates <- remove_isolates(nw)
expect_identical(nw_no_isolates, remove_isolates(nw_no_isolates))


# bipartite
bmat <- igraph::as_adjacency_matrix(big, sparse = FALSE)[1:6, 7:14]
colnames(bmat) <- rownames(bmat) <- NULL
bnw <- network::network.bipartite(bmat, network::as.network.matrix(bmat))
expect_equal(extract_isolates(bnw), c(6, 9, 13, 14))
expect_equal(extract_isolates(bnw, loops = TRUE), c(6, 9, 13, 14))
expect_true(network::network.size(bnw) == 14)
expect_true(network::network.edgecount(bnw) == 14) # since undirected, edges count twice in `network`
expect_true(network::network.size(remove_isolates(bnw)) == 10)
expect_true(network::network.edgecount(remove_isolates(bnw)) == 14)
bnw_no_isolates <- remove_isolates(bnw)
expect_identical(bnw_no_isolates, remove_isolates(bnw_no_isolates))

network::set.vertex.attribute(bnw, "vertex.names", 
                              c(paste("person", LETTERS[1:6]), 
                                paste("event", letters[1:8])))
expect_true(network::network.size(bnw) == 14)
expect_true(network::network.edgecount(bnw) == 14)  # counts twice!
expect_true(network::network.size(remove_isolates(bnw)) == 10)
expect_true(network::network.edgecount(remove_isolates(bnw)) == 14)
expect_equal(network::get.vertex.attribute(remove_isolates(bnw), "vertex.names"),
                       c("person A", "person B", "person C", "person D", "person E",
                         "event a", "event b", "event d", "event e", "event f" ))
bnw_no_isolates <- remove_isolates(bnw)
expect_identical(bnw_no_isolates, remove_isolates(bnw_no_isolates))






# remove_loops -----------------------------------------------------------------

x <- matrix(c(1, 1, 0, 0, 0, 1, 1, 0, 1), ncol = 3, byrow = TRUE)
expect_true(all(diag(remove_loops(x)) == c(0, 0, 0)))
g_n <- snafun::to_network(x)
expect_true(all(diag(snafun::to_matrix(g_n)) == c(1, 0, 1)))
expect_true(all(diag(remove_loops(g_n) |> snafun::to_matrix() == c(0, 0, 0))))
expect_true(all(diag(remove_loops(snafun::to_igraph(x)) |> snafun::to_matrix() == c(0, 0, 0))))


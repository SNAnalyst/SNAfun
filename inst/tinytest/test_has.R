


# has_vertexnames ----

# PREP
mat <- structure(c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 
                   0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 
                   0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                   0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 
                   0, 1, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 1, 1, 0, 0, 0, 0, 1, 0), 
                 dim = c(10L, 10L))
ig <- igraph::graph_from_adjacency_matrix(mat)
nw <- network::as.network.matrix(mat, loops = TRUE)
nw2 <- network::delete.vertex.attribute(nw, "vertex.names")

expect_error(has_vertexnames(mat), "class")
expect_error(has_vertexnames(mat), "igraph")
expect_error(has_vertexnames(mat), "network")

# without names
expect_false(has_vertexnames(ig))
expect_false(has_vertexnames(nw))
expect_false(has_vertexnames(nw2))

# without names
network::set.vertex.attribute(nw, "vertex.names", value = LETTERS[1:10])
igraph::V(ig)$name <- LETTERS[1:10]
expect_true(has_vertexnames(ig))
expect_true(has_vertexnames(nw))


# has_edge_attributes ----
## igraph
expect_false(has_edge_attributes(ig))
ig2 <- igraph::set_edge_attr(ig, "gewicht", value = runif(igraph::ecount(ig)))
expect_true(has_edge_attributes(ig2))
ig3 <- igraph::set_edge_attr(ig2, "gewicht2", value = runif(igraph::ecount(ig)))
expect_true(has_edge_attributes(ig3))

## network
i_n <- to_network(ig)
expect_false(has_edge_attributes(i_n))
network::set.edge.attribute(i_n, attrname = "gewicht", value = runif(network::network.edgecount(i_n)))
expect_true(has_edge_attributes(i_n))
network::delete.edge.attribute(i_n, "gewicht")
expect_false(has_edge_attributes(i_n))






# has_isolates
data(soccer98, package = "snafun")
expect_true(has_isolates(soccer98))
g <- remove_isolates(soccer98)
expect_false(has_isolates(g))


data(florentine, package = "snafun")
has_isolates(florentine$flobusiness)
has_isolates(florentine$flomarriage)
expect_false(has_isolates(remove_isolates(florentine$flobusiness)))
expect_false(has_isolates(remove_isolates(florentine$flomarriage)))



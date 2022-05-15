
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


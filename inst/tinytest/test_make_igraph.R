
report_side_effects()


# NETWORK TO IGRAPH
g_n <- sapply(runif(10, 0, 1), rep, 10)
g_n <- sna::rgraph(10, tprob = g_n)
g2_n <- network::as.network.matrix(g_n)
# network changes the object in place! So, attr1 is added to g2_n
g3_n <- network::set.vertex.attribute(g2_n, "attr1", LETTERS[20:11])
g4_n <- network::set.vertex.attribute(g3_n, "attr2", 111:120)
g5_n <- network::set.vertex.attribute(g4_n, "attr3", letters[12:21])

g_i <- make_igraph(g_n)
g2_i <- make_igraph(g2_n)
g3_i <- make_igraph(g3_n)
g4_i <- make_igraph(g4_n)
g5_i <- make_igraph(g5_n)


expect_equal(igraph::vcount(g2_i), network::network.size(g2_n))
expect_equal(igraph::ecount(g2_i), network::network.edgecount(g2_n))
expect_true(length(igraph::edge_attr_names(g2_i)) == 0)
expect_true(network::list.edge.attributes(g2_n) == "na")
expect_true(length(igraph::vertex_attr_names(g2_i)) == 3)
expect_true(all(c("attr1", "na", "vertex.names") %in% network::list.vertex.attributes(g2_n)))

expect_equal(igraph::vcount(g3_i), network::network.size(g3_n))
expect_equal(igraph::ecount(g3_i), network::network.edgecount(g3_n))
expect_true(length(igraph::edge_attr_names(g3_i)) == 0)
expect_true(network::list.edge.attributes(g3_n) == "na")
expect_true(length(igraph::vertex_attr_names(g3_i)) == 4)
expect_true(all(c("attr1", "attr2", "na", "vertex.names") %in% network::list.vertex.attributes(g3_n)))

expect_equal(igraph::vcount(g4_i), network::network.size(g4_n))
expect_equal(igraph::ecount(g4_i), network::network.edgecount(g4_n))
expect_true(length(igraph::edge_attr_names(g4_i)) == 0)
expect_true(network::list.edge.attributes(g4_n) == "na")
expect_true(length(igraph::vertex_attr_names(g4_i)) == 5)
expect_true(all(c("attr1", "attr2", "attr3", "na", "vertex.names") %in% network::list.vertex.attributes(g4_n)))

expect_equal(igraph::vcount(g5_i), network::network.size(g5_n))
expect_equal(igraph::ecount(g5_i), network::network.edgecount(g5_n))
expect_true(length(igraph::edge_attr_names(g5_i)) == 0)
expect_true(network::list.edge.attributes(g5_n) == "na")
expect_true(length(igraph::vertex_attr_names(g5_i)) == 5)
expect_true(all(c("attr1", "attr2", "attr3", "na", "vertex.names") %in% network::list.vertex.attributes(g5_n)))



g <- igraph::sample_gnp(10, 2/10)
mat <- igraph::as_adjacency_matrix(g, sparse = FALSE)
ig <- make_igraph(mat)
expect_true(inherits(ig, "igraph"))
expect_equal(igraph::vcount(ig), nrow(mat))
expect_equal(igraph::ecount(ig), sum(mat)/2)  # undirected
expect_true(length(igraph::list.edge.attributes(ig)) == 0)
expect_true(length(igraph::list.vertex.attributes(ig)) == 0)

g <- igraph::make_ring(10)
igraph::E(g)$weight <- seq_len(igraph::ecount(g))
mat <- igraph::as_adjacency_matrix(g, sparse = FALSE, attr = "weight")
ig <- make_igraph(mat)
expect_true(inherits(ig, "igraph"))
expect_equal(igraph::vcount(ig), nrow(mat))
expect_equal(igraph::ecount(ig), sum(mat != 0)/2)  # undirected, weighted
expect_equal(sum(igraph::E(ig)$weight), sum(mat)/2) # undirected
expect_true(length(igraph::list.edge.attributes(ig)) == 1)
expect_true(length(igraph::list.vertex.attributes(ig)) == 0)

# bipartite network, even nodes are one type, odd vertices another type
g <- igraph::make_bipartite_graph( rep(0:1,length=10), c(1:10))
mat <- igraph::as_adjacency_matrix(g, sparse = FALSE)
ig <- make_igraph(mat)  # same network, but not officially bipartite
expect_true(inherits(ig, "igraph"))
expect_false(igraph::is.bipartite(ig))
expect_equal(igraph::vcount(ig), nrow(mat))
expect_equal(igraph::ecount(ig), sum(mat != 0)/2)

mat2 <- igraph::as_incidence_matrix(g, sparse = FALSE)
ig2 <- make_igraph(mat2, bipartite = TRUE)
expect_true(inherits(ig2, "igraph"))
expect_true(igraph::is.bipartite(ig2))
expect_equal(igraph::vcount(ig2), nrow(mat2) + ncol(mat2))
expect_equal(igraph::ecount(ig2), sum(mat2))













report_side_effects()


# NETWORK TO IGRAPH ----
# ## unipartite
g_n <- sapply(runif(10, 0, 1), rep, 10)
g_n <- sna::rgraph(10, tprob = g_n)
g2_n <- network::as.network.matrix(g_n)
# network changes the object in place! So, attr1 is added to g2_n
g3_n <- network::set.vertex.attribute(g2_n, "attr1", LETTERS[20:11])
g4_n <- network::set.vertex.attribute(g3_n, "attr2", 111:120)
g5_n <- network::set.vertex.attribute(g4_n, "attr3", letters[12:21])

g_i <- to_igraph(g_n)
g2_i <- to_igraph(g2_n)
g3_i <- to_igraph(g3_n)
g4_i <- to_igraph(g4_n)
g5_i <- to_igraph(g5_n)


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
ig <- to_igraph(mat)
expect_true(inherits(ig, "igraph"))
expect_equal(igraph::vcount(ig), nrow(mat))
expect_equal(igraph::ecount(ig), sum(mat)/2)  # undirected
expect_false(snafun::is_directed(ig)) # undirected
expect_true(length(igraph::edge_attr_names(ig)) == 0)
expect_true(length(igraph::vertex_attr_names(ig)) == 0)

# directed matrix
g <- igraph::sample_gnp(10, 2/10, directed = TRUE)
mat <- igraph::as_adjacency_matrix(g, sparse = FALSE)
ig <- to_igraph(mat)
expect_true(inherits(ig, "igraph"))
expect_true(snafun::is_directed(ig)) # directed



g <- igraph::make_ring(10)
igraph::E(g)$weight <- seq_len(igraph::ecount(g))
mat <- igraph::as_adjacency_matrix(g, sparse = FALSE, attr = "weight")
ig <- to_igraph(mat)
expect_true(inherits(ig, "igraph"))
expect_equal(igraph::vcount(ig), nrow(mat))
expect_equal(igraph::ecount(ig), sum(mat != 0)/2)  # undirected, weighted
expect_equal(sum(igraph::E(ig)$weight), sum(mat)/2) # undirected
expect_true(length(igraph::edge_attr_names(ig)) == 1)
expect_true(length(igraph::vertex_attr_names(ig)) == 0)


## edge attributes
g6_n <- add_edge_attributes(g5_n, "eatt1", runif(network::network.edgecount(g5_n)))
g6_i <- to_igraph(g6_n)
expect_true("eatt1" %in% list_edge_attributes(g6_i))
edges_n <- to_edgelist(g6_n)
eid_n <- extract_edge_id(g6_n, edgelist = edges_n[, 1:2])$eid
eid_i <- extract_edge_id(g6_i, edgelist = edges_n[, 1:2])$eid
reorder <- match(eid_i, eid_n)
expect_equal(network::get.edge.value(g6_n, "eatt1"), 
             igraph::edge_attr(g6_i, "eatt1")[reorder])
g7_n <- add_edge_attributes(g6_n, "eatt2", runif(network::network.edgecount(g5_n)))
g7_i <- to_igraph(g7_n)
expect_true("eatt1" %in% list_edge_attributes(g7_i))
expect_true("eatt2" %in% list_edge_attributes(g7_i))
edges_n <- to_edgelist(g7_n)
eid_n <- extract_edge_id(g7_n, edgelist = edges_n[, 1:2])$eid
eid_i <- extract_edge_id(g7_i, edgelist = edges_n[, 1:2])$eid
reorder <- match(eid_i, eid_n)
expect_equal(network::get.edge.value(g7_n, "eatt2"), 
             igraph::edge_attr(g7_i, "eatt2")[reorder])
expect_equal(network::get.edge.value(g7_n, "eatt1"), 
             igraph::edge_attr(g7_i, "eatt1")[reorder])




# bipartite network, even nodes are one type, odd vertices another type
g <- igraph::make_bipartite_graph( rep(0:1,length=10), c(1:10))
mat <- igraph::as_adjacency_matrix(g, sparse = FALSE)
ig <- to_igraph(mat)  # same network, but not officially bipartite
expect_true(inherits(ig, "igraph"))
expect_false(igraph::is_bipartite(ig))
expect_equal(igraph::vcount(ig), nrow(mat))
expect_equal(igraph::ecount(ig), sum(mat != 0)/2)

mat2 <- igraph::as_biadjacency_matrix(g, sparse = FALSE)
ig2 <- to_igraph(mat2, bipartite = TRUE)
expect_true(inherits(ig2, "igraph"))
expect_true(igraph::is_bipartite(ig2))
expect_equal(igraph::vcount(ig2), nrow(mat2) + ncol(mat2))
expect_equal(igraph::ecount(ig2), sum(mat2))


# EDGELIST TO IGRAPH ----
relations <- data.frame(from = c("Bob", "Cecil", "Cecil", "David", 
    "David", "Esmeralda"), 
    to = c("Alice", "Bob", "Alice", "Alice", "Bob", "Alice"),
    same.dept = c(FALSE, FALSE, TRUE, FALSE, FALSE, TRUE), 
    friendship = c(4, 5, 5, 2, 1, 1), advice = c(4, 5, 5, 4, 2, 3))

aa <- data.frame(from = c(1,1,2,2,3,3,4,4), 
    to = c(11, 12, 13, 14, 15, 16, 17, 18))


ig_rel <- to_igraph(relations)
ig_aa <- to_igraph(aa, bipartite = TRUE)

expect_error(to_igraph(relations, TRUE))
expect_false(igraph::is_bipartite(ig_rel))
expect_true(igraph::vcount(ig_rel) == 5)
expect_true(igraph::ecount(ig_rel) == 6)
expect_message(to_igraph(aa, FALSE), "unipartite")
expect_true(igraph::is_bipartite(ig_aa))
expect_true(igraph::vcount(ig_aa) == 12)
expect_true(igraph::ecount(ig_aa) == 8)

### check the vertices argument
verts <- data.frame(c(1:4, 11:18), 
                    role = c(rep("person", 4), rep("firm", 8)),
                    attr1 = LETTERS[1:12],
                    attr2 = letters[11:22])
ig_vert <- suppressMessages(to_igraph(aa, vertices = verts))

expect_equal(igraph::V(ig_vert)$role, c("person", "person", "person", "person", "firm", "firm", "firm", "firm", "firm", "firm", "firm", "firm"))
expect_equal(igraph::V(ig_vert)$attr1, LETTERS[1:12])
# include a vertex that does not occur in the vertex list
verts2 <- data.frame(c(1:4, 11:19), 
                    role = c(rep("person", 4), rep("firm", 9)),
                    attr1 = LETTERS[1:13],
                    attr2 = letters[11:23])
ig_vert2 <- suppressMessages(to_igraph(aa, vertices = verts2))
expect_true(igraph::vcount(ig_vert2) == 13)

# 1 vertex that does not occur in the vertices and 
# 4 vertices are missing in verts3
verts3 <- data.frame(c(1:4, 15:18), 
                     role = c(rep("person", 4), rep("firm", 4)),
                     attr1 = LETTERS[1:8],
                     attr2 = letters[11:18])
expect_error(to_igraph(aa, vertices = verts3))
# check if the correct error is triggered
fout <- tryCatch(to_igraph(aa, vertices = verts3), error = function(e) e)
expect_true(grepl("^Some vertices that occur in your edgelist are", x = fout$message))

# MATRIX TO IGRAPH ----
### random directed matrix
mat <- matrix(sample(c(0, 1), 100, replace = TRUE), nrow = 10)
diag(mat) <- 0
ig <- to_igraph(mat)
expect_equal(igraph::vcount(ig), nrow(mat))
expect_equal(igraph::ecount(ig), sum(mat))
expect_true(snafun::is_directed(ig))
## add names
rownames(mat) <- colnames(mat) <- LETTERS[1:nrow(mat)]
ig <- to_igraph(mat)
expect_equal(igraph::vcount(ig), nrow(mat))
expect_equal(igraph::ecount(ig), sum(mat))
expect_true(snafun::is_directed(ig))
expect_true(has_vertexnames(ig))
expect_identical(igraph::vertex_attr(ig, "name"), LETTERS[1:nrow(mat)])


# random undirected matrix
mat <- matrix(0, nrow = 10, ncol = 10)
mat[upper.tri(mat)] <- sample(c(0, 1), 10*9/2, replace = TRUE)
mat <- sna::symmetrize(mat, rule = "upper")
expect_true(isSymmetric(mat))
ig <- to_igraph(mat)
expect_equal(igraph::vcount(ig), nrow(mat))
expect_equal(igraph::ecount(ig), sum(mat)/2)
expect_false(snafun::is_directed(ig))
## add names
rownames(mat) <- colnames(mat) <- LETTERS[1:nrow(mat)]
ig <- to_igraph(mat)
expect_equal(igraph::vcount(ig), nrow(mat))
expect_equal(igraph::ecount(ig), sum(mat)/2)
expect_false(snafun::is_directed(ig))
expect_true(has_vertexnames(ig))
expect_identical(igraph::vertex_attr(ig, "name"), LETTERS[1:nrow(mat)])


# random bipartite matrix
g <- igraph::make_bipartite_graph(c(rep(0, times = 5), rep(1, times = 10)), 
                                  c(1, 10, 1, 11, 2, 12, 3, 14, 3, 15, 4, 13, 5, 15))
mat <- igraph::as_biadjacency_matrix(g, sparse = FALSE)
## has row and column names
ig <- to_igraph(mat) # automatically bipartite
expect_equal(igraph::vcount(ig), nrow(mat) + ncol(mat))
expect_equal(igraph::ecount(ig), sum(mat))
expect_true(is_bipartite(ig))
expect_identical(igraph::vertex_attr(ig, "name"), as.character(1:15))

ig2 <- to_igraph(mat, bipartite = TRUE)
expect_equal(igraph::vcount(ig2), nrow(mat) + ncol(mat))
expect_equal(igraph::ecount(ig2), sum(mat))
expect_true(is_bipartite(ig2))
expect_identical(igraph::vertex_attr(ig2, "name"), as.character(1:15))

ig3 <- to_igraph(mat, bipartite = FALSE)
expect_equal(igraph::vcount(ig3), nrow(mat) + ncol(mat))
expect_equal(igraph::ecount(ig3), sum(mat))
expect_true(is_bipartite(ig3))  # this is odd, it should be false?????
expect_identical(igraph::vertex_attr(ig3, "name"), as.character(1:15))

rownames(mat) <- colnames(mat) <- NULL
ig <- to_igraph(mat)
expect_equal(igraph::vcount(ig), nrow(mat) + ncol(mat))
expect_equal(igraph::ecount(ig), sum(mat))
expect_true(is_bipartite(ig))
expect_null(igraph::vertex_attr(ig, "name"))


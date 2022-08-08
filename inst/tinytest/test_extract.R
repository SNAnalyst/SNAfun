


# extract_subgraph -------------------------------------------------------------

## igraph ----------------------------------------------------------------------
g <- igraph::make_ring(10)
g1 <- extract_subgraph(g, v_to_keep = 3:8)
expect_equal(igraph::vcount(g1), 6)
expect_equal(igraph::ecount(g1), 5)

g2 <- extract_subgraph(g, e_to_keep = 4:8)
expect_equal(igraph::vcount(g1), 6)
expect_equal(igraph::ecount(g1), 5)
expect_equal(to_matrix(g1), to_matrix(g2))

g3 <- suppressWarnings(extract_subgraph(g, v_to_keep = 3:8, e_to_keep = 4:8))
expect_equal(igraph::vcount(g3), 6)
expect_equal(igraph::ecount(g3), 5)
expect_equal(to_matrix(g1), to_matrix(g3))
expect_warning(extract_subgraph(g, v_to_keep = 3:8, e_to_keep = 4:8), 
               "When both 'v_to_keep' and 'e_to_keep' are specified")
rm(g1, g2, g3)

## network ---------------------------------------------------------------------
g <- to_network(g)
g$gal$directed <- FALSE
g1 <- extract_subgraph(g, v_to_keep = 3:8)
expect_equal(network::network.size(g1), 6)
# expect_equal(network::network.edgecount(g1), 5)

g2 <- extract_subgraph(g, e_to_keep = 4:8)
expect_equal(network::network.size(g1), 6)
# note that the edges are numbered differently from the igraph object

g3 <- suppressWarnings(extract_subgraph(g, v_to_keep = 3:8, e_to_keep = 4:8))
expect_equal(network::network.size(g3), 6)
# expect_equal(network::network.edgecount(g3), 5)
expect_equal(to_matrix(g1), to_matrix(g3))
expect_warning(extract_subgraph(g, v_to_keep = 3:8, e_to_keep = 4:8), 
               "When both 'v_to_keep' and 'e_to_keep' are specified")

rm(g, g1, g2, g3)

### emon dataset
data(emon, package = "network")
g <- emon$MtStHelens
g1 <- extract_subgraph(g, e_to_keep = which(extract_edge_attribute(g, 'Frequency') == 2))
expect_equal(network::network.size(g1), 10)
expect_equal(network::network.edgecount(g1), 12)

g_i <- to_igraph(g)
g1_i <- extract_subgraph(g_i, e_to_keep = which(extract_edge_attribute(g_i, 'Frequency') == 2))
expect_equal(igraph::vcount(g1_i), 10)
expect_equal(igraph::ecount(g1_i), 12)

# # delete the edge weight, b/c it does not exist in g1
# g1_i <- igraph::remove.edge.attribute(g1_i, "weight")
# g_m <- to_matrix(g1)
# g_i_m <- to_matrix(g1_i)
# expect_equal(g_m, g_i_m)





# extract_egonet ---------------------------------------------------------------
g <- igraph::graph_from_literal(One --+ Two +-+ Three +-- Four --+ Five +-- Six +-- Seven +-+ Eight +-+ One +-+ Five)
g_m <- snafun::to_matrix(g)
m <- extract_egonet(g, vertices = 1, order = 1, type = "in")[[1]] |> snafun::to_matrix()
expect_equal(dim(m), c(3, 3))
expect_equal(colnames(m), c("One", "Five", "Eight"))
m <- extract_egonet(g, vertices = 1, order = 1, type = "out")[[1]] |> snafun::to_matrix()
expect_equal(dim(m), c(4, 4))
expect_equal(colnames(m), c("One", "Two", "Five", "Eight"))
m <- extract_egonet(g, vertices = 1, order = 1, type = "all")[[1]] |> snafun::to_matrix()
expect_equal(dim(m), c(4, 4))
expect_equal(colnames(m), c("One", "Two", "Five", "Eight"))
m <- extract_egonet(g, vertices = 1, order = 2, type = "in")[[1]] |> snafun::to_matrix()
expect_equal(dim(m), c(6, 6))
expect_equal(colnames(m), c("One", "Four", "Five", "Six", "Seven", "Eight"))
m <- extract_egonet(g, vertices = 1, order = 2, type = "out")[[1]] |> snafun::to_matrix()
expect_equal(dim(m), c(6, 6))
expect_equal(colnames(m), c("One", "Two", "Three", "Five", "Seven", "Eight"))
m <- extract_egonet(g, vertices = 1, order = 2, type = "all")[[1]] |> snafun::to_matrix()
expect_equal(dim(m), c(8, 8))
expect_equal(colnames(m), c("One", "Two", "Three", "Four", "Five", "Six", "Seven", "Eight"))
m <- extract_egonet(g, vertices = c("One"), order = 2, type = "in")[[1]] |> snafun::to_matrix()
expect_equal(dim(m), c(6, 6))
expect_equal(colnames(m), c("One", "Four", "Five", "Six", "Seven", "Eight"))
m <- extract_egonet(g, vertices = c("One", "Three"), order = 2, type = "in")
expect_equal(length(m), 2)
expect_inherits(m[[1]], "igraph")
expect_true(count_vertices(m[[1]]) == 6)
expect_true(count_edges(m[[1]]) == 9)

g_n <- snafun::to_network(g)
m <- extract_egonet(g_n, vertices = 1, order = 1, type = "in")[[1]] |> snafun::to_matrix()
expect_equal(dim(m), c(3, 3))
expect_equal(colnames(m), c("One", "Five", "Eight"))
m <- extract_egonet(g_n, vertices = 1, order = 1, type = "out")[[1]] |> snafun::to_matrix()
expect_equal(dim(m), c(4, 4))
expect_equal(colnames(m), c("One", "Two", "Five", "Eight"))
m <- extract_egonet(g_n, vertices = 1, order = 1, type = "all")[[1]] |> snafun::to_matrix()
expect_equal(dim(m), c(4, 4))
expect_equal(colnames(m), c("One", "Two", "Five", "Eight"))
m <- extract_egonet(g_n, vertices = 1, order = 2, type = "in")[[1]] |> snafun::to_matrix()
expect_equal(dim(m), c(6, 6))
expect_equal(colnames(m), c("One", "Four", "Five", "Six", "Seven", "Eight"))
m <- extract_egonet(g_n, vertices = 1, order = 2, type = "out")[[1]] |> snafun::to_matrix()
expect_equal(dim(m), c(6, 6))
expect_equal(colnames(m), c("One", "Two", "Three", "Five", "Seven", "Eight"))
m <- extract_egonet(g_n, vertices = 1, order = 2, type = "all")[[1]] |> snafun::to_matrix()
expect_equal(dim(m), c(8, 8))
expect_equal(colnames(m), c("One", "Two", "Three", "Four", "Five", "Six", "Seven", "Eight"))
m <- extract_egonet(g_n, vertices = c("One"), order = 2, type = "in")[[1]] |> snafun::to_matrix()
expect_equal(dim(m), c(6, 6))
expect_equal(colnames(m), c("One", "Four", "Five", "Six", "Seven", "Eight"))
m <- extract_egonet(g_n, vertices = c("One", "Three"), order = 2, type = "in")
expect_equal(length(m), 2)
expect_inherits(m[[1]], "network")
expect_true(count_vertices(m[[1]]) == 6)
expect_true(count_edges(m[[1]]) == 9)




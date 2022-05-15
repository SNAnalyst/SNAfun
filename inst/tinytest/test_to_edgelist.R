
g_i <- igraph::sample_gnp(10, 2/10)

g1_i <- g_i
igraph::V(g1_i)$name <- LETTERS[seq_len(igraph::gorder(g1_i))]

g2_i <- g1_i
igraph::E(g2_i)$weight <- seq_len(igraph::ecount(g2_i))
to_edgelist(g2_i)

# add further edge attribute
g3_i <- g2_i |> 
    igraph::set_edge_attr("color", value = "red")


## from a matrix
g_n <- sapply(runif(10, 0, 1), rep, 10)
g_n <- sna::rgraph(10, tprob = g_n)
el_n <- to_edgelist(g_n)

g_b <- igraph::make_bipartite_graph(c(rep(0, 5), rep(1, 3)), c(1,6,2,7,3,6,3,7,4,8,5,8))
el_b <- to_edgelist(g_b)

g2_n <- network::as.network.matrix(g_n)
el2_n <- to_edgelist(g2_n)



##  ............................................................................
##  TESTS                                                                   ####

report_side_effects()

# igraph
expect_identical(to_edgelist(g_i, sort = "to"), igraph::as_data_frame(g_i))
expect_identical(to_edgelist(g1_i, sort = "to"), igraph::as_data_frame(g1_i))
expect_identical(to_edgelist(g2_i, sort = "weight"), igraph::as_data_frame(g2_i))
expect_identical(colnames(to_edgelist(g3_i)), c("from", "to", "weight", "color"))

# matrix
expect_true(length(unique(el_n$from)) == 10)
# all edges included?
expect_true(nrow(el_n) == sum(g_n))
expect_true(ncol(el_n) == 2)
expect_true(ncol(el_b) == 2)
expect_true(nrow(el_b) == 6)
expect_true(length(intersect(el_b$from, el_b$to)) == 0)

# network
expect_true(ncol(el2_n) == 2)
expect_true(nrow(el2_n) == network::network.edgecount(g2_n))

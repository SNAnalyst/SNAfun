
##  ............................................................................
##  TESTS                                                                   ####

report_side_effects()

# matrix ----
g_m <- sapply(runif(10, 0, 1), rep, 10)
g_m <- sna::rgraph(10, tprob = g_m)
e_m <- to_edgelist(g_m)
expect_equal(nrow(e_m), sum(g_m))
expect_inherits(e_m$from, "numeric")
expect_inherits(e_m$to, "numeric")
expect_true(ncol(e_m) == 2)

e_m2 <- to_edgelist(g_m, named = FALSE)
expect_identical(e_m, e_m2)

colnames(g_m) <- rownames(g_m) <- LETTERS[1:ncol(g_m)]
e_m3 <- to_edgelist(g_m, named = FALSE)
expect_identical(e_m, e_m3)

e_m4 <- to_edgelist(g_m, named = TRUE)
expect_equal(nrow(e_m4), sum(g_m))
expect_inherits(e_m4$from, "character")
expect_inherits(e_m4$to, "character")
expect_true(ncol(e_m4) == 2)

# check if sorting worked well
n_edges <- nrow(e_m4)
expect_true(all(e_m4$from[1:(n_edges - 1)] <= e_m4$from[2:n_edges]))
e_m5 <- to_edgelist(g_m, named = TRUE, sort = "to")
expect_true(all(e_m5$to[1:(n_edges - 1)] <= e_m5$to[2:n_edges]))

# weighted
data(florentine, package = "snafun")
flomar_i <- florentine$flomarriage
igraph::E(flomar_i)$weight = 1:igraph::ecount(flomar_i)
flomar_m <- to_matrix(flomar_i)
flomar_e <- to_edgelist(flomar_m, named = TRUE)
expect_equal(flomar_e$weight, 1:igraph::ecount(flomar_i))
flomar_e2 <- to_edgelist(flomar_m, named = FALSE)
expect_equal(flomar_e$weight, flomar_e2$weight)

## bipartite
edges <- c(1,6,2,7,3,6,3,7,4,8,5,8)
g_b <- igraph::make_bipartite_graph(c(rep(0, 5), rep(1, 3)), edges)
g_bm <- to_matrix(g_b)
g_e <- to_edgelist(g_bm)
edges <- matrix(edges, ncol = 2, byrow = TRUE)
expect_true(all(g_e == edges))





# igraph ----
g_i <- igraph::sample_gnp(10, 2/10)  # not named
g1_i <- g_i
igraph::V(g1_i)$name <- LETTERS[seq_len(igraph::gorder(g1_i))]  # named
g2_i <- g1_i
igraph::E(g2_i)$weight <- seq_len(igraph::ecount(g2_i))  # named and weighted
# add further edge attribute
g3_i <- g2_i |> 
  igraph::set_edge_attr("color", value = "red")

expect_identical(to_edgelist(g_i, sort = "to"), igraph::as_data_frame(g_i))
expect_identical(to_edgelist(g1_i, sort = "to"), igraph::as_data_frame(g1_i))
expect_identical(to_edgelist(g2_i, sort = "weight"), igraph::as_data_frame(g2_i))
expect_identical(colnames(to_edgelist(g3_i)), c("from", "to", "weight", "color"))

expect_identical(to_edgelist(g_i), to_edgelist(g_i, named = FALSE))
expect_identical(to_edgelist(g3_i)$weight, to_edgelist(g3_i, named = FALSE)$weight)
expect_identical(to_edgelist(g3_i)$color, to_edgelist(g3_i, named = FALSE)$color)




# network ----
g_n <- to_network(g_i)
g1_n <- to_network(g1_i)
g2_n <- to_network(g2_i)
g3_n <- to_network(g3_i)

expect_true(all(to_edgelist(g_n, sort = "to") == network::as.data.frame.network(g_n)))
expect_true(all(to_edgelist(g1_n, sort = "to") == network::as.data.frame.network(g1_n)))
expect_true(all(to_edgelist(g2_n, sort = "to") == network::as.data.frame.network(g2_n)))
expect_equal(to_edgelist(g2_n, sort = "weight")$weight, 1:network::network.edgecount(g2_n))
expect_identical(colnames(to_edgelist(g3_n)), c("from", "to", "weight", "color"))

expect_identical(to_edgelist(g_n), to_edgelist(g_n, named = FALSE))
expect_identical(to_edgelist(g3_n)$weight, to_edgelist(g3_n, named = FALSE)$weight)
expect_identical(to_edgelist(g3_n)$color, to_edgelist(g3_n, named = FALSE)$color)



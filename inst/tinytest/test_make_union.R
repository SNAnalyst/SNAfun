
expect_equal(1, 1)
expect_equal(1 + 1, 2)

net1 <- snafun::create_manual_graph(
  D - A:B:F:G, A - C - F - A, B - E - G - B, A - B, F - G,
  H - F:G, H - I - J
)
net2 <- snafun::create_manual_graph(D - A:F:Y, B - A - X - F - H - Z, F - Y)
assign("net1", net1, envir = .GlobalEnv)
assign("net2", net2, envir = .GlobalEnv)


v1 <- snafun::make_union(net1, net2)
v2 <- snafun::make_union(snafun::to_network(net1), net2)
v3 <- snafun::make_union(net1, snafun::to_network(net2))
v4 <- snafun::make_union(snafun::to_matrix(net1), net2)
v5 <- snafun::make_union(net1, snafun::to_matrix(net2))
v6 <- igraph::union(net1, net2)  # check against igraph

expect_error(snafun::make_union(snafun::to_edgelist(net1), snafun::to_matrix(net2)))
expect_error(snafun::make_union(snafun::to_edgelist(net1), snafun::to_matrix(net2)),
             pattern = "^Provide actual graphs")

expect_inherits(v1, "igraph")
expect_inherits(v2, "network")
expect_inherits(v3, "igraph")
expect_inherits(v4, "matrix")
expect_inherits(v5, "igraph")

expect_equal(snafun::to_matrix(v1), snafun::to_matrix(v2))
expect_equal(snafun::to_matrix(v1), snafun::to_matrix(v3))
expect_equal(snafun::to_matrix(v1), snafun::to_matrix(v4))
expect_equal(snafun::to_matrix(v1), snafun::to_matrix(v5))
expect_equal(snafun::to_matrix(v1), snafun::to_matrix(v6))

expect_equal(snafun::count_edges(v1), 21)
expect_equal(snafun::count_vertices(v1), 13)


g <- snafun::create_manual_graph(1-2, 1-3, 2-3, 2-4, 3-5, 4-5, 4-6, 4-7, 5-6, 6-7)
h <- snafun::extract_subgraph(g, v_to_keep = 1:5)
k <- snafun::create_manual_graph(4-6, 4-7, 5-6, 6-7)
assign("h", h, envir = .GlobalEnv)
assign("k", k, envir = .GlobalEnv)
h_k <- snafun::make_union(h,k)
expect_equal(snafun::to_matrix(g), snafun::to_matrix(h_k))




# now with three graphs
net3 <- snafun::create_manual_graph(H-I:K, I-E, X-Q-Z)
assign("net3", net3, envir = .GlobalEnv)
v7 <- snafun::make_union(net1, net2, net3)
v8 <- snafun::make_union(net1, net3, net2)
v9 <- snafun::make_union(net3, net2, net1)
v10 <-igraph::union(net2, net3, net1)
expect_equal(snafun::count_vertices(v7), snafun::count_vertices(v8))
expect_equal(snafun::count_vertices(v7), snafun::count_vertices(v9))
expect_equal(snafun::count_vertices(v7), snafun::count_vertices(v10))
expect_equal(snafun::count_edges(v7), snafun::count_edges(v8))
expect_equal(snafun::count_edges(v7), snafun::count_edges(v9))
expect_equal(snafun::count_edges(v7), snafun::count_edges(v10))
v7_mat <- snafun::to_matrix(v7)
v7_mat <- v7_mat[sort(colnames(v7_mat)), sort(rownames(v7_mat))]
v10_mat <- snafun::to_matrix(v10)
v10_mat <- v10_mat[sort(colnames(v10_mat)), sort(rownames(v10_mat))]
expect_identical(v7_mat, v10_mat)







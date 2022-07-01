
report_side_effects()


# igraph -----------------------------------------------------------------------

g <- igraph::make_graph("Zachary")
expect_equal(extract_neighbors(g, 1), c(2, 3, 4, 5, 6, 7, 8, 9, 11, 12, 13, 14, 18, 20, 22, 32))
expect_equal(extract_neighbors(g, 27), c(30, 34))
expect_error(extract_neighbors(g, 100), "This vertex is in not the graph")
expect_error(suppressWarnings(extract_neighbors(g, "a")))

# make vertices named
igraph::V(g)$name <- c(letters, LETTERS[1:8])
expect_equal(extract_neighbors(g, 1), c(2, 3, 4, 5, 6, 7, 8, 9, 11, 12, 13, 14, 18, 20, 22, 32))
expect_equal(extract_neighbors(g, 27), c(30, 34))
expect_error(extract_neighbors(g, 100), "This vertex is in not the graph")
expect_equal(extract_neighbors(g, "A"), c("D", "H"))
expect_error(extract_neighbors(g, c(27, 3)), "You need to specify exactly 1 vertex")


m <- matrix(0, 3, 3)
m[1, 2] <- 1; m[2, 3] <- 1; m[3, 1] <- 1
g_i <- snafun::to_igraph(m)
expect_equal(extract_neighbors(g_i, 1, "out"), 2)
expect_equal(extract_neighbors(g_i, 1, "in"), 3)
expect_equal(extract_neighbors(g_i, 1, "all"), c(2, 3))


# network ----------------------------------------------------------------------
g_n <- igraph::make_graph("Zachary") |> 
  snafun::to_network()
expect_equal(extract_neighbors(g_n, 1), c(2, 3, 4, 5, 6, 7, 8, 9, 11, 12, 13, 14, 18, 20, 22, 32))
expect_equal(extract_neighbors(g_n, 27), c(30, 34))
expect_equal(extract_neighbors(g_n, 100), numeric(0))
expect_error(suppressWarnings(extract_neighbors(g_n, "a")))


network::network.vertex.names(g_n) <- c(letters, LETTERS[1:8])
expect_equal(extract_neighbors(g_n, 1), c(2, 3, 4, 5, 6, 7, 8, 9, 11, 12, 13, 14, 18, 20, 22, 32))
expect_equal(extract_neighbors(g_n, 27), c(30, 34))
expect_equal(extract_neighbors(g_n, 100), numeric(0))
expect_equal(extract_neighbors(g_n, "A"), c("D", "H"))
expect_error(extract_neighbors(g_n, c(27, 3)), "You need to specify exactly 1 vertex")




m <- matrix(0, 3, 3)
m[1, 2] <- 1; m[2, 3] <- 1; m[3, 1] <- 1
g_n <- snafun::to_network(m)
expect_equal(extract_neighbors(g_n, 1, "out"), 2)
expect_equal(extract_neighbors(g_n, 1, "in"), 3)
expect_equal(extract_neighbors(g_n, 1, "all"), c(2, 3))


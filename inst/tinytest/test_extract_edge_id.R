

# igraph ----
edges <- data.frame(from = c(1, 1, 2, 2, 3, 3, 4), 
                    to = c(2, 3, 1, 4, 4, 5, 1),
                    weight = 1:7,
                    code = LETTERS[1:7])
i_g <- igraph::graph_from_data_frame(edges)

expect_equal(extract_edge_id(i_g, ego = c(1, 3))[, "eid"], c(1, 2, 5, 6))
expect_equal(extract_edge_id(i_g, ego = c(1, 3), alter = c(2))[, "eid"], c(1, 0))
expect_equal(extract_edge_id(i_g, alter = c(1, 3))[, "eid"], c(2, 3, 7))
expect_equal(extract_edge_id(i_g, ego = c(1, 3), alter = c(2, 3, 4))[, "eid"], c(1, 2, 0, 0, 0, 5))
expect_equal(extract_edge_id(i_g, ego = 1, alter = 2)[, "eid"], 1)
expect_equal(extract_edge_id(i_g, edgelist = data.frame(a = c(1, 2), b = c(2, 3)))[, "eid"], c(1, 0))

# name the vertices
igraph::V(i_g)$name <- LETTERS[1:5]
# does it work without using the names
expect_equal(extract_edge_id(i_g, ego = c(1, 3))[, "eid"], c(1, 2, 5, 6))
expect_equal(extract_edge_id(i_g, ego = c(1, 3), alter = 2)[, "eid"], c(1, 0))
expect_equal(extract_edge_id(i_g, alter = c(1, 3))[, "eid"], c(2, 3, 7))
# does it work using names?
expect_error(extract_edge_id(i_g, ego = c("A", "C")), 
             "Please provide numeric vertex id's for 'ego'")
expect_error(extract_edge_id(i_g, ego = c("A", "C"), alter = "B"), 
             "Please provide numeric vertex id's for 'ego' and 'alter'")
expect_error(extract_edge_id(i_g, alter = c("A", "C")), 
             "Please provide numeric vertex id's for 'alter'")

rm(i_g, edges)
# undirected
data(florentine, package = "snafun")
flobus_i <- florentine$flobusiness
expect_equal(extract_edge_id(flobus_i, ego = c(2, 4, 6))[, "eid"], c(5, 6, 7, 2, 10))
expect_null(extract_edge_id(flobus_i, ego = 2))
# same dyads (b/c the graph is undirected), but differently ordered somehow
expect_equal(extract_edge_id(flobus_i, alter = c(2, 4, 6))[, "eid"], c(2, 5, 6, 10, 7))
expect_equal(extract_edge_id(flobus_i, alter = c(2, 4, 6), ego = 10:11)[, "eid"], 
             c(0, 0, 0, 0, 7, 0))



# network ----
edges <- data.frame(from = c(1, 1, 2, 2, 3, 3, 4), 
                    to = c(2, 3, 1, 4, 4, 5, 1),
                    weight = 1:7,
                    code = LETTERS[1:7])
net <- to_network(edges)
expect_equal(extract_edge_id(net, ego = c(1, 3))[, "eid"], c(1, 2, 5, 6))
expect_equal(extract_edge_id(net, ego = c(1, 3), alter = c(2))[, "eid"], c(1, 0))
expect_equal(extract_edge_id(net, alter = c(1, 3))[, "eid"], c(2, 3, 7))
expect_equal(extract_edge_id(net, ego = c(1, 3), alter = c(2, 3, 4))[, "eid"], c(1, 2, 0, 0, 0, 5))
expect_equal(extract_edge_id(net, ego = 1, alter = 2)[, "eid"], 1)
expect_equal(extract_edge_id(net, edgelist = data.frame(a = c(1, 2), b = c(2, 3)))[, "eid"], c(1, 0))


# name the vertices
network::set.vertex.attribute(net, "vertex.names", value = LETTERS[1:5])
# does it work without using the names
expect_equal(extract_edge_id(net, ego = c(1, 3))[, "eid"], c(1, 2, 5, 6))
expect_equal(extract_edge_id(net, ego = c(1, 3), alter = 2)[, "eid"], c(1, 0))
expect_equal(extract_edge_id(net, alter = c(1, 3))[, "eid"], c(2, 3, 7))
# does it work using names?
expect_error(extract_edge_id(net, ego = c("A", "C")), 
             "Please provide numeric vertex id's for 'ego'")
expect_error(extract_edge_id(net, ego = c("A", "C"), alter = "B"), 
             "Please provide numeric vertex id's for 'ego' and 'alter'")
expect_error(extract_edge_id(net, alter = c("A", "C")), 
             "Please provide numeric vertex id's for 'alter'")

rm(net, edges)
# undirected
data(florentine, package = "snafun")
flobus_i <- florentine$flobusiness
flobus_n <- to_network(flobus_i)
expect_equal(extract_edge_id(flobus_n, ego = c(2, 4, 6))[, "eid"], c(5, 6, 7, 2, 10))
expect_null(extract_edge_id(flobus_n, ego = 2))
# same dyads (b/c the graph is undirected), but differently ordered somehow
expect_equal(extract_edge_id(flobus_n, alter = c(2, 4, 6))[, "eid"], c(2, 5, 6, 10, 7))
expect_equal(extract_edge_id(flobus_n, alter = c(2, 4, 6), ego = 10:11)[, "eid"], 
             c(0, 0, 0, 0, 7, 0))


# otherwise ----
expect_error(extract_edge_id(florentine, ego = 1, alter = 2), 
             "'x' should be of class `igraph`, `network`")


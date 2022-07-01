

data(florentine, package = "snafun")
data(judge_net, package = "snafun")

remove_na_attr <- function(x) {
  x[!(x == "na")]
}

rename_vertex.names <- function(x) {
  x[x == "vertex.names"] <- "name"
  x
}

rename_name <- function(x) {
  x[x == "name"] <- "vertex.names"
  x
}


### from igraph to matrix----

## undirected
flobus_i <- florentine$flobusiness
flomar_i <- florentine$flomarriage
flobus_m <- to_matrix(flobus_i)
flomar_m <- to_matrix(flomar_i)
judge_i <- judge_net
judge_m <- to_matrix(judge_i)

expect_equal(nrow(flobus_m), igraph::vcount(flobus_i))
expect_equal(ncol(flobus_m), igraph::vcount(flobus_i))
expect_equal(nrow(flomar_m), igraph::vcount(flomar_i))
expect_equal(ncol(flomar_m), igraph::vcount(flomar_i))
expect_equal(nrow(judge_m), igraph::vcount(judge_i))
expect_equal(ncol(judge_m), igraph::vcount(judge_i))

expect_equal(sum(flobus_m), igraph::ecount(flobus_i) * 2)
expect_equal(sum(flomar_m), igraph::ecount(flomar_i) * 2)
expect_equal(sum(judge_m), igraph::ecount(judge_i) * 2)

expect_equal(sum(diag(flobus_m)), 0)
expect_equal(sum(diag(flomar_m)), 0)
expect_equal(sum(diag(judge_m)), 0)

## unweighted
expect_false(any(flobus_m > 1))
expect_false(any(flomar_m > 1))
expect_false(any(judge_m > 1))

expect_equal(colnames(flobus_m), igraph::V(flobus_i)$name)
expect_equal(colnames(flomar_m), igraph::V(flomar_i)$name)
expect_equal(colnames(judge_m), igraph::V(judge_i)$name)


### directed
net <- igraph::sample_gnm(10, 20, directed = TRUE)
mat <- to_matrix(net)
expect_equal(nrow(mat), igraph::vcount(net))
expect_equal(ncol(mat), igraph::vcount(net))
expect_equal(sum(mat), igraph::ecount(net))

### weighted
rm(mat)
val <- runif(igraph::ecount(net))
net <- igraph::set_edge_attr(net, name = "weight", value = val)
mat <- to_matrix(net)
expect_equal(nrow(mat), igraph::vcount(net))
expect_equal(ncol(mat), igraph::vcount(net))
expect_equal(sum(mat), sum(val))


### bipartite
b_i <- igraph::bipartite.random.game(n1 = 20, n2 = 30, type = "gnp", p = .15, directed = FALSE, mode = "out")
b_m <- to_matrix(b_i)
expect_equal(nrow(b_m), 20)
expect_equal(ncol(b_m), 30)
expect_equal(sum(b_m), igraph::ecount(b_i))

rm(mat, net, b_i, b_m)





### from network to matrix----

## undirected
flobus_i <- florentine$flobusiness
flomar_i <- florentine$flomarriage
flobus_n <- to_network(flobus_i)
flomar_n <- to_network(flomar_i)
judge_i <- judge_net
judge_n <- suppressWarnings(to_network(judge_i))

expect_equal(nrow(flobus_m), network::network.size(flobus_n))
expect_equal(ncol(flobus_m), network::network.size(flobus_n))
expect_equal(nrow(flomar_m), network::network.size(flomar_n))
expect_equal(ncol(flomar_m), network::network.size(flomar_n))
expect_equal(nrow(judge_m), network::network.size(judge_n))
expect_equal(ncol(judge_m), network::network.size(judge_n))

expect_equal(sum(flobus_m), network::network.edgecount(flobus_n)*2)
expect_equal(sum(flomar_m), network::network.edgecount(flomar_n)*2)
expect_equal(sum(judge_m), network::network.edgecount(judge_n)*2)

expect_equal(sum(diag(flobus_m)), 0)
expect_equal(sum(diag(flomar_m)), 0)
expect_equal(sum(diag(judge_m)), 0)

## unweighted
expect_false(any(flobus_m > 1))
expect_false(any(flomar_m > 1))
expect_false(any(judge_m > 1))

expect_equal(colnames(flobus_m), igraph::V(flobus_i)$name)
expect_equal(colnames(flomar_m), igraph::V(flomar_i)$name)
expect_equal(colnames(judge_m), igraph::V(judge_i)$name)


### directed
net <- sna::rgnm(1, 10, 20, mode = "graph") |> 
  network::as.network.matrix()
mat <- to_matrix(net)
expect_equal(nrow(mat), network::network.size(net))
expect_equal(ncol(mat), network::network.size(net))
expect_equal(sum(mat), network::network.edgecount(net))

### weighted
rm(mat)
val <- runif(network::network.edgecount(net))
network::set.edge.attribute(net, attrname = "weight", value = val) 
mat <- to_matrix(net)
expect_equal(nrow(mat), network::network.size(net))
expect_equal(ncol(mat), network::network.size(net))
expect_equal(sum(mat), sum(val))


### bipartite, weighted
g <- igraph::make_bipartite_graph(c(rep(0, 3), rep(1, 7)), c(1,4,2,6,2,7,1,8,3,9,1,4,2,6,3,5))
mat <- igraph::as_incidence_matrix(g, sparse = FALSE)
b_n <- to_network(mat, bipartite = TRUE)
b_m <- to_matrix(b_n)
expect_identical(mat, b_m)


b_n <- create_bipartite(5, 10, strategy = "gnm", m = 15, graph = "network")
b_m <- to_matrix(b_n)
expect_equal(nrow(b_m), 5)
expect_equal(ncol(b_m), 10)
expect_equal(sum(b_m), 15)


# matrix to matrix ----
b_m2 <- to_matrix(b_m)
expect_identical(b_m, b_m2)


rm(b_m, b_m2, mat, net, g, b_n)
# edgelist to matrix ----
edges <- matrix(
  c(1, 2, 
    1, 3,
    2, 6, 
    3, 2,
    5, 6,
    8, 9,
    2, 9,
    1, 5), ncol = 2, byrow = TRUE
) |> 
  as.data.frame()

# adjacency matrix
mat <- to_matrix(edges)
expect_equal(nrow(mat), ncol(mat))
expect_equal(nrow(mat), 7)
expect_equal(sum(mat), nrow(edges))

# repeat some edges
edges <- rbind(edges, edges[c(1,1,1,2,3), ])
mat <- to_matrix(edges)
expect_equal(nrow(mat), ncol(mat))
expect_equal(nrow(mat), 7)
expect_equal(sum(mat), nrow(edges))

# named vertices
edges <- matrix(
  c("one", "two", 
    "one", "three",
    "two", "six", 
    "three", "two",
    "five", "six",
    "eight", "nine",
    "two", "nine",
    "one", "five"), ncol = 2, byrow = TRUE
) |> 
  as.data.frame()

# adjacency matrix
mat <- to_matrix(edges)
expect_equal(nrow(mat), ncol(mat))
expect_equal(nrow(mat), 7)
expect_equal(sum(mat), nrow(edges))
expect_true(all(colnames(mat) %in% unique(c(edges[, 1], edges[, 2]))))

# repeat some edges
edges <- rbind(edges, edges[c(1,1,1,2,3), ])
mat <- to_matrix(edges)
expect_equal(nrow(mat), ncol(mat))
expect_equal(nrow(mat), 7)
expect_equal(sum(mat), nrow(edges))




rm(mat, edges)

# bipartite edgelist
edges <- matrix(
  c(1, 12, 
    1, 13,
    2, 16, 
    3, 12,
    5, 16,
    8, 19,
    2, 19,
    1, 15,
    8, 10,
    8, 11,
    8, 12), ncol = 2, byrow = TRUE
) |> 
  as.data.frame()
mat <- to_matrix(edges)
expect_equal(nrow(mat), length(unique(edges[, 1])))
expect_equal(ncol(mat), length(unique(edges[, 2])))
expect_equal(sum(mat), nrow(edges))
expect_false(nrow(mat) == ncol(mat)) # bipartite, not square


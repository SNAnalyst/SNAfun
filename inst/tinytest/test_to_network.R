

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


### from igraph ----

flobus_i <- florentine$flobusiness
flomar_i <- florentine$flomarriage
flobus_m <- to_matrix(flobus_i)
flomar_m <- to_matrix(flomar_i)
flobus_n <- to_network(flobus_i)
flomar_n <- to_network(flomar_i)
judge_i <- judge_net
judge_m <- to_matrix(judge_i)
judge_n <- suppressWarnings(to_network(judge_i))


expect_equal(network::network.size(flobus_n), igraph::vcount(flobus_i))
expect_equal(network::network.size(flomar_n), igraph::vcount(flomar_i))
expect_equal(network::network.size(judge_n), igraph::vcount(judge_i))
expect_equal(network::network.edgecount(flobus_n), igraph::ecount(flobus_i)*2)
expect_equal(network::network.size(flomar_n), igraph::vcount(flomar_i))
expect_equal(network::network.size(judge_n), igraph::vcount(judge_i))
# check if all vertex attrs are carried over
expect_true(all(
  network::list.vertex.attributes(flobus_n) |> remove_na_attr()|> rename_vertex.names() %in%
    igraph::list.vertex.attributes(flobus_i)
))
expect_true(all(
  network::list.vertex.attributes(flomar_n) |> remove_na_attr()|> rename_vertex.names() %in%
    igraph::list.vertex.attributes(flomar_i)
))
expect_true(all(
  network::list.vertex.attributes(judge_n) |> remove_na_attr()|> rename_vertex.names() %in%
    igraph::list.vertex.attributes(judge_i)
))

expect_equal(
  network::get.vertex.attribute(flobus_n, "Wealth"),
  igraph::get.vertex.attribute(flobus_i, "Wealth")
)
expect_equal(
  network::get.vertex.attribute(flobus_n, "NumberPriorates"),
  igraph::get.vertex.attribute(flobus_i, "NumberPriorates")
)
expect_equal(
  network::get.vertex.attribute(flobus_n, "vertex.names"),
  igraph::get.vertex.attribute(flobus_i, "name")
)
expect_equal(
  network::get.vertex.attribute(flomar_n, "Wealth"),
  igraph::get.vertex.attribute(flomar_i, "Wealth")
)
expect_equal(
  network::get.vertex.attribute(flomar_n, "NumberPriorates"),
  igraph::get.vertex.attribute(flomar_i, "NumberPriorates")
)

expect_equal(
  network::get.vertex.attribute(flomar_n, "vertex.names"),
  igraph::get.vertex.attribute(flomar_i, "name")
)

### bipartite
b_i <- igraph::bipartite.random.game(n1 = 20, n2 = 30, type = "gnp", p = .15, directed = FALSE, mode = "out")
b_n <- to_network(b_i)
expect_true(is_bipartite(b_n))
expect_true(b_n$gal$bipartite == 20)
expect_equal(igraph::edge_density(b_i), sna::gden(b_n))
expect_equal(network::network.size(b_n), igraph::vcount(b_i))
expect_equal(network::network.edgecount(b_n), igraph::ecount(b_i))





print("edge attributes are not copied over from igraph to network!!!")
##############################
##############################edge attributes aren't copied over yet!!!

expect_equal(
  network::get.vertex.attribute(judge_n, "color"),
  igraph::get.vertex.attribute(judge_i, "color")
)
expect_equal(
  network::get.vertex.attribute(judge_n, "JudgeSex"),
  igraph::get.vertex.attribute(judge_i, "JudgeSex")
)
expect_equal(
  network::get.vertex.attribute(judge_n, "vertex.names"),
  igraph::get.vertex.attribute(judge_i, "name")
)



i1 <- igraph::random.graph.game(100, p.or.m = .15, type = "gnp", directed = TRUE)
n1 <- to_network(i1)
expect_inherits(n1, "network")
expect_equal(network::network.size(n1), igraph::vcount(i1))
expect_equal(network::network.edgecount(n1), igraph::ecount(i1))
i2 <- igraph::random.graph.game(100, p.or.m = .15, type = "gnp", directed = FALSE)
n2 <- to_network(i2)
expect_inherits(n2, "network")
expect_equal(network::network.size(n2), igraph::vcount(i2))
expect_equal(network::network.edgecount(n2), igraph::ecount(i2) * 2) # undirected


### from network ----
i1 <- sna::rgraph(100, tprob = .15, mode = "digraph") |> network::as.network.matrix()
expect_identical(i1, to_network(i1))
i1 <- sna::rgraph(100, tprob = .15, mode = "graph") |> network::as.network.matrix(directed = FALSE)
expect_identical(i1, to_network(i1))


### from matrix ----
data(florentine, package = "snafun") 
flobus_i <- florentine$flobusiness
flomar_i <- florentine$flomarriage
flobus_m <- to_matrix(flobus_i)
flomar_m <- to_matrix(flomar_i)
data("judge_net", package = "snafun")
j_i <- judge_net
j_m <- to_matrix(j_i)

expect_equal(sum(flobus_m), igraph::ecount(flobus_i) * 2)
expect_equal(sum(flomar_m), igraph::ecount(flomar_i) * 2)
expect_equal(rownames(flobus_m), igraph::V(flobus_i)$name)
expect_equal(rownames(flomar_m), igraph::V(flomar_i)$name)
expect_equal(rownames(j_m), igraph::V(j_i)$name)

m1 <- sna::rgraph(100, tprob = .15, mode = "digraph")
n1 <- network::as.network.matrix(m1, directed = TRUE)
expect_equal(sum(m1), network::network.edgecount(n1))

## bipartite
b_i <- igraph::bipartite.random.game(n1 = 20, n2 = 30, type = "gnp", p = .15, directed = FALSE, mode = "out")
b_n <- to_network(b_i)
b_m <- network::as.matrix.network.adjacency(b_n)
expect_equal(sum(b_m), network::network.edgecount(b_n))
expect_true(b_n$gal$bipartite == nrow(b_m))
expect_true(is_bipartite(b_n))




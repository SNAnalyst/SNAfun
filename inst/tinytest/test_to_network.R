

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


flobus_i <- florentine$flobusiness
flomar_i <- florentine$flomarriage
# add two edge attributes
flomar_i <- igraph::set_edge_attr(flomar_i, "att1", value = round(runif(igraph::ecount(flomar_i)), digits = 2))
flomar_i <- igraph::set_edge_attr(flomar_i, "att2", value = 10*round(runif(igraph::ecount(flomar_i)), digits = 2))

flobus_m <- to_matrix(flobus_i)
flomar_m <- to_matrix(flomar_i)
judge_i <- judge_net
judge_m <- to_matrix(judge_i)
flobus_n <- to_network(flobus_i)
flomar_n <- to_network(flomar_i)
judge_n <- suppressWarnings(to_network(judge_i))

# random networks, no name attribute
undirected_g_i <- igraph::random.graph.game(n = 10, p.or.m = 12, type = "gnm", directed = FALSE)
undirected_g2_i <- add_edge_attributes(undirected_g_i, attr_name = "weight", 1:count_edges(undirected_g_i))
undirected_g3_i <- add_edge_attributes(undirected_g2_i, attr_name = "blabla", count_edges(undirected_g2_i):1)
directed_g_i <- igraph::random.graph.game(n = 10, p.or.m = 12, type = "gnm", directed = TRUE)
directed_g2_i <- add_edge_attributes(directed_g_i, attr_name = "weight", 1:count_edges(directed_g_i))
directed_g3_i <- add_edge_attributes(directed_g2_i, attr_name = "blabla", count_edges(directed_g2_i):1)
# add name
undirected_g_i_name <- igraph::set_vertex_attr(undirected_g_i, "name", value = LETTERS[1:igraph::vcount(undirected_g_i)])
undirected_g2_i_name <- igraph::set_vertex_attr(undirected_g2_i, "name", value = LETTERS[1:igraph::vcount(undirected_g2_i)])
undirected_g3_i_name <- igraph::set_vertex_attr(undirected_g3_i, "name", value = LETTERS[1:igraph::vcount(undirected_g3_i)])
directed_g_i_name <- igraph::set_vertex_attr(directed_g_i, "name", value = LETTERS[1:igraph::vcount(directed_g_i)])
directed_g2_i_name <- igraph::set_vertex_attr(directed_g2_i, "name", value = LETTERS[1:igraph::vcount(directed_g2_i)])
directed_g3_i_name <- igraph::set_vertex_attr(directed_g3_i, "name", value = LETTERS[1:igraph::vcount(directed_g3_i)])






# from igraph to network ----
expect_equal(network::network.size(flobus_n), igraph::vcount(flobus_i))
expect_equal(network::network.size(flomar_n), igraph::vcount(flomar_i))
expect_equal(network::network.size(judge_n), igraph::vcount(judge_i))
expect_equal(network::network.edgecount(flobus_n), igraph::ecount(flobus_i))
expect_equal(network::network.edgecount(flomar_n), igraph::ecount(flomar_i))
expect_equal(network::network.edgecount(judge_n), igraph::ecount(judge_i))

# check if all vertex attrs are carried over
expect_true(all(
  network::list.vertex.attributes(flobus_n) |> remove_na_attr()|> rename_vertex.names() %in%
    igraph::vertex_attr_names(flobus_i)
))
expect_true(all(
  network::list.vertex.attributes(flomar_n) |> remove_na_attr()|> rename_vertex.names() %in%
    igraph::vertex_attr_names(flomar_i)
))
expect_true(all(
  network::list.vertex.attributes(judge_n) |> remove_na_attr()|> rename_vertex.names() %in%
    igraph::vertex_attr_names(judge_i)
))

expect_equal(
  network::get.vertex.attribute(flobus_n, "Wealth"),
  igraph::vertex_attr(flobus_i, "Wealth")
)
expect_equal(
  network::get.vertex.attribute(flobus_n, "NumberPriorates"),
  igraph::vertex_attr(flobus_i, "NumberPriorates")
)
expect_equal(
  network::get.vertex.attribute(flobus_n, "vertex.names"),
  igraph::vertex_attr(flobus_i, "name")
)
expect_equal(
  network::get.vertex.attribute(flomar_n, "Wealth"),
  igraph::vertex_attr(flomar_i, "Wealth")
)
expect_equal(
  network::get.vertex.attribute(flomar_n, "NumberPriorates"),
  igraph::vertex_attr(flomar_i, "NumberPriorates")
)

expect_equal(
  network::get.vertex.attribute(flomar_n, "vertex.names"),
  igraph::vertex_attr(flomar_i, "name")
)

# vertex attributes, but not a name attribute
g <- igraph::delete_vertex_attr(flomar_i, "name")
g_n <- to_network(g)
expect_equal(
  network::get.vertex.attribute(g_n, "Wealth"),
  igraph::vertex_attr(g, "Wealth")
)
expect_equal(
  network::get.vertex.attribute(g_n, "NumberPriorates"),
  igraph::vertex_attr(g, "NumberPriorates")
)



### graph with edge attributes
edges <- data.frame(from = c(1, 1, 2, 2, 3, 3, 4), 
                    to = c(2, 3, 1, 4, 4, 5, 1),
                    weight = 1:7,
                    code = LETTERS[1:7])
i_g <- igraph::graph_from_data_frame(edges)
i_n <- to_network(i_g)
expect_equal(network::network.size(i_n), igraph::vcount(i_g))
expect_equal(network::network.edgecount(i_n), igraph::ecount(i_g))
# to properly check the edge values, we need to make sure we evaluate them 
# in the same order for both i_n and i_g
eid_i <- extract_edge_id(i_g, edgelist = edges[, 1:2])
eid_n <- extract_edge_id(i_n, edgelist = edges[, 1:2])
expect_equal(eid_n[, 1:2], eid_i[, 1:2])
reorder <- match(eid_n$eid, eid_i$eid)
expect_equal(network::get.edge.attribute(i_n, "weight")[reorder], 
             igraph::edge_attr(i_g, "weight"))
expect_equal(network::get.edge.attribute(i_n, "code")[reorder], 
             igraph::edge_attr(i_g, "code"))

# flomar has 'weight' edge attribute
# first remove names to make sure the edgelist uses vertex id's instead of names
flomar_i <- igraph::delete_vertex_attr(flomar_i, "name")
edges <- to_edgelist(flomar_i)
eid_i <- extract_edge_id(flomar_i, edgelist = edges[, 1:2])
eid_n <- extract_edge_id(flomar_n, edgelist = edges[, 1:2])
expect_equal(eid_n[, 1:2], eid_i[, 1:2])
reorder <- match(eid_n$eid, eid_i$eid)
expect_equal(network::get.edge.attribute(flomar_n, "weight")[reorder], 
             igraph::edge_attr(flomar_i, "weight"))
expect_equal(network::get.edge.attribute(flomar_n, "att1")[reorder], 
             igraph::edge_attr(flomar_i, "att1"))
expect_equal(network::get.edge.attribute(flomar_n, "att2")[reorder], 
             igraph::edge_attr(flomar_i, "att2"))


# random graphs
undirected_g_n <- to_network(undirected_g_i)
undirected_g2_n <- to_network(undirected_g2_i)
undirected_g3_n <- to_network(undirected_g3_i)
directed_g_n <- to_network(directed_g_i)
directed_g2_n <- to_network(directed_g2_i)
directed_g3_n <- to_network(directed_g3_i)
undirected_g_n_name <- to_network(undirected_g_i_name)
undirected_g2_n_name <- to_network(undirected_g2_i_name)
undirected_g3_n_name <- to_network(undirected_g3_i_name)
directed_g_n_name <- to_network(directed_g_i_name)
directed_g2_n_name <- to_network(directed_g2_i_name)
directed_g3_n_name <- to_network(directed_g3_i_name)

expect_false(is_directed(undirected_g_n))
expect_false(is_directed(undirected_g2_n))
expect_false(is_directed(undirected_g3_n))
expect_false(is_directed(undirected_g_n_name))
expect_false(is_directed(undirected_g2_n_name))
expect_false(is_directed(undirected_g3_n_name))
expect_true(is_directed(directed_g_n))
expect_true(is_directed(directed_g2_n))
expect_true(is_directed(directed_g3_n))
expect_true(is_directed(directed_g_n_name))
expect_true(is_directed(directed_g2_n_name))
expect_true(is_directed(directed_g3_n_name))
expect_equal(list_edge_attributes(undirected_g_n), "na")   # network objects always have this "na" vertex attribute
expect_equal(list_edge_attributes(directed_g_n), "na") 
expect_equal(list_edge_attributes(undirected_g2_n), c("na", "weight"))
expect_equal(list_edge_attributes(directed_g2_n), c("na", "weight"))
expect_equal(list_edge_attributes(undirected_g3_n), c("blabla", "na", "weight"))  # probably in alphabetical order
expect_equal(list_edge_attributes(directed_g3_n), c("blabla", "na", "weight"))
expect_equal(list_edge_attributes(undirected_g_n_name), "na")   # network objects always have this "na" vertex attribute
expect_equal(list_edge_attributes(directed_g_n_name), "na") 
expect_equal(list_edge_attributes(undirected_g2_n_name), c("na", "weight"))
expect_equal(list_edge_attributes(directed_g2_n_name), c("na", "weight"))
expect_equal(list_edge_attributes(undirected_g3_n_name), c("blabla", "na", "weight"))  # probably in alphabetical order
expect_equal(list_edge_attributes(directed_g3_n_name), c("blabla", "na", "weight"))

expect_equal(list_vertex_attributes(undirected_g_i), character(0))
expect_equal(list_vertex_attributes(undirected_g_n), c("na", "vertex.names"))
expect_equal(list_vertex_attributes(undirected_g2_i), character(0))
expect_equal(list_vertex_attributes(undirected_g2_n), c("na", "vertex.names"))
expect_equal(list_vertex_attributes(undirected_g3_i), character(0))
expect_equal(list_vertex_attributes(undirected_g3_n), c("na", "vertex.names"))
expect_equal(list_vertex_attributes(undirected_g_i_name), "name")
expect_equal(list_vertex_attributes(undirected_g_n_name), c("na", "vertex.names"))
expect_equal(list_vertex_attributes(undirected_g2_i_name), "name")
expect_equal(list_vertex_attributes(undirected_g2_n_name), c("na", "vertex.names"))
expect_equal(list_vertex_attributes(undirected_g3_i_name), "name")
expect_equal(list_vertex_attributes(undirected_g3_n_name), c("na", "vertex.names"))

expect_equal(list_vertex_attributes(directed_g_i), character(0))
expect_equal(list_vertex_attributes(directed_g_n), c("na", "vertex.names"))
expect_equal(list_vertex_attributes(directed_g2_i), character(0))
expect_equal(list_vertex_attributes(directed_g2_n), c("na", "vertex.names"))
expect_equal(list_vertex_attributes(directed_g3_i), character(0))
expect_equal(list_vertex_attributes(directed_g3_n), c("na", "vertex.names"))
expect_equal(list_vertex_attributes(directed_g_i_name), "name")
expect_equal(list_vertex_attributes(directed_g_n_name), c("na", "vertex.names"))
expect_equal(list_vertex_attributes(directed_g2_i_name), "name")
expect_equal(list_vertex_attributes(directed_g2_n_name), c("na", "vertex.names"))
expect_equal(list_vertex_attributes(directed_g3_i_name), "name")
expect_equal(list_vertex_attributes(directed_g3_n_name), c("na", "vertex.names"))


### bipartite
b_i <- igraph::sample_bipartite(n1 = 20, n2 = 30, type = "gnp", p = .15, directed = FALSE, mode = "out")
b_n <- to_network(b_i)
expect_true(is_bipartite(b_n))
expect_true(b_n$gal$bipartite == 20)
expect_equal(igraph::edge_density(b_i), sna::gden(b_n))
expect_equal(network::network.size(b_n), igraph::vcount(b_i))
expect_equal(network::network.edgecount(b_n), igraph::ecount(b_i))

# add edge values
val <- round(runif(igraph::ecount(b_i)), digits = 2)
b_i2 <- add_edge_attributes(b_i, "new_attr", value = val)
edges <- to_edgelist(b_i2)
b_n2 <- to_network(b_i2)
eid_i <- extract_edge_id(b_i2, edgelist = edges[, 1:2])
eid_n <- extract_edge_id(b_n2, edgelist = edges[, 1:2])
expect_equal(eid_n[, 1:2], eid_i[, 1:2])
reorder <- match(eid_n$eid, eid_i$eid)
expect_equal(network::get.edge.attribute(b_n2, "new_attr")[reorder], 
             igraph::edge_attr(b_i2, "new_attr"))



# continue with vertex attributes
expect_equal(
  network::get.vertex.attribute(judge_n, "color"),
  igraph::vertex_attr(judge_i, "color")
)
expect_equal(
  network::get.vertex.attribute(judge_n, "JudgeSex"),
  igraph::vertex_attr(judge_i, "JudgeSex")
)
expect_equal(
  network::get.vertex.attribute(judge_n, "vertex.names"),
  igraph::vertex_attr(judge_i, "name")
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
expect_equal(network::network.edgecount(n2), igraph::ecount(i2)) # undirected

rm(b_i, b_i2, b_n, b_n2, edges, eid_i, eid_n, i_g, i_n, i1, i2, n1, n2, reorder, val)







# from network ----
i1 <- sna::rgraph(100, tprob = .15, mode = "digraph") |> network::as.network.matrix()
expect_identical(i1, to_network(i1))
i1 <- sna::rgraph(100, tprob = .15, mode = "graph") |> network::as.network.matrix(directed = FALSE)
expect_identical(i1, to_network(i1))

rm(i1)



# from matrix ----
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
b_i <- igraph::sample_bipartite(n1 = 20, n2 = 30, type = "gnp", p = .15, directed = FALSE, mode = "out")
b_n <- to_network(b_i)
b_m <- network::as.matrix.network.adjacency(b_n)
expect_equal(sum(b_m), network::network.edgecount(b_n))
expect_true(b_n$gal$bipartite == nrow(b_m))
expect_true(is_bipartite(b_n))

rm(b_i, b_m, b_n, j_i, j_m, m1, n1)


# from edgelist ----
i_g <- igraph::erdos.renyi.game(n = 15, p.or.m = .2, type = "gnp", directed = TRUE)
i_g <- add_edge_attributes(i_g, "att1", value = round(runif(igraph::ecount(i_g)), digits = 2))
i_g <- add_edge_attributes(i_g, "att2", value = 100*round(runif(igraph::ecount(i_g)), digits = 2))
igraph::V(i_g)$name <- LETTERS[1:15]
edges <- to_edgelist(i_g)
i_n <- to_network(edges)
expect_true("att1" %in% network::list.edge.attributes(i_n))
expect_true("att2" %in% network::list.edge.attributes(i_n))
expect_equal(nrow(edges), network::network.edgecount(i_n))
# expect_equal(15, network::network.size(i_n))   # check!!!

no_10 <- edges[10, ]
from <- which(sapply(i_n$val, function(z) z$vertex.names == no_10$from))
to <- which(sapply(i_n$val, function(z) z$vertex.names == no_10$to))
eid <- lapply(i_n$mel, function(z) {
    z$outl == from & z$inl == to
    }) |> unlist() |> which()
expect_equal(network::get.edge.value(i_n, attrname = "att1")[eid], edges[10, "att1"])
expect_equal(network::get.edge.value(i_n, attrname = "att2")[eid], edges[10, "att2"])

no_20 <- edges[20, ]
from <- which(sapply(i_n$val, function(z) z$vertex.names == no_20$from))
to <- which(sapply(i_n$val, function(z) z$vertex.names == no_20$to))
eid <- lapply(i_n$mel, function(z) {
  z$outl == from & z$inl == to
}) |> unlist() |> which()
expect_equal(network::get.edge.value(i_n, attrname = "att1")[eid], edges[20, "att1"])
expect_equal(network::get.edge.value(i_n, attrname = "att2")[eid], edges[20, "att2"])

rm(eid, i_g, edges, i_n)
# bipartite
b_i <- create_bipartite(n_type1 = 10, n_type2 = 15, strategy = "gnp", p = .2)
edges <- to_edgelist(b_i)
b_n <- to_network(edges, bipartite = TRUE)
expect_true(is_bipartite(b_n))
expect_equal(network::network.size(b_n), length(unique(unlist(edges))))
expect_equal(network::network.edgecount(b_n), igraph::ecount(b_i))


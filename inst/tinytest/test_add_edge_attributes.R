

#### network ----
## undirected
mat <- sna::rgnm(n = 1, nv = 20, m = 30, mode = "graph")
net <- network::network(mat, directed = FALSE)
edges <- to_edgelist(mat)
edges$one <- 1:nrow(edges)
edges$two <- 100 - edges$one

net_edges <- add_edge_attributes(net, edgelist = edges)
expect_equal(network::get.edge.attribute(net_edges, "one"), edges$one)
expect_equal(network::get.edge.attribute(net_edges, "two"), edges$two)
expect_true(all(c("one", "two") %in% network::list.edge.attributes(net_edges)))


net_edges <- add_edge_attributes(net, attr_name = "one", edgelist = edges)
expect_equal(network::get.edge.attribute(net_edges, "one"), edges$one)
expect_null(network::get.edge.attribute(net_edges, "two"))
expect_true(sum(c("one", "two") %in% network::list.edge.attributes(net_edges)) == 1)

net_edges <- add_edge_attributes(net, attr_name = "two", edgelist = edges)
expect_null(network::get.edge.attribute(net_edges, "one"))
expect_equal(network::get.edge.attribute(net_edges, "two"), edges$two)
expect_true(sum(c("one", "two") %in% network::list.edge.attributes(net_edges)) == 1)

expect_error(add_edge_attributes(net, attr_name = "three", edgelist = edges))
expect_error(add_edge_attributes(net, attr_name = c("one", "three"), edgelist = edges))
expect_error(add_edge_attributes(net, attr_name = "one", value = 3, edgelist = edges),
             "You can only use either 'value' or 'edgelist', but not both."
)
expect_error(add_edge_attributes(net, attr_name = "one"),
             "Please specify a 'value' or 'edgelist'"
)

## reorder the edges and this should still give the correct result
edges_reo <- sample(1:nrow(edges), size = nrow(edges), replace = FALSE)
edges_reo <- edges[edges_reo, ]
net_edges_reo <- add_edge_attributes(net, edgelist = edges_reo)
# check that they are still included in the original, correct order
expect_equal(network::get.edge.attribute(net_edges_reo, "one"), edges$one)
expect_equal(network::get.edge.attribute(net_edges_reo, "two"), edges$two)
expect_true(all(c("one", "two") %in% network::list.edge.attributes(net_edges_reo)))


## introduce missings
edges_na <- edges
edges_na[10, "one"] <- NA  # the 10th dyad gets value NA
net_edges_na <- add_edge_attributes(net, edgelist = edges_na)
expect_equal(network::get.edge.attribute(net_edges_na, "one"), edges_na$one)
expect_equal(network::get.edge.attribute(net_edges_na, "two"), edges_na$two)
expect_true(all(c("one", "two") %in% network::list.edge.attributes(net_edges_na)))
expect_true(is.na(network::get.edge.attribute(net_edges_na, "one")[10]))

## remove edges
edges_na <- edges[-10, ] #the 10th dyad is now completely missing
expect_error(add_edge_attributes(net, edgelist = edges_na),
             "^Some edges from 'object' are missing in your 'edgelist'"
)
# remove one more dyad
edges_na <- edges_na[-10, ]
expect_error(add_edge_attributes(net, edgelist = edges_na),
             "^Some edges from 'object' are missing in your 'edgelist'"
)
# check that it still works if the missing dyad is added to the bottom
edges_na <- rbind(edges_na, edges[10:11, ])
net_edges <- add_edge_attributes(net, attr_name = "one", edgelist = edges_na)
expect_equal(network::get.edge.attribute(net_edges, "one"), edges$one)




# add a nonexisting edge
edges_xtra <- rbind(edges, edges[10,])

expect_error(add_edge_attributes(net, edgelist = edges_xtra),
             "^There are duplicate edges you try to set values for"
)



rm(edges, edges_na, edges_reo, edges_xtra,
   mat, net, net_edges, net_edges_na, net_edges_reo)
## directed
mat <- sna::rgnm(n = 1, nv = 20, m = 30, mode = "digraph")
net <- network::network(mat, directed = TRUE)
edges <- to_edgelist(mat)
edges$one <- 1:nrow(edges)
edges$two <- 100 - edges$one

net_edges <- add_edge_attributes(net, edgelist = edges)
# to check the edge values, it is important to set the edges in the same 
# order as in the network itself
volgorde <- unlist(network::get.dyads.eids(net, tails = edges[, 1], heads = edges[, 2]))
edges_ord <- edges[order(volgorde), ]
expect_equal(network::get.edge.attribute(net_edges, "one"), edges_ord$one)
expect_equal(network::get.edge.attribute(net_edges, "two"), edges_ord$two)
expect_true(all(c("one", "two") %in% network::list.edge.attributes(net_edges)))


net_edges <- add_edge_attributes(net, attr_name = "one", edgelist = edges_ord)
expect_equal(network::get.edge.attribute(net_edges, "one"), edges_ord$one)
expect_null(network::get.edge.attribute(net_edges, "two"))
expect_true(sum(c("one", "two") %in% network::list.edge.attributes(net_edges)) == 1)

net_edges <- add_edge_attributes(net, attr_name = "two", edgelist = edges_ord)
expect_null(network::get.edge.attribute(net_edges, "one"))
expect_equal(network::get.edge.attribute(net_edges, "two"), edges_ord$two)
expect_true(sum(c("one", "two") %in% network::list.edge.attributes(net_edges)) == 1)

expect_error(add_edge_attributes(net, attr_name = "three", edgelist = edges))
expect_error(add_edge_attributes(net, attr_name = c("one", "three"), edgelist = edges_ord))
expect_error(add_edge_attributes(net, attr_name = "one", value = 3, edgelist = edges_ord),
             "You can only use either 'value' or 'edgelist', but not both."
)
expect_error(add_edge_attributes(net, attr_name = "one"),
             "Please specify a 'value' or 'edgelist'"
)


## attr_name + value
val <- runif(n = network::network.edgecount(net))
net_edges <- add_edge_attributes(net, value = val, attr_name = "hiephoi")
expect_equal(network::get.edge.attribute(net_edges, "hiephoi"), val)
expect_true("hiephoi" %in% network::list.edge.attributes(net_edges))
expect_error(add_edge_attributes(net, value = val, attr_name = c("hiephoi", "boe")),
             "'attr_name' and 'value' do not have matching dimensions"
)

val <- matrix(runif(n = 2 * network::network.edgecount(net)), ncol = 2)
net_edges <- add_edge_attributes(net, value = val, attr_name = c("one", "two"))
expect_equal(network::get.edge.attribute(net_edges, "one"), val[, 1])
expect_equal(network::get.edge.attribute(net_edges, "two"), val[, 2])
expect_true(all(c("one", "two") %in% network::list.edge.attributes(net_edges)))

expect_error(add_edge_attributes(net, value = val, attr_name = c("one")),
             "'attr_name' and 'value' do not have matching dimensions"
)


#### igraph ----
## undirected
rm(edges, edges_ord, mat, net, net_edges, val, volgorde)
net <- igraph::sample_gnm(n = 20, m = 30, directed = FALSE)
edges <- igraph::as_edgelist(net) |> as.data.frame()
edges$one <- 1:nrow(edges)
edges$two <- 100 - edges$one

net_edges <- add_edge_attributes(net, edgelist = edges)
expect_equal(igraph::get.edge.attribute(net_edges, "one"), edges$one)
expect_equal(igraph::get.edge.attribute(net_edges, "two"), edges$two)
expect_true(all(c("one", "two") %in% igraph::list.edge.attributes(net_edges)))


net_edges <- add_edge_attributes(net, attr_name = "one", edgelist = edges)
expect_equal(igraph::get.edge.attribute(net_edges, "one"), edges$one)
expect_null(igraph::get.edge.attribute(net_edges, "two"))
expect_true(sum(c("one", "two") %in% igraph::list.edge.attributes(net_edges)) == 1)


net_edges <- add_edge_attributes(net, attr_name = "two", edgelist = edges)
expect_null(igraph::get.edge.attribute(net_edges, "one"))
expect_equal(igraph::get.edge.attribute(net_edges, "two"), edges$two)
expect_true(sum(c("one", "two") %in% igraph::list.edge.attributes(net_edges)) == 1)

expect_error(add_edge_attributes(net, attr_name = "three", edgelist = edges))
expect_error(add_edge_attributes(net, attr_name = c("one", "three"), edgelist = edges))
expect_error(add_edge_attributes(net, attr_name = "one", value = 3, edgelist = edges),
             "You can only use either 'value' or 'edgelist', but not both."
)
expect_error(add_edge_attributes(net, attr_name = "one"),
             "Please specify a 'value' or 'edgelist'"
)

## reorder the edges and this should still give the correct result
edges_reo <- sample(1:nrow(edges), size = nrow(edges), replace = FALSE)
edges_reo <- edges[edges_reo, ]
net_edges_reo <- add_edge_attributes(net, edgelist = edges_reo)
# check that they are still included in the original, correct order
expect_equal(igraph::get.edge.attribute(net_edges_reo, "one"), edges$one)
expect_equal(igraph::get.edge.attribute(net_edges_reo, "two"), edges$two)
expect_true(all(c("one", "two") %in% igraph::list.edge.attributes(net_edges_reo)))


## introduce missings
edges_na <- edges
edges_na[10, "one"] <- NA  # the 10th dyad gets value NA
net_edges_na <- add_edge_attributes(net, edgelist = edges_na)
expect_equal(igraph::get.edge.attribute(net_edges_na, "one"), edges_na$one)
expect_equal(igraph::get.edge.attribute(net_edges_na, "two"), edges_na$two)
expect_true(all(c("one", "two") %in% igraph::list.edge.attributes(net_edges_na)))
expect_true(is.na(igraph::get.edge.attribute(net_edges_na, "one")[10]))

## remove edges
edges_na <- edges[-10, ] #the 10th dyad is now completely missing
expect_error(add_edge_attributes(net, edgelist = edges_na),
             "^Some edges from 'object' are missing in your 'edgelist'"
)
# remove one more dyad
edges_na <- edges_na[-10, ]
expect_error(add_edge_attributes(net, edgelist = edges_na),
             "^Some edges from 'object' are missing in your 'edgelist'"
)
# check that it still works if the missing dyad is added to the bottom
edges_na <- rbind(edges_na, edges[10:11, ])
net_edges <- add_edge_attributes(net, attr_name = "one", edgelist = edges_na)
expect_equal(igraph::get.edge.attribute(net_edges, "one"), edges$one)



# add a nonexisting edge
edges_xtra <- rbind(edges, edges[10,])

expect_error(add_edge_attributes(net, edgelist = edges_xtra),
             "^There are duplicate edges you try to set values for"
)


rm(edges, edges_na, edges_reo, edges_xtra, 
   net, net_edges, net_edges_na, net_edges_reo)
## directed
mat <- sna::rgnm(n = 1, nv = 20, m = 30, mode = "digraph")
net <- network::network(mat, directed = TRUE) |> 
  to_igraph(net)
edges <- to_edgelist(mat)
edges$one <- 1:nrow(edges)
edges$two <- 100 - edges$one

net_edges <- add_edge_attributes(net, edgelist = edges)
# to check the edge values, it is important to set the edges in the same 
# order as in the network itself
vp <- t(edges[, 1:2]) |> matrix(nrow = 1, byrow = TRUE) |> as.vector()
volgorde <- igraph::get.edge.ids(net, vp = vp)

edges_ord <- edges[order(volgorde), ]
expect_equal(igraph::get.edge.attribute(net_edges, "one"), edges_ord$one)
expect_equal(igraph::get.edge.attribute(net_edges, "two"), edges_ord$two)
expect_true(all(c("one", "two") %in% igraph::list.edge.attributes(net_edges)))


net_edges <- add_edge_attributes(net, attr_name = "one", edgelist = edges_ord)
expect_equal(igraph::get.edge.attribute(net_edges, "one"), edges_ord$one)
expect_null(igraph::get.edge.attribute(net_edges, "two"))
expect_true(sum(c("one", "two") %in% igraph::list.edge.attributes(net_edges)) == 1)

net_edges <- add_edge_attributes(net, attr_name = "two", edgelist = edges_ord)
expect_null(igraph::get.edge.attribute(net_edges, "one"))
expect_equal(igraph::get.edge.attribute(net_edges, "two"), edges_ord$two)
expect_true(sum(c("one", "two") %in% igraph::list.edge.attributes(net_edges)) == 1)

expect_error(add_edge_attributes(net, attr_name = "three", edgelist = edges))
expect_error(add_edge_attributes(net, attr_name = c("one", "three"), edgelist = edges_ord))
expect_error(add_edge_attributes(net, attr_name = "one", value = 3, edgelist = edges_ord),
             "You can only use either 'value' or 'edgelist', but not both."
)
expect_error(add_edge_attributes(net, attr_name = "one"),
             "Please specify a 'value' or 'edgelist'"
)


## attr_name + value
val <- runif(n = igraph::ecount(net))
net_edges <- add_edge_attributes(net, value = val, attr_name = "hiephoi")
expect_equal(igraph::get.edge.attribute(net_edges, "hiephoi"), val)
expect_true("hiephoi" %in% igraph::list.edge.attributes(net_edges))
expect_error(add_edge_attributes(net, value = val, attr_name = c("hiephoi", "boe")),
             "'attr_name' and 'value' do not have matching dimensions"
)

val <- matrix(runif(n = 2 * igraph::ecount(net)), ncol = 2)
net_edges <- add_edge_attributes(net, value = val, attr_name = c("one", "two"))
expect_equal(igraph::get.edge.attribute(net_edges, "one"), val[, 1])
expect_equal(igraph::get.edge.attribute(net_edges, "two"), val[, 2])
expect_true(all(c("one", "two") %in% igraph::list.edge.attributes(net_edges)))

expect_error(add_edge_attributes(net, value = val, attr_name = c("one")),
             "'attr_name' and 'value' do not have matching dimensions"
)

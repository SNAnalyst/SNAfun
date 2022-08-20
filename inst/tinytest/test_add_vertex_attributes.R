


#### igraph ----
g <- igraph::graph.ring(5)

# check for error throwing
expect_error(add_vertex_attributes(g, 1:15), "You forgot to specify a 'value'") 
expect_error(add_vertex_attributes(g, value = 1:15), "not specify a 'name'")
expect_error(add_vertex_attributes(g, "naam", value = 1:14), "number of vertices")
expect_error(add_vertex_attributes(g, value = data.frame(1:15)), "number of vertices")
expect_error(add_vertex_attributes(g, "naam", value = data.frame(1:15)), 
                       "Not all names from 'name' occur in 'value'")
expect_error(add_vertex_attributes(g, "naam", value = data.frame(naam = 1:15)), 
                       "number of vertices")

# check if adding is correct
expect_equal(
  add_vertex_attributes(g, "at1", 1:5) |> igraph::get.vertex.attribute("at1"), 1:5
)
expect_equal(
  add_vertex_attributes(g, "naam", value = data.frame(naam = 1:5)) |> 
    igraph::get.vertex.attribute("naam"), 1:5
)
expect_equal(
  add_vertex_attributes(g, value = data.frame(naam = 1:5)) |> 
    igraph::get.vertex.attribute("naam"), 1:5
)


g1 <- add_vertex_attributes(g, value = data.frame(een = 11:15, twee = 21:25))
expect_equal(igraph::get.vertex.attribute(g1, "een"), 11:15)
expect_equal(igraph::get.vertex.attribute(g1, "twee"), 21:25)

g1 <- add_vertex_attributes(g, c("twee", "een"), 
                  value = data.frame(een = 11:15, twee = 21:25, drie = 31:35))
expect_equal(igraph::get.vertex.attribute(g1, "een"), 11:15)
expect_equal(igraph::get.vertex.attribute(g1, "twee"), 21:25)
expect_null(igraph::get.vertex.attribute(g1, "drie")) # does no exist

### check if reading from a matrix works
mat <- matrix(c(11:15, 21:25, 31:35), ncol = 3, byrow = FALSE)
expect_error(add_vertex_attributes(g, c("twee", "een"), value = mat), 
                       "matrix 'value' has no colnames"
)
# now add column names to the matrix
colnames(mat) <- c("een", "twee", "drie")
g1 <- add_vertex_attributes(g, c("twee", "een"), value = mat)
expect_true(all(c("een", "twee") %in% igraph::list.vertex.attributes(g1)))

# add vertex names
g1 <- add_vertex_names(g1, 1:count_vertices(g1))
expect_equal(extract_vertex_names(g1), 1:count_vertices(g1))
g1 <- add_vertex_names(g1, count_vertices(g1):1)  # use 'add' to change existing names
expect_equal(extract_vertex_names(g1), count_vertices(g1):1)
g1 <- add_vertex_names(g1, c("A", "B"), vids = c(1, 3))
expect_equal(extract_vertex_names(g1), c("A", 4, "B", 2, 1))

rm(g, g1, mat)



#### network ----
g <- sna::rgraph(10, tprob = .2) |> network::as.network()

# check for error throwing
expect_error(add_vertex_attributes(g, 1:15), "You forgot to specify a 'value'") 
expect_error(add_vertex_attributes(g, value = 1:15), "not specify a 'name'")
expect_error(add_vertex_attributes(g, "naam", value = 1:14), "number of vertices")
expect_error(add_vertex_attributes(g, value = data.frame(1:15)), "number of vertices")
expect_error(add_vertex_attributes(g, "naam", value = data.frame(1:15)), 
                       "Not all names from 'name' occur in 'value'")
expect_error(add_vertex_attributes(g, "naam", value = data.frame(naam = 1:15)), 
                       "number of vertices")

# check if adding is correct
expect_equal(
  add_vertex_attributes(g, "at1", 1:10) |> network::get.vertex.attribute("at1"), 1:10
)
expect_equal(
  add_vertex_attributes(g, "naam", value = data.frame(naam = 1:10)) |> 
    network::get.vertex.attribute("naam"), 1:10
)
expect_equal(
  add_vertex_attributes(g, value = data.frame(naam = 1:10)) |> 
    network::get.vertex.attribute("naam"), 1:10
)


g1 <- add_vertex_attributes(g, value = data.frame(een = 10:19, twee = 20:29))
expect_equal(network::get.vertex.attribute(g1, "een"), 10:19)
expect_equal(network::get.vertex.attribute(g1, "twee"), 20:29)

g1 <- add_vertex_attributes(g, c("twee", "een"), 
                  value = data.frame(een = 10:19, twee = 20:29, drie = 30:39))
expect_equal(network::get.vertex.attribute(g1, "een"), 10:19)
expect_equal(network::get.vertex.attribute(g1, "twee"), 20:29)
expect_equal(network::get.vertex.attribute(g1, "drie"), rep(NA, 10)) # all NA, does not exist


mat <- matrix(c(10:19, 20:29, 30:39), ncol = 3, byrow = FALSE)
expect_error(add_vertex_attributes(g, c("twee", "een"), value = mat), 
                       "matrix 'value' has no colnames"
)
colnames(mat) <- c("een", "twee", "drie")
g1 <- add_vertex_attributes(g, c("twee", "een"), value = mat)
network::list.vertex.attributes(g1)   #  "twee" "een" "na" "vertex.names"
expect_true(all(c("een", "twee") %in% network::list.vertex.attributes(g1)))

# add vertex names
g1 <- add_vertex_names(g1, 1:count_vertices(g1))
expect_equal(extract_vertex_names(g1), 1:count_vertices(g1))
g1 <- add_vertex_names(g1, count_vertices(g1):1)  # use 'add' to change existing names
expect_equal(extract_vertex_names(g1), count_vertices(g1):1)
g1 <- add_vertex_names(g1, c("A", "B"), vids = c(1, 3))
expect_equal(extract_vertex_names(g1), c("A", "9", "B", "7", "6", "5", "4", "3", "2", "1"))
expect_equal(extract_vertex_names(g1), c("A", 9, "B", 7, 6, 5, 4, 3, 2, 1))

rm(g, g1, mat)



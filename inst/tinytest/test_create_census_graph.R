
report_side_effects()



g <- create_census_graph(10, mut = 45, asym = 0, null = 0, method = "exact")
expect_inherits(g, "igraph")
expect_equal((snafun::count_dyads(g))$Mutual, 45)
expect_equal((snafun::count_dyads(g))$Null, 0)

# triggers an error
expect_error(create_census_graph(10, mut = 145, asym = 0, null = 0, method = "exact"))

# empty graph
g <- create_census_graph(10, mut = 0, asym = 0, null = 45, method = "exact")
expect_inherits(g, "igraph")
expect_equal((snafun::count_dyads(g))$Mutual, 0)
expect_equal((snafun::count_dyads(g))$Null, 45)

expect_message(create_census_graph(10, mut = 0, asym = 0, null = 45, method = "probability"))

g <- create_census_graph(10, mut = 0, asym = 0, null = 1, method = "probability")
expect_inherits(g, "igraph")
expect_equal((snafun::count_dyads(g))$Mutual, 0)
expect_equal((snafun::count_dyads(g))$Null, 45)


expect_inherits(create_census_graph(10, method = "probability", graph = "igraph"), "igraph")
expect_inherits(create_census_graph(10, method = "probability", graph = "network"), "network")
expect_inherits(create_census_graph(10, method = "probability", graph = "edgelist"), "data.frame")
expect_inherits(create_census_graph(10, method = "probability", graph = "matrix"), "matrix")


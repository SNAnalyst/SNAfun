


# degree_distribution ----------------------------------------------------------

## igraph ----------------------------------------------------------------------

g <- snafun::create_random_graph(1000, "gnp", p = .01)

expect_true(
  sum(g_degree_distribution(g, type = "density", mode = "out") - 
        igraph::degree_distribution(g, mode = "out")) == 0
)
expect_true(
  sum(g_degree_distribution(g, type = "density", mode = "in") - 
        igraph::degree_distribution(g, mode = "in")) == 0
)
expect_true(
  sum(g_degree_distribution(g, type = "density", mode = "all") - 
        igraph::degree_distribution(g, mode = "all")) == 0
)
expect_true(
  sum(g_degree_distribution(g, type = "density", mode = "out", loops = TRUE) - 
        igraph::degree_distribution(g, mode = "out", loops = TRUE)) == 0
)
expect_true(
  sum(g_degree_distribution(g, type = "density", mode = "in", loops = TRUE) - 
        igraph::degree_distribution(g, mode = "in", loops = TRUE)) == 0
)
expect_true(
  sum(g_degree_distribution(g, type = "density", mode = "all", loops = TRUE) - 
        igraph::degree_distribution(g, mode = "all", loops = TRUE)) == 0
)


### network --------------------------------------------------------------------
# it has already been tested that the results for g were correct, so take those 
# as the benchmark to test for g_n against
g_n <- to_network(g)
expect_true(
  sum(g_degree_distribution(g, type = "density", mode = "out") - 
        g_degree_distribution(g_n, type = "density", mode = "out")) == 0
)
expect_true(
  sum(g_degree_distribution(g, type = "density", mode = "in") - 
        g_degree_distribution(g_n, type = "density", mode = "in")) == 0
)
expect_true(
  sum(g_degree_distribution(g, type = "density", mode = "all") - 
        g_degree_distribution(g_n, type = "density", mode = "all")) == 0
)
expect_true(
  sum(g_degree_distribution(g, type = "density", mode = "out", loops = TRUE) - 
        g_degree_distribution(g_n, type = "density", mode = "out", loops = TRUE)) == 0
)
expect_true(
  sum(g_degree_distribution(g, type = "density", mode = "in", loops = TRUE) - 
        g_degree_distribution(g_n, type = "density", mode = "in", loops = TRUE)) == 0
)
expect_true(
  sum(g_degree_distribution(g, type = "density", mode = "all", loops = TRUE) - 
        g_degree_distribution(g_n, type = "density", mode = "all", loops = TRUE)) == 0
)

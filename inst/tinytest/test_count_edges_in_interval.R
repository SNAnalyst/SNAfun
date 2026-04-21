report_side_effects()


# Build a small temporal network with one tie in the first interval and three
# ties in the second interval. Two of those second-interval ties represent the
# same undirected dyad in opposite directions.
nw <- network::network.initialize(3, directed = TRUE)
network::add.edges(nw, tail = c(1, 2, 2, 1), head = c(2, 1, 3, 2))
nd <- networkDynamic::networkDynamic(nw)
networkDynamic::activate.edges(
  nd,
  onset = c(0.2, 0.8, 0.8, 0.8),
  terminus = c(0.3, 0.9, 0.9, 0.9),
  e = 1:4
)


counts_all <- snafun::count_edges_in_interval(nd, start = 0, end = 1, number = 2)
expect_equal(as.numeric(counts_all), c(1, 3))
expect_equal(length(counts_all), 2)


# Directed counting should keep 1->2 and 2->1 separate in the same interval.
counts_unique_directed <- snafun::count_unique_edges_in_interval(
  nd,
  start = 0,
  end = 1,
  number = 2,
  directed = TRUE
)
expect_equal(as.numeric(counts_unique_directed), c(1, 3))
expect_equal(length(counts_unique_directed), 2)


# Undirected counting should collapse those two opposite-direction ties.
counts_unique_undirected <- snafun::count_unique_edges_in_interval(
  nd,
  start = 0,
  end = 1,
  number = 2,
  directed = FALSE
)
expect_equal(as.numeric(counts_unique_undirected), c(1, 2))
expect_equal(length(counts_unique_undirected), 2)

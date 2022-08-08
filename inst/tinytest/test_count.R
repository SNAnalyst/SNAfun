
data(florentine, package = "snafun")
data(judge_net, package = "snafun")

flobus_i <- florentine$flobusiness
flomar_i <- florentine$flomarriage
flobus_m <- to_matrix(flobus_i)
flomar_m <- to_matrix(flomar_i)
flobus_n <- to_network(flobus_i)
flomar_n <- to_network(flomar_i)

judge_i <- judge_net
judge_m <- to_matrix(judge_i)
judge_n <- suppressWarnings(to_network(judge_i))


expect_error(count_dyads(florentine), "class")
expect_error(count_dyads(florentine), "igraph")
expect_error(count_dyads(florentine), "network")


expect_true(all(count_dyads(flobus_i, echo = FALSE) == c(15, 105)))
expect_true(all(count_dyads(flomar_i, echo = FALSE) == c(20, 100)))
expect_true(all(count_dyads(judge_i, echo = FALSE) == c(94, 686)))

expect_true(all(count_dyads(flobus_n, echo = FALSE) == c(15, 105)))
expect_true(all(count_dyads(flomar_n, echo = FALSE) == c(20, 100)))
expect_true(all(count_dyads(judge_n, echo = FALSE) == c(94, 686)))

i1 <- igraph::random.graph.game(100, p.or.m = .1, type = "gnp", directed = TRUE)
n1 <- to_network(i1)
expect_inherits(n1, "network")

expect_equal(
  count_dyads(i1, echo = FALSE),
  count_dyads(n1, echo = FALSE)
)



expect_error(count_triads(florentine), "class")
expect_error(count_triads(florentine), "igraph")
expect_error(count_triads(florentine), "network")

expect_equal(
  count_triads(i1, echo = FALSE),
  count_triads(n1, echo = FALSE)
)

expect_equal(
  count_triads(flomar_n, echo = FALSE),
  count_triads(flomar_i, echo = FALSE)
)
expect_equal(
  count_triads(flobus_n, echo = FALSE),
  count_triads(flobus_i, echo = FALSE)
)
expect_equal(
  count_triads(judge_n, echo = FALSE),
  count_triads(judge_i, echo = FALSE)
)

## undirected
i2 <- igraph::random.graph.game(100, p.or.m = .15, type = "gnp", directed = FALSE)
n2 <- to_network(i2)

expect_equal(
  count_dyads(i2, echo = FALSE),
  count_dyads(n2, echo = FALSE)   # undirected
)

expect_equal(
  count_triads(i2, echo = FALSE),
  count_triads(n2, echo = FALSE)
)

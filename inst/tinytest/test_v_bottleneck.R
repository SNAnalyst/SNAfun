xi <- igraph::graph(c(1,2,2,3,3,4,4,2))
xn <- snafun::to_network(xi)
xm <- snafun::to_matrix(xi)

v1 <- v_bottleneck(xi)
v2 <- v_bottleneck(xi, vids = c(1, 2, 4))
v3 <- v_bottleneck(xi, mode = "out")
v4 <- v_bottleneck(xi, mode = "in")

expect_equal(v1$bottleneck, v_bottleneck(xn)$bottleneck)
expect_equal(v2$bottleneck, v_bottleneck(xn, vids = c(1, 2, 4))$bottleneck)
expect_equal(v3$bottleneck, v_bottleneck(xn, mode = "out")$bottleneck)
expect_equal(v4$bottleneck, v_bottleneck(xn, mode = "in")$bottleneck)

expect_equal(v1$bottleneck, v_bottleneck(xm)$bottleneck)
expect_equal(v2$bottleneck, v_bottleneck(xm, vids = c(1, 2, 4))$bottleneck)
expect_equal(v3$bottleneck, v_bottleneck(xm, mode = "out")$bottleneck)
expect_equal(v4$bottleneck, v_bottleneck(xm, mode = "in")$bottleneck)



xi <- igraph::make_star(10, mode = "undirected")
xn <- snafun::to_network(xi)
xm <- snafun::to_matrix(xi)
expect_equal(v_bottleneck(xi)$bottleneck, v_bottleneck(xn)$bottleneck)
expect_equal(v_bottleneck(xi)$bottleneck, v_bottleneck(xm)$bottleneck)




xi <- igraph::make_ring(10)
xn <- snafun::to_network(xi)
xm <- snafun::to_matrix(xi)
v1 <- v_bottleneck(xi)
v2 <- v_bottleneck(xi, n = Inf)
expect_equal(v1$bottleneck, v_bottleneck(xn)$bottleneck)
expect_equal(v2$bottleneck, v_bottleneck(xn, n = Inf)$bottleneck)
expect_equal(v1$bottleneck, v_bottleneck(xm)$bottleneck)
expect_equal(v2$bottleneck, v_bottleneck(xm, n = Inf)$bottleneck)




for (trial in 1:10) {
  xi <- snafun::create_random_graph(sample(5:100, 1), "gnp", p = runif(1, .1, 1))
  xn <- snafun::to_network(xi)
  xm <- snafun::to_matrix(xi)
  vi <- v_bottleneck(xi)
  vn <- v_bottleneck(xn)
  vm <- v_bottleneck(xm)
  expect_equal(vi$bottleneck, vn$bottleneck)
  expect_equal(vi$bottleneck, vm$bottleneck)
}

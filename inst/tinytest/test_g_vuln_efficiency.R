

for (trial in 1:20) {
  x <- snafun::create_random_graph(sample(20:40, 1), "gnm", m = runif(1, 80, 200))
  vi <- g_vuln_efficiency(x, method = "harmonic", mode = "all", disconnected = "size")[, c("vulnerability", "vuln_prop")]
  xn <- snafun::to_network(x)
  xm <- snafun::to_matrix(x)
  vn <- g_vuln_efficiency(xn, method = "harmonic", mode = "all", disconnected = "size")[, c("vulnerability", "vuln_prop")]
  vm <- g_vuln_efficiency(xm, method = "harmonic", mode = "all", disconnected = "size")[, c("vulnerability", "vuln_prop")]

  expect_equal(vi, vn)
  expect_equal(vi, vm)
}




for (trial in 1:20) {
  x <- snafun::create_random_graph(sample(20:40, 1), "gnm", m = runif(1, 80, 200))
  vi <- g_vuln_efficiency(x, method = "sum", mode = "all", disconnected = "size")[, c("vulnerability", "vuln_prop")]
  xn <- snafun::to_network(x)
  xm <- snafun::to_matrix(x)
  vn <- g_vuln_efficiency(xn, method = "sum", mode = "all", disconnected = "size")[, c("vulnerability", "vuln_prop")]
  vm <- g_vuln_efficiency(xm, method = "sum", mode = "all", disconnected = "size")[, c("vulnerability", "vuln_prop")]
  
  expect_equal(vi, vn)
  expect_equal(vi, vm)
}





for (trial in 1:20) {
  x <- snafun::create_random_graph(sample(20:40, 1), "gnm", m = runif(1, 80, 200))
  vi <- g_vuln_efficiency(x, method = "harmonic", mode = "in", disconnected = "size")[, c("vulnerability", "vuln_prop")]
  xn <- snafun::to_network(x)
  xm <- snafun::to_matrix(x)
  vn <- g_vuln_efficiency(xn, method = "harmonic", mode = "in", disconnected = "size")[, c("vulnerability", "vuln_prop")]
  vm <- g_vuln_efficiency(xm, method = "harmonic", mode = "in", disconnected = "size")[, c("vulnerability", "vuln_prop")]

  expect_equal(vi, vn)
  expect_equal(vi, vm)
}




for (trial in 1:20) {
  x <- snafun::create_random_graph(sample(20:40, 1), "gnm", m = runif(1, 80, 200))
  vi <- g_vuln_efficiency(x, method = "sum", mode = "in", disconnected = "size")[, c("vulnerability", "vuln_prop")]
  xn <- snafun::to_network(x)
  xm <- snafun::to_matrix(x)
  vn <- g_vuln_efficiency(xn, method = "sum", mode = "in", disconnected = "size")[, c("vulnerability", "vuln_prop")]
  vm <- g_vuln_efficiency(xm, method = "sum", mode = "in", disconnected = "size")[, c("vulnerability", "vuln_prop")]

  expect_equal(vi, vn)
  expect_equal(vi, vm)
}








for (trial in 1:20) {
  x <- snafun::create_random_graph(sample(20:40, 1), "gnm", m = runif(1, 80, 200))
  vi <- g_vuln_efficiency(x, method = "harmonic", mode = "out", disconnected = "size")[, c("vulnerability", "vuln_prop")]
  xn <- snafun::to_network(x)
  xm <- snafun::to_matrix(x)
  vn <- g_vuln_efficiency(xn, method = "harmonic", mode = "out", disconnected = "size")[, c("vulnerability", "vuln_prop")]
  vm <- g_vuln_efficiency(xm, method = "harmonic", mode = "out", disconnected = "size")[, c("vulnerability", "vuln_prop")]

  expect_equal(vi, vn)
  expect_equal(vi, vm)
}




for (trial in 1:20) {
  x <- snafun::create_random_graph(sample(20:40, 1), "gnm", m = runif(1, 80, 200))
  vi <- g_vuln_efficiency(x, method = "sum", mode = "out", disconnected = "size")[, c("vulnerability", "vuln_prop")]
  xn <- snafun::to_network(x)
  xm <- snafun::to_matrix(x)
  vn <- g_vuln_efficiency(xn, method = "sum", mode = "out", disconnected = "size")[, c("vulnerability", "vuln_prop")]
  vm <- g_vuln_efficiency(xm, method = "sum", mode = "out", disconnected = "size")[, c("vulnerability", "vuln_prop")]

  expect_equal(vi, vn)
  expect_equal(vi, vm)
}





for (trial in 1:20) {
  x <- snafun::create_random_graph(sample(20:40, 1), "gnm", m = runif(1, 80, 200))
  vi <- g_vuln_efficiency(x, method = "harmonic", mode = "all", disconnected = "max")[, c("vulnerability", "vuln_prop")]
  xn <- snafun::to_network(x)
  xm <- snafun::to_matrix(x)
  vn <- g_vuln_efficiency(xn, method = "harmonic", mode = "all", disconnected = "max")[, c("vulnerability", "vuln_prop")]
  vm <- g_vuln_efficiency(xm, method = "harmonic", mode = "all", disconnected = "max")[, c("vulnerability", "vuln_prop")]

  expect_equal(vi, vn)
  expect_equal(vi, vm)
}




for (trial in 1:20) {
  x <- snafun::create_random_graph(sample(20:40, 1), "gnm", m = runif(1, 80, 200))
  vi <- g_vuln_efficiency(x, method = "sum", mode = "all", disconnected = "max")[, c("vulnerability", "vuln_prop")]
  xn <- snafun::to_network(x)
  xm <- snafun::to_matrix(x)
  vn <- g_vuln_efficiency(xn, method = "sum", mode = "all", disconnected = "max")[, c("vulnerability", "vuln_prop")]
  vm <- g_vuln_efficiency(xm, method = "sum", mode = "all", disconnected = "max")[, c("vulnerability", "vuln_prop")]

  expect_equal(vi, vn)
  expect_equal(vi, vm)
}





for (trial in 1:20) {
  x <- snafun::create_random_graph(sample(20:40, 1), "gnm", m = runif(1, 80, 200))
  vi <- g_vuln_efficiency(x, method = "harmonic", mode = "in", disconnected = "max")[, c("vulnerability", "vuln_prop")]
  xn <- snafun::to_network(x)
  xm <- snafun::to_matrix(x)
  vn <- g_vuln_efficiency(xn, method = "harmonic", mode = "in", disconnected = "max")[, c("vulnerability", "vuln_prop")]
  vm <- g_vuln_efficiency(xm, method = "harmonic", mode = "in", disconnected = "max")[, c("vulnerability", "vuln_prop")]

  expect_equal(vi, vn)
  expect_equal(vi, vm)
}




for (trial in 1:20) {
  x <- snafun::create_random_graph(sample(20:40, 1), "gnm", m = runif(1, 80, 200))
  vi <- g_vuln_efficiency(x, method = "sum", mode = "in", disconnected = "max")[, c("vulnerability", "vuln_prop")]
  xn <- snafun::to_network(x)
  xm <- snafun::to_matrix(x)
  vn <- g_vuln_efficiency(xn, method = "sum", mode = "in", disconnected = "max")[, c("vulnerability", "vuln_prop")]
  vm <- g_vuln_efficiency(xm, method = "sum", mode = "in", disconnected = "max")[, c("vulnerability", "vuln_prop")]

  expect_equal(vi, vn)
  expect_equal(vi, vm)
}








for (trial in 1:20) {
  x <- snafun::create_random_graph(sample(20:40, 1), "gnm", m = runif(1, 80, 200))
  vi <- g_vuln_efficiency(x, method = "harmonic", mode = "out", disconnected = "max")[, c("vulnerability", "vuln_prop")]
  xn <- snafun::to_network(x)
  xm <- snafun::to_matrix(x)
  vn <- g_vuln_efficiency(xn, method = "harmonic", mode = "out", disconnected = "max")[, c("vulnerability", "vuln_prop")]
  vm <- g_vuln_efficiency(xm, method = "harmonic", mode = "out", disconnected = "max")[, c("vulnerability", "vuln_prop")]

  expect_equal(vi, vn)
  expect_equal(vi, vm)
}




for (trial in 1:20) {
  x <- snafun::create_random_graph(sample(20:40, 1), "gnm", m = runif(1, 80, 200))
  vi <- g_vuln_efficiency(x, method = "sum", mode = "out", disconnected = "max")[, c("vulnerability", "vuln_prop")]
  xn <- snafun::to_network(x)
  xm <- snafun::to_matrix(x)
  vn <- g_vuln_efficiency(xn, method = "sum", mode = "out", disconnected = "max")[, c("vulnerability", "vuln_prop")]
  vm <- g_vuln_efficiency(xm, method = "sum", mode = "out", disconnected = "max")[, c("vulnerability", "vuln_prop")]

  expect_equal(vi, vn)
  expect_equal(vi, vm)
}






for (trial in 1:20) {
  x <- snafun::create_random_graph(sample(20:40, 1), "gnm", m = runif(1, 80, 200))
  vi <- g_vuln_efficiency(x, method = "harmonic", mode = "all", disconnected = "infinite")[, c("vulnerability", "vuln_prop")]
  xn <- snafun::to_network(x)
  xm <- snafun::to_matrix(x)
  vn <- g_vuln_efficiency(xn, method = "harmonic", mode = "all", disconnected = "infinite")[, c("vulnerability", "vuln_prop")]
  vm <- g_vuln_efficiency(xm, method = "harmonic", mode = "all", disconnected = "infinite")[, c("vulnerability", "vuln_prop")]

  expect_equal(vi, vn)
  expect_equal(vi, vm)
}




for (trial in 1:20) {
  x <- snafun::create_random_graph(sample(20:40, 1), "gnm", m = runif(1, 80, 200))
  vi <- g_vuln_efficiency(x, method = "sum", mode = "all", disconnected = "infinite")[, c("vulnerability", "vuln_prop")]
  xn <- snafun::to_network(x)
  xm <- snafun::to_matrix(x)
  vn <- g_vuln_efficiency(xn, method = "sum", mode = "all", disconnected = "infinite")[, c("vulnerability", "vuln_prop")]
  vm <- g_vuln_efficiency(xm, method = "sum", mode = "all", disconnected = "infinite")[, c("vulnerability", "vuln_prop")]

  expect_equal(vi, vn)
  expect_equal(vi, vm)
}





for (trial in 1:20) {
  x <- snafun::create_random_graph(sample(20:40, 1), "gnm", m = runif(1, 80, 200))
  vi <- g_vuln_efficiency(x, method = "harmonic", mode = "in", disconnected = "infinite")[, c("vulnerability", "vuln_prop")]
  xn <- snafun::to_network(x)
  xm <- snafun::to_matrix(x)
  vn <- g_vuln_efficiency(xn, method = "harmonic", mode = "in", disconnected = "infinite")[, c("vulnerability", "vuln_prop")]
  vm <- g_vuln_efficiency(xm, method = "harmonic", mode = "in", disconnected = "infinite")[, c("vulnerability", "vuln_prop")]

  expect_equal(vi, vn)
  expect_equal(vi, vm)
}




for (trial in 1:20) {
  x <- snafun::create_random_graph(sample(20:40, 1), "gnm", m = runif(1, 80, 200))

  vi <- g_vuln_efficiency(x, method = "sum", mode = "in", disconnected = "infinite")[, c("vulnerability", "vuln_prop")]
  xn <- snafun::to_network(x)
  xm <- snafun::to_matrix(x)
  vn <- g_vuln_efficiency(xn, method = "sum", mode = "in", disconnected = "infinite")[, c("vulnerability", "vuln_prop")]
  vm <- g_vuln_efficiency(xm, method = "sum", mode = "in", disconnected = "infinite")[, c("vulnerability", "vuln_prop")]

  expect_equal(vi, vn)
  expect_equal(vi, vm)
}








for (trial in 1:20) {
  x <- snafun::create_random_graph(sample(20:40, 1), "gnm", m = runif(1, 80, 200))
  vi <- g_vuln_efficiency(x, method = "harmonic", mode = "out", disconnected = "infinite")[, c("vulnerability", "vuln_prop")]
  xn <- snafun::to_network(x)
  xm <- snafun::to_matrix(x)
  vn <- g_vuln_efficiency(xn, method = "harmonic", mode = "out", disconnected = "infinite")[, c("vulnerability", "vuln_prop")]
  vm <- g_vuln_efficiency(xm, method = "harmonic", mode = "out", disconnected = "infinite")[, c("vulnerability", "vuln_prop")]

  expect_equal(vi, vn)
  expect_equal(vi, vm)
}




for (trial in 1:20) {
  x <- snafun::create_random_graph(sample(20:40, 1), "gnm", m = runif(1, 80, 200))
  vi <- g_vuln_efficiency(x, method = "sum", mode = "out", disconnected = "infinite")[, c("vulnerability", "vuln_prop")]
  xn <- snafun::to_network(x)
  xm <- snafun::to_matrix(x)
  vn <- g_vuln_efficiency(xn, method = "sum", mode = "out", disconnected = "infinite")[, c("vulnerability", "vuln_prop")]
  vm <- g_vuln_efficiency(xm, method = "sum", mode = "out", disconnected = "infinite")[, c("vulnerability", "vuln_prop")]

  expect_equal(vi, vn)
  expect_equal(vi, vm)
}





data("soccer98", package = "snafun")
xn <- soccer98
xi <- snafun::to_igraph(xn)
xm <- snafun::to_matrix(xn)

vi <- g_vuln_efficiency(xi)
vn <- g_vuln_efficiency(xn)
vm <- g_vuln_efficiency(xm)

expect_equal(vi, vn)
expect_equal(vi, vm)








for (trial in 1:20) {
  x <- snafun::create_random_graph(sample(20:100, 1), "gnp", p = runif(1, 0, 1))
  vi <- g_vuln_paths(x, mode = "all")[, c("vulnerability", "vuln_prop")]
  xn <- snafun::to_network(x)
  xm <- snafun::to_matrix(x)
  vn <- g_vuln_paths(xn, mode = "all")[, c("vulnerability", "vuln_prop")]
  vm <- g_vuln_paths(xm, mode = "all")[, c("vulnerability", "vuln_prop")]
  
  expect_equal(vi, vn)
  expect_equal(vi, vm)
}



for (trial in 1:20) {
  x <- snafun::create_random_graph(sample(20:100, 1), "gnp", p = runif(1, 0, 1))
  vi <- g_vuln_paths(x, mode = "in")[, c("vulnerability", "vuln_prop")]
  xn <- snafun::to_network(x)
  xm <- snafun::to_matrix(x)
  vn <- g_vuln_paths(xn, mode = "in")[, c("vulnerability", "vuln_prop")]
  vm <- g_vuln_paths(xm, mode = "in")[, c("vulnerability", "vuln_prop")]
  
  expect_equal(vi, vn)
  expect_equal(vi, vm)
}





for (trial in 1:20) {
  x <- snafun::create_random_graph(sample(20:100, 1), "gnp", p = runif(1, 0, 1))
  vi <- g_vuln_paths(x, mode = "out")[, c("vulnerability", "vuln_prop")]
  xn <- snafun::to_network(x)
  xm <- snafun::to_matrix(x)
  vn <- g_vuln_paths(xn, mode = "out")[, c("vulnerability", "vuln_prop")]
  vm <- g_vuln_paths(xm, mode = "out")[, c("vulnerability", "vuln_prop")]
  
  expect_equal(vi, vn)
  expect_equal(vi, vm)
}



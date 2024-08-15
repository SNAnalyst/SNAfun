

for (trial in 1:10) {
  x <- snafun::create_random_graph(sample(20:40, 1), "gnm", m = runif(1, 80, 200))
  vi <- g_vuln_attack(x)
  xn <- snafun::to_network(x)
  xm <- snafun::to_matrix(x)
  vn <- g_vuln_attack(xn)
  vm <- g_vuln_attack(xm)
  
  Random <- which(colnames(vi) == "Random")
  expect_equal(vi[, -Random], vn[, -Random])
  expect_equal(vi[, -Random], vm[, -Random])
}


data("soccer98", package = "snafun")
xn <- soccer98
xi <- snafun::to_igraph(xn)
xm <- snafun::to_matrix(xn)

vi <- g_vuln_attack(xi)
vn <- g_vuln_attack(xn)
vm <- g_vuln_attack(xm)

Random <- which(colnames(vi) %in% c("Random"))
expect_equal(vi[, -Random], vn[, -Random])
expect_equal(vi[, -Random], vm[, -Random])





for (trial in 1:100) {
  x <- snafun::create_random_graph(sample(20:40, 1), "gnm", m = runif(1, 80, 200))
  # we prevent here to work with graphs that have isolates, as their conversion 
  # has not been fully implemented yet
  while(has_isolates(x)) {
    x <- snafun::create_random_graph(sample(20:40, 1), "gnm", m = runif(1, 80, 200))
  }
  
  vi <- g_efficiency(x)
  xn <- snafun::to_network(x)
  xd <- snafun::to_edgelist(x)
  xm <- snafun::to_matrix(x)
  vn <- g_efficiency(xn)
  vd <- g_efficiency(xd)
  vm <- g_efficiency(xm)
  
  expect_equal(vi, vn)
  expect_equal(vi, vd)
  expect_equal(vi, vm)
}


data("soccer98", package = "snafun")
xn <- soccer98
xi <- snafun::to_igraph(xn)
xd <- snafun::to_edgelist(xn)
xm <- snafun::to_matrix(xn)

vi <- g_efficiency(xi)
vn <- g_efficiency(xn)
vd <- g_efficiency(xd)
vm <- g_efficiency(xm)

expect_equal(vi, vn)
expect_equal(vi, vd)
expect_equal(vi, vm)



# fully connected
xm <- matrix(1, ncol = 10, nrow = 10)
xi <- snafun::to_igraph(xm)
xd <- snafun::to_edgelist(xm)
xn <- snafun::to_network(xm)
vi <- g_efficiency(xi)
vn <- g_efficiency(xn)
# vd <- g_efficiency(xd)
vm <- g_efficiency(xm)
expect_equal(vi, vn)
# expect_equal(vi, vd)
expect_equal(vi, vm)


vi <- g_efficiency(xi, diag = TRUE)
vn <- g_efficiency(xn, diag = TRUE)
vm <- g_efficiency(xm, diag = TRUE)
expect_equal(vi, vn)
expect_equal(vi, vm)




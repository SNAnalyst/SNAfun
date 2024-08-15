

for (trial in 1:20) {
  x <- snafun::create_random_graph(sample(20:100, 1), "gnp", p = runif(1, 0.05, .95))
  vi <- g_secrecy(x, type = 0, p = .25)
  xn <- snafun::to_network(x)
  xm <- snafun::to_matrix(x)
  vn <- g_secrecy(xn, type = 0, p = .25)
  vm <- g_secrecy(xm, type = 0, p = .25)

  expect_equal(vi, vn)
  expect_equal(vi, vm)
}



for (trial in 1:20) {
  x <- snafun::create_random_graph(sample(20:100, 1), "gnp", p = runif(1, 0.05, .95))
  vi <- g_secrecy(x, type = 1, p = .25)
  xn <- snafun::to_network(x)
  xm <- snafun::to_matrix(x)
  vn <- g_secrecy(xn, type = 1, p = .25)
  vm <- g_secrecy(xm, type = 1, p = .25)

  expect_equal(vi, vn)
  expect_equal(vi, vm)
}


for (trial in 1:20) {
  x <- snafun::create_random_graph(sample(20:100, 1), "gnp", p = runif(1, 0.05, .95))
  vi <- g_secrecy(x, type = 2, p = .25)
  xn <- snafun::to_network(x)
  xm <- snafun::to_matrix(x)
  vn <- g_secrecy(xn, type = 2, p = .25)
  vm <- g_secrecy(xm, type = 2, p = .25)

  expect_equal(vi, vn)
  expect_equal(vi, vm)
}





for (trial in 1:20) {
  x <- snafun::create_random_graph(sample(20:100, 1), "gnp", p = runif(1, 0.05, .95))
  vi <- g_secrecy(x, type = 3, p = .25)
  xn <- snafun::to_network(x)
  xm <- snafun::to_matrix(x)
  vn <- g_secrecy(xn, type = 3, p = .25)
  vm <- g_secrecy(xm, type = 3, p = .25)

  expect_equal(vi, vn)
  expect_equal(vi, vm)
}





for (trial in 1:20) {
  x <- snafun::create_random_graph(sample(20:100, 1), "gnp", p = runif(1, 0.05, .95))
  vi <- g_secrecy(x, type = 0, p = .05)
  xn <- snafun::to_network(x)
  xm <- snafun::to_matrix(x)
  vn <- g_secrecy(xn, type = 0, p = .05)
  vm <- g_secrecy(xm, type = 0, p = .05)

  expect_equal(vi, vn)
  expect_equal(vi, vm)
}



for (trial in 1:20) {
  x <- snafun::create_random_graph(sample(20:100, 1), "gnp", p = runif(1, 0.05, .95))
  vi <- g_secrecy(x, type = 1, p = .05)
  xn <- snafun::to_network(x)
  xm <- snafun::to_matrix(x)
  vn <- g_secrecy(xn, type = 1, p = .05)
  vm <- g_secrecy(xm, type = 1, p = .05)

  expect_equal(vi, vn)
  expect_equal(vi, vm)
}


for (trial in 1:20) {
  x <- snafun::create_random_graph(sample(20:100, 1), "gnp", p = runif(1, 0.05, .95))
  vi <- g_secrecy(x, type = 2, p = .05)
  xn <- snafun::to_network(x)
  xm <- snafun::to_matrix(x)
  vn <- g_secrecy(xn, type = 2, p = .05)
  vm <- g_secrecy(xm, type = 2, p = .05)

  expect_equal(vi, vn)
  expect_equal(vi, vm)
}


for (trial in 1:20) {
  x <- snafun::create_random_graph(sample(20:100, 1), "gnp", p = runif(1, 0.05, .95))
  vi <- g_secrecy(x, type = 3, p = .05)
  xn <- snafun::to_network(x)
  xm <- snafun::to_matrix(x)
  vn <- g_secrecy(xn, type = 3, p = .05)
  vm <- g_secrecy(xm, type = 3, p = .05)

  expect_equal(vi, vn)
  expect_equal(vi, vm)
}





for (trial in 1:20) {
  x <- snafun::create_random_graph(sample(20:100, 1), "gnp", p = runif(1, 0.05, .95))
  vi <- g_secrecy(x, type = 0, p = .85)
  xn <- snafun::to_network(x)
  xm <- snafun::to_matrix(x)
  vn <- g_secrecy(xn, type = 0, p = .85)
  vm <- g_secrecy(xm, type = 0, p = .85)

  expect_equal(vi, vn)
  expect_equal(vi, vm)
}



for (trial in 1:20) {
  x <- snafun::create_random_graph(sample(20:100, 1), "gnp", p = runif(1, 0.05, .95))
  vi <- g_secrecy(x, type = 1, p = .85)
  xn <- snafun::to_network(x)
  xm <- snafun::to_matrix(x)
  vn <- g_secrecy(xn, type = 1, p = .85)
  vm <- g_secrecy(xm, type = 1, p = .85)

  expect_equal(vi, vn)
  expect_equal(vi, vm)
}


for (trial in 1:20) {
  x <- snafun::create_random_graph(sample(20:100, 1), "gnp", p = runif(1, 0.05, .95))
  vi <- g_secrecy(x, type = 2, p = .85)
  xn <- snafun::to_network(x)
  xm <- snafun::to_matrix(x)
  vn <- g_secrecy(xn, type = 2, p = .85)
  vm <- g_secrecy(xm, type = 2, p = .85)

  expect_equal(vi, vn)
  expect_equal(vi, vm)
}


for (trial in 1:20) {
  x <- snafun::create_random_graph(sample(20:100, 1), "gnp", p = runif(1, 0.05, .95))
  vi <- g_secrecy(x, type = 3, p = .85)
  xn <- snafun::to_network(x)
  xm <- snafun::to_matrix(x)
  vn <- g_secrecy(xn, type = 3, p = .85)
  vm <- g_secrecy(xm, type = 3, p = .85)

  expect_equal(vi, vn)
  expect_equal(vi, vm)
}






data("soccer98", package = "snafun")
xn <- soccer98
xi <- snafun::to_igraph(xn)
xm <- snafun::to_matrix(xn)

vi <- g_secrecy(xi)
vn <- g_secrecy(xn)
vm <- g_secrecy(xm)

expect_equal(vi, vn)
expect_equal(vi, vm)







for (trial in 1:20) {
  xi <- snafun::create_random_graph(sample(20:100, 1), "gnp", p = runif(1, 0.05, .95))
  xn <- snafun::to_network(xi)
  xm <- snafun::to_matrix(xi)
  vi <- v_secrecy(xi, type = 0, p = .25)
  vn <- v_secrecy(xn, type = 0, p = .25)
  vm <- v_secrecy(xm, type = 0, p = .25)
  expect_equal(sum(vi - vn), 0)
  expect_equal(sum(vi - vm), 0)
}

for (trial in 1:20) {
  xi <- snafun::create_random_graph(sample(20:100, 1), "gnp", p = runif(1, 0.05, .95))
  xn <- snafun::to_network(xi)
  xm <- snafun::to_matrix(xi)
  vi <- v_secrecy(xi, type = 1, p = .25)
  vn <- v_secrecy(xn, type = 1, p = .25)
  vm <- v_secrecy(xm, type = 1, p = .25)
  expect_equal(sum(vi - vn), 0)
  expect_equal(sum(vi - vm), 0)
}

for (trial in 1:20) {
  xi <- snafun::create_random_graph(sample(20:100, 1), "gnp", p = runif(1, 0.05, .95))
  xn <- snafun::to_network(xi)
  xm <- snafun::to_matrix(xi)
  vi <- v_secrecy(xi, type = 2, p = .25)
  vn <- v_secrecy(xn, type = 2, p = .25)
  vm <- v_secrecy(xm, type = 2, p = .25)
  expect_equal(sum(vi - vn), 0)
  expect_equal(sum(vi - vm), 0)
}

for (trial in 1:20) {
  xi <- snafun::create_random_graph(sample(20:100, 1), "gnp", p = runif(1, 0.05, .95))
  xn <- snafun::to_network(xi)
  xm <- snafun::to_matrix(xi)
  vi <- v_secrecy(xi, type = 3, p = .25)
  vn <- v_secrecy(xn, type = 3, p = .25)
  vm <- v_secrecy(xm, type = 3, p = .25)
  expect_equal(sum(vi - vn), 0)
  expect_equal(sum(vi - vm), 0)
}



for (trial in 1:20) {
  xi <- snafun::create_random_graph(sample(20:100, 1), "gnp", p = runif(1, 0.05, .95))
  xn <- snafun::to_network(xi)
  xm <- snafun::to_matrix(xi)
  vi <- v_secrecy(xi, type = 0, p = .85)
  vn <- v_secrecy(xn, type = 0, p = .85)
  vm <- v_secrecy(xm, type = 0, p = .85)
  expect_equal(sum(vi - vn), 0)
  expect_equal(sum(vi - vm), 0)
}

for (trial in 1:20) {
  xi <- snafun::create_random_graph(sample(20:100, 1), "gnp", p = runif(1, 0.05, .95))
  xn <- snafun::to_network(xi)
  xm <- snafun::to_matrix(xi)
  vi <- v_secrecy(xi, type = 1, p = .85)
  vn <- v_secrecy(xn, type = 1, p = .85)
  vm <- v_secrecy(xm, type = 1, p = .85)
  expect_equal(sum(vi - vn), 0)
  expect_equal(sum(vi - vm), 0)
}

for (trial in 1:20) {
  xi <- snafun::create_random_graph(sample(20:100, 1), "gnp", p = runif(1, 0.05, .95))
  xn <- snafun::to_network(xi)
  xm <- snafun::to_matrix(xi)
  vi <- v_secrecy(xi, type = 2, p = .85)
  vn <- v_secrecy(xn, type = 2, p = .85)
  vm <- v_secrecy(xm, type = 2, p = .85)
  expect_equal(sum(vi - vn), 0)
  expect_equal(sum(vi - vm), 0)
}

for (trial in 1:20) {
  xi <- snafun::create_random_graph(sample(20:100, 1), "gnp", p = runif(1, 0.05, .95))
  xn <- snafun::to_network(xi)
  xm <- snafun::to_matrix(xi)
  vi <- v_secrecy(xi, type = 3, p = .85)
  vn <- v_secrecy(xn, type = 3, p = .85)
  vm <- v_secrecy(xm, type = 3, p = .85)
  expect_equal(sum(vi - vn), 0)
  expect_equal(sum(vi - vm), 0)
}




for (trial in 1:20) {
  xi <- snafun::create_random_graph(sample(20:100, 1), "gnp", p = runif(1, 0.05, .95))
  xn <- snafun::to_network(xi)
  xm <- snafun::to_matrix(xi)
  vi <- v_secrecy(xi, type = 0, p = .05)
  vn <- v_secrecy(xn, type = 0, p = .05)
  vm <- v_secrecy(xm, type = 0, p = .05)
  expect_equal(sum(vi - vn), 0)
  expect_equal(sum(vi - vm), 0)
}

for (trial in 1:20) {
  xi <- snafun::create_random_graph(sample(20:100, 1), "gnp", p = runif(1, 0.05, .95))
  xn <- snafun::to_network(xi)
  xm <- snafun::to_matrix(xi)
  vi <- v_secrecy(xi, type = 1, p = .05)
  vn <- v_secrecy(xn, type = 1, p = .05)
  vm <- v_secrecy(xm, type = 1, p = .05)
  expect_equal(sum(vi - vn), 0)
  expect_equal(sum(vi - vm), 0)
}

for (trial in 1:20) {
  xi <- snafun::create_random_graph(sample(20:100, 1), "gnp", p = runif(1, 0.05, .95))
  xn <- snafun::to_network(xi)
  xm <- snafun::to_matrix(xi)
  vi <- v_secrecy(xi, type = 2, p = .05)
  vn <- v_secrecy(xn, type = 2, p = .05)
  vm <- v_secrecy(xm, type = 2, p = .05)
  expect_equal(sum(vi - vn), 0)
  expect_equal(sum(vi - vm), 0)
}

for (trial in 1:20) {
  xi <- snafun::create_random_graph(sample(20:100, 1), "gnp", p = runif(1, 0.05, .95))
  xn <- snafun::to_network(xi)
  xm <- snafun::to_matrix(xi)
  vi <- v_secrecy(xi, type = 3, p = .05)
  vn <- v_secrecy(xn, type = 3, p = .05)
  vm <- v_secrecy(xm, type = 3, p = .05)
  expect_equal(sum(vi - vn), 0)
  expect_equal(sum(vi - vm), 0)
}



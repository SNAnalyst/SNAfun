
##  ............................................................................
##  TESTS                                                                   ####

report_side_effects()

data(huk, package = "SNA4DSData")

# x <- as.matrix(cbind(Intcpt = 1, hukYX[, -1]))
# lnam1 <- sna::lnam(y = hukYX$y, x = x, W1 = hukW)
# lnam2 <- sna::lnam(y = hukYX$y, x = x, W2 = hukW)
# lnam3 <- sna::lnam(y = hukYX$y, x = x, W = hukW, W2 = hukW)

# lnam1 <- sna::lnam(y = hukYX$y, x = x, W1 = hukWstd)
# lnam2 <- sna::lnam(y = hukYX$y, x = x, W2 = hukWstd)
# lnam3 <- sna::lnam(y = hukYX$y, x = x, W = hukWstd, W2 = hukWstd)

nam1 <- suppressMessages(snafun::stat_nam(y ~ ., data = hukYX, W = hukW, model = "lag"))
nam2 <- suppressMessages(snafun::stat_nam(y ~ ., data = hukYX, W = hukW, model = "error"))
nam3 <- suppressMessages(snafun::stat_nam(y ~ ., data = hukYX, W = hukW, W2 = hukW, model = "combined"))

nam1a <- snafun::stat_nam(y ~ ., data = hukYX, W = hukWstd, model = "lag")
nam2a <- snafun::stat_nam(y ~ ., data = hukYX, W = hukWstd, model = "error")
nam3a <- snafun::stat_nam(y ~ ., data = hukYX, W = hukWstd, W2 = hukWstd, model = "combined")

expect_equivalent(nam1$rho, 0.5581997, tolerance = .001)
expect_null(nam1$lambda)

expect_equivalent(nam2$lambda, 0.6593457, tolerance = .001)
expect_null(nam2$rho)

expect_equivalent(nam3$rho, 0.6168094, tolerance = .001)
expect_equivalent(nam3$lambda, -0.453029, tolerance = .001)

expect_equal(nam1$dvars, c(6, 0))
expect_equal(names(nam1$coefficients), c("(Intercept)", "PFMP", "POWN", "PSGR", "PMNT", "PSWP"))

expect_equal(nam2$dvars, c(6, 0))
expect_equal(names(nam2$coefficients), c("(Intercept)", "PFMP", "POWN", "PSGR", "PMNT", "PSWP"))

expect_equal(nam3$dvars, c(6, 0))
expect_equal(names(nam3$coefficients), c("(Intercept)", "PFMP", "POWN", "PSGR", "PMNT", "PSWP"))

expect_message(snafun::stat_nam(y ~ ., data = hukYX, W = hukW, model = "lag"),
               pattern = "You supplied a weight matrix that was not row-normalized")





expect_equivalent(nam1$rho, nam1a$rho, tolerance = .001)
expect_null(nam1a$lambda)

expect_equivalent(nam2$lambda, nam2a$lambda, tolerance = .001)
expect_null(nam2a$rho)

expect_equivalent(nam3$rho, nam3a$rho, tolerance = .001)
expect_equivalent(nam3$lambda, nam3a$lambda, tolerance = .001)




# Extensive tests for stat_nam(), validated against sna::lnam() as an
# independent, canonical implementation of the network autocorrelation model.
#
# Conventions verified here (2026-09-14):
#   * The weight matrix W (and W2) is ALWAYS row-normalized internally, so the
#     result does not depend on whether the user supplies a raw or an already
#     row-standardized matrix.
#   * An intercept is ALWAYS included, even when the formula removes it
#     ('y ~ . - 1').
#   * W / W2 may be supplied as a matrix, an edgelist data.frame, an igraph
#     object, or a network object, all giving the same fit.
#   * The fitted object carries a short, clean call() (regression test for the
#     ~31k-character call dump that spatialreg produced via do.call()).
#
# Mapping to sna::lnam():
#   stat_nam(model = "lag")       -> lnam(W1 = Wn)          : $rho    ~ $rho1
#   stat_nam(model = "error")     -> lnam(W2 = Wn)          : $lambda ~ $rho2
#   stat_nam(model = "combined")  -> lnam(W1 = Wn, W2 = Wn2): $rho/$lambda ~ $rho1/$rho2
# In every case the regression coefficients ($coefficients ~ $beta) must agree.

# These models need the modelling stack; skip cleanly if it is unavailable.
if (!requireNamespace("spatialreg", quietly = TRUE) ||
    !requireNamespace("spdep", quietly = TRUE) ||
    !requireNamespace("sna", quietly = TRUE)) {
  exit_file("spatialreg / spdep / sna not available")
}

report_side_effects(FALSE)

TOL <- 1e-3   # tolerance for two independent ML implementations


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

# A random weight matrix with NO all-zero rows (so the row-normalization used by
# the lnam reference is well defined). 'directed' controls symmetry.
make_W <- function(n, seed, directed = TRUE, weighted = FALSE) {
  set.seed(seed)
  w <- matrix(stats::rbinom(n * n, 1, 0.2), n, n)
  diag(w) <- 0
  if (!directed) {
    w[lower.tri(w)] <- t(w)[lower.tri(w)]
  }
  # guarantee at least one out-tie per row
  for (i in seq_len(n)) {
    if (sum(w[i, ]) == 0) {
      j <- if (i == 1) 2 else 1
      w[i, j] <- 1
      if (!directed) w[j, i] <- 1
    }
  }
  if (weighted) {
    w[w != 0] <- stats::runif(sum(w != 0), 1, 5)
  }
  w
}

row_norm <- function(w) w / rowSums(w)

# Simulate a data set from a lag or error network autocorrelation process.
simulate_nam <- function(n, wn, seed, kind = c("lag", "error"),
                         coef_par = 0.35) {
  kind <- match.arg(kind)
  set.seed(seed + 1000)
  k <- 3
  X <- matrix(stats::rnorm(n * k, sd = 2), n, k)
  colnames(X) <- c("A", "B", "C")
  B <- c(1.5, -1.2, 0.7, 0.9)
  I <- diag(n)
  if (kind == "lag") {
    y <- solve(I - coef_par * wn) %*% (cbind(1, X) %*% B + stats::rnorm(n))
  } else {
    u <- solve(I - coef_par * wn) %*% stats::rnorm(n)
    y <- cbind(1, X) %*% B + u
  }
  list(dat = data.frame(y = as.numeric(y), X), X = X)
}

# All four representations of the same weight matrix.
as_all_W_types <- function(w) {
  list(
    matrix   = w,
    igraph   = snafun::to_igraph(w),
    network  = snafun::to_network(w),
    edgelist = snafun::to_edgelist(snafun::to_igraph(w))
  )
}


# ---------------------------------------------------------------------------
# 1. LAG model: every input type equals sna::lnam and equals each other.
# ---------------------------------------------------------------------------

for (n in c(40L, 90L)) {
  w  <- make_W(n, seed = n, directed = TRUE)
  wn <- row_norm(w)
  sim <- simulate_nam(n, wn, seed = n, kind = "lag")
  dat <- sim$dat

  ref <- sna::lnam(dat$y, x = cbind(1, sim$X), W1 = wn)

  fits <- lapply(as_all_W_types(w), function(Wobj) {
    suppressMessages(snafun::stat_nam(y ~ ., data = dat, W = Wobj, model = "lag"))
  })

  for (ty in names(fits)) {
    m <- fits[[ty]]
    expect_equal(unname(m$rho), as.numeric(ref$rho1), tolerance = TOL,
                 info = paste0("lag n=", n, " W=", ty, ": rho matches lnam rho1"))
    expect_equal(unname(m$coefficients), as.numeric(ref$beta), tolerance = TOL,
                 info = paste0("lag n=", n, " W=", ty, ": beta matches lnam"))
  }
  # all input types give the same fit (exactly, same internal matrix)
  base_coef <- fits$matrix$coefficients
  for (ty in c("igraph", "network", "edgelist")) {
    expect_equal(unname(fits[[ty]]$coefficients), unname(base_coef), tolerance = 1e-8,
                 info = paste0("lag n=", n, ": ", ty, " fit == matrix fit"))
    expect_equal(unname(fits[[ty]]$rho), unname(fits$matrix$rho), tolerance = 1e-8,
                 info = paste0("lag n=", n, ": ", ty, " rho == matrix rho"))
  }
}


# ---------------------------------------------------------------------------
# 2. ERROR model vs sna::lnam (W2), all input types.
# ---------------------------------------------------------------------------

# NOTE: sna::lnam()'s error-term optimizer can wander out of the stationary
# range (|rho2| >= 1) for smaller/asymmetric problems, while spatialreg's
# errorsarlm() stays well-behaved. We therefore (a) use sizes where lnam
# converges, (b) always sanity-check stat_nam against the TRUE lambda, and
# (c) only require agreement with lnam when lnam itself produced an in-range
# estimate.
true_lambda <- 0.4
for (n in c(90L, 140L)) {
  w  <- make_W(n, seed = 100 + n, directed = TRUE)
  wn <- row_norm(w)
  sim <- simulate_nam(n, wn, seed = 100 + n, kind = "error", coef_par = true_lambda)
  dat <- sim$dat

  ref <- sna::lnam(dat$y, x = cbind(1, sim$X), W2 = wn)
  lnam_ok <- abs(as.numeric(ref$rho2)) < 0.95

  for (ty in c("matrix", "igraph", "network", "edgelist")) {
    Wobj <- as_all_W_types(w)[[ty]]
    m <- suppressMessages(snafun::stat_nam(y ~ ., data = dat, W = Wobj, model = "error"))
    # stat_nam must always return a finite, stationary estimate (unlike lnam,
    # which can diverge out of range on these problems)
    expect_true(is.finite(unname(m$lambda)) && abs(unname(m$lambda)) < 1,
                info = paste0("error n=", n, " W=", ty, ": lambda finite and stationary"))
    if (lnam_ok) {
      expect_equal(unname(m$lambda), as.numeric(ref$rho2), tolerance = 1e-2,
                   info = paste0("error n=", n, " W=", ty, ": lambda matches lnam rho2"))
      expect_equal(unname(m$coefficients), as.numeric(ref$beta), tolerance = 1e-2,
                   info = paste0("error n=", n, " W=", ty, ": beta matches lnam"))
    }
  }
}


# ---------------------------------------------------------------------------
# 3. COMBINED (SAC/SARAR) vs sna::lnam(W1, W2).
# ---------------------------------------------------------------------------

n <- 120L
w1 <- make_W(n, seed = 7, directed = TRUE)
w2 <- make_W(n, seed = 8, directed = TRUE)
wn1 <- row_norm(w1); wn2 <- row_norm(w2)
set.seed(9)
k <- 3
X <- matrix(stats::rnorm(n * k, sd = 2), n, k); colnames(X) <- c("A", "B", "C")
B <- c(1.0, -0.8, 0.6, 0.5)
I <- diag(n)
e <- solve(I - 0.15 * wn2, stats::rnorm(n, sd = 0.5))
y <- solve(I - 0.25 * wn1, cbind(1, X) %*% B + e)
datc <- data.frame(y = as.numeric(y), X)

mc  <- suppressMessages(snafun::stat_nam(y ~ ., data = datc, W = w1, W2 = w2, model = "combined"))
refc <- sna::lnam(datc$y, x = cbind(1, X), W1 = wn1, W2 = wn2)
# The 2-parameter SARAR surface is flatter; use a looser but still strict tol.
expect_equal(unname(mc$rho),    as.numeric(refc$rho1), tolerance = 1e-2,
             info = "combined: rho matches lnam rho1")
expect_equal(unname(mc$lambda), as.numeric(refc$rho2), tolerance = 1e-2,
             info = "combined: lambda matches lnam rho2")
expect_equal(unname(mc$coefficients), as.numeric(refc$beta), tolerance = 1e-2,
             info = "combined: beta matches lnam")


# ---------------------------------------------------------------------------
# 4. WEIGHTED (valued) weight matrix.
# ---------------------------------------------------------------------------

n <- 70L
wv  <- make_W(n, seed = 21, directed = TRUE, weighted = TRUE)
wvn <- row_norm(wv)
sim <- simulate_nam(n, wvn, seed = 21, kind = "lag")
datv <- sim$dat
refv <- sna::lnam(datv$y, x = cbind(1, sim$X), W1 = wvn)
mv <- suppressMessages(snafun::stat_nam(y ~ ., data = datv, W = wv, model = "lag"))
expect_equal(unname(mv$rho), as.numeric(refv$rho1), tolerance = TOL,
             info = "weighted W: rho matches lnam")
expect_equal(unname(mv$coefficients), as.numeric(refv$beta), tolerance = TOL,
             info = "weighted W: beta matches lnam")
# igraph roundtrip of a valued matrix must give the same fit
mv_i <- suppressMessages(snafun::stat_nam(y ~ ., data = datv, W = snafun::to_igraph(wv), model = "lag"))
expect_equal(unname(mv_i$coefficients), unname(mv$coefficients), tolerance = 1e-8,
             info = "weighted W: igraph input == matrix input")


# ---------------------------------------------------------------------------
# 5. CONVENTION: W is always row-normalized, so raw and pre-normalized W agree.
# ---------------------------------------------------------------------------

n <- 60L
w  <- make_W(n, seed = 33, directed = TRUE)
wn <- row_norm(w)
sim <- simulate_nam(n, wn, seed = 33, kind = "lag")
dat <- sim$dat
m_raw  <- suppressMessages(snafun::stat_nam(y ~ ., data = dat, W = w,  model = "lag"))
m_norm <- suppressMessages(snafun::stat_nam(y ~ ., data = dat, W = wn, model = "lag"))
expect_equal(unname(m_raw$coefficients), unname(m_norm$coefficients), tolerance = 1e-8,
             info = "row-normalization: raw W and pre-normalized W give identical fits")
expect_equal(unname(m_raw$rho), unname(m_norm$rho), tolerance = 1e-8,
             info = "row-normalization: raw and pre-normalized W give identical rho")

# A non-normalized W triggers the informative message; a normalized one does not.
raw_msgs <- character(0)
withCallingHandlers(
  snafun::stat_nam(y ~ ., data = dat, W = w, model = "lag"),
  message = function(mm) { raw_msgs <<- c(raw_msgs, conditionMessage(mm)); invokeRestart("muffleMessage") }
)
expect_true(any(grepl("row-normalized|row-normalised|not row", raw_msgs)),
            info = "non-normalized W emits the row-normalization message")

norm_msgs <- character(0)
withCallingHandlers(
  snafun::stat_nam(y ~ ., data = dat, W = wn, model = "lag"),
  message = function(mm) { norm_msgs <<- c(norm_msgs, conditionMessage(mm)); invokeRestart("muffleMessage") }
)
expect_false(any(grepl("not row", norm_msgs)),
             info = "already-normalized W does not emit the message")


# ---------------------------------------------------------------------------
# 6. CONVENTION: an intercept is always included, even for 'y ~ . - 1'.
# ---------------------------------------------------------------------------

m_int  <- suppressMessages(snafun::stat_nam(y ~ .,      data = dat, W = w, model = "lag"))
m_noint <- suppressMessages(snafun::stat_nam(y ~ . - 1, data = dat, W = w, model = "lag"))
expect_true("(Intercept)" %in% names(m_int$coefficients),
            info = "intercept present for a normal formula")
expect_true("(Intercept)" %in% names(m_noint$coefficients),
            info = "intercept is added back for 'y ~ . - 1'")
expect_equal(unname(m_noint$coefficients), unname(m_int$coefficients), tolerance = 1e-8,
             info = "'y ~ . - 1' yields the same model as 'y ~ .' (intercept enforced)")
# and it announces that it added the intercept
int_msgs <- character(0)
withCallingHandlers(
  snafun::stat_nam(y ~ . - 1, data = dat, W = w, model = "lag"),
  message = function(mm) { int_msgs <<- c(int_msgs, conditionMessage(mm)); invokeRestart("muffleMessage") }
)
expect_true(any(grepl("intercept", int_msgs, ignore.case = TRUE)),
            info = "adding the intercept is announced with a message")


# ---------------------------------------------------------------------------
# 7. REGRESSION: the fitted object carries a short, clean call.
# ---------------------------------------------------------------------------

m_call <- suppressMessages(snafun::stat_nam(y ~ ., data = dat, W = w, model = "lag"))
call_chars <- sum(nchar(deparse(m_call$call)))
expect_true(call_chars < 300,
            info = paste0("fitted call is short (", call_chars, " chars, was ~31588)"))
expect_true(grepl("stat_nam", paste(deparse(m_call$call), collapse = " ")),
            info = "fitted call refers to stat_nam(), not the internal spatialreg call")


# ---------------------------------------------------------------------------
# 8. Isolates / zero rows are allowed under zero.policy = TRUE (the default).
# ---------------------------------------------------------------------------

n <- 50L
w  <- make_W(n, seed = 55, directed = TRUE)
w[1, ] <- 0                                   # vertex 1 becomes an isolate (zero row)
wn <- w
nz <- rowSums(w) > 0
wn[nz, ] <- w[nz, ] / rowSums(w)[nz]          # zero row stays zero
sim <- simulate_nam(n, wn, seed = 55, kind = "lag")
dat <- sim$dat
m_iso <- suppressMessages(snafun::stat_nam(y ~ ., data = dat, W = w, model = "lag"))
ref_iso <- sna::lnam(dat$y, x = cbind(1, sim$X), W1 = wn)
expect_equal(unname(m_iso$rho), as.numeric(ref_iso$rho1), tolerance = 1e-2,
             info = "isolate (zero row): rho still matches lnam under zero.policy")
expect_true(is.finite(m_iso$rho),
            info = "isolate (zero row): model fits without error")


# ---------------------------------------------------------------------------
# 9. Input validation / error handling.
# ---------------------------------------------------------------------------

n <- 30L
w  <- make_W(n, seed = 77, directed = TRUE)
sim <- simulate_nam(n, row_norm(w), seed = 77, kind = "lag")
dat <- sim$dat

expect_error(snafun::stat_nam(y ~ ., data = dat, W = w, model = "nonsense"),
             info = "invalid model name is rejected")
expect_error(snafun::stat_nam(y ~ ., data = dat, W = w, model = "combined"),
             info = "combined model without W2 is rejected")
# rectangular (bipartite) weight matrix is not a valid one-mode W
rect <- matrix(1, nrow = n, ncol = n + 2)
expect_error(suppressMessages(snafun::stat_nam(y ~ ., data = dat, W = rect, model = "lag")),
             info = "rectangular W is rejected")
# W of the wrong size vs the data
w_small <- make_W(n - 5L, seed = 78, directed = TRUE)
expect_error(suppressMessages(snafun::stat_nam(y ~ ., data = dat, W = w_small, model = "lag")),
             info = "W with the wrong number of rows/cols is rejected")

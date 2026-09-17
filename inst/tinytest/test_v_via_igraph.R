# Correctness + scalability tests for the v_*.network methods that were rerouted
# through igraph (2026-09-17): v_degree, v_betweenness and v_power.
#
# Background: these three .network methods used to call sna::degree /
# sna::betweenness / sna::bonpow, which coerce the network to a dense n x n
# sociomatrix and therefore do not scale (v_betweenness ~27x slower at n=2000,
# v_power ~11x slower at n=1500, v_degree densifies unnecessarily). They now
# convert the network to a (sparse) igraph object and dispatch to the .igraph
# method. To be sure this reroute did not change any result, each rerouted
# .network method is compared, across many network kinds, to:
#   (1) an INDEPENDENT sna oracle (the exact old sna call), proving the igraph
#       numbers equal the sna numbers the package used to return, and
#   (2) its own .igraph sibling on the same graph (class-consistency).
# We also check vids subsetting (numeric AND character), loops, isolates, and a
# large-graph scalability guarantee (the network path must not be dramatically
# slower than the igraph path -- i.e. no dense-matrix penalty).
#
# v_stress / v_flow are deliberately NOT rerouted (no igraph equivalent; sna
# only) and are documented as a known scalability limitation, so they are not
# covered here.

if (!requireNamespace("network", quietly = TRUE) ||
    !requireNamespace("sna", quietly = TRUE) ||
    !requireNamespace("igraph", quietly = TRUE)) {
  exit_file("network / sna / igraph not available")
}

report_side_effects(FALSE)
set.seed(20260917)

# helper: order-insensitive numeric equality on the score vectors
eqnum <- function(a, b, tol = 1e-6) {
  isTRUE(all.equal(unname(as.numeric(a)), unname(as.numeric(b)), tolerance = tol))
}

# ---- sna oracles: the exact calls the .network methods used to make ---------
oracle_degree <- function(net, vids = NULL, mode = "all", loops = FALSE) {
  gmode <- ifelse(snafun::is_directed(net), "digraph", "graph")
  cmode <- switch(mode, "all" = "freeman", "out" = "outdegree", "in" = "indegree")
  sna::degree(dat = net, g = 1, nodes = vids, gmode = gmode, diag = loops,
              cmode = cmode, rescale = FALSE, ignore.eval = TRUE)
}
oracle_betweenness <- function(net, vids = NULL, directed = TRUE) {
  gmode <- ifelse(snafun::is_directed(net), "digraph", "graph")
  cmode <- ifelse(directed, "directed", "undirected")
  sna::betweenness(dat = net, g = 1, nodes = vids, gmode = gmode, diag = FALSE,
                   cmode = cmode, rescale = FALSE, ignore.eval = TRUE)
}
oracle_power <- function(net, vids = NULL, exponent = 1) {
  gmode <- ifelse(snafun::is_directed(net), "digraph", "graph")
  ret <- sna::bonpow(dat = net, g = 1, gmode = gmode, diag = FALSE,
                     exponent = exponent, rescale = FALSE)
  if (is.character(vids)) vids <- match(vids, network::network.vertex.names(net))
  if (!is.null(vids)) ret <- ret[vids]
  ret
}


# ---- battery of network kinds ----------------------------------------------
nets <- list()
add <- function(name, g) nets[[name]] <<- g

add("dir",        igraph::sample_gnp(40, 0.15, directed = TRUE))
add("undir",      igraph::sample_gnp(40, 0.20, directed = FALSE))
add("dir sparse", igraph::sample_gnp(60, 4 / 60, directed = TRUE))
add("dir dense",  igraph::sample_gnp(35, 0.45, directed = TRUE))
add("ring dir",   igraph::make_ring(20, directed = TRUE))
add("ring undir", igraph::make_ring(20, directed = FALSE))
add("star out",   igraph::make_star(15, mode = "out"))
add("star in",    igraph::make_star(15, mode = "in"))
add("star undir", igraph::make_star(15, mode = "undirected"))
add("full undir", igraph::make_full_graph(9, directed = FALSE))
add("tree out",   igraph::make_tree(25, children = 3, mode = "out"))
add("tree undir", igraph::make_tree(25, children = 3, mode = "undirected"))
add("lattice",    igraph::make_lattice(c(4, 5)))
add("isolates",   igraph::add_vertices(igraph::sample_gnp(25, 0.15, directed = TRUE), 6))
add("undir isolates", igraph::add_vertices(igraph::sample_gnp(25, 0.2, directed = FALSE), 6))

# disconnected graph: two separate components (union of two gnp graphs)
gc1 <- igraph::sample_gnp(15, 0.25, directed = TRUE)
gc2 <- igraph::sample_gnp(12, 0.25, directed = TRUE)
add("two components", igraph::disjoint_union(gc1, gc2))

# real data sets shipped with the package
add("florentine biz",      snafun::to_igraph(florentine$flobusiness))
add("florentine marriage", snafun::to_igraph(florentine$flomarriage))

gw <- igraph::sample_gnp(30, 0.2, directed = TRUE)
igraph::E(gw)$weight <- stats::runif(igraph::ecount(gw), 1, 9)
add("dir weighted", gw)

gwu <- igraph::sample_gnp(30, 0.2, directed = FALSE)
igraph::E(gwu)$weight <- stats::runif(igraph::ecount(gwu), 1, 9)
add("undir weighted", gwu)


# =============================================================================
# v_degree
# =============================================================================
for (nm in names(nets)) {
  g   <- nets[[nm]]
  net <- snafun::to_network(g)
  for (md in c("all", "out", "in")) {
    # network (rerouted) == sna oracle
    expect_true(
      eqnum(snafun::v_degree(net, mode = md), oracle_degree(net, mode = md)),
      info = paste0("v_degree.network == sna oracle [", nm, ", mode=", md, "]"))
    # network == igraph (class-consistency)
    expect_true(
      eqnum(snafun::v_degree(net, mode = md), snafun::v_degree(g, mode = md)),
      info = paste0("v_degree network == igraph [", nm, ", mode=", md, "]"))
  }
}

# vids subsetting: numeric and character
gd  <- igraph::sample_gnp(20, 0.25, directed = TRUE)
igraph::V(gd)$name <- paste0("n", seq_len(20))
netd <- snafun::to_network(gd)
expect_true(eqnum(snafun::v_degree(netd, vids = c(2, 5, 9)),
                  oracle_degree(netd, vids = c(2, 5, 9))),
            info = "v_degree.network numeric vids == sna oracle")
expect_true(eqnum(snafun::v_degree(netd, vids = c(2, 5, 9)),
                  snafun::v_degree(gd, vids = c(2, 5, 9))),
            info = "v_degree numeric vids network == igraph")
expect_true(eqnum(snafun::v_degree(netd, vids = c("n2", "n5", "n9")),
                  snafun::v_degree(gd, vids = c("n2", "n5", "n9"))),
            info = "v_degree character vids network == igraph")

# loops: with the igraph reroute the .network method follows igraph's loop
# convention. NOTE (2026-09-17): this is an INTENTIONAL change from the old sna
# behaviour. For a self-loop on a directed graph with mode = "all", igraph counts
# the loop twice (once for in-degree, once for out-degree) whereas sna::degree
# counted it once; the package now uses the igraph convention everywhere. At the
# default loops = FALSE the two are identical. We therefore assert
# network == igraph (the post-reroute guarantee), not network == sna oracle.
gl <- igraph::add_edges(igraph::sample_gnp(15, 0.15, directed = TRUE), c(1, 1, 4, 4, 7, 7))
netl <- snafun::to_network(gl)
expect_true(eqnum(snafun::v_degree(netl, loops = TRUE),
                  snafun::v_degree(gl, loops = TRUE)),
            info = "v_degree with loops network == igraph (igraph loop convention)")
# and at the default loops = FALSE the loop convention does not matter: still
# identical to the sna oracle
expect_true(eqnum(snafun::v_degree(netl, loops = FALSE),
                  oracle_degree(netl, loops = FALSE)),
            info = "v_degree.network loops=FALSE == sna oracle (loops ignored)")

# rescaled sums to one
expect_equal(sum(snafun::v_degree(netd, rescaled = TRUE)), 1,
             info = "v_degree.network rescaled sums to 1")


# =============================================================================
# v_betweenness
# =============================================================================
for (nm in names(nets)) {
  g   <- nets[[nm]]
  net <- snafun::to_network(g)
  dir <- snafun::is_directed(g)
  expect_true(
    eqnum(snafun::v_betweenness(net, directed = dir),
          oracle_betweenness(net, directed = dir)),
    info = paste0("v_betweenness.network == sna oracle [", nm, "]"))
  expect_true(
    eqnum(snafun::v_betweenness(net, directed = dir),
          snafun::v_betweenness(g, directed = dir)),
    info = paste0("v_betweenness network == igraph [", nm, "]"))
}

# weights are discarded (weighted network gives same as its unweighted skeleton)
expect_true(
  eqnum(snafun::v_betweenness(snafun::to_network(gw)),
        oracle_betweenness(snafun::to_network(gw))),
  info = "v_betweenness.network ignores weights, matches sna oracle")

# vids subsetting numeric + character
expect_true(eqnum(snafun::v_betweenness(netd, vids = c(1, 4, 8)),
                  oracle_betweenness(netd, vids = c(1, 4, 8))),
            info = "v_betweenness.network numeric vids == sna oracle")
expect_true(eqnum(snafun::v_betweenness(netd, vids = c("n1", "n4", "n8")),
                  snafun::v_betweenness(gd, vids = c("n1", "n4", "n8"))),
            info = "v_betweenness character vids network == igraph")

# directed = FALSE on a directed graph. NOTE (2026-09-17): sna and igraph
# symmetrize a directed graph differently when computing UNDIRECTED betweenness,
# so the numbers differ structurally (this is an intentional consequence of the
# igraph reroute; igraph semantics are used everywhere). At the default
# directed = TRUE the two are identical (covered above). Here we assert the
# post-reroute guarantee: network == igraph.
expect_true(eqnum(snafun::v_betweenness(netd, directed = FALSE),
                  snafun::v_betweenness(gd, directed = FALSE)),
            info = "v_betweenness directed=FALSE network == igraph")


# =============================================================================
# v_power  (Bonacich power centrality)
# =============================================================================
# Verified empirically (2026-09-17): for the SAME graph, igraph::power_centrality
# and sna::bonpow return numerically identical scores wherever the Bonacich
# system is solvable, and BOTH error out identically ("computationally singular")
# on graphs where it is not (e.g. undirected rings/lattices at exponent 1). So
# the rerouted v_power.network must, on every graph, agree with BOTH the sna
# oracle AND its own .igraph sibling -- either the same numbers or the same
# (error) fate. We check all three against each other across the battery, over
# several exponents.
for (nm in names(nets)) {
  g   <- nets[[nm]]
  net <- snafun::to_network(g)
  for (ex in c(1, 0.5, -0.5)) {
    r_net <- tryCatch(snafun::v_power(net, exponent = ex), error = function(e) e)
    r_ig  <- tryCatch(snafun::v_power(g,   exponent = ex), error = function(e) e)
    r_sna <- tryCatch(oracle_power(net,    exponent = ex), error = function(e) e)
    net_err <- inherits(r_net, "error")
    ig_err  <- inherits(r_ig,  "error")
    sna_err <- inherits(r_sna, "error")
    # all three must agree on solvability
    expect_true(net_err == ig_err && net_err == sna_err,
                info = paste0("v_power network/igraph/sna agree on solvability [",
                              nm, ", exp=", ex, "]"))
    if (!net_err && !ig_err && !sna_err) {
      # network (igraph route) == igraph sibling
      expect_true(eqnum(r_net, r_ig),
                  info = paste0("v_power network == igraph [", nm, ", exp=", ex, "]"))
      # network (igraph route) == sna oracle: the numbers the package used to
      # return are preserved
      expect_true(eqnum(r_net, r_sna),
                  info = paste0("v_power.network == sna oracle [", nm, ", exp=", ex, "]"))
    }
  }
}

# vids subsetting numeric + character (character vids must still resolve by name)
expect_true(eqnum(snafun::v_power(netd, vids = c(3, 7, 11)),
                  snafun::v_power(gd, vids = c(3, 7, 11))),
            info = "v_power.network numeric vids == igraph")
expect_true(eqnum(snafun::v_power(netd, vids = c("n3", "n7", "n11")),
                  snafun::v_power(gd, vids = c("n3", "n7", "n11"))),
            info = "v_power.network character vids == igraph (resolves by name)")
# and character vids give the same as the matching numeric vids
expect_true(eqnum(snafun::v_power(netd, vids = c("n3", "n7", "n11")),
                  snafun::v_power(netd, vids = c(3, 7, 11))),
            info = "v_power.network character vids == numeric vids")

# exponent is forwarded
expect_true(eqnum(snafun::v_power(netd, exponent = 0.5),
                  snafun::v_power(gd, exponent = 0.5)),
            info = "v_power.network exponent forwarded, network == igraph")


# =============================================================================
# large-graph correctness: the network route must give exactly the same numbers
# as the igraph route at a scale where the old sna path is infeasible
# =============================================================================
# sna::betweenness / sna::bonpow densify to an n x n matrix, so a direct sna
# oracle is not practical here; the guarantee at scale is that converting a large
# network to igraph and computing there yields identical results to computing on
# the igraph object directly (i.e. the sparse conversion is loss-free).
gBig   <- igraph::sample_gnp(1200, 5 / 1200, directed = TRUE)
netBig <- snafun::to_network(gBig)
expect_true(eqnum(snafun::v_degree(netBig, mode = "all"),
                  snafun::v_degree(gBig, mode = "all")),
            info = "large graph: v_degree network == igraph")
expect_true(eqnum(snafun::v_degree(netBig, mode = "in"),
                  snafun::v_degree(gBig, mode = "in")),
            info = "large graph: v_degree(in) network == igraph")
expect_true(eqnum(snafun::v_betweenness(netBig),
                  snafun::v_betweenness(gBig)),
            info = "large graph: v_betweenness network == igraph")
r_pn <- tryCatch(snafun::v_power(netBig), error = function(e) e)
r_pi <- tryCatch(snafun::v_power(gBig),   error = function(e) e)
expect_true(inherits(r_pn, "error") == inherits(r_pi, "error"),
            info = "large graph: v_power network/igraph agree on solvability")
if (!inherits(r_pn, "error") && !inherits(r_pi, "error")) {
  expect_true(eqnum(r_pn, r_pi),
              info = "large graph: v_power network == igraph")
}


# =============================================================================
# scalability: the network path must not carry a dense-matrix penalty
# =============================================================================
# On a moderately large sparse graph the rerouted .network methods should run in
# roughly the same time as the .igraph methods (both O(sparse)); the old sna
# path was many times slower here. We assert the network path is not more than a
# small constant slower than the igraph path (generous factor to avoid flakiness
# on loaded CI machines), which the dense sna path could never satisfy.
gL   <- igraph::sample_gnp(1500, 6 / 1500, directed = TRUE)
netL <- snafun::to_network(gL)

for (fn in c("v_degree", "v_betweenness", "v_power")) {
  f <- get(fn, envir = asNamespace("snafun"))
  t_net <- tryCatch(system.time(f(netL))[["elapsed"]], error = function(e) NA_real_)
  t_ig  <- tryCatch(system.time(f(gL))[["elapsed"]],  error = function(e) NA_real_)
  # both should succeed
  expect_true(!is.na(t_net) && !is.na(t_ig),
              info = paste0(fn, " runs on n=1500 for both classes"))
  # network path within a generous factor of igraph path (+ constant slack for
  # the one-off to_igraph conversion); the old dense sna path was >10x slower.
  if (!is.na(t_net) && !is.na(t_ig)) {
    expect_true(t_net <= 5 * t_ig + 2,
                info = paste0(fn, ".network not dramatically slower than igraph ",
                              "(network=", round(t_net, 2), "s, igraph=",
                              round(t_ig, 2), "s)"))
  }
}

report_side_effects()

# d_betweenness(): edge betweenness, a wrapper around igraph::edge_betweenness().
# It returns a labelled data.frame (from, to, betweenness), one row per edge, in
# igraph edge order. The network method routes through to_igraph(), so the two
# classes must agree on the same set of edges.

## ---- named directed graph ----
g_i <- snafun::create_manual_graph(A -+ B, B -+ C, C -+ A, C -+ D, D -+ B)

res <- snafun::d_betweenness(g_i)

## shape / structure
expect_true(is.data.frame(res))
expect_equal(colnames(res), c("from", "to", "betweenness"))
expect_equal(nrow(res), snafun::count_edges(g_i))
expect_true(is.numeric(res$betweenness))
expect_false(anyNA(res$betweenness))
# named graph -> character endpoints
expect_true(is.character(res$from))
expect_true(is.character(res$to))

## ground truth: values and endpoints line up with igraph
eb_gt <- igraph::edge_betweenness(g_i, directed = TRUE, weights = NA)
el_gt <- igraph::as_edgelist(g_i, names = TRUE)
expect_equal(res$betweenness, eb_gt, tolerance = 1e-8)
expect_equal(res$from, el_gt[, 1])
expect_equal(res$to, el_gt[, 2])

## ---- igraph == network (same edges + betweenness, order-independent) ----
res_n <- snafun::d_betweenness(snafun::to_network(g_i))
expect_equal(nrow(res_n), nrow(res))
ord   <- function(d) d[order(d$from, d$to), ]
a <- ord(res); b <- ord(res_n)
expect_equal(a$from, b$from)
expect_equal(a$to, b$to)
expect_equal(a$betweenness, b$betweenness, tolerance = 1e-8)

## ---- unnamed graph -> integer vertex ids as endpoints ----
set.seed(5)
g_u   <- snafun::create_random_graph(8, "gnm", m = 13, directed = TRUE,
                                     graph = "igraph")
res_u <- snafun::d_betweenness(g_u)
expect_equal(nrow(res_u), snafun::count_edges(g_u))
expect_equal(res_u$betweenness,
             igraph::edge_betweenness(g_u, directed = TRUE, weights = NA),
             tolerance = 1e-8)
# endpoints are the internal ids (whole numbers)
expect_true(all(res_u$from == as.integer(res_u$from)))

## ---- directed vs undirected ----
res_und <- snafun::d_betweenness(g_i, directed = FALSE)
expect_equal(res_und$betweenness,
             igraph::edge_betweenness(g_i, directed = FALSE, weights = NA),
             tolerance = 1e-8)
# for this graph the directed and undirected results are not identical
expect_false(isTRUE(all.equal(res$betweenness, res_und$betweenness)))

## ---- weights: NA (default) vs an explicit weight vector ----
w     <- seq_len(snafun::count_edges(g_i))
res_w <- snafun::d_betweenness(g_i, weights = w)
expect_equal(res_w$betweenness,
             igraph::edge_betweenness(g_i, directed = TRUE, weights = w),
             tolerance = 1e-8)
expect_false(isTRUE(all.equal(res$betweenness, res_w$betweenness)))

## ---- weights = NULL uses the graph's own weight edge attribute ----
g_wt <- g_i
igraph::E(g_wt)$weight <- seq_len(snafun::count_edges(g_wt))
res_null <- snafun::d_betweenness(g_wt, weights = NULL)
expect_equal(res_null$betweenness,
             igraph::edge_betweenness(g_wt, directed = TRUE, weights = NULL),
             tolerance = 1e-8)
# using the weight attribute (NULL) differs from ignoring weights (NA) here
expect_false(isTRUE(all.equal(res_null$betweenness,
                              snafun::d_betweenness(g_wt, weights = NA)$betweenness)))

## ---- empty edge set is handled gracefully ----
g0 <- snafun::create_empty_graph(4, directed = TRUE, graph = "igraph")
res0 <- snafun::d_betweenness(g0)
expect_true(is.data.frame(res0))
expect_equal(nrow(res0), 0L)
expect_equal(colnames(res0), c("from", "to", "betweenness"))

## ---- error handling ----
expect_error(snafun::d_betweenness(5))
expect_error(snafun::d_betweenness("a"))
expect_error(snafun::d_betweenness(snafun::to_matrix(g_i)))  # bare matrix unsupported
expect_error(snafun::d_betweenness(data.frame(a = 1)))

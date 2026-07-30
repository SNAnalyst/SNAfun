report_side_effects()

# v_power(): Bonacich power centrality. Computed class-native: igraph objects via
# igraph::power_centrality(), network objects via sna::bonpow(). The two engines
# return identical values, so a central point of these tests is that the igraph
# and network paths agree, and that both agree with their underlying engine.

set.seed(1)
g_i <- snafun::create_random_graph(12, "gnm", m = 22, directed = TRUE,
                                   graph = "igraph")
g_n <- snafun::to_network(g_i)
mat <- snafun::to_matrix(g_i)

## ---- basic shape ----
p_i <- snafun::v_power(g_i)
expect_true(is.numeric(p_i))
expect_equal(length(p_i), snafun::count_vertices(g_i))
expect_false(anyNA(p_i))

## ---- igraph == network (class-native but identical results) ----
p_n <- snafun::v_power(g_n)
expect_equal(unname(p_i), unname(p_n), tolerance = 1e-8)

## ---- ground truth against each engine ----
expect_equal(unname(p_i),
             unname(igraph::power_centrality(g_i, exponent = 1, loops = FALSE,
                                             rescale = FALSE)),
             tolerance = 1e-8)
expect_equal(unname(p_n),
             unname(sna::bonpow(mat, gmode = "digraph", diag = FALSE,
                                exponent = 1, rescale = FALSE)),
             tolerance = 1e-8)

## ---- exponent variants: igraph and network agree for several betas ----
for (b in c(-0.5, 0, 0.5)) {
  expect_equal(unname(snafun::v_power(g_i, exponent = b)),
               unname(snafun::v_power(g_n, exponent = b)),
               tolerance = 1e-8)
}
# a different exponent gives genuinely different scores
expect_false(isTRUE(all.equal(unname(snafun::v_power(g_i, exponent = 0)),
                              unname(p_i))))
# exponent = 0 matches the igraph engine at exponent 0 too
expect_equal(unname(snafun::v_power(g_i, exponent = 0)),
             unname(igraph::power_centrality(g_i, exponent = 0, loops = FALSE,
                                             rescale = FALSE)),
             tolerance = 1e-8)

## ---- vids subsetting ----
sub   <- c(2, 4, 6)
p_sub <- snafun::v_power(g_i, vids = sub)
expect_equal(length(p_sub), length(sub))
expect_equal(unname(p_sub), unname(p_i[sub]), tolerance = 1e-8)
expect_equal(unname(snafun::v_power(g_n, vids = sub)), unname(p_i[sub]),
             tolerance = 1e-8)
expect_equal(unname(snafun::v_power(g_i, vids = 5)), unname(p_i[5]),
             tolerance = 1e-8)

## ---- rescaled: scores divided by their sum (sum here is non-zero) ----
expect_true(abs(sum(p_i)) > 1e-6)
p_res <- snafun::v_power(g_i, rescaled = TRUE)
expect_equal(sum(p_res), 1, tolerance = 1e-8)
expect_equal(unname(p_res), unname(p_i / sum(p_i)), tolerance = 1e-8)

## ---- undirected graph: gmode follows the graph ----
set.seed(3)
g_u  <- snafun::create_random_graph(9, "gnm", m = 14, directed = FALSE,
                                    graph = "igraph")
pu_i <- snafun::v_power(g_u, exponent = 0.3)
pu_n <- snafun::v_power(snafun::to_network(g_u), exponent = 0.3)
expect_equal(unname(pu_i), unname(pu_n), tolerance = 1e-8)
expect_equal(unname(pu_i),
             unname(igraph::power_centrality(g_u, exponent = 0.3, loops = FALSE,
                                             rescale = FALSE)),
             tolerance = 1e-8)

## ---- vids given as vertex NAMES (character), igraph and network ----
g_nm  <- snafun::create_manual_graph(A -+ B, B -+ C, C -+ A, C -+ D, D -+ B)
g_nmn <- snafun::to_network(g_nm)
full  <- snafun::v_power(g_nm, exponent = 0.2)          # named numeric vector
expect_true(!is.null(names(full)))
expect_equal(unname(snafun::v_power(g_nm, vids = c("A", "C"), exponent = 0.2)),
             unname(full[c("A", "C")]), tolerance = 1e-8)
# the network path translates the names to ids and must give the same subset
expect_equal(unname(snafun::v_power(g_nmn, vids = c("A", "C"), exponent = 0.2)),
             unname(full[c("A", "C")]), tolerance = 1e-8)
# a single name works too
expect_equal(unname(snafun::v_power(g_nmn, vids = "C", exponent = 0.2)),
             unname(full["C"]), tolerance = 1e-8)

## ---- error handling ----
expect_error(snafun::v_power(5))
expect_error(snafun::v_power("a"))
expect_error(snafun::v_power(mat))                  # a bare matrix is not supported
expect_error(snafun::v_power(data.frame(a = 1)))

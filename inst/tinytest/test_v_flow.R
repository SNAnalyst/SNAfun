report_side_effects()

# v_flow(): flow betweenness, a thin wrapper around sna::flowbet(). The tests
# below check: output shape, igraph == network, agreement with the sna ground
# truth for every `mode`, the `rescaled` convention, vids subsetting, undirected
# handling, and error handling for unsupported input.

set.seed(42)
g_i  <- snafun::create_random_graph(12, strategy = "gnm", m = 22,
                                    directed = TRUE, graph = "igraph")
g_n  <- snafun::to_network(g_i)
mat  <- snafun::to_matrix(g_i)

## ---- basic shape ----
fl_i <- snafun::v_flow(g_i)
expect_true(is.numeric(fl_i))
expect_equal(length(fl_i), snafun::count_vertices(g_i))
expect_true(all(fl_i >= 0))
expect_false(anyNA(fl_i))

## ---- igraph == network ----
fl_n <- snafun::v_flow(g_n)
expect_equal(unname(fl_i), unname(fl_n), tolerance = 1e-8)

## ---- ground truth: matches sna::flowbet for each mode ----
gt_raw  <- sna::flowbet(mat, gmode = "digraph", cmode = "rawflow",
                        rescale = FALSE, ignore.eval = TRUE)
gt_norm <- sna::flowbet(mat, gmode = "digraph", cmode = "normflow",
                        rescale = FALSE, ignore.eval = TRUE)
gt_frac <- sna::flowbet(mat, gmode = "digraph", cmode = "fracflow",
                        rescale = FALSE, ignore.eval = TRUE)

expect_equal(unname(fl_i), unname(gt_raw), tolerance = 1e-8)
expect_equal(unname(snafun::v_flow(g_i, mode = "normflow")), unname(gt_norm),
             tolerance = 1e-8)
expect_equal(unname(snafun::v_flow(g_i, mode = "fracflow")), unname(gt_frac),
             tolerance = 1e-8)
# and network path gives the same for a non-default mode too
expect_equal(unname(snafun::v_flow(g_n, mode = "fracflow")), unname(gt_frac),
             tolerance = 1e-8)

## ---- the three modes are genuinely different here ----
expect_false(isTRUE(all.equal(unname(gt_raw), unname(gt_norm))))
expect_false(isTRUE(all.equal(unname(gt_raw), unname(gt_frac))))

## ---- rescaled convention: scores sum to 1 ----
fl_res <- snafun::v_flow(g_i, rescaled = TRUE)
expect_equal(sum(fl_res), 1, tolerance = 1e-8)
expect_equal(length(fl_res), snafun::count_vertices(g_i))
# rescaling preserves the relative ordering of the raw scores
expect_equal(order(fl_res), order(fl_i))

## ---- vids subsetting ----
sub    <- c(1, 3, 5, 8)
fl_sub <- snafun::v_flow(g_i, vids = sub)
expect_equal(length(fl_sub), length(sub))
expect_equal(unname(fl_sub), unname(fl_i[sub]), tolerance = 1e-8)
# a single vid
expect_equal(unname(snafun::v_flow(g_i, vids = 5)), unname(fl_i[5]),
             tolerance = 1e-8)

## ---- undirected graph: direction is taken from the graph (gmode = "graph") ----
set.seed(7)
g_u   <- snafun::create_random_graph(10, strategy = "gnm", m = 15,
                                     directed = FALSE, graph = "igraph")
mat_u <- snafun::to_matrix(g_u)
fl_u  <- snafun::v_flow(g_u)
expect_equal(unname(fl_u),
             unname(sna::flowbet(mat_u, gmode = "graph", cmode = "rawflow",
                                 rescale = FALSE, ignore.eval = TRUE)),
             tolerance = 1e-8)
expect_equal(unname(fl_u), unname(snafun::v_flow(snafun::to_network(g_u))),
             tolerance = 1e-8)

## ---- error handling: unsupported input dispatches to the default method ----
expect_error(snafun::v_flow(5))
expect_error(snafun::v_flow("a"))
expect_error(snafun::v_flow(mat))                 # a bare matrix is not supported
expect_error(snafun::v_flow(data.frame(a = 1)))
# an invalid mode is rejected
expect_error(snafun::v_flow(g_i, mode = "bogus"))

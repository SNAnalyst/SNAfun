
##  ............................................................................
##  TESTS                                                                   ####

report_side_effects()

g <- snafun::create_random_graph(20, "gnm", m = 100) |> snafun::to_matrix()
g1 <- make_perturbed_graph(g, prob_tot = .3)
expect_identical(diag(g), diag(g1))
expect_identical(class(g), class(g1))

g2 <- make_perturbed_graph(g, prob_0_to_1 = 1)
expect_identical(diag(g), diag(g2))
expect_true(snafun::g_density(g2) == 1)

g3 <- make_perturbed_graph(g, prob_1_to_0 = 1)
expect_identical(diag(g), diag(g3))
expect_true(snafun::g_density(g3) == 0)

g4 <- make_perturbed_graph(g, prob_tot = .3, prob_0_to_1 = 1, combined = TRUE)
expect_identical(diag(g), diag(g4))
expect_true(snafun::g_density(g4) < 1)

g5 <- make_perturbed_graph(g, prob_tot = 0, prob_0_to_1 = 1, combined = TRUE)
expect_identical(diag(g), diag(g5))
expect_true(snafun::g_density(g5) == 1)

expect_inherits(make_perturbed_graph(snafun::to_igraph(g), prob_tot = .3), "igraph")
expect_inherits(make_perturbed_graph(snafun::to_network(g), prob_tot = .3), "network")

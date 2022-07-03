
# repeat the same procedure a few times, to allow for random 
# variation in the generated networks

g <- igraph::barabasi.game(10)
expect_identical(v_geokpath(g) |> unname(),
                 v_geokpath(snafun::to_network(g)) |> unname())
g <- igraph::barabasi.game(10)
expect_identical(v_geokpath(g) |> unname(),
                 v_geokpath(snafun::to_network(g)) |> unname())
g <- igraph::barabasi.game(10)
expect_identical(v_geokpath(g) |> unname(),
                 v_geokpath(snafun::to_network(g)) |> unname())


g <- igraph::barabasi.game(100)
expect_identical(v_geokpath(g) |> unname(),
                 v_geokpath(snafun::to_network(g)) |> unname())
g <- igraph::barabasi.game(100)
expect_identical(v_geokpath(g) |> unname(),
                 v_geokpath(snafun::to_network(g)) |> unname())
g <- igraph::barabasi.game(100)
expect_identical(v_geokpath(g) |> unname(),
                 v_geokpath(snafun::to_network(g)) |> unname())


g <- sna::rgnm(n = 1, nv = 50, m = 30, mode = "digraph") |> 
  network::as.network(directed = TRUE)
expect_identical(v_geokpath(g) |> unname(),
                 v_geokpath(snafun::to_igraph(g)) |> unname())
g <- sna::rgnm(n = 1, nv = 50, m = 30, mode = "digraph") |> 
  network::as.network(directed = TRUE)
expect_identical(v_geokpath(g) |> unname(),
                 v_geokpath(snafun::to_igraph(g)) |> unname())
g <- sna::rgnm(n = 1, nv = 50, m = 30, mode = "digraph") |> 
  network::as.network(directed = TRUE)
expect_identical(v_geokpath(g) |> unname(),
                 v_geokpath(snafun::to_igraph(g)) |> unname())



g <- sna::rgnm(n = 1, nv = 50, m = 30, mode = "graph") |> 
  network::as.network(directed = FALSE)
expect_identical(v_geokpath(g) |> unname(),
                 v_geokpath(snafun::to_igraph(g)) |> unname())
g <- sna::rgnm(n = 1, nv = 50, m = 30, mode = "graph") |> 
  network::as.network(directed = FALSE)
expect_identical(v_geokpath(g) |> unname(),
                 v_geokpath(snafun::to_igraph(g)) |> unname())
g <- sna::rgnm(n = 1, nv = 50, m = 30, mode = "graph") |> 
  network::as.network(directed = FALSE)
expect_identical(v_geokpath(g) |> unname(),
                 v_geokpath(snafun::to_igraph(g)) |> unname())


data(florentine, package = "snafun")
data(judge_net, package = "snafun")

g1_i <- florentine$flobusiness
g2_i <- florentine$flomarriage
g1_m <- to_matrix(g1_i)
g2_m <- to_matrix(g2_i)
g1_n <- to_network(g1_i)
g2_n <- to_network(g2_i)

# check all comis
kor <- g_correlation(g1_i, g2_i)
expect_equal(kor, g_correlation(g1_i, g2_n))
expect_equal(kor, g_correlation(g1_i, g2_m))
expect_equal(kor, g_correlation(g1_n, g2_i))
expect_equal(kor, g_correlation(g1_n, g2_m))
expect_equal(kor, g_correlation(g1_n, g2_n))
expect_equal(kor, g_correlation(g1_m, g2_i))
expect_equal(kor, g_correlation(g1_m, g2_n))
expect_equal(kor, g_correlation(g1_m, g2_m))
expect_equal(1, g_correlation(g1_i, g1_i))
expect_equal(1, g_correlation(g2_i, g2_i))
expect_equal(1, g_correlation(g1_i, g1_n))
expect_equal(1, g_correlation(g2_i, g2_n))
expect_equal(1, g_correlation(g1_i, g1_m))
expect_equal(1, g_correlation(g2_i, g2_m))
expect_equal(1, g_correlation(g1_n, g1_i))
expect_equal(1, g_correlation(g2_n, g2_i))
expect_equal(1, g_correlation(g1_n, g1_n))
expect_equal(1, g_correlation(g2_n, g2_n))
expect_equal(1, g_correlation(g1_n, g1_m))
expect_equal(1, g_correlation(g2_n, g2_m))
expect_equal(1, g_correlation(g1_m, g1_i))
expect_equal(1, g_correlation(g2_m, g2_i))
expect_equal(1, g_correlation(g1_m, g1_n))
expect_equal(1, g_correlation(g2_m, g2_n))
expect_equal(1, g_correlation(g1_m, g1_m))
expect_equal(1, g_correlation(g2_m, g2_m))
rm(g1_i, g1_n, g1_m, g2_i, g2_m, g2_n, kor)

g1_i <- judge_net
g1_m <- to_matrix(g1_i)
g1_n <- suppressWarnings(to_network(g1_i))
expect_equal(1, g_correlation(g1_i, g1_i))
expect_equal(1, g_correlation(g1_i, g1_n))
expect_equal(1, g_correlation(g1_i, g1_m))
expect_equal(1, g_correlation(g1_n, g1_i))
expect_equal(1, g_correlation(g1_n, g1_n))
expect_equal(1, g_correlation(g1_n, g1_m))
expect_equal(1, g_correlation(g1_m, g1_i))
expect_equal(1, g_correlation(g1_m, g1_n))
expect_equal(1, g_correlation(g1_m, g1_m))
rm(g1_i, g1_n, g1_m, kor)



# started vanaf 'network'
g1_n <- snafun::create_random_graph(50, "gnp", p = .15, directed = TRUE, graph = "network")
g1_m <- to_matrix(g1_n)
g1_i <- to_igraph(g1_n)
g2_n <- snafun::create_random_graph(50, "gnm", m = 300, directed = TRUE, graph = "network")
g2_m <- to_matrix(g2_n)
g2_i <- to_igraph(g2_n)
kor <- g_correlation(g1_i, g2_i)
expect_equal(kor, g_correlation(g1_i, g2_n))
expect_equal(kor, g_correlation(g1_i, g2_m))
expect_equal(kor, g_correlation(g1_n, g2_i))
expect_equal(kor, g_correlation(g1_n, g2_m))
expect_equal(kor, g_correlation(g1_n, g2_n))
expect_equal(kor, g_correlation(g1_m, g2_i))
expect_equal(kor, g_correlation(g1_m, g2_n))
expect_equal(kor, g_correlation(g1_m, g2_m))
expect_equal(1, g_correlation(g1_i, g1_i))
expect_equal(1, g_correlation(g2_i, g2_i))
expect_equal(1, g_correlation(g1_i, g1_n))
expect_equal(1, g_correlation(g2_i, g2_n))
expect_equal(1, g_correlation(g1_i, g1_m))
expect_equal(1, g_correlation(g2_i, g2_m))
expect_equal(1, g_correlation(g1_n, g1_i))
expect_equal(1, g_correlation(g2_n, g2_i))
expect_equal(1, g_correlation(g1_n, g1_n))
expect_equal(1, g_correlation(g2_n, g2_n))
expect_equal(1, g_correlation(g1_n, g1_m))
expect_equal(1, g_correlation(g2_n, g2_m))
expect_equal(1, g_correlation(g1_m, g1_i))
expect_equal(1, g_correlation(g2_m, g2_i))
expect_equal(1, g_correlation(g1_m, g1_n))
expect_equal(1, g_correlation(g2_m, g2_n))
expect_equal(1, g_correlation(g1_m, g1_m))
expect_equal(1, g_correlation(g2_m, g2_m))
rm(g1_i, g1_n, g1_m, g2_i, g2_m, g2_n, kor)





# started vanaf 'network', undirected
g1_n <- snafun::create_random_graph(50, "gnp", p = .15, directed = FALSE, graph = "network")
g1_m <- to_matrix(g1_n)
g1_i <- to_igraph(g1_n)
g2_n <- snafun::create_random_graph(50, "gnm", m = 250, directed = FALSE, graph = "network")
g2_m <- to_matrix(g2_n)
g2_i <- to_igraph(g2_n)
kor <- g_correlation(g1_i, g2_i)
expect_equal(kor, g_correlation(g1_i, g2_n))
expect_equal(kor, g_correlation(g1_i, g2_m))
expect_equal(kor, g_correlation(g1_n, g2_i))
expect_equal(kor, g_correlation(g1_n, g2_m))
expect_equal(kor, g_correlation(g1_n, g2_n))
expect_equal(kor, g_correlation(g1_m, g2_i))
expect_equal(kor, g_correlation(g1_m, g2_n))
expect_equal(kor, g_correlation(g1_m, g2_m))
expect_equal(1, g_correlation(g1_i, g1_i))
expect_equal(1, g_correlation(g2_i, g2_i))
expect_equal(1, g_correlation(g1_i, g1_n))
expect_equal(1, g_correlation(g2_i, g2_n))
expect_equal(1, g_correlation(g1_i, g1_m))
expect_equal(1, g_correlation(g2_i, g2_m))
expect_equal(1, g_correlation(g1_n, g1_i))
expect_equal(1, g_correlation(g2_n, g2_i))
expect_equal(1, g_correlation(g1_n, g1_n))
expect_equal(1, g_correlation(g2_n, g2_n))
expect_equal(1, g_correlation(g1_n, g1_m))
expect_equal(1, g_correlation(g2_n, g2_m))
expect_equal(1, g_correlation(g1_m, g1_i))
expect_equal(1, g_correlation(g2_m, g2_i))
expect_equal(1, g_correlation(g1_m, g1_n))
expect_equal(1, g_correlation(g2_m, g2_n))
expect_equal(1, g_correlation(g1_m, g1_m))
expect_equal(1, g_correlation(g2_m, g2_m))
rm(g1_i, g1_n, g1_m, g2_i, g2_m, g2_n, kor)



# weighted
g <- SNA4DSData::DSstudents
igraph::E(g)$weight = igraph::E(g)$frequency
g1_i <- snafun::extract_subgraph(g, v_to_keep = 1:40)
g2_i <- snafun::extract_subgraph(g, v_to_keep = 41:80)
g1_m <- to_matrix(g1_i)
g2_m <- to_matrix(g2_i)
g1_n <- to_network(g1_i)
g2_n <- to_network(g2_i)
rm(g)
kor <- g_correlation(g1_i, g2_i)
# expect_equal(kor, g_correlation(g1_i, g2_n))
# expect_equal(kor, g_correlation(g1_i, g2_m))
# expect_equal(kor, g_correlation(g1_n, g2_i))
# expect_equal(kor, g_correlation(g1_n, g2_m))
# expect_equal(kor, g_correlation(g1_n, g2_n))
# expect_equal(kor, g_correlation(g1_m, g2_i))
# expect_equal(kor, g_correlation(g1_m, g2_n))
# expect_equal(kor, g_correlation(g1_m, g2_m))
# expect_equal(1, g_correlation(g1_i, g1_i))
# expect_equal(1, g_correlation(g2_i, g2_i))
# 
# expect_equal(1, g_correlation(g1_i, g1_n))
# expect_equal(1, g_correlation(g2_i, g2_n))
# expect_equal(1, g_correlation(g1_i, g1_m))
# expect_equal(1, g_correlation(g2_i, g2_m))
# expect_equal(1, g_correlation(g1_n, g1_i))
# expect_equal(1, g_correlation(g2_n, g2_i))
# expect_equal(1, g_correlation(g1_n, g1_n))
# expect_equal(1, g_correlation(g2_n, g2_n))
# expect_equal(1, g_correlation(g1_n, g1_m))
# expect_equal(1, g_correlation(g2_n, g2_m))
# expect_equal(1, g_correlation(g1_m, g1_i))
# expect_equal(1, g_correlation(g2_m, g2_i))
# expect_equal(1, g_correlation(g1_m, g1_n))
# expect_equal(1, g_correlation(g2_m, g2_n))
# expect_equal(1, g_correlation(g1_m, g1_m))
# expect_equal(1, g_correlation(g2_m, g2_m))
# rm(g1_i, g1_n, g1_m, g2_i, g2_m, g2_n)
# 
# 
# 
# 
# 
# igraph::as_adjacency_matrix(g1_i, attr = "weight")
# mi <- igraph::as_adjacency_matrix(g1_i, attr = "weight", sparse = FALSE)
# mn <- network::as.matrix.network(g1_n, attrname = "weight")
# mm <- to_matrix(g1_i)
# sum(mm != mi) # 0, identiek
# sum(mm != mn)  # 49
# 
# 
# 
# het gaat mis in t-Nnetwork bij 
# network::set.edge.attribute(res, attrname = eat, value = eattrs[, eat], e = eids)
# waar de eids niet lijken te kloppen
# 
# 
# # de nullen staan op dezlfde plek
# to_matrix(mn)[which(to_matrix(mi) == 0, arr.ind = TRUE)] |> sum()
# to_matrix(mi)[which(to_matrix(mn) == 0, arr.ind = TRUE)] |> sum()
# mm[which(to_matrix(mn) == 0, arr.ind = TRUE)] |> sum()
# to_matrix(mn)[which(mm == 0, arr.ind = TRUE)] |> sum()
# sum(mn) == sum(mi)   # zelfde totaal, maar dus op andere plekken
# sum(mn) == sum(mm)
# all(table(mn) == table(mi))
# all(colnames(mi) == rownames(mi))
# all(colnames(mn) == rownames(mn))
# all(colnames(mi) == rownames(mn))
# 
# extract_edge_attribute(g1_i, "weight") == extract_edge_attribute(g1_n, "weight")
# 
# cbind(mi[7, ], mn[7,])
# cbind(mi[17, ], mn[17,])

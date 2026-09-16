# Exhaustive correctness tests for the sparse to_igraph.network() (2026-09-16).
#
# to_igraph.network() no longer materializes a dense sociomatrix
# (sna::as.sociomatrix.sna() + graph_from_*_matrix(), ~4s on enwiki); it now
# builds the graph from the sparse edge list. To be ABSOLUTELY sure the
# conversion stays correct and complete in every circumstance, each result is
# compared -- via an order-independent fingerprint of structure AND all
# attributes -- to an INDEPENDENT reference that reproduces the previous dense
# implementation (the "dense oracle"). Covers directed/undirected, weighted/
# unweighted, vertex attributes, multiple edge attributes, isolates, loops,
# bipartite (weighted/unweighted), named vertices, and a large graph.

if (!requireNamespace("network", quietly = TRUE) ||
    !requireNamespace("sna", quietly = TRUE)) {
  exit_file("network / sna not available")
}

report_side_effects(FALSE)
set.seed(20260916)


# ---- order-independent fingerprint of structure + all attributes ------------
fingerprint <- function(g) {
  nm <- igraph::V(g)$name
  if (is.null(nm)) nm <- as.character(seq_len(igraph::vcount(g)))
  ord_v <- order(nm)
  vattr <- sort(igraph::vertex_attr_names(g))
  vlist <- lapply(vattr, function(a) paste(format(igraph::vertex_attr(g, a)[ord_v]), collapse = "|"))
  em <- igraph::as_edgelist(g, names = TRUE)
  eattr <- sort(igraph::edge_attr_names(g))
  if (nrow(em) == 0) {
    ekeys <- character(0)
  } else {
    endp <- if (igraph::is_directed(g)) {
      paste(em[, 1], em[, 2], sep = "->")
    } else {
      paste(pmin(em[, 1], em[, 2]), pmax(em[, 1], em[, 2]), sep = "--")
    }
    evals <- vapply(seq_len(nrow(em)), function(i) {
      if (length(eattr) == 0) return("")
      paste(vapply(eattr, function(a) format(igraph::edge_attr(g, a)[i]), character(1)), collapse = ",")
    }, character(1))
    ekeys <- sort(paste(endp, evals, sep = "#"))
  }
  list(vcount = igraph::vcount(g), ecount = igraph::ecount(g),
       directed = igraph::is_directed(g), bipartite = igraph::is_bipartite(g),
       weighted = igraph::is_weighted(g), vnames = sort(nm),
       vattr_names = vattr, vattrs = vlist, eattr_names = eattr, edges = ekeys)
}

# ---- dense oracle: the previous to_igraph.network() implementation -----------
to_igraph_dense_reference <- function(x) {
  attr <- names(x[[3]][[1]])
  isna <- which(attr == "na")
  if (length(isna) > 0) attr <- attr[-isna]
  if (network::is.bipartite(x)) {
    if ("weight" %in% network::list.edge.attributes(x)) {
      graph <- sna::as.sociomatrix.sna(x, attrname = "weight")
      graph <- igraph::graph_from_incidence_matrix(graph, weighted = TRUE)
    } else {
      graph <- sna::as.sociomatrix.sna(x)
      graph <- igraph::graph_from_incidence_matrix(graph)
    }
  } else {
    if ("weight" %in% network::list.edge.attributes(x)) {
      graph <- sna::as.sociomatrix.sna(x, attrname = "weight")
      graph <- igraph::graph_from_adjacency_matrix(graph, weighted = TRUE,
                  mode = ifelse(x$gal$directed, "directed", "undirected"))
    } else {
      graph <- sna::as.sociomatrix.sna(x)
      graph <- igraph::graph_from_adjacency_matrix(graph,
                  mode = ifelse(x$gal$directed, "directed", "undirected"))
    }
  }
  if (snafun::has_edge_attributes(x)) {
    graph <- snafun::add_edge_attributes(graph, edgelist = snafun::to_edgelist(x), overwrite = TRUE)
  }
  if (length(attr) > 1) {
    for (a in attr[1:length(attr)]) {
      values <- sapply(x[[3]], "[[", a)
      igraph::vertex_attr(graph, name = a) <- values
    }
  }
  graph
}


# ---- battery of network objects --------------------------------------------
nets <- list()
add <- function(name, net) nets[[name]] <<- net
frm <- function(g) snafun::to_network(g)

add("dir unweighted",   frm(igraph::sample_gnp(40, 0.15, directed = TRUE)))
add("undir unweighted", frm(igraph::sample_gnp(40, 0.2, directed = FALSE)))

gw <- igraph::sample_gnp(30, 0.2, directed = TRUE); igraph::E(gw)$weight <- stats::runif(igraph::ecount(gw), 1, 9)
add("dir weighted", frm(gw))
gwu <- igraph::sample_gnp(30, 0.2, directed = FALSE); igraph::E(gwu)$weight <- stats::runif(igraph::ecount(gwu), 1, 9)
add("undir weighted", frm(gwu))

gv <- igraph::sample_gnp(25, 0.2, directed = TRUE)
igraph::V(gv)$name <- paste0("v", seq_len(25)); igraph::V(gv)$grp <- rep(c("a","b","c","d","e"), 5)
igraph::V(gv)$score <- stats::rnorm(25)
add("dir vertex attrs", frm(gv))

ge <- igraph::sample_gnp(25, 0.2, directed = TRUE)
igraph::E(ge)$weight <- stats::runif(igraph::ecount(ge), 1, 5)
igraph::E(ge)$kind <- sample(c("x", "y"), igraph::ecount(ge), TRUE)
add("dir multi edge attrs", frm(ge))

add("dir isolates",   frm(igraph::add_vertices(igraph::sample_gnp(20, 0.15, directed = TRUE), 6)))
add("undir isolates", frm(igraph::add_vertices(igraph::sample_gnp(20, 0.2, directed = FALSE), 6)))
add("dir loops",      frm(igraph::add_edges(igraph::sample_gnp(18, 0.15, directed = TRUE), c(1,1,3,3))))
add("star out",       frm(igraph::make_star(12, mode = "out")))
add("ring dir",       frm(igraph::make_ring(15, directed = TRUE)))
add("full undir",     frm(igraph::make_full_graph(8, directed = FALSE)))

inc <- matrix(sample(0:1, 6 * 4, replace = TRUE), nrow = 6)
add("bipartite unweighted", snafun::to_network(inc, bipartite = TRUE))
incw <- inc; incw[incw == 1] <- stats::runif(sum(incw == 1), 1, 5)
add("bipartite weighted", snafun::to_network(incw, bipartite = TRUE))

add("large dir", frm(igraph::sample_gnp(400, 5 / 400, directed = TRUE)))


# ---- the core guarantee: sparse == dense oracle, in every case --------------
for (nm in names(nets)) {
  net <- nets[[nm]]
  expect_equal(fingerprint(snafun::to_igraph(net)),
               fingerprint(to_igraph_dense_reference(net)),
               info = paste0("to_igraph.network == dense reference (", nm, ")"))
}


# ---- explicit spot checks --------------------------------------------------

# weights preserved and numerically correct
gw2 <- igraph::sample_gnp(20, 0.3, directed = TRUE)
igraph::E(gw2)$weight <- seq_len(igraph::ecount(gw2))
gi <- snafun::to_igraph(snafun::to_network(gw2))
expect_true(snafun::is_weighted(gi), info = "weights survive to_igraph.network")
# total weight matches the original
expect_equal(sum(igraph::E(gi)$weight), sum(igraph::E(gw2)$weight),
             info = "total edge weight preserved")

# isolates preserved
gi2 <- snafun::to_igraph(snafun::to_network(igraph::add_vertices(igraph::sample_gnp(15, 0.2, directed = TRUE), 5)))
expect_equal(igraph::vcount(gi2), 20L, info = "isolates preserved (vertex count)")

# bipartite type preserved
gb <- snafun::to_igraph(nets[["bipartite unweighted"]])
expect_true(igraph::is_bipartite(gb), info = "bipartite graph stays bipartite")
expect_true("type" %in% igraph::vertex_attr_names(gb), info = "bipartite 'type' attribute present")

# directedness preserved
expect_true(igraph::is_directed(snafun::to_igraph(nets[["dir unweighted"]])),
            info = "directed network -> directed igraph")
expect_false(igraph::is_directed(snafun::to_igraph(nets[["undir unweighted"]])),
             info = "undirected network -> undirected igraph")

# vertex attributes preserved
giv <- snafun::to_igraph(nets[["dir vertex attrs"]])
expect_true(all(c("grp", "score") %in% igraph::vertex_attr_names(giv)),
            info = "vertex attributes preserved")


# ---- scalability: must be fast and correct on a large network --------------
gbig <- igraph::sample_gnp(3000, 8 / 3000, directed = TRUE)
netbig <- snafun::to_network(gbig)
t_big <- system.time(gib <- snafun::to_igraph(netbig))[["elapsed"]]
expect_equal(igraph::vcount(gib), igraph::vcount(gbig), info = "large: vertex count correct")
expect_equal(igraph::ecount(gib), igraph::ecount(gbig), info = "large: edge count correct")
expect_true(t_big < 10,
            info = paste0("large to_igraph.network is fast (", round(t_big, 2), "s)"))

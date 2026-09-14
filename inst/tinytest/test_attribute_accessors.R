# Coverage tests for the attribute accessors and graph-construction helpers that
# previously had no direct tests: add/extract/list/remove graph attributes,
# has_*/remove_* vertex and edge attributes, remove_vertex_names,
# contract_vertices, create_community_graph, create_components_graph.
#
# BUG FOUND & FIXED (2026-09-14): add_graph_attribute() dispatched with
# UseMethod("add_graph_attributes") (plural), for which no methods exist, so
# every call failed with "no applicable method". The methods and NAMESPACE use
# the singular name. Section 1 is the regression guard.

report_side_effects(FALSE)

set.seed(20260914)
g  <- igraph::sample_gnp(10, 0.3, directed = FALSE)
igraph::V(g)$name <- paste0("v", seq_len(igraph::vcount(g)))
gnet <- snafun::to_network(g)


# ---------------------------------------------------------------------------
# 1. Graph-attribute round-trip (REGRESSION: add_graph_attribute must work).
# ---------------------------------------------------------------------------

for (obj_name in c("igraph", "network")) {
  x0 <- if (obj_name == "igraph") g else gnet
  x1 <- snafun::add_graph_attribute(x0, "title", "mygraph")
  expect_true("title" %in% snafun::list_graph_attributes(x1),
              info = paste0("add_graph_attribute stores the attribute (", obj_name, ")"))
  expect_equal(snafun::extract_graph_attribute(x1, "title"), "mygraph",
               info = paste0("extract_graph_attribute reads it back (", obj_name, ")"))
  x2 <- snafun::remove_graph_attribute(x1, "title")
  expect_false("title" %in% snafun::list_graph_attributes(x2),
               info = paste0("remove_graph_attribute deletes it (", obj_name, ")"))
}


# ---------------------------------------------------------------------------
# 2. Vertex-attribute has / remove (igraph and network).
# ---------------------------------------------------------------------------

for (obj_name in c("igraph", "network")) {
  base <- if (obj_name == "igraph") g else gnet
  gv <- snafun::add_vertex_attributes(base, "grp",
                                      value = rep(c("a", "b"), length.out = snafun::count_vertices(base)))
  expect_true(snafun::has_vertex_attribute(gv, "grp"),
              info = paste0("has_vertex_attribute TRUE for present attr (", obj_name, ")"))
  expect_false(snafun::has_vertex_attribute(gv, "does_not_exist"),
               info = paste0("has_vertex_attribute FALSE for absent attr (", obj_name, ")"))
  expect_true(snafun::has_vertex_attributes(gv),
              info = paste0("has_vertex_attributes TRUE when attrs exist (", obj_name, ")"))
  gv2 <- snafun::remove_vertex_attribute(gv, "grp")
  expect_false(snafun::has_vertex_attribute(gv2, "grp"),
               info = paste0("remove_vertex_attribute removes it (", obj_name, ")"))
}


# ---------------------------------------------------------------------------
# 3. Edge-attribute has / remove and weight removal.
# ---------------------------------------------------------------------------

gw <- g
igraph::E(gw)$weight <- stats::runif(igraph::ecount(gw), 1, 5)
expect_true(snafun::has_edge_attribute(gw, "weight"),
            info = "has_edge_attribute TRUE for weight")
expect_false(snafun::has_edge_attribute(g, "weight"),
             info = "has_edge_attribute FALSE when absent")
expect_false(snafun::is_weighted(snafun::remove_edge_weight(gw)),
             info = "remove_edge_weight makes the graph unweighted")
expect_false(snafun::has_edge_attribute(snafun::remove_edge_attribute(gw, "weight"), "weight"),
             info = "remove_edge_attribute removes the named edge attribute")


# ---------------------------------------------------------------------------
# 4. remove_vertex_names.
# ---------------------------------------------------------------------------

expect_true(snafun::has_vertexnames(g), info = "graph starts with vertex names")
expect_false(snafun::has_vertexnames(snafun::remove_vertex_names(g)),
             info = "remove_vertex_names strips the names")


# ---------------------------------------------------------------------------
# 5. contract_vertices.
# ---------------------------------------------------------------------------

gc <- snafun::contract_vertices(g, rep(1:5, each = 2))
expect_true(inherits(gc, "igraph"), info = "contract_vertices returns an igraph (default)")
expect_true(snafun::count_vertices(gc) < snafun::count_vertices(g),
            info = "contract_vertices reduces the vertex count")


# ---------------------------------------------------------------------------
# 6. create_community_graph and create_components_graph.
# ---------------------------------------------------------------------------

cg <- snafun::create_community_graph(c(5, 5, 5))
expect_true(is.list(cg) && all(c("graph", "group") %in% names(cg)),
            info = "create_community_graph returns list(graph, group)")
expect_true(inherits(cg$graph, "igraph"),
            info = "create_community_graph$graph is an igraph")
expect_equal(igraph::vcount(cg$graph), 15L,
             info = "create_community_graph has sum(sizes) vertices")
expect_equal(length(cg$group), 15L,
             info = "create_community_graph$group has one entry per vertex")

comp <- snafun::create_components_graph(20, membership = rep(1:2, each = 10))
expect_true(inherits(comp, "igraph"), info = "create_components_graph returns an igraph (default)")
expect_equal(snafun::count_vertices(comp), 20L,
             info = "create_components_graph has the requested number of vertices")
expect_true(snafun::count_components(comp) >= 2,
            info = "create_components_graph produces the requested components")

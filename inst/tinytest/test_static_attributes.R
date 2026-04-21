report_side_effects()



make_static_attribute_variants <- function() {
  igraph_none <- igraph::make_empty_graph(n = 4, directed = TRUE)
  igraph_none <- igraph::add_edges(igraph_none, c(1, 2, 2, 3, 3, 4))
  igraph_no_edges <- igraph::make_empty_graph(n = 4, directed = TRUE)
  igraph::vertex_attr(igraph_no_edges, "group") <- c("A", "A", "B", "B")
  
  igraph_vertex_one <- igraph_none
  igraph::vertex_attr(igraph_vertex_one, "group") <- c("A", "A", "B", "B")
  
  igraph_vertex_many <- igraph_none
  igraph::vertex_attr(igraph_vertex_many, "group") <- c("A", "A", "B", "B")
  igraph::vertex_attr(igraph_vertex_many, "score") <- c(1.5, 2.5, 3.5, 4.5)
  igraph::vertex_attr(igraph_vertex_many, "flag") <- c(TRUE, FALSE, TRUE, FALSE)
  
  igraph_edge_one <- igraph_none
  igraph::edge_attr(igraph_edge_one, "weight") <- c(1, 2, 3)
  
  igraph_edge_many <- igraph_none
  igraph::edge_attr(igraph_edge_many, "weight") <- c(1.1, 2.2, 3.3)
  igraph::edge_attr(igraph_edge_many, "kind") <- c("x", "y", "z")
  igraph::edge_attr(igraph_edge_many, "flag") <- c(TRUE, FALSE, TRUE)
  
  igraph_both <- igraph_none
  igraph::vertex_attr(igraph_both, "group") <- c("A", "A", "B", "B")
  igraph::vertex_attr(igraph_both, "score") <- c(10, 20, 30, 40)
  igraph::edge_attr(igraph_both, "weight") <- c(4.5, 5.5, 6.5)
  igraph::edge_attr(igraph_both, "kind") <- c("alpha", "beta", "gamma")
  
  network_none <- snafun::to_network(igraph_none)
  network_no_edges <- snafun::to_network(igraph_no_edges)
  
  network_vertex_one <- snafun::to_network(igraph_none)
  network::set.vertex.attribute(network_vertex_one, "group", c("A", "A", "B", "B"))
  
  network_vertex_many <- snafun::to_network(igraph_none)
  network::set.vertex.attribute(network_vertex_many, "group", c("A", "A", "B", "B"))
  network::set.vertex.attribute(network_vertex_many, "score", c(1.5, 2.5, 3.5, 4.5))
  network::set.vertex.attribute(network_vertex_many, "flag", c(TRUE, FALSE, TRUE, FALSE))
  
  network_edge_one <- snafun::to_network(igraph_none)
  network::set.edge.attribute(network_edge_one, "weight", c(1, 2, 3))
  
  network_edge_many <- snafun::to_network(igraph_none)
  network::set.edge.attribute(network_edge_many, "weight", c(1.1, 2.2, 3.3))
  network::set.edge.attribute(network_edge_many, "kind", c("x", "y", "z"))
  network::set.edge.attribute(network_edge_many, "flag", c(TRUE, FALSE, TRUE))
  
  network_both <- snafun::to_network(igraph_none)
  network::set.vertex.attribute(network_both, "group", c("A", "A", "B", "B"))
  network::set.vertex.attribute(network_both, "score", c(10, 20, 30, 40))
  network::set.edge.attribute(network_both, "weight", c(4.5, 5.5, 6.5))
  network::set.edge.attribute(network_both, "kind", c("alpha", "beta", "gamma"))
  
  list(
    igraph_none = igraph_none,
    igraph_no_edges = igraph_no_edges,
    igraph_vertex_one = igraph_vertex_one,
    igraph_vertex_many = igraph_vertex_many,
    igraph_edge_one = igraph_edge_one,
    igraph_edge_many = igraph_edge_many,
    igraph_both = igraph_both,
    network_none = network_none,
    network_no_edges = network_no_edges,
    network_vertex_one = network_vertex_one,
    network_vertex_many = network_vertex_many,
    network_edge_one = network_edge_one,
    network_edge_many = network_edge_many,
    network_both = network_both
  )
}



expect_static_vertex_accessors <- function(x) {
  if (inherits(x, "igraph")) {
    expected_names <- igraph::vertex_attr_names(x)
    extractor <- function(name) igraph::vertex_attr(x, name = name)
  } else {
    expected_names <- network::list.vertex.attributes(x)
    extractor <- function(name) network::get.vertex.attribute(x, attrname = name)
  }
  
  expect_equal(
    snafun::list_vertex_attributes(x),
    expected_names
  )
  
  for (name in expected_names) {
    expect_equal(
      snafun::extract_vertex_attribute(x, name),
      extractor(name)
    )
  }
}



expect_static_edge_accessors <- function(x) {
  if (inherits(x, "igraph")) {
    expected_names <- igraph::edge_attr_names(x)
    extractor <- function(name) igraph::edge_attr(x, name = name)
  } else {
    expected_names <- network::list.edge.attributes(x)
    extractor <- function(name) network::get.edge.attribute(x, attrname = name)
  }
  
  expect_equal(
    snafun::list_edge_attributes(x),
    expected_names
  )
  
  for (name in expected_names) {
    expect_equal(
      snafun::extract_edge_attribute(x, name),
      extractor(name)
    )
  }
}



variants <- make_static_attribute_variants()

for (x in variants) {
  expect_static_vertex_accessors(x)
  expect_static_edge_accessors(x)
}



# Check explicit expectations for missing attributes.
expect_equal(
  snafun::list_vertex_attributes(variants$igraph_none),
  character(0)
)
expect_equal(
  snafun::list_edge_attributes(variants$igraph_none),
  character(0)
)
expect_equal(
  snafun::list_edge_attributes(variants$igraph_no_edges),
  character(0)
)
expect_equal(
  snafun::list_vertex_attributes(variants$network_none),
  c("na", "vertex.names")
)
expect_equal(
  snafun::list_edge_attributes(variants$network_none),
  "na"
)
expect_equal(
  snafun::list_vertex_attributes(variants$network_no_edges),
  c("group", "na", "vertex.names")
)
expect_equal(
  snafun::list_edge_attributes(variants$network_no_edges),
  character(0)
)
expect_equal(
  snafun::extract_vertex_attribute(variants$network_no_edges, "group"),
  c("A", "A", "B", "B")
)



# Single-attribute cases should stay simple and not pick up unrelated names.
expect_equal(
  snafun::list_vertex_attributes(variants$igraph_vertex_one),
  "group"
)
expect_equal(
  snafun::list_edge_attributes(variants$igraph_edge_one),
  "weight"
)
expect_equal(
  snafun::extract_vertex_attribute(variants$igraph_vertex_one, "group"),
  c("A", "A", "B", "B")
)
expect_equal(
  snafun::extract_edge_attribute(variants$igraph_edge_one, "weight"),
  c(1, 2, 3)
)



# Multiple attributes of different types should all survive unchanged.
expect_equal(
  snafun::extract_vertex_attribute(variants$igraph_vertex_many, "score"),
  c(1.5, 2.5, 3.5, 4.5)
)
expect_equal(
  snafun::extract_vertex_attribute(variants$igraph_vertex_many, "flag"),
  c(TRUE, FALSE, TRUE, FALSE)
)
expect_equal(
  snafun::extract_edge_attribute(variants$igraph_edge_many, "kind"),
  c("x", "y", "z")
)
expect_equal(
  snafun::extract_edge_attribute(variants$igraph_edge_many, "flag"),
  c(TRUE, FALSE, TRUE)
)

expect_equal(
  snafun::extract_vertex_attribute(variants$network_vertex_many, "score"),
  c(1.5, 2.5, 3.5, 4.5)
)
expect_equal(
  snafun::extract_vertex_attribute(variants$network_vertex_many, "flag"),
  c(TRUE, FALSE, TRUE, FALSE)
)
expect_equal(
  snafun::extract_edge_attribute(variants$network_edge_many, "kind"),
  c("x", "y", "z")
)
expect_equal(
  snafun::extract_edge_attribute(variants$network_edge_many, "flag"),
  c(TRUE, FALSE, TRUE)
)



# Static accessors should reject the active API on non-networkDynamic inputs.
expect_error(
  snafun::list_vertex_attributes(variants$igraph_both, active = TRUE, at = 1),
  "only implemented for 'networkDynamic' objects"
)
expect_error(
  snafun::list_edge_attributes(variants$network_both, active = TRUE, at = 1),
  "only implemented for 'networkDynamic' objects"
)
expect_error(
  snafun::extract_vertex_attribute(variants$igraph_both, "group", active = TRUE, at = 1),
  "only implemented for 'networkDynamic' objects"
)
expect_error(
  snafun::extract_edge_attribute(variants$network_both, "weight", active = TRUE, at = 1),
  "only implemented for 'networkDynamic' objects"
)



# data.frame edge lists are currently not supported by these attribute
# accessors, so we pin that behavior down explicitly.
plain_edgelist <- data.frame(from = c(1, 2, 3), to = c(2, 3, 4))
attributed_edgelist <- data.frame(
  from = c(1, 2, 3),
  to = c(2, 3, 4),
  weight = c(1.5, 2.5, 3.5),
  kind = c("a", "b", "c")
)

expect_error(
  snafun::list_vertex_attributes(plain_edgelist),
  "'x' should be of class"
)
expect_error(
  snafun::list_edge_attributes(plain_edgelist),
  "'x' should be of class"
)
expect_error(
  snafun::extract_vertex_attribute(plain_edgelist, "group"),
  "'x' should be of class"
)
expect_error(
  snafun::extract_edge_attribute(plain_edgelist, "weight"),
  "'x' should be of class"
)

expect_error(
  snafun::list_vertex_attributes(attributed_edgelist),
  "'x' should be of class"
)
expect_error(
  snafun::list_edge_attributes(attributed_edgelist),
  "'x' should be of class"
)
expect_error(
  snafun::extract_vertex_attribute(attributed_edgelist, "group"),
  "'x' should be of class"
)
expect_error(
  snafun::extract_edge_attribute(attributed_edgelist, "weight"),
  "'x' should be of class"
)

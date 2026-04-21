report_side_effects()



make_test_networkdynamic <- function(vertex_static = FALSE,
                                     vertex_dynamic = FALSE,
                                     edge_static = FALSE,
                                     edge_dynamic = FALSE,
                                     with_edges = TRUE) {
  nw <- network::network.initialize(4, directed = TRUE)
  
  if (vertex_static) {
    network::set.vertex.attribute(nw, "group", c("A", "A", "B", "B"))
    network::set.vertex.attribute(nw, "importance", c(10, 20, 30, 40))
  }
  
  if (with_edges) {
    network::add.edges(nw, tail = c(1, 2, 3), head = c(2, 3, 4))
    
    if (edge_static) {
      network::set.edge.attribute(nw, "label", c("e12", "e23", "e34"))
      network::set.edge.attribute(nw, "capacity", c(5, 6, 7))
    }
  }
  
  nd <- NULL
  invisible(
    utils::capture.output(
      nd <- suppressMessages(suppressWarnings(networkDynamic::networkDynamic(nw)))
    )
  )
  
  if (with_edges) {
    invisible(
      utils::capture.output(
        suppressMessages(
          suppressWarnings(
            networkDynamic::activate.edges(
              nd,
              onset = c(0, 1, 2),
              terminus = c(8, 7, 6),
              e = 1:3
            )
          )
        )
      )
    )
  }
  
  if (vertex_dynamic) {
    invisible(
      utils::capture.output(
        suppressMessages(
          suppressWarnings(
            networkDynamic::activate.vertex.attribute(
              nd,
              "status",
              c(TRUE, FALSE, TRUE, FALSE),
              onset = 0,
              terminus = 3,
              v = 1:4
            )
          )
        )
      )
    )
    invisible(
      utils::capture.output(
        suppressMessages(
          suppressWarnings(
            networkDynamic::activate.vertex.attribute(
              nd,
              "status",
              c(FALSE, TRUE, FALSE, TRUE),
              onset = 3,
              terminus = 8,
              v = 1:4
            )
          )
        )
      )
    )
    invisible(
      utils::capture.output(
        suppressMessages(
          suppressWarnings(
            networkDynamic::activate.vertex.attribute(
              nd,
              "score",
              c(1.5, 2.5, 3.5, 4.5),
              onset = 0,
              terminus = 2,
              v = 1:4
            )
          )
        )
      )
    )
    invisible(
      utils::capture.output(
        suppressMessages(
          suppressWarnings(
            networkDynamic::activate.vertex.attribute(
              nd,
              "score",
              c(10.5, 20.5, 30.5, 40.5),
              onset = 2,
              terminus = 8,
              v = 1:4
            )
          )
        )
      )
    )
  }
  
  if (edge_dynamic && with_edges) {
    invisible(
      utils::capture.output(
        suppressMessages(
          suppressWarnings(
            networkDynamic::activate.edge.attribute(
              nd,
              "weight",
              c(1.2, 2.2, 3.2),
              onset = 0,
              terminus = 3,
              e = 1:3
            )
          )
        )
      )
    )
    invisible(
      utils::capture.output(
        suppressMessages(
          suppressWarnings(
            networkDynamic::activate.edge.attribute(
              nd,
              "weight",
              c(4.2, 5.2, 6.2),
              onset = 3,
              terminus = 8,
              e = 1:3
            )
          )
        )
      )
    )
    invisible(
      utils::capture.output(
        suppressMessages(
          suppressWarnings(
            networkDynamic::activate.edge.attribute(
              nd,
              "kind",
              c("x", "y", "z"),
              onset = 0,
              terminus = 8,
              e = 1:3
            )
          )
        )
      )
    )
  }
  
  nd
}



normalize_active_name <- function(name) {
  sub("\\.active$", "", name)
}



expect_same_vertex_accessors <- function(nd) {
  expect_equal(
    snafun::list_vertex_attributes(nd),
    network::list.vertex.attributes(nd)
  )
  expect_equal(
    snafun::list_vertex_attributes(nd, active = TRUE, at = 1),
    networkDynamic::list.vertex.attributes.active(nd, at = 1)
  )
  expect_equal(
    snafun::list_vertex_attributes(nd, active = TRUE, onset = 0, terminus = 8),
    networkDynamic::list.vertex.attributes.active(nd, onset = 0, terminus = 8)
  )
  expect_equal(
    snafun::list_vertex_attributes(nd, active = TRUE, at = 1, dynamic.only = TRUE),
    networkDynamic::list.vertex.attributes.active(nd, at = 1, dynamic.only = TRUE)
  )
  
  static_names <- network::list.vertex.attributes(nd)
  for (name in static_names) {
    expect_equal(
      snafun::extract_vertex_attribute(nd, name),
      network::get.vertex.attribute(nd, attrname = name)
    )
  }
  
  active_names_at <- networkDynamic::list.vertex.attributes.active(nd, at = 1)
  for (name in active_names_at) {
    query_name <- normalize_active_name(name)
    expect_equal(
      snafun::extract_vertex_attribute(nd, query_name, active = TRUE, at = 1),
      networkDynamic::get.vertex.attribute.active(nd, query_name, at = 1)
    )
  }
  
  active_names_interval <- networkDynamic::list.vertex.attributes.active(
    nd,
    onset = 0,
    terminus = 8
  )
  for (name in active_names_interval) {
    query_name <- normalize_active_name(name)
    expect_equal(
      snafun::extract_vertex_attribute(
        nd,
        query_name,
        active = TRUE,
        onset = 0,
        terminus = 8,
        rule = "latest"
      ),
      networkDynamic::get.vertex.attribute.active(
        nd,
        query_name,
        onset = 0,
        terminus = 8,
        rule = "latest"
      )
    )
  }
}



expect_same_edge_accessors <- function(nd) {
  expect_equal(
    snafun::list_edge_attributes(nd),
    network::list.edge.attributes(nd)
  )
  expect_equal(
    snafun::list_edge_attributes(nd, active = TRUE, at = 1),
    networkDynamic::list.edge.attributes.active(nd, at = 1)
  )
  expect_equal(
    snafun::list_edge_attributes(nd, active = TRUE, onset = 0, terminus = 8),
    networkDynamic::list.edge.attributes.active(nd, onset = 0, terminus = 8)
  )
  expect_equal(
    snafun::list_edge_attributes(nd, active = TRUE, at = 1, dynamic.only = TRUE),
    networkDynamic::list.edge.attributes.active(nd, at = 1, dynamic.only = TRUE)
  )
  
  static_names <- network::list.edge.attributes(nd)
  for (name in static_names) {
    expect_equal(
      snafun::extract_edge_attribute(nd, name),
      network::get.edge.attribute(nd, attrname = name)
    )
  }
  
  active_names_at <- networkDynamic::list.edge.attributes.active(nd, at = 1)
  for (name in active_names_at) {
    query_name <- normalize_active_name(name)
    expect_equal(
      snafun::extract_edge_attribute(nd, query_name, active = TRUE, at = 1),
      networkDynamic::get.edge.attribute.active(nd, query_name, at = 1)
    )
  }
  
  active_names_interval <- networkDynamic::list.edge.attributes.active(
    nd,
    onset = 0,
    terminus = 8
  )
  for (name in active_names_interval) {
    query_name <- normalize_active_name(name)
    expect_equal(
      snafun::extract_edge_attribute(
        nd,
        query_name,
        active = TRUE,
        onset = 0,
        terminus = 8,
        rule = "earliest"
      ),
      networkDynamic::get.edge.attribute.active(
        nd,
        query_name,
        onset = 0,
        terminus = 8,
        rule = "earliest"
      )
    )
  }
}



variants <- list(
  none = make_test_networkdynamic(),
  vertex_static = make_test_networkdynamic(vertex_static = TRUE),
  vertex_dynamic = make_test_networkdynamic(vertex_dynamic = TRUE),
  edge_static = make_test_networkdynamic(edge_static = TRUE),
  edge_dynamic = make_test_networkdynamic(edge_dynamic = TRUE),
  both = make_test_networkdynamic(
    vertex_static = TRUE,
    vertex_dynamic = TRUE,
    edge_static = TRUE,
    edge_dynamic = TRUE
  ),
  no_edges = make_test_networkdynamic(with_edges = FALSE)
)



for (nd in variants) {
  expect_true(networkDynamic::is.networkDynamic(nd))
  expect_same_vertex_accessors(nd)
  expect_same_edge_accessors(nd)
}



# The wrappers should pass through additional backend arguments such as
# subsets, rule selection, and unlist handling.
nd_both <- variants$both

expect_equal(
  snafun::extract_vertex_attribute(
    nd_both,
    "group",
    active = TRUE,
    at = 1,
    v = 2:3
  ),
  networkDynamic::get.vertex.attribute.active(
    nd_both,
    "group",
    at = 1,
    v = 2:3
  )
)

expect_equal(
  snafun::extract_vertex_attribute(
    nd_both,
    "status",
    active = TRUE,
    onset = 0,
    terminus = 8,
    rule = "all",
    v = 2:3,
    unlist = FALSE
  ),
  networkDynamic::get.vertex.attribute.active(
    nd_both,
    "status",
    onset = 0,
    terminus = 8,
    rule = "all",
    v = 2:3,
    unlist = FALSE
  )
)

expect_equal(
  snafun::extract_edge_attribute(
    nd_both,
    "weight",
    active = TRUE,
    onset = 0,
    terminus = 8,
    rule = "latest",
    e = 1:2
  ),
  networkDynamic::get.edge.attribute.active(
    nd_both,
    "weight",
    onset = 0,
    terminus = 8,
    rule = "latest",
    e = 1:2
  )
)

expect_equal(
  snafun::extract_edge_attribute(
    nd_both,
    "active",
    active = TRUE,
    at = 1,
    unlist = FALSE
  ),
  networkDynamic::get.edge.attribute.active(
    nd_both,
    "active",
    at = 1,
    unlist = FALSE
  )
)



# On static graph classes, requesting active attributes should fail clearly.
ig <- igraph::make_ring(4)
nw <- snafun::to_network(ig)

expect_error(
  snafun::list_vertex_attributes(ig, active = TRUE, at = 1),
  "only implemented for 'networkDynamic' objects"
)
expect_error(
  snafun::list_edge_attributes(nw, active = TRUE, at = 1),
  "only implemented for 'networkDynamic' objects"
)
expect_error(
  snafun::extract_vertex_attribute(ig, "name", active = TRUE, at = 1),
  "only implemented for 'networkDynamic' objects"
)
expect_error(
  snafun::extract_edge_attribute(nw, "na", active = TRUE, at = 1),
  "only implemented for 'networkDynamic' objects"
)



# Directly mirror the motivating examples from the classroom data.
data("McFarland_cls33_10_16_96", package = "networkDynamic")
classroom <- cls33_10_16_96

expect_equal(
  snafun::list_vertex_attributes(classroom, active = TRUE),
  networkDynamic::list.vertex.attributes.active(classroom)
)
expect_equal(
  snafun::extract_vertex_attribute(classroom, "gender", active = TRUE, at = 1),
  networkDynamic::get.vertex.attribute.active(classroom, "gender", at = 1)
)
expect_equal(
  snafun::extract_vertex_attribute(
    classroom,
    "gender",
    active = TRUE,
    onset = 0,
    terminus = 49
  ),
  networkDynamic::get.vertex.attribute.active(
    classroom,
    "gender",
    onset = 0,
    terminus = 49
  )
)
expect_equal(
  snafun::list_edge_attributes(classroom, active = TRUE, at = 1),
  networkDynamic::list.edge.attributes.active(classroom, at = 1)
)
expect_equal(
  snafun::list_edge_attributes(classroom, active = TRUE, onset = 0, terminus = 49),
  networkDynamic::list.edge.attributes.active(classroom, onset = 0, terminus = 49)
)
expect_equal(
  snafun::extract_edge_attribute(classroom, "active", active = TRUE, at = 1),
  networkDynamic::get.edge.attribute.active(classroom, "active", at = 1)
)

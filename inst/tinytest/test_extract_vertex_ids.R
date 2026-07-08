

# extract_vertex_ids -----------------------------------------------------------

# igraph, network and sna all number vertices 1..n internally. The point of this
# function is that you get that numbering from any supported object, without
# having to remember which package stores it where.

## igraph ----------------------------------------------------------------------
g <- igraph::make_ring(10)
expect_equal(extract_vertex_ids(g), 1:10)
expect_true(is.integer(extract_vertex_ids(g)))

## network ---------------------------------------------------------------------
n <- to_network(g)
expect_equal(extract_vertex_ids(n), 1:10)
expect_true(is.integer(extract_vertex_ids(n)))

## the same graph, both classes, the same numbering ----------------------------
expect_equal(extract_vertex_ids(g), extract_vertex_ids(to_network(g)))

## named graphs still yield numbers, not names ---------------------------------
# extract_vertex_names() is the function that returns names; these two are
# counterparts and must not be confused.
gn <- igraph::make_ring(4)
gn <- igraph::set_vertex_attr(gn, "name", value = c("A", "B", "C", "D"))
expect_true(has_vertexnames(gn))
expect_equal(extract_vertex_ids(gn), 1:4)
expect_equal(extract_vertex_names(gn), c("A", "B", "C", "D"))
expect_equal(extract_vertex_ids(to_network(gn)), 1:4)

## the id's are usable where names are not -------------------------------------
# extract_edge_id() documents that it accepts numeric vertex id's only. Feeding
# it the output of extract_vertex_ids() therefore has to work.
ids <- extract_vertex_ids(gn)
eid <- extract_edge_id(gn, ego = ids[1], alter = ids[2])
expect_equal(nrow(eid), 1L)
expect_true(eid[["eid"]] > 0)

## degenerate and unsupported input --------------------------------------------
empty <- igraph::make_empty_graph(0)
expect_equal(extract_vertex_ids(empty), integer(0))

single <- igraph::make_empty_graph(1)
expect_equal(extract_vertex_ids(single), 1L)

expect_error(extract_vertex_ids(1:10), pattern = "should be of class")
expect_error(extract_vertex_ids(matrix(0, 2, 2)), pattern = "should be of class")


#' Remove vertices
#' 
#' Remove specific vertices from the graph
#' 
#' Removes specific vertices from a graph (potentially bipartite) of 
#' class \code{network} or \code{igraph}.
#' 
#' The vertices to be removed can either be their id's or their names (if they 
#' exist in the graph object). The algorithm will first check if the 
#' \code{vertices} coincide with vertex names that are present in the object 
#' (if they exist) and, if they match, the vertices are removed according to 
#' their names. If they do not match, or if there are no vertex names in the 
#' graph object, \code{vertices} is assumed to refer to vertex id's.
#'
#' @param x graph object of class \code{network} or \code{igraph}
#' @param vertices vector with either vertex names or vertex id's.
#'
#' @return graph without the removed vertices
#' @export 
remove_vertices <- function(x, vertices) {
  UseMethod("remove_vertices")
}



#' @export
remove_vertices.default <- function(x, vertices) {
  txt <- methods_error_message("x", "remove_vertices")
  stop(txt)
}




#' @export
remove_vertices.igraph <- function(x, vertices) {
  igraph::delete_vertices(x, v = vertices)
}


#' @export
remove_vertices.network <- function(x, vertices) {
  namen <- network::get.vertex.attribute(x, "vertex.names")
  if (all(vertices %in% namen)) {
    vertices <- match(vertices, namen)
  } else if (!is.numeric(vertices)) {
    stop("You need to specify only vertex names or vertex numbers and they should all exist in the network")
  }
  graph <- network::delete.vertices(x, vid = vertices)
  return(graph)
}





#' Remove all isolates from the graph
#' 
#' Remove all isolates from the graph
#' 
#' Removes the isolates from an object of class \code{igraph} or \code{network}.
#' First, the algorithm identifies the isolates, using the \code{\link{extract_isolates}} 
#' function. Then, the identified isolates are removed and the new, smaller, 
#' graph is returned. Obviously, if \code{x} does not contain any isolates, 
#' \code{x} will be returned unaltered.
#' 
#' @param x graph of class \code{igraph} or \code{network}
#' @param loops should self-loops count when deciding if a vertex is an isolate? 
#' See  \code{\link{extract_isolates}} for details.
#'
#' @return graph with the isolates (if any) removed
#' @export
#' @seealso Finding isolates: \code{\link{extract_isolates}}
remove_isolates <- function(x, loops = FALSE) {
  UseMethod("remove_isolates")
}




#' @export
remove_isolates.default <- function(x, loops = FALSE) {
  txt <- methods_error_message("x", "remove_isolates")
  stop(txt)
}



#' @export
remove_isolates.igraph <- function(x, loops = FALSE) {
  isols <- extract_isolates.igraph(x, names = FALSE, loops = loops)
  if (length(isols) > 0) {
    remove_vertices.igraph(x, isols)
  } else {
    x
  }
}


#' @export
remove_isolates.network <- function(x, loops = FALSE) {
  isols <- extract_isolates.network(x, names = FALSE, loops = loops)
  if (length(isols) > 0) {
    remove_vertices.network(x, isols)
  } else {
    x
  }
}





# remove_loops -----------------------------------------------------------------

#' Remove loops
#' 
#' Remove loops from the graph
#' 
#' Remove loops (ie. a tie from a vertex to itself) from the graph object
#'
#' @param x graph of class \code{igraph}, \code{network}, or \code{matrix}
#'
#' @return graph with the same class as the input
#' @export
#'
#' @examples
#' x <- matrix(c(1, 1, 0, 0, 0, 1, 1, 0, 1), ncol = 3, byrow = TRUE)
#' remove_loops(x)
#' g_n <- snafun::to_network(x)
#' snafun::to_matrix(g_n)
#' remove_loops(g_n) |> snafun::to_matrix()
#' remove_loops(snafun::to_igraph(x)) |> snafun::to_matrix()
remove_loops <- function(x) {
  UseMethod("remove_loops")
}

#' @export
remove_loops.default <- function(x) {
  txt <- methods_error_message("x", "remove_loops")
  stop(txt)
}

#' @export
remove_loops.igraph <- function(x) {
  igraph::simplify(x, remove.multiple = FALSE, remove.loops = TRUE)
}


#' @export
remove_loops.network <- function(x) {
  # sna::diag.remove() might be a suitable alternative
  el <- to_edgelist(x)
  loops <- which(el$from == el$to)
  x <- network::delete.edges(x, eid = loops)
  x
}


#' @export
remove_loops.matrix <- function(x) {
  diag(x) <- 0
  x
}





# remove_multiple_edges --------------------------------------------------------

#' Remove multiple edges
#'
#' Remove parallel edges so that at most one edge remains per dyad.
#'
#' For each dyad, the first edge in the current edge order is retained and any
#' later copies are merged into that remaining edge. Edge attributes are
#' combined explicitly instead of being discarded. By default, a \code{weight}
#' attribute is summed while all other attributes keep the first encountered
#' value.
#'
#' In directed graphs, \code{i -> j} and \code{j -> i} are treated as different
#' dyads. In undirected graphs they are treated as the same dyad.
#'
#' Edgelists are also supported. When an edgelist was created by
#' \code{snafun::to_edgelist()}, the hidden vertex metadata attached by that
#' function are preserved so isolates and vertex attributes survive the
#' simplification step.
#'
#' Matrices are deliberately not supported because they do not retain separate
#' edge instances or stable edge ids. Use \code{\link{has_multiple_edges}} on a
#' matrix if you only need to detect collapsed multiplicities.
#'
#' @param x graph of class \code{igraph}, \code{network}, or \code{data.frame}
#' interpreted as an edgelist
#' @param edge_attr_combine Named list with per-attribute combination rules.
#' The default keeps \code{weight} meaningful by summing it when parallel edges
#' are merged. Rules can be one of \code{"first"}, \code{"last"},
#' \code{"sum"}, \code{"mean"}, \code{"min"}, \code{"max"}, \code{"any"},
#' \code{"all"}, \code{"concat_unique"}, or a custom function.
#' @param default Default combination rule for edge attributes that are not
#' named in \code{edge_attr_combine}. Defaults to \code{"first"}.
#'
#' @return graph with the same class as the input
#' @export
#'
#' @examples
#' g <- snafun::to_igraph(
#'   data.frame(from = c(1, 1, 2), to = c(2, 2, 3)),
#'   directed = TRUE
#' )
#' g <- snafun::add_edge_attributes(g, "weight", value = c(2, 5, 7))
#' g <- snafun::add_edge_attributes(g, "label", value = c("a", "b", "c"))
#' remove_multiple_edges(g)
#'
#' nw <- snafun::to_network(
#'   data.frame(from = c(1, 1, 2), to = c(2, 2, 3)),
#'   directed = TRUE
#' )
#' nw <- snafun::add_edge_attributes(nw, "weight", value = c(2, 5, 7))
#' remove_multiple_edges(nw)
#'
#' el <- data.frame(
#'   from = c(1, 1, 2),
#'   to = c(2, 2, 3),
#'   weight = c(2, 5, 7)
#' )
#' remove_multiple_edges(el)
remove_multiple_edges <- function(x,
                                  edge_attr_combine = list(weight = "sum"),
                                  default = "first") {
  UseMethod("remove_multiple_edges")
}


#' @export
remove_multiple_edges.default <- function(x,
                                          edge_attr_combine = list(weight = "sum"),
                                          default = "first") {
  txt <- methods_error_message("x", "remove_multiple_edges")
  stop(txt)
}


#' Normalize multiple-edge combination rules
#'
#' Turn the user-facing rule labels into functions that can be applied in a
#' backend-independent way. For igraph we can hand the resulting functions
#' directly to \code{igraph::simplify()}, and for network objects we reuse the
#' exact same functions manually while aggregating edge attributes.
#'
#' @param rule Character scalar or function.
#'
#' @return Function that combines a vector of values into one value.
#' @keywords internal
#' @noRd
normalize_multiple_edge_rule <- function(rule) {
  if (is.function(rule)) {
    return(rule)
  }
  
  if (!is.character(rule) || length(rule) != 1) {
    stop(
      "'edge_attr_combine' rules should each be a single character label or a function.",
      call. = FALSE
    )
  }
  
  switch(
    rule,
    first = function(values) values[[1]],
    last = function(values) values[[length(values)]],
    sum = function(values) sum(values),
    mean = function(values) mean(values),
    min = function(values) min(values),
    max = function(values) max(values),
    any = function(values) any(values),
    all = function(values) all(values),
    concat_unique = function(values) paste(unique(as.character(values)), collapse = " | "),
    stop(
      paste0(
        "Unknown edge attribute combination rule '", rule,
        "'. Use one of 'first', 'last', 'sum', 'mean', 'min', 'max', ",
        "'any', 'all', 'concat_unique', or a function."
      ),
      call. = FALSE
    )
  )
}


#' Choose the rule for one edge attribute
#'
#' @param attr_name Name of the edge attribute.
#' @param edge_attr_combine Named list with explicit attribute rules.
#' @param default Default rule for unspecified attributes.
#'
#' @return Function that combines the values for one attribute.
#' @keywords internal
#' @noRd
multiple_edge_rule_for_attribute <- function(attr_name, edge_attr_combine, default) {
  if (!is.list(edge_attr_combine)) {
    stop("'edge_attr_combine' should be a named list.", call. = FALSE)
  }
  
  explicit_names <- names(edge_attr_combine)
  if (!is.null(explicit_names) && attr_name %in% explicit_names) {
    return(normalize_multiple_edge_rule(edge_attr_combine[[attr_name]]))
  }
  
  normalize_multiple_edge_rule(default)
}


#' Prepare an igraph edge.attr.comb specification
#'
#' @param attr_names Character vector with edge attribute names present on the graph.
#' @param edge_attr_combine Named list with explicit attribute rules.
#' @param default Default rule for unspecified attributes.
#'
#' @return Named list suitable for \code{igraph::simplify()}.
#' @keywords internal
#' @noRd
igraph_multiple_edge_attr_combine <- function(attr_names, edge_attr_combine, default) {
  combine_rules <- lapply(attr_names, function(one_attr) {
    multiple_edge_rule_for_attribute(
      attr_name = one_attr,
      edge_attr_combine = edge_attr_combine,
      default = default
    )
  })
  names(combine_rules) <- attr_names
  combine_rules$.default <- normalize_multiple_edge_rule(default)
  combine_rules
}


#' Collapse repeated dyads in an edge table
#'
#' Aggregate the rows belonging to the same dyad while preserving the first row
#' order. This helper is shared by the \code{network} and \code{data.frame}
#' methods so both backends follow the same attribute-combination semantics.
#'
#' @param x data.frame whose first two columns contain edge endpoints
#' @param directed logical, whether edge direction matters for duplicate
#' detection
#' @param edge_attr_combine Named list with explicit attribute rules
#' @param default Default rule for unspecified attributes
#'
#' @return List with collapsed edgelist, kept row ids, duplicate row ids, and
#' row groups per dyad.
#' @keywords internal
#' @noRd
collapse_multiple_edge_table <- function(x,
                                         directed,
                                         edge_attr_combine,
                                         default) {
  if (inherits(x, "tbl_df")) {
    x <- as.data.frame(x)
  }
  
  edge_keys <- multiple_edge_keys(
    from = x[[1]],
    to = x[[2]],
    directed = directed
  )
  duplicate_rows <- which(duplicated(edge_keys))
  edge_groups <- split(seq_len(nrow(x)), edge_keys)
  kept_rows <- vapply(edge_groups, function(one_group) one_group[[1]], integer(1))
  
  collapsed <- x[kept_rows, , drop = FALSE]
  rownames(collapsed) <- NULL
  
  edge_attr_names <- colnames(x)[-(1:2)]
  if (length(edge_attr_names) > 0) {
    for (one_attr in edge_attr_names) {
      combine_fun <- multiple_edge_rule_for_attribute(
        attr_name = one_attr,
        edge_attr_combine = edge_attr_combine,
        default = default
      )
      combined_values <- lapply(edge_groups, function(one_group) {
        combine_fun(x[one_group, one_attr])
      })
      collapsed[[one_attr]] <- do.call(c, combined_values)
    }
  }
  
  list(
    edgelist = collapsed,
    kept_rows = kept_rows,
    duplicate_rows = duplicate_rows,
    edge_groups = edge_groups
  )
}


#' Reattach hidden snafun metadata to an edgelist
#'
#' @param original Original edgelist
#' @param collapsed Collapsed edgelist
#'
#' @return Edgelist with stored vertex and graph metadata copied over.
#' @keywords internal
#' @noRd
restore_hidden_edgelist_metadata <- function(original, collapsed) {
  attach_edgelist_metadata(
    edgelist = collapsed,
    vertex_metadata = extract_stored_edgelist_vertices(original),
    bipartite = extract_stored_edgelist_bipartite(original),
    directed = extract_stored_edgelist_directed(original)
  )
}


#' @export
remove_multiple_edges.igraph <- function(x,
                                         edge_attr_combine = list(weight = "sum"),
                                         default = "first") {
  if (!snafun::has_multiple_edges(x)) {
    return(x)
  }
  
  combine_rules <- igraph_multiple_edge_attr_combine(
    attr_names = igraph::edge_attr_names(x),
    edge_attr_combine = edge_attr_combine,
    default = default
  )
  igraph::simplify(
    x,
    remove.multiple = TRUE,
    remove.loops = FALSE,
    edge.attr.comb = combine_rules
  )
}


#' @export
remove_multiple_edges.network <- function(x,
                                          edge_attr_combine = list(weight = "sum"),
                                          default = "first") {
  duplicate_edges <- snafun::extract_multiple_edges(x)
  if (is.null(duplicate_edges)) {
    return(x)
  }
  
  # We deliberately avoid snafun::to_edgelist() here because that helper sorts
  # rows for presentation. For multiple-edge removal we need the raw edge order
  # to stay aligned with the network edge ids returned by x$mel and
  # extract_multiple_edges().
  edge_table <- network::as.data.frame.network(x, unit = "edges", na.rm = FALSE)
  colnames(edge_table)[c(1, 2)] <- c("from", "to")
  collapsed <- collapse_multiple_edge_table(
    x = edge_table,
    directed = snafun::is_directed(x),
    edge_attr_combine = edge_attr_combine,
    default = default
  )
  kept_edges <- collapsed$kept_rows
  
  edge_attr_names <- setdiff(colnames(edge_table), c("from", "to"))
  if (length(edge_attr_names) > 0) {
    for (one_attr in edge_attr_names) {
      for (one_index in seq_along(kept_edges)) {
        x <- network::set.edge.attribute(
          x,
          attrname = one_attr,
          value = collapsed$edgelist[[one_attr]][[one_index]],
          e = kept_edges[[one_index]]
        )
      }
    }
  }
  
  network::delete.edges(x, eid = duplicate_edges)
  x
}


#' @export
remove_multiple_edges.data.frame <- function(x,
                                             edge_attr_combine = list(weight = "sum"),
                                             default = "first") {
  if (inherits(x, "tbl_df")) {
    x <- as.data.frame(x)
  }
  
  directed <- TRUE
  if (!is.null(attr(x, "snafun_directed", exact = TRUE))) {
    directed <- extract_stored_edgelist_directed(x)
  }
  
  collapsed <- collapse_multiple_edge_table(
    x = x,
    directed = directed,
    edge_attr_combine = edge_attr_combine,
    default = default
  )
  
  if (length(collapsed$duplicate_rows) == 0) {
    return(x)
  }
  
  restore_hidden_edgelist_metadata(
    original = x,
    collapsed = collapsed$edgelist
  )
}








# remove (describeIn) ----------------------------------------------------------

#' Remove parts of the graph
#' 
#' Remove parts of the graph
#' 
#' @param x input graph (class \code{igraph} or \code{network})
#' @param attr_name name of the attribute to be removed
#'
#' @return graph, without the removed elements
#'
#' @examples
#' data(emon, package = "network")
#' texas <- emon$Texas
#' remove_edge_attribute(texas, "Frequency")
#' remove_vertex_attribute(texas, "Formalization")
#' remove_graph_attribute(texas, "directed")
#' 
#' texas_i <- to_igraph(texas)
#' remove_edge_attribute(texas_i, "Frequency")
#' remove_vertex_attribute(texas_i, "Formalization")
#' @name remove
NULL




#' @describeIn remove remove an edge attribute from the graph
#' @export
remove_edge_attribute <- function(x, attr_name) {
  UseMethod("remove_edge_attribute")
}


#' @export
remove_edge_attribute.default <- function(x, attr_name) {
  txt <- methods_error_message("x", "remove_edge_attribute")
  stop(txt)
}



#' @export
remove_edge_attribute.igraph <- function(x, attr_name) {
  x <- igraph::delete_edge_attr(graph = x, name = attr_name)
  x
}


#' @export
remove_edge_attribute.network <- function(x, attr_name) {
  network::delete.edge.attribute(x, attrname = attr_name)
  x
}






#' @describeIn remove remove an edge weight attribute from the graph. 
#' If \code{x} is a matrix, this function will dichotomize
#' it by turning every non-zero cell into a 1.
#' @export
remove_edge_weight <- function(x) {
  UseMethod("remove_edge_weight")
}


#' @export
remove_edge_weight.default <- function(x) {
  txt <- methods_error_message("x", "remove_edge_weight")
  stop(txt)
}



#' @export
remove_edge_weight.igraph <- function(x) {
  x <- igraph::delete_edge_attr(graph = x, name = "weight")
  x
}


#' @export
remove_edge_weight.network <- function(x) {
  network::delete.edge.attribute(x, attrname = "weight")
  x
}

#' @export
remove_edge_weight.matrix <- function(x) {
  x[x != 0] <- 1
  x
}







#' @describeIn remove remove a vertex attribute from the graph
#' @export
remove_vertex_attribute <- function(x, attr_name) {
  UseMethod("remove_vertex_attribute")
}


#' @export
remove_vertex_attribute.default <- function(x, attr_name) {
  txt <- methods_error_message("x", "remove_vertex_attribute")
  stop(txt)
}



#' @export
remove_vertex_attribute.igraph <- function(x, attr_name) {
  x <- igraph::delete_vertex_attr(graph = x, name = attr_name)
  x
}


#' @export
remove_vertex_attribute.network <- function(x, attr_name) {
  network::delete.vertex.attribute(x, attrname = attr_name)
  x
}






#' @describeIn remove remove a graph attribute from the graph
#' @export
remove_graph_attribute <- function(x, attr_name) {
  UseMethod("remove_graph_attribute")
}


#' @export
remove_graph_attribute.default <- function(x, attr_name) {
  txt <- methods_error_message("x", "remove_graph_attribute")
  stop(txt)
}



#' @export
remove_graph_attribute.igraph <- function(x, attr_name) {
  x <- igraph::delete_graph_attr(graph = x, name = attr_name)
  x
}


#' @export
remove_graph_attribute.network <- function(x, attr_name) {
  network::delete.network.attribute(x, attrname = attr_name)
  x
}








#' @describeIn remove remove vertex names from the graph
#' @export
remove_vertex_names <- function(x) {
  UseMethod("remove_vertex_names")
}


#' @export
remove_vertex_names.default <- function(x) {
  txt <- methods_error_message("x", "remove_vertex_names")
  stop(txt)
}



#' @export
remove_vertex_names.igraph <- function(x) {
  x <- igraph::delete_vertex_attr(graph = x, name = "name")
  x
}


#' @export
remove_vertex_names.network <- function(x) {
  network::delete.vertex.attribute(x, attrname = "vertex.names")
  x
}

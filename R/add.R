

### add_vertex_attributes ------------------------------------------------------

#' Add vertex attributes to a graph
#' 
#' Add one or more vertex attributes to a graph
#' 
#' Consistent API to easily add one or more vertex attributes to a graph of 
#' class \code{igraph} or \code{network}. Both the \code{igraph} and \code{statnet} 
#' packages do not have (simple) functions for this.
#' 
#' Adding vertex attributes is easy, There are several options:
#' \itemize{
#' \item a vector with values specified in \code{value} and a name for the 
#' attribute specified in \code{attr_name}. 
#' \item a \code{data.frame} for the \code{value}. All columns in the \code{data.frame} 
#' will be added to the graph as vertex attributes and the names in the 
#' \code{data.frame} are used as the names of 
#' the new attributes,
#' \item a \code{matrix} for the \code{value}. The matrix needs to have appropriate 
#' column names. All columns in the \code{matrix} will be added to the graph as 
#' vertex attributes and the column names in the \code{matrix} are used as the names of 
#' the new attributes,
#' \item a \code{data.frame} or \code{matrix} (with column names) AND a character 
#' vector with variable names in \code{attr_name}. The variables with the names 
#' specified in the \code{attr_name} argument will be taken from the  \code{data.frame} 
#' or \code{matrix} and will be added to the graph as vertex attributes. 
#' This is useful in case you have a larger \code{data.frame} of which you only 
#' want to add some of the columns as vertex attributes.
#' }
#' 
#' In all cases, the attributes will be added for all vertices, so you need to 
#' make sure the values in the correct order and exist for all of the vertices. 
#' 
#' @param x graph object to which the vertex attributes need to be added
#' @param attr_name optional, character vector with attribute names (see 'Details')
#' @param value \code{data.frame}, \code{matrix} (with column names), or a vector
#' (see 'Details')
#'
#' @return graph object with new attributes added
#' @export
#'
#' @examples
#' g <- snafun::create_manual_graph(1 -- 2 -- 3 -- 4 -- 5 -- 1)
#' add_vertex_attributes(g, "at1", 1:5) |>
#'   snafun::extract_vertex_attribute("at1")
#'
#' add_vertex_attributes(g, value = data.frame(naam = 1:5)) |>
#'   snafun::extract_vertex_attribute("naam")
#' 
#' add_vertex_attributes(g, value = data.frame(een = 11:15, twee = 21:25))
#' 
#' add_vertex_attributes(g, c("twee", "een"), 
#'   value = data.frame(een = 11:15, twee = 21:25, drie = 31:35))
#' 
#' g <- snafun::create_random_graph(10, "gnp", p = .2, graph = "network")
#' add_vertex_attributes(g, "naam", value = data.frame(naam = 1:10))
#' 
#' add_vertex_attributes(g, value = data.frame(naam = 1:10))
#' 
#' add_vertex_attributes(g, value = data.frame(een = 10:19, twee = 20:29))
#' 
#' add_vertex_attributes(g, c("twee", "een"), 
#'   value = data.frame(een = 10:19, twee = 20:29, drie = 30:39))
add_vertex_attributes <- function(x, attr_name = NULL, value) {
  UseMethod("add_vertex_attributes")  
}


#' @export
add_vertex_attributes.default <- function(x, attr_name = NULL, value) {
  txt <- methods_error_message("x", "add_vertex_attributes")
  stop(txt) 
}


#' @export
add_vertex_attributes.igraph <- function(x, attr_name = NULL, value) {
  index <- igraph::V(x)
  if (is.null(as.list(match.call())$value)) stop("You forgot to specify a 'value'")
  if (inherits(value, "matrix") && is.null(colnames(value))) stop("matrix 'value' has no colnames")
  # check is 'value' is convertible to data.frame
  if (inherits(value, "matrix") || (inherits(value, "tbl_df"))) {
    value <- as.data.frame(value)
  }
  value_is_df <- ifelse(inherits(value, "data.frame"), TRUE, FALSE)
  
  if (!value_is_df) {   # single vector attribute
    if (!is.null(dim(value))) {
      stop("'value' should either be a data.frame, named matrix, or vector")
    }
    if (is.null(attr_name)) stop("You did not specify a 'name' for this attribute value")
    if (length(attr_name) > 1) stop("Please only provide a single 'name' for this 'value'")
    if (length(index) != length(value)) stop("The length of 'value' does not correspond with the number of vertices for which it is to be used")
    # name holds a single attribute name and value contains the value of just 1 attribute
    x <- igraph::set_vertex_attr(x, name = attr_name, value = value)
    
  } else { # value is a df
    if (!is.null(attr_name)) {
      if (!all(attr_name %in% colnames(value))) {
        stop("Not all names from 'name' occur in 'value'")
      }
      value <- value[attr_name]
    }
    if (length(index) != nrow(value)) {
      stop("The length of 'value' does not correspond with the number of vertices for which it is to be used")
    }
    value <- as.list(value)
    
    for (kol in names(value)) {
      x <- igraph::set_vertex_attr(graph = x,
                                   name = kol,
                                   index = index,
                                   value = value[[kol]])
    }
  }
  x
}



#' @export
add_vertex_attributes.network <- function(x, attr_name = NULL, value) {
  index <- seq_len(network::network.size(x))
  if (is.null(as.list(match.call())$value)) stop("You forgot to specify a 'value'")
  if (inherits(value, "matrix") && is.null(colnames(value))) stop("matrix 'value' has no colnames")
  # check is 'value' is convertible to data.frame
  if (inherits(value, "matrix") || (inherits(value, "tbl_df"))) {
    value <- as.data.frame(value)
  }
  value_is_df <- ifelse(inherits(value, "data.frame"), TRUE, FALSE)
  
  if (!value_is_df) {   # single vector attribute
    if (!is.null(dim(value))) {
      stop("'value' should either be a data.frame, named matrix, or vector")
    }
    if (is.null(attr_name)) stop("You did not specify a 'name' for this attribute value")
    if (length(attr_name) > 1) stop("Please only provide a single 'name' for this 'value'")
    if (length(index) != length(value)) stop("The length of 'value' does not correspond with the number of vertices for which it is to be used")
    # name holds a single attribute name and value contains the value of just 1 attribute
    x <- network::set.vertex.attribute(x, attrname = attr_name, value = value)
    
  } else { # value is a df
    # read in all attributes at once, this works by using a named list and igraph::vertex_attr
    if (!is.null(attr_name)) {
      if (!all(attr_name %in% colnames(value))) {
        stop("Not all names from 'name' occur in 'value'")
      }
      value <- value[attr_name]
    }
    if (length(index) != nrow(value)) {
      stop("The length of 'value' does not correspond with the number of vertices for which it is to be used")
    }
    value <- as.list(value)
    for (zz in 1:length(value)) {
      network::set.vertex.attribute(x, attrname = names(value)[zz], value[[zz]])
    }
  }
  return(x)
}






### add_vertex_names ------------------------------------------------------

#' Add vertex names to a graph
#' 
#' Add vertex names to a graph
#' 
#' Consistent API to easily add vertex names to a graph of 
#' class \code{igraph} or \code{network}. 
#' The vertex attribute that holds the vertex names is called differently for 
#' \code{igraph} and \code{network} objects, so 
#' if you use the incorrect name for the attribute, the new names will not be 
#' recognized in subsequent steps of the network analysis pipeline. 
#' 
#' This function puts the vertex names in the correct vertex attribute, so you 
#' do not need to remember what they are called.
#' 
#' @param x graph object to which the vertex attributes need to be added
#' @param value the vertex names to be set
#' @param vids the id's of the vertices for which the names are to be set.
#' This is useful if you only want to set/correct the names of a few vertices. 
#' For \code{igraph} objects, these can be the number or the name of the vertices. 
#' For \code{network} objects, only the numeric ID is allowed. 
#' The default is \code{NULL}, which selects all vertices, in the order in which 
#' they occur in the graph object. 
#'
#' @return graph object with new attributes added
#' @export
#'
#' @examples
#' g <- snafun::create_random_graph(5, "gnp", p = .1, graph = "igraph")
#' # there are no names yet
#' snafun::extract_vertex_names(g)  # NULL
#' g <- snafun::add_vertex_names(g, LETTERS[5:1])
#' snafun::extract_vertex_names(g) # "E" "D" "C" "B" "A"
#' g <- snafun::add_vertex_names(g, "X", vids = 2)
#' snafun::extract_vertex_names(g) # "E" "X" "C" "B" "A"
#' g <- snafun::add_vertex_names(g, c("Y", "Z"), vids = c("C", "A"))
#' snafun::extract_vertex_names(g) # ""E" "X" "Y" "B" "Z"
#' 
#' g <- snafun::create_random_graph(5, "gnp", p = .1, graph = "network")
#' snafun::extract_vertex_names(g)  # 1 2 3 4 5
#' g <- snafun::add_vertex_names(g, LETTERS[5:1])
#' snafun::extract_vertex_names(g) # "E" "D" "C" "B" "A"
#' g <- snafun::add_vertex_names(g, "X", vids = 2)
#' snafun::extract_vertex_names(g) # "E" "X" "C" "B" "A"
#' # network objects accept only numeric id's
#' g <- snafun::add_vertex_names(g, c("Y", "Z"), vids = c(3, 5))
#' snafun::extract_vertex_names(g) # ""E" "X" "Y" "B" "Z"
add_vertex_names <- function(x, value, vids = NULL) {
  UseMethod("add_vertex_names")  
}


#' @export
add_vertex_names.default <- function(x, value, vids = NULL) {
  txt <- methods_error_message("x", "add_vertex_names")
  stop(txt) 
}


#' @export
add_vertex_names.igraph <- function(x, value, vids = NULL) {
  if (is.null(vids)) {
    vids = igraph::V(x)
  }
  x <- igraph::set_vertex_attr(x, name = "name", 
                               index = vids, value = value)
  x
}



#' @export
add_vertex_names.network <- function(x, value, vids = NULL) {
  if (is.null(vids)) {
    vids = seq_len(network::get.network.attribute(x, "n"))
  }
  x <- network::set.vertex.attribute(x, attrname = "vertex.names", 
                                value = value, v = vids)
  x
}








### add_edge_attributes --------------------------------------------------------

#' Add edge attributes to a graph
#' 
#' Add one or more edge attributes to a graph
#' 
#' Consistent API to easily add one or more edge attributes to a graph of 
#' class \code{igraph} or \code{network}. Both the \code{igraph} and \code{statnet} 
#' packages do not have (simple) functions for this.
#' 
#' Adding edge attributes is easy, There are several options:
#' \itemize{
#' \item a vector with values specified in \code{value} and a name for the 
#' attribute specified in \code{attr_name}. 
#' 
#' This is only a very safe option if you know for sure that the order of the edges 
#' in \code{value} are identical to the order of the edges inside the graph object.!
#' \item a \code{data.frame} or \code{matrix} for \code{value}. 
#' Provide the names that the edge attributes should get in \code{attr_name} (which 
#' can be a vector with more than one name: the same number of names as there 
#' are columns in \code{value}). Note: any column names in \code{value} will be 
#' overridden by whatever is provided in \code{attr_name}.
#' 
#' This is only a very safe option if you know for sure that the order of the edges 
#' in \code{value} are identical to the order of the edges inside the graph object!
#' \item an edgelist (\code{data.frame} or \code{matrix}) in \code{edgelist}. 
#' If \code{attrname} is not provided, then all attributes in \code{edgelist} are 
#' added to the graph. 
#' If \code{attrname} is provided, only the columns in \code{edgelist} are added 
#' to the graph that occur in \code{attr_name}.
#' 
#' The edgelist is assumed to have at least three columns. The first column 
#' contains the sender and the second column contains the receiver of the edge 
#' (this is irrespective of the names of these columns and the 
#' sender-receiver order is irrelevant for undirected graphs). 
#' Any further columns are assumed to be edge attributes.
#' 
#' This is the safe option, as the function makes sure that the 
#' edge attributes are added to the correct edge.
#' }
#' 
#' @param object graph
#' @param attr_name vector with one or more names of attributes
#' @param value vector, matrix, or data.frame 
#' @param edgelist matrix or dataframe containing the edgelist and edge attributes
#' @param overwrite logical, should an edge attribute be overwritten if it already 
#' exists inside the graph?
#'
#' @return graph with the added edge attributes
#' @export
add_edge_attributes <- function(object, attr_name, value, edgelist, overwrite = FALSE) {
  UseMethod("add_edge_attributes")  
}


#' @export
add_edge_attributes.default <- function(object, attr_name, value, edgelist, overwrite = FALSE) {
  txt <- methods_error_message("object", "add_edge_attributes")
  stop(txt) 
}




#' @export
add_edge_attributes.igraph <- function(object, attr_name, value, edgelist, overwrite = FALSE) {
  
  is_attr_name <- !methods::missingArg(attr_name)
  is_value <- !methods::missingArg(value)
  is_edgelist <- !methods::missingArg(edgelist)
  is_object <- !methods::missingArg(object)
  
  if (!is_object) {
    stop("You need to specify a graph 'object'")
  }
  if (is_value & is_edgelist) {
    stop("You can only use either 'value' or 'edgelist', but not both.")
  }
  if (is_attr_name && (!(is_value | is_edgelist))) {
    stop("Please specify a 'value' or 'edgelist'")
  }
  if (is_value && !is_attr_name) {
    stop("You need to specify a 'attr_name' for the 'value'")
  }
  if (is_edgelist) {
    attr_names_el <- names(edgelist[, -c(1, 2)])
  }
  if (is_attr_name && is_edgelist) {
    if (!all(attr_name %in% attr_names_el)) {
      stop("You specified (an) attribute(s) in 'attr_name' that are/is not present in 'edgelist'.")
    } else {  # only keep columns from 'attr_name' in the edgelist
      keep_cols <- which(colnames(edgelist) %in% attr_name)
      edgelist <- edgelist[, c(1, 2, keep_cols)]
    }
  }
  
  if (is_edgelist) {
    vp <- t(edgelist[, 1:2]) |> matrix(nrow = 1, byrow = TRUE) |> as.vector()
    eids <- igraph::get_edge_ids(object, vp = vp)
    
    # check for duplicate edges in the edgelist
    dups <- which(duplicated(eids))
    if (length(dups) > 0) { # there are duplicate dyads in edgelist
      stop("There are duplicate edges you try to set values for, these are rows ", dups)
    }
    # check if all edges in the graph are included in the edgelist
    # Use explicit edge id sequences here. This avoids precedence ambiguity in
    # expressions like !1:n %in% eids and guarantees that only valid positive
    # edge ids are passed to igraph::ends().
    miss <- setdiff(seq_len(igraph::ecount(object)), eids)
    if (length(miss) > 0) {  # some edges are missing in the edgelist
      miss_edges <- igraph::ends(object, es = miss, names = FALSE)
      miss_edges <- paste0(miss_edges[, 1], "-", miss_edges[, 2]) |> paste(collapse = ", ")
      stop("Some edges from 'object' are missing in your 'edgelist', these are (sender-receiver): ",
           miss_edges)
    }
    to_be_added <- names(edgelist)[-c(1, 2)]
    if (any(to_be_added %in% igraph::edge_attr_names(object)) && !overwrite) {
      stop("The 'edgelist' contains (an) edge attribute(s) that are/is already present in the network,
          remove it from the 'edgelist' or set 'overwrite' to TRUE.")
    }
    
    for (att in to_be_added) {
      igraph::edge_attr(object, att, index = eids) <- edgelist[, att]
    }
  } else { # attr_name and value are specified
    if (length(attr_name) == 1) { # only 1 attribute needs to be added
      if (is.null(dim(value))) { #value is a vector
        igraph::edge_attr(object, name = attr_name) <- value
        return(object)
      } else if (dim(value)[2] == 1) {  # value has only 1 column
        igraph::edge_attr(object, name = attr_name) <- value
        return(object)
      } else {
        stop("'attr_name' and 'value' do not have matching dimensions")
      }
    } else { # multiple attributes to be added
      if (length(attr_name) > 1 && is.null(ncol(value))) {
        stop("'attr_name' and 'value' do not have matching dimensions")
      } else if (length(attr_name) != ncol(value)) {
        stop("'attr_name' and 'value' do not have matching dimensions")
      } else {
        for (att in 1:length(attr_name)) {
          igraph::edge_attr(object, name = attr_name[att]) <- value[, att]
        }
      }
    }
  }
  return(object)
}


#' @export
add_edge_attributes.network <- function(object, attr_name, value, edgelist, overwrite = FALSE) {
  
  is_attr_name <- !methods::missingArg(attr_name)
  is_value <- !methods::missingArg(value)
  is_edgelist <- !methods::missingArg(edgelist)
  is_object <- !methods::missingArg(object)
  
  if (!is_object) {
    stop("You need to specify a graph 'object'")
  }
  if (is_value & is_edgelist) {
    stop("You can only use either 'value' or 'edgelist', but not both.")
  }
  if (is_attr_name && (!(is_value | is_edgelist))) {
    stop("Please specify a 'value' or 'edgelist'")
  }
  if (is_value && !is_attr_name) {
    stop("You need to specify a 'attr_name' for the 'value'")
  }
  if (is_edgelist) {
    attr_names_el <- names(edgelist[, -c(1, 2)])
  }
  if (is_attr_name && is_edgelist) {
    if (!all(attr_name %in% attr_names_el)) {
      stop("You specified (an) attribute(s) in 'attr_name' that are/is not present in 'edgelist'.")
    } else {  # only keep columns from 'attr_name' in the edgelist
      keep_cols <- which(colnames(edgelist) %in% attr_name)
      edgelist <- edgelist[, c(1, 2, keep_cols)]
    }
  }

    if (is_edgelist) {
    eids <- sapply(1:nrow(edgelist),
                   function(z) {network::get.edgeIDs(object,
                                                     v = edgelist[z, 1],
                                                     alter = edgelist[z, 2])}
    )

    # check for duplicate edges in the edgelist
    dups <- which(duplicated(eids))
    if (length(dups) > 0) { # there are duplicate dyads in edgelist
      stop("There are duplicate edges you try to set values for, these are rows ", dups)
    }
    # check if all edges in the graph are included in the edgelist
    # Mirror the igraph path above: compute the missing edge ids explicitly so
    # only valid ids are inspected further.
    miss <- setdiff(seq_len(network::network.edgecount(object)), eids)
    if (length(miss) > 0) {  # some edges are missing in the edgelist
      miss_edges <- matrix(ncol = 2, nrow = length(miss))
      for (mm in 1:length(miss)) {
        dyad <- object$mel[[miss[mm]]][c("inl", "outl")] |> unlist() |> unname()
        miss_edges[mm, ] <- dyad
      }
      miss_edges <- paste0(miss_edges[, 1], "-", miss_edges[, 2]) |> paste(collapse = ", ")
      stop("Some edges from 'object' are missing in your 'edgelist', these are (sender-receiver): ", 
           miss_edges)
    }
    
    to_be_added <- names(edgelist)[-c(1, 2)]
    if (any(to_be_added %in% network::list.edge.attributes(object)) && !overwrite) {
      stop("The 'edgelist' contains (an) edge attribute(s) that are/is already present in the network,
          remove it from the 'edgelist' or set 'overwrite' to TRUE.")
    }
    
    for (att in to_be_added) {
      network::set.edge.attribute(object, att, value = edgelist[, att], e = eids)
    }
  } else { # attr_name and value are specified
    if (length(attr_name) == 1) { # only 1 attribute needs to be added
      if (is.null(dim(value))) { #value is a vector
        network::set.edge.attribute(object, attrname = attr_name,
                                    value = value)
        return(object)
      } else if (dim(value)[2] == 1) {  # value has only 1 column
        network::set.edge.attribute(object, attrname = attr_name,
                                    value = value)
        return(object)
      } else {
        stop("'attr_name' and 'value' do not have matching dimensions")
      }
    } else { # multiple attributes to be added
      if (length(attr_name) > 1 && is.null(ncol(value))) {
        stop("'attr_name' and 'value' do not have matching dimensions")
      } else if (length(attr_name) != ncol(value)) {
        stop("'attr_name' and 'value' do not have matching dimensions")
      } else {
        for (att in 1:length(attr_name)) {
          network::set.edge.attribute(object, attrname = attr_name[att], 
                                      value = value[, att])
        }
      }
    }
  }
  return(object)
}




### add_vertices ---------------------------------------------------------------

#' Add vertices to a graph
#'
#' Add one or more vertices to a graph or graph-like representation.
#'
#' This function extends the student-facing \pkg{snafun} API to the common graph
#' representations used throughout the package: \code{igraph}, \code{network},
#' adjacency matrices, and edgelists stored as \code{data.frame}s.
#'
#' For graph objects, the function behaves as expected: new vertices are added
#' and optional names and attributes can be attached immediately. For matrices,
#' rows and columns of zeros are added. For edgelists, isolates are stored in the
#' hidden vertex metadata that \code{snafun} already uses internally for
#' roundtrips through \code{to_edgelist()}.
#'
#' The \code{vertices} argument can be specified in two convenient ways:
#' \itemize{
#' \item as a single positive integer, giving the number of unnamed vertices to
#' add;
#' \item as a character vector, giving the names of the new vertices.
#' }
#'
#' For bipartite inputs, the partition of the new vertices must be specified via
#' \code{type}. Following the \code{igraph} convention, \code{FALSE} means the
#' first partition and \code{TRUE} the second partition.
#'
#' @param x graph object of class \code{igraph}, \code{network}, \code{matrix},
#' or \code{data.frame} edgelist
#' @param vertices either a single positive integer giving the number of new
#' vertices, or a character vector with the names of the new vertices
#' @param attributes optional vertex metadata for the new vertices. This can be
#' a \code{data.frame}, a named \code{matrix}, or a named \code{list}. The
#' number of rows (or elements per column) should equal the number of added
#' vertices. Matrix inputs do not support arbitrary vertex attributes beyond
#' optional row and column names.
#' @param type optional logical scalar or logical vector indicating the bipartite
#' partition of the new vertices. Required when \code{x} is bipartite.
#'
#' @return object of the same class as \code{x}
#' @export
#'
#' @examples
#' g <- snafun::create_manual_graph(A -- B -- C)
#' g <- snafun::add_vertices(g, c("D", "E"))
#' snafun::count_vertices(g)
#' snafun::extract_vertex_names(g)
#'
#' g <- snafun::add_vertices(
#'   g,
#'   "F",
#'   attributes = data.frame(group = "new", score = 10)
#' )
#' snafun::extract_vertex_attribute(g, "group")
#'
#' nw <- snafun::to_network(snafun::create_manual_graph(A -- B))
#' nw <- snafun::add_vertices(nw, c("C", "D"))
#' snafun::extract_vertex_names(nw)
#'
#' mat <- matrix(c(0, 1, 1, 0), nrow = 2, byrow = TRUE)
#' rownames(mat) <- colnames(mat) <- c("A", "B")
#' snafun::add_vertices(mat, "C")
#'
#' el <- data.frame(from = c("A", "B"), to = c("B", "A"))
#' el <- snafun::add_vertices(el, c("C", "D"))
#' snafun::count_vertices(snafun::to_igraph(el))
add_vertices <- function(x, vertices, attributes = NULL, type = NULL) {
  UseMethod("add_vertices")
}


#' @export
add_vertices.default <- function(x, vertices, attributes = NULL, type = NULL) {
  txt <- methods_error_message("x", "add_vertices")
  stop(txt)
}


#' @export
add_vertices.igraph <- function(x, vertices, attributes = NULL, type = NULL) {
  spec <- normalize_vertices_to_add(vertices = vertices, attributes = attributes, type = type)
  old_vertex_count <- snafun::count_vertices(x)
  is_bipartite_graph <- snafun::is_bipartite(x)

  if (is_bipartite_graph && is.null(spec$type)) {
    stop("Please specify 'type' when adding vertices to a bipartite graph.")
  }
  if (!is_bipartite_graph && !is.null(spec$type)) {
    stop("'type' is only meaningful for bipartite graphs.")
  }

  x <- igraph::add_vertices(x, nv = spec$n)
  new_vids <- seq.int(from = old_vertex_count + 1L, length.out = spec$n)

  if (!is.null(spec$vertex_names)) {
    x <- snafun::add_vertex_names(x, value = spec$vertex_names, vids = new_vids)
  }
  if (!is.null(spec$type)) {
    x <- igraph::set_vertex_attr(x, name = "type", index = new_vids, value = spec$type)
  }
  if (!is.null(spec$attributes)) {
    x <- set_vertex_attributes_subset_igraph(
      x = x,
      vids = new_vids,
      attributes = spec$attributes
    )
  }

  x
}


#' @export
add_vertices.network <- function(x, vertices, attributes = NULL, type = NULL) {
  spec <- normalize_vertices_to_add(vertices = vertices, attributes = attributes, type = type)
  is_bipartite_graph <- snafun::is_bipartite(x)

  if (is_bipartite_graph) {
    # The network class stores bipartite partition membership as a graph-level
    # count of the first partition, so inserting vertices into the correct block
    # is substantially simpler and safer through the igraph roundtrip.
    graph_i <- snafun::to_igraph(x)
    graph_i <- snafun::add_vertices(
      x = graph_i,
      vertices = vertices,
      attributes = attributes,
      type = type
    )
    return(snafun::to_network(graph_i))
  }

  if (!is.null(spec$type)) {
    stop("'type' is only meaningful for bipartite graphs.")
  }

  old_vertex_count <- snafun::count_vertices(x)
  network::add.vertices(x, nv = spec$n)
  new_vids <- seq.int(from = old_vertex_count + 1L, length.out = spec$n)

  if (!is.null(spec$vertex_names)) {
    x <- snafun::add_vertex_names(x, value = spec$vertex_names, vids = new_vids)
  }
  if (!is.null(spec$attributes)) {
    x <- set_vertex_attributes_subset_network(
      x = x,
      vids = new_vids,
      attributes = spec$attributes
    )
  }

  x
}


#' @export
add_vertices.matrix <- function(x, vertices, attributes = NULL, type = NULL) {
  spec <- normalize_vertices_to_add(vertices = vertices, attributes = attributes, type = type)
  is_bipartite_matrix <- nrow(x) != ncol(x)

  if (!is_bipartite_matrix && !is.null(spec$type)) {
    stop("'type' is only meaningful for bipartite graphs.")
  }
  if (!is.null(spec$attributes)) {
    stop("Matrices do not store arbitrary vertex attributes; use names only.")
  }

  if (!is_bipartite_matrix) {
    return(add_vertices_to_square_matrix(x = x, spec = spec))
  }

  if (is.null(spec$type)) {
    stop("Please specify 'type' when adding vertices to a bipartite matrix.")
  }
  add_vertices_to_bipartite_matrix(x = x, spec = spec)
}


#' @export
add_vertices.data.frame <- function(x, vertices, attributes = NULL, type = NULL) {
  if (inherits(x, "tbl_df")) {
    x <- as.data.frame(x)
  }
  spec <- normalize_vertices_to_add(vertices = vertices, attributes = attributes, type = type)
  stored_vertices <- extract_stored_edgelist_vertices(x)
  stored_bipartite <- attr(x, "snafun_bipartite", exact = TRUE)
  bipartite <- if (!is.null(stored_bipartite)) {
    isTRUE(stored_bipartite)
  } else {
    !is.null(spec$type)
  }
  vertex_table <- if (!is.null(stored_vertices)) {
    stored_vertices
  } else if (nrow(x) == 0L || ncol(x) < 2L) {
    data.frame(name = numeric(0))
  } else {
    vertex_keys <- unique(c(x[[1]], x[[2]]))
    vertices_existing <- data.frame(name = vertex_keys, stringsAsFactors = FALSE)
    if (bipartite) {
      vertices_existing$type <- infer_bipartite_vertex_types(x = x, vertex_keys = vertex_keys)
    }
    vertices_existing
  }

  if (bipartite && is.null(spec$type)) {
    stop("Please specify 'type' when adding vertices to a bipartite edgelist.")
  }
  if (!bipartite && !is.null(spec$type)) {
    stop("'type' is only meaningful for bipartite graphs.")
  }

  if (is.null(spec$vertex_names)) {
    spec$vertex_names <- default_new_vertex_keys(
      existing_keys = if (nrow(vertex_table) > 0) vertex_table[[1]] else NULL,
      n = spec$n
    )
  }

  if (nrow(vertex_table) > 0 && any(spec$vertex_names %in% vertex_table[[1]])) {
    stop("At least one of the supplied vertices already exists.")
  }

  new_vertices <- data.frame(name = spec$vertex_names, stringsAsFactors = FALSE)
  if (nrow(vertex_table) > 0) {
    new_vertices[[1]] <- coerce_like_template(spec$vertex_names, vertex_table[[1]])
  }

  if (!is.null(spec$type)) {
    new_vertices$type <- spec$type
    if (!"type" %in% colnames(vertex_table)) {
      vertex_table$type <- if (nrow(vertex_table) == 0L) logical(0) else rep(NA, nrow(vertex_table))
    }
  }

  if (!is.null(spec$attributes)) {
    new_vertices <- cbind(new_vertices, spec$attributes, stringsAsFactors = FALSE)
  }

  vertex_table <- bind_vertex_metadata_rows(vertex_table, new_vertices)
  store_edgelist_vertex_table(
    edgelist = x,
    vertex_table = vertex_table,
    keep_bipartite = !is.null(stored_bipartite) || !is.null(spec$type),
    bipartite = bipartite,
    keep_directed = !is.null(attr(x, "snafun_directed", exact = TRUE)),
    directed = attr(x, "snafun_directed", exact = TRUE)
  )
}




### add_edges ------------------------------------------------------------------

#' Add edges to a graph
#'
#' Add one or more edges to a graph or graph-like representation.
#'
#' The new edges can be supplied as a two-column edgelist or as a vector that
#' alternates senders and receivers. Additional columns in the edgelist are
#' interpreted as edge attributes. Alternatively, such attributes can be
#' provided separately through the \code{attributes} argument.
#'
#' For matrices, only a single numeric edge value can be stored per dyad. If no
#' value is supplied, newly added edges are coded as 1. If a one-mode matrix is
#' undirected, the symmetric cell is updated as well. Bipartite matrices are
#' supported, but they cannot contain arbitrary edge attributes beyond the
#' stored numeric cell value.
#'
#' For edgelists, rows are appended to the visible data.frame and hidden
#' \code{snafun} metadata are preserved where present.
#'
#' @param x graph object of class \code{igraph}, \code{network}, \code{matrix},
#' or \code{data.frame} edgelist
#' @param edges edge specification. This can be a vector of alternating senders
#' and receivers, or a \code{matrix}/\code{data.frame} with at least two columns
#' containing the edge endpoints
#' @param attributes optional edge attributes supplied separately from
#' \code{edges}. This can be a named \code{list}, a \code{data.frame}, or a
#' named \code{matrix}. The number of rows should equal the number of edges to
#' add. For matrices, at most one numeric attribute can be stored.
#'
#' @return object of the same class as \code{x}
#' @export
#'
#' @examples
#' g <- snafun::create_manual_graph(A -- B -- C)
#' g <- snafun::add_vertices(g, "D")
#' g <- snafun::add_edges(g, data.frame(from = c("A", "C"), to = c("C", "D")))
#' snafun::count_edges(g)
#'
#' g <- snafun::add_edges(
#'   g,
#'   data.frame(from = "A", to = "D", weight = 5, relation = "new")
#' )
#' snafun::extract_edge_attribute(g, "weight")
#'
#' nw <- snafun::to_network(snafun::create_manual_graph(A -- B))
#' nw <- snafun::add_vertices(nw, "C")
#' nw <- snafun::add_edges(nw, data.frame(from = "B", to = "C"))
#' snafun::count_edges(nw)
#'
#' mat <- matrix(0, nrow = 2, ncol = 2)
#' rownames(mat) <- colnames(mat) <- c("A", "B")
#' snafun::add_edges(mat, data.frame(from = "A", to = "B", weight = 2))
#'
#' el <- data.frame(from = c("A", "B"), to = c("B", "A"))
#' snafun::add_edges(el, data.frame(from = "B", to = "C", weight = 1))
add_edges <- function(x, edges, attributes = NULL) {
  UseMethod("add_edges")
}


#' @export
add_edges.default <- function(x, edges, attributes = NULL) {
  txt <- methods_error_message("x", "add_edges")
  stop(txt)
}


#' @export
add_edges.igraph <- function(x, edges, attributes = NULL) {
  edge_data <- normalize_edges_to_add(edges = edges, attributes = attributes)
  old_edge_count <- snafun::count_edges(x)
  if (!is.numeric(edge_data$from) || !is.numeric(edge_data$to)) {
    vertex_names <- snafun::extract_vertex_names(x)
    if (is.null(vertex_names) ||
        any(!as.character(edge_data$from) %in% as.character(vertex_names)) ||
        any(!as.character(edge_data$to) %in% as.character(vertex_names))) {
      stop("At least one of the supplied vertices does not exist in the graph.")
    }
  }
  edge_vector <- as.vector(t(edge_data[, c("from", "to"), drop = FALSE]))
  x <- igraph::add_edges(x, edges = edge_vector)

  if (ncol(edge_data) > 2L) {
    new_eids <- seq.int(from = old_edge_count + 1L, length.out = nrow(edge_data))
    x <- set_edge_attributes_subset_igraph(
      x = x,
      eids = new_eids,
      attributes = edge_data[, -(1:2), drop = FALSE]
    )
  }

  x
}


#' @export
add_edges.network <- function(x, edges, attributes = NULL) {
  edge_data <- normalize_edges_to_add(edges = edges, attributes = attributes)

  tails <- edge_data$from
  heads <- edge_data$to
  if (!is.numeric(tails) || !is.numeric(heads)) {
    vertex_names <- snafun::extract_vertex_names(x)
    tails <- match(as.character(tails), as.character(vertex_names))
    heads <- match(as.character(heads), as.character(vertex_names))
  }
  if (any(is.na(tails)) || any(is.na(heads))) {
    stop("At least one of the supplied vertices does not exist in the graph.")
  }

  old_edge_count <- snafun::count_edges(x)
  network::add.edges(x, tail = tails, head = heads)
  new_eids <- seq.int(from = old_edge_count + 1L, length.out = nrow(edge_data))

  if (ncol(edge_data) > 2L) {
    x <- set_edge_attributes_subset_network(
      x = x,
      eids = new_eids,
      attributes = edge_data[, -(1:2), drop = FALSE]
    )
  }

  x
}


#' @export
add_edges.matrix <- function(x, edges, attributes = NULL) {
  edge_data <- normalize_edges_to_add(edges = edges, attributes = attributes)
  is_bipartite_matrix <- nrow(x) != ncol(x)
  values <- edge_values_for_matrix(edge_data)

  if (!is_bipartite_matrix) {
    is_directed_matrix <- snafun::is_directed(x)
    from_ids <- resolve_matrix_vertex_positions(edge_data$from, rownames(x), nrow(x), "from")
    to_ids <- resolve_matrix_vertex_positions(edge_data$to, colnames(x), ncol(x), "to")
    for (row_id in seq_len(nrow(edge_data))) {
      x[from_ids[row_id], to_ids[row_id]] <- values[row_id]
      if (!is_directed_matrix) {
        x[to_ids[row_id], from_ids[row_id]] <- values[row_id]
      }
    }
    return(x)
  }

  from_ids <- resolve_matrix_vertex_positions(edge_data$from, rownames(x), nrow(x), "from")
  to_ids <- resolve_matrix_vertex_positions(edge_data$to, colnames(x), ncol(x), "to")
  for (row_id in seq_len(nrow(edge_data))) {
    x[from_ids[row_id], to_ids[row_id]] <- values[row_id]
  }
  x
}


#' @export
add_edges.data.frame <- function(x, edges, attributes = NULL) {
  if (inherits(x, "tbl_df")) {
    x <- as.data.frame(x)
  }
  edge_data <- normalize_edges_to_add(edges = edges, attributes = attributes)
  stored_vertices <- extract_stored_edgelist_vertices(x)
  stored_bipartite <- attr(x, "snafun_bipartite", exact = TRUE)
  stored_directed <- attr(x, "snafun_directed", exact = TRUE)

  out <- bind_edgelist_rows(x, edge_data)

  if (!is.null(stored_vertices)) {
    out <- update_edgelist_metadata_for_new_endpoints(
      edgelist = out,
      edge_data = edge_data,
      vertex_table = stored_vertices,
      bipartite = stored_bipartite,
      directed = stored_directed
    )
  } else {
    attr(out, "snafun_vertices") <- NULL
    attr(out, "snafun_bipartite") <- stored_bipartite
    attr(out, "snafun_directed") <- stored_directed
  }

  out
}


#' Normalize a vertex-addition request
#'
#' @param vertices user-supplied \code{vertices} argument
#' @param attributes optional vertex metadata
#' @param type optional bipartite partition indicator
#'
#' @return standardized list describing the new vertices
#' @keywords internal
#' @noRd
normalize_vertices_to_add <- function(vertices, attributes = NULL, type = NULL) {
  vertex_names <- NULL

  if (length(vertices) == 1L && is.numeric(vertices) && !is.na(vertices)) {
    if (vertices < 1 || vertices != as.integer(vertices)) {
      stop("When 'vertices' is numeric, it should be a single positive integer.")
    }
    n <- as.integer(vertices)
  } else if (is.character(vertices)) {
    vertex_names <- as.character(vertices)
    n <- length(vertex_names)
    if (n == 0L) {
      stop("Please supply at least one vertex to add.")
    }
  } else {
    stop("'vertices' should either be a single positive integer or a character vector.")
  }

  attributes <- normalize_vertex_attribute_input(attributes = attributes, n = n)

  if (!is.null(type)) {
    if (length(type) == 1L) {
      type <- rep(as.logical(type), n)
    } else if (length(type) == n) {
      type <- as.logical(type)
    } else {
      stop("'type' should have length 1 or the same length as the number of added vertices.")
    }
    if (any(is.na(type))) {
      stop("'type' should not contain missing values.")
    }
  }

  list(
    n = n,
    vertex_names = vertex_names,
    attributes = attributes,
    type = type
  )
}


#' Normalize vertex attribute input
#'
#' @param attributes optional vertex metadata
#' @param n expected number of rows
#'
#' @return data.frame or \code{NULL}
#' @keywords internal
#' @noRd
normalize_vertex_attribute_input <- function(attributes, n) {
  if (is.null(attributes)) {
    return(NULL)
  }

  if (is.matrix(attributes)) {
    if (is.null(colnames(attributes))) {
      stop("Matrix 'attributes' should have column names.")
    }
    attributes <- as.data.frame(attributes, stringsAsFactors = FALSE)
  } else if (is.list(attributes) && !inherits(attributes, "data.frame")) {
    if (is.null(names(attributes)) || any(names(attributes) == "")) {
      stop("List 'attributes' should be named.")
    }
    attributes <- as.data.frame(attributes, stringsAsFactors = FALSE)
  } else if (!inherits(attributes, "data.frame")) {
    stop("'attributes' should be a data.frame, named matrix, or named list.")
  }

  if (nrow(attributes) != n) {
    stop("The number of rows in 'attributes' should equal the number of added vertices.")
  }

  attributes
}


#' Set vertex attributes for a subset of igraph vertices
#'
#' @keywords internal
#' @noRd
set_vertex_attributes_subset_igraph <- function(x, vids, attributes) {
  for (attribute_name in colnames(attributes)) {
    x <- igraph::set_vertex_attr(
      graph = x,
      name = attribute_name,
      index = vids,
      value = attributes[[attribute_name]]
    )
  }
  x
}


#' Set vertex attributes for a subset of network vertices
#'
#' @keywords internal
#' @noRd
set_vertex_attributes_subset_network <- function(x, vids, attributes) {
  for (attribute_name in colnames(attributes)) {
    network::set.vertex.attribute(
      x,
      attrname = attribute_name,
      value = attributes[[attribute_name]],
      v = vids
    )
  }
  x
}


#' Add vertices to a square adjacency matrix
#'
#' @keywords internal
#' @noRd
add_vertices_to_square_matrix <- function(x, spec) {
  old_n <- nrow(x)
  new_n <- old_n + spec$n
  out <- matrix(0, nrow = new_n, ncol = new_n)
  out[seq_len(old_n), seq_len(old_n)] <- x

  existing_names <- rownames(x)
  if (!is.null(existing_names) || !is.null(spec$vertex_names)) {
    if (is.null(existing_names)) {
      existing_names <- as.character(seq_len(old_n))
    }
    new_names <- if (is.null(spec$vertex_names)) {
      default_new_vertex_keys(existing_names, spec$n)
    } else {
      spec$vertex_names
    }
    rownames(out) <- c(existing_names, new_names)
    colnames(out) <- c(existing_names, new_names)
  }

  out
}


#' Add vertices to a bipartite incidence matrix
#'
#' @keywords internal
#' @noRd
add_vertices_to_bipartite_matrix <- function(x, spec) {
  type_false <- sum(!spec$type)
  type_true <- sum(spec$type)
  out <- matrix(0, nrow = nrow(x) + type_false, ncol = ncol(x) + type_true)
  out[seq_len(nrow(x)), seq_len(ncol(x))] <- x

  existing_row_names <- rownames(x)
  existing_col_names <- colnames(x)

  if (!is.null(existing_row_names) || !is.null(existing_col_names) || !is.null(spec$vertex_names)) {
    if (is.null(existing_row_names)) {
      existing_row_names <- as.character(seq_len(nrow(x)))
    }
    if (is.null(existing_col_names)) {
      existing_col_names <- as.character(seq_len(ncol(x)))
    }

    false_names <- if (type_false == 0L) {
      character(0)
    } else if (!is.null(spec$vertex_names)) {
      spec$vertex_names[!spec$type]
    } else {
      default_new_vertex_keys(existing_row_names, type_false)
    }

    true_names <- if (type_true == 0L) {
      character(0)
    } else if (!is.null(spec$vertex_names)) {
      spec$vertex_names[spec$type]
    } else {
      default_new_vertex_keys(existing_col_names, type_true)
    }

    rownames(out) <- c(existing_row_names, false_names)
    colnames(out) <- c(existing_col_names, true_names)
  }

  out
}


#' Generate default vertex identifiers
#'
#' @keywords internal
#' @noRd
default_new_vertex_keys <- function(existing_keys, n) {
  if (is.null(existing_keys) || length(existing_keys) == 0L) {
    return(seq_len(n))
  }

  if (is.numeric(existing_keys)) {
    max_key <- suppressWarnings(max(existing_keys, na.rm = TRUE))
    if (!is.finite(max_key)) {
      return(seq_len(n))
    }
    return(seq.int(from = max_key + 1, length.out = n))
  }

  base_names <- paste0("vertex_", seq_len(length(existing_keys) + n))
  base_names[seq.int(from = length(existing_keys) + 1L, length.out = n)]
}


#' Build the current vertex table for an edgelist
#'
#' @keywords internal
#' @noRd
existing_edgelist_vertex_table <- function(x) {
  stored_vertices <- extract_stored_edgelist_vertices(x)
  if (!is.null(stored_vertices)) {
    return(stored_vertices)
  }

  if (nrow(x) == 0L || ncol(x) < 2L) {
    return(data.frame(name = numeric(0)))
  }

  vertex_keys <- unique(c(x[[1]], x[[2]]))
  vertices <- data.frame(name = vertex_keys, stringsAsFactors = FALSE)

  if (edgelist_bipartite_flag(x)) {
    vertices$type <- infer_bipartite_vertex_types(x = x, vertex_keys = vertex_keys)
  }

  vertices
}


#' Determine whether an edgelist should be treated as bipartite
#'
#' @keywords internal
#' @noRd
edgelist_bipartite_flag <- function(x) {
  if (!is.null(attr(x, "snafun_bipartite", exact = TRUE))) {
    return(isTRUE(attr(x, "snafun_bipartite", exact = TRUE)))
  }
  is_visibly_bipartite_edgelist(x)
}


#' Infer bipartite vertex types from an edgelist
#'
#' @keywords internal
#' @noRd
infer_bipartite_vertex_types <- function(x, vertex_keys) {
  from_keys <- unique(x[[1]])
  to_keys <- unique(x[[2]])
  inferred_type <- rep(NA, length(vertex_keys))
  inferred_type[vertex_keys %in% from_keys] <- FALSE
  inferred_type[vertex_keys %in% to_keys] <- TRUE
  inferred_type
}


#' Bind vertex metadata rows while preserving all columns
#'
#' @keywords internal
#' @noRd
bind_vertex_metadata_rows <- function(existing, new) {
  if (nrow(existing) == 0L) {
    return(new)
  }
  if (nrow(new) == 0L) {
    return(existing)
  }

  all_columns <- union(colnames(existing), colnames(new))
  for (column_name in setdiff(all_columns, colnames(existing))) {
    existing[[column_name]] <- NA
  }
  for (column_name in setdiff(all_columns, colnames(new))) {
    new[[column_name]] <- NA
  }
  new <- new[, all_columns, drop = FALSE]
  existing <- existing[, all_columns, drop = FALSE]
  rbind(existing, new)
}


#' Store edgelist metadata after a vertex update
#'
#' @keywords internal
#' @noRd
store_edgelist_vertex_table <- function(edgelist,
                                        vertex_table,
                                        keep_bipartite = FALSE,
                                        bipartite = NULL,
                                        keep_directed = FALSE,
                                        directed = NULL) {
  attr(edgelist, "snafun_vertices") <- vertex_table
  if (isTRUE(keep_bipartite)) {
    attr(edgelist, "snafun_bipartite") <- bipartite
  } else {
    attr(edgelist, "snafun_bipartite") <- NULL
  }
  if (isTRUE(keep_directed)) {
    attr(edgelist, "snafun_directed") <- directed
  } else {
    attr(edgelist, "snafun_directed") <- NULL
  }
  edgelist
}


#' Coerce values to the class of a template vector
#'
#' @keywords internal
#' @noRd
coerce_like_template <- function(values, template) {
  if (is.numeric(template)) {
    return(as.numeric(values))
  }
  if (is.integer(template)) {
    return(as.integer(values))
  }
  as.character(values)
}


#' Normalize an edge-addition request
#'
#' @keywords internal
#' @noRd
normalize_edges_to_add <- function(edges, attributes = NULL) {
  edge_data <- if (is.atomic(edges) && !is.matrix(edges) && !is.data.frame(edges)) {
    if (length(edges) %% 2 != 0) {
      stop("Atomic edge vectors should contain an even number of values.")
    }
    data.frame(
      from = edges[seq(1, length(edges), by = 2)],
      to = edges[seq(2, length(edges), by = 2)],
      stringsAsFactors = FALSE
    )
  } else if (is.matrix(edges)) {
    if (ncol(edges) < 2L) {
      stop("Matrix 'edges' should have at least two columns.")
    }
    as.data.frame(edges, stringsAsFactors = FALSE)
  } else if (is.data.frame(edges)) {
    if (ncol(edges) < 2L) {
      stop("Data-frame 'edges' should have at least two columns.")
    }
    as.data.frame(edges, stringsAsFactors = FALSE)
  } else {
    stop("'edges' should be an atomic vector, matrix, or data.frame.")
  }

  colnames(edge_data)[1:2] <- c("from", "to")

  if (!is.null(attributes)) {
    if (ncol(edge_data) > 2L) {
      stop("Please specify edge attributes either inside 'edges' or in 'attributes', not both.")
    }
    attributes <- normalize_edge_attribute_input(attributes = attributes, n = nrow(edge_data))
    edge_data <- cbind(edge_data, attributes, stringsAsFactors = FALSE)
  }

  edge_data
}


#' Normalize edge attribute input
#'
#' @keywords internal
#' @noRd
normalize_edge_attribute_input <- function(attributes, n) {
  if (is.matrix(attributes)) {
    if (is.null(colnames(attributes))) {
      stop("Matrix 'attributes' should have column names.")
    }
    attributes <- as.data.frame(attributes, stringsAsFactors = FALSE)
  } else if (is.list(attributes) && !inherits(attributes, "data.frame")) {
    if (is.null(names(attributes)) || any(names(attributes) == "")) {
      stop("List 'attributes' should be named.")
    }
    attributes <- as.data.frame(attributes, stringsAsFactors = FALSE)
  } else if (!inherits(attributes, "data.frame")) {
    stop("'attributes' should be a data.frame, named matrix, or named list.")
  }

  if (nrow(attributes) != n) {
    stop("The number of rows in 'attributes' should equal the number of added edges.")
  }

  attributes
}


#' Set edge attributes for a subset of igraph edges
#'
#' @keywords internal
#' @noRd
set_edge_attributes_subset_igraph <- function(x, eids, attributes) {
  for (attribute_name in colnames(attributes)) {
    x <- igraph::set_edge_attr(
      graph = x,
      name = attribute_name,
      index = eids,
      value = attributes[[attribute_name]]
    )
  }
  x
}


#' Set edge attributes for a subset of network edges
#'
#' @keywords internal
#' @noRd
set_edge_attributes_subset_network <- function(x, eids, attributes) {
  for (attribute_name in colnames(attributes)) {
    network::set.edge.attribute(
      x,
      attrname = attribute_name,
      value = attributes[[attribute_name]],
      e = eids
    )
  }
  x
}


#' Resolve edge values for matrix updates
#'
#' @keywords internal
#' @noRd
edge_values_for_matrix <- function(edge_data) {
  attr_columns <- setdiff(colnames(edge_data), c("from", "to"))
  if (length(attr_columns) == 0L) {
    return(rep(1, nrow(edge_data)))
  }
  if (length(attr_columns) > 1L) {
    stop("Matrices can store at most one edge value per dyad.")
  }
  values <- edge_data[[attr_columns]]
  if (!is.numeric(values)) {
    stop("Matrices require numeric edge values.")
  }
  values
}


#' Resolve matrix vertex positions from ids or names
#'
#' @keywords internal
#' @noRd
resolve_matrix_vertex_positions <- function(values, dim_names, max_position, argument_name) {
  if (!is.null(dim_names)) {
    positions <- match(as.character(values), as.character(dim_names))
    if (any(is.na(positions)) && is.numeric(values)) {
      numeric_positions <- as.integer(values)
      positions[is.na(positions)] <- numeric_positions[is.na(positions)]
    }
  } else {
    if (!is.numeric(values)) {
      stop("Unnamed matrices require numeric vertex ids in the edgelist.")
    }
    positions <- as.integer(values)
  }

  if (any(is.na(positions)) || any(positions < 1L) || any(positions > max_position)) {
    stop("At least one of the supplied '", argument_name, "' vertices does not exist in the matrix.")
  }
  positions
}


#' Bind new rows to an edgelist while preserving all columns
#'
#' @keywords internal
#' @noRd
bind_edgelist_rows <- function(existing, new) {
  all_columns <- union(colnames(existing), colnames(new))
  for (column_name in setdiff(all_columns, colnames(existing))) {
    existing[[column_name]] <- NA
  }
  for (column_name in setdiff(all_columns, colnames(new))) {
    new[[column_name]] <- NA
  }
  existing <- existing[, all_columns, drop = FALSE]
  new <- new[, all_columns, drop = FALSE]
  out <- rbind(existing, new)
  rownames(out) <- NULL
  out
}


#' Update hidden edgelist metadata after new endpoints appear
#'
#' @keywords internal
#' @noRd
update_edgelist_metadata_for_new_endpoints <- function(edgelist,
                                                       edge_data,
                                                       vertex_table,
                                                       bipartite,
                                                       directed) {
  current_keys <- vertex_table[[1]]
  visible_keys <- unique(c(edge_data$from, edge_data$to))
  missing_keys <- visible_keys[!visible_keys %in% current_keys]

  if (length(missing_keys) == 0L) {
    return(
      store_edgelist_vertex_table(
        edgelist = edgelist,
        vertex_table = vertex_table,
        keep_bipartite = !is.null(bipartite),
        bipartite = bipartite,
        keep_directed = !is.null(directed),
        directed = directed
      )
    )
  }

  new_vertices <- data.frame(name = missing_keys, stringsAsFactors = FALSE)
  new_vertices[[1]] <- coerce_like_template(missing_keys, current_keys)
  if ("type" %in% colnames(vertex_table)) {
    if (isTRUE(bipartite)) {
      new_vertices$type <- infer_vertex_types_from_new_edges(
        edge_data = edge_data,
        vertex_keys = missing_keys
      )
    } else {
      new_vertices$type <- rep(NA, length(missing_keys))
    }
  }

  vertex_table <- bind_vertex_metadata_rows(vertex_table, new_vertices)
  store_edgelist_vertex_table(
    edgelist = edgelist,
    vertex_table = vertex_table,
    keep_bipartite = !is.null(bipartite),
    bipartite = bipartite,
    keep_directed = !is.null(directed),
    directed = directed
  )
}


#' Infer bipartite types for new edgelist vertices
#'
#' @keywords internal
#' @noRd
infer_vertex_types_from_new_edges <- function(edge_data, vertex_keys) {
  inferred <- rep(NA, length(vertex_keys))
  for (index in seq_along(vertex_keys)) {
    key <- vertex_keys[index]
    in_from <- any(edge_data$from == key)
    in_to <- any(edge_data$to == key)
    if (in_from && in_to) {
      stop("A vertex in a bipartite edgelist cannot occur in both endpoint columns.")
    }
    if (in_from) {
      inferred[index] <- FALSE
    }
    if (in_to) {
      inferred[index] <- TRUE
    }
  }
  inferred
}





### add_graph_attributes ------------------------------------------------------

#' Add a graph attribute
#' 
#' Add a graph-level attribute to the graph object
#'
#' @param x graph of class \code{igraph} or \code{network}
#' @param attr_name character, name to be used for the attribute
#' @param value the value to be given to the attribute
#'
#' @return graph with the added graph attribute
#' @export
add_graph_attribute <- function(x, attr_name = NULL, value) {
  UseMethod("add_graph_attributes")  
}


#' @export
add_graph_attribute.default <- function(x, attr_name = NULL, value) {
  txt <- methods_error_message("x", "add_vertex_attribute")
  stop(txt) 
}


#' @export
add_graph_attribute.igraph <- function(x, attr_name = NULL, value) {
  igraph::set_graph_attr(graph = x, name = attr_name, value = value)
}



#' @export
add_graph_attribute.network <- function(x, attr_name = NULL, value) {
  network::set.network.attribute(x = x, attrname = attr_name, value = value)
  return(x)
}




### separate functions ------------------------------------------------------

lowest_col_first <- function(x) {
  first_larger <- which(x[, 1] > x[, 2])
  for (rij in first_larger) {
    x[rij, ] <- x[rij, c(2, 1)]
  }
  x
}



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
#' g <- igraph::graph.ring(5)
#' add_vertex_attributes(g, "at1", 1:5) |> 
#'   igraph::get.vertex.attribute("at1")
#' 
#' add_vertex_attributes(g, value = data.frame(naam = 1:5)) |> 
#'   igraph::get.vertex.attribute("naam")
#' 
#' add_vertex_attributes(g, value = data.frame(een = 11:15, twee = 21:25))
#' 
#' add_vertex_attributes(g, c("twee", "een"), 
#'   value = data.frame(een = 11:15, twee = 21:25, drie = 31:35))
#' 
#' g <- sna::rgraph(10, tprob = .2) |> network::as.network()
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
    x <- igraph::set.vertex.attribute(x, name = attr_name, value = value)
    
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
    eids <- igraph::get.edge.ids(object, vp = vp)
    
    # check for duplicate edges in the edgelist
    dups <- which(duplicated(eids))
    if (length(dups) > 0) { # there are duplicate dyads in edgelist
      stop("There are duplicate edges you try to set values for, these are rows ", dups)
    }
    # check if all edges in the graph are included in the edgelist
    miss <- which(!1:igraph::ecount(object) %in% eids)
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
    miss <- which(!1:network::network.edgecount(object) %in% eids)
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

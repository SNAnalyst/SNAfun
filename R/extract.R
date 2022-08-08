
#' Extract edge id's
#' 
#' Extract the edge id's for given sets of ego(s) and alter(s)
#' 
#' This is a utility function that extracts the id of specific edges. 
#' This is mainly useful when working with the same graph, both as a \code{network} 
#' and \code{igraph} object, because these objects are constructed differently 
#' and internally order the edges differently.
#' 
#' There are three arguments that help determine for which edges the id's are extracted.
#' 
#' When only \code{ego} is specified, \emph{all} edges are included that have an 'ego' 
#' node as the sender (in case of a directed graph) or as a sender or receiver 
#' (in case of an undirected graph).
#' 
#' When only \code{alter} is specified, \emph{all} edges are included that have an 'alter' 
#' node as the receiver (in case of a directed graph) or as a sender or receiver 
#' (in case of an undirected graph).
#' 
#' When both \code{ego} and \code{alter} are specified, edges are included that 
#' have an 'ego' as the sender and an 'alter' as receiver (or vice versa, in 
#' case of an undirected graph).
#' 
#' When only \code{edgelist} is specified, the exact edges specified in that 
#' edgelist are used.
#' 
#' In case an id is requested for an edge that does not occur in the graph, 
#' zero is returned for that non-existant edge(s).
#' 
#' Note that \code{ego}, \code{vertex}, and \code{edgelist} should only contain 
#' numeric vertex id's and will not accept vertex names in the current implementation 
#' of this function.
#' 
#' @param object graph of class \code{network} or \code{igraph}
#' @param ego numeric vector with the vertex id's
#' @param alter numeric vector with the vertex id's
#' @param edgelist matrix or data.frame with two numeric columns, c
#' ontaining the vertex id's. Is ignored when \code{ego} and/or \code{alter} 
#' are/is specified.
#'
#' @return data.frame with three columns: ego (=sender), alter (=receiver), 
#' eid (= edge id)
#' @export
extract_edge_id <- function(object, ego, alter, edgelist) {
  UseMethod("extract_edge_id")
}

#' @export
extract_edge_id.default <- function(object, ego, alter, edgelist) {
  txt <- methods_error_message("object", "extract_edge_id")
  stop(txt)
}


#' @export
extract_edge_id.network <- function(object, ego, alter, edgelist) {
  directed = is_directed(object)
  check_ego <- !missing(ego)
  check_alter <- !missing(alter)
  check_edgelist <- !missing(edgelist)
  
  if (check_ego & check_alter) {
    if (!is.numeric(ego) | !is.numeric(ego)) {
      stop("Please provide numeric vertex id's for 'ego' and 'alter'")
    }
    edgelist <- expand.grid(ego, alter)
  }
  
  if (check_ego & !check_alter) {
    if (!is.numeric(ego)) {
      stop("Please provide numeric vertex id's for 'ego'")
    }
    alter <- lapply(ego, function(z) {
      j <- network::get.neighborhood(object, v = z, type = "out")
      if (length(j) > 0) {
        data.frame(i = z, j = j)
      } else {
        NULL
      }
    })
    edgelist <- do.call("rbind", alter) # turn the dataframes in the list to one dataframe
  }
  
  if (!check_ego & check_alter) {
    if (!is.numeric(alter)) {
      stop("Please provide numeric vertex id's for 'alter'")
    }
    ego <- lapply(alter, function(z) {
      i <- network::get.neighborhood(object, v = z, type = "in")
      if (length(i) > 0) {
        data.frame(i = i, j = z)
      } else {
        NULL
      }
    })
    edgelist <- do.call("rbind", ego) # turn the dataframes in the list to one dataframe
  }
  
  if (is.null(edgelist)) {
    return(NULL)
  }
  
  edgelist_is_numeric <- sapply(edgelist, is.numeric)
  if (!all(edgelist_is_numeric)) {
    stop("Provide only numeric id's for the requested vertices")
  }

  colnames(edgelist) <- c("ego", "alter")
  eids <- network::get.dyads.eids(object, 
                                  tails = edgelist[, "ego"], 
                                  heads = edgelist[, "alter"],
                                  neighborhood = "out")
  # if en edge does not exist, fill this out as eid "0"
  eids <- lapply(eids, function(z) {
    if (length(z) > 0) {
      return(z)
    } else {
      return(0)
    }
  }) |> 
    unlist()
  
  out <- cbind(edgelist, eid = eids)
  out <- out[!duplicated(out), ]
  out <- out[order(out[, 1], out[, 2]), ]
  out
}


#' @export
extract_edge_id.igraph <- function(object, ego, alter, edgelist) {
  directed = is_directed(object)
  check_ego <- !missing(ego)
  check_alter <- !missing(alter)
  check_edgelist <- !missing(edgelist)
  
  if (check_ego & check_alter) {
    if (!is.numeric(ego) | !is.numeric(ego)) {
      stop("Please provide numeric vertex id's for 'ego' and 'alter'")
    }
    edgelist <- expand.grid(ego, alter)
  }
  
  if (check_ego & !check_alter) {
    if (!is.numeric(ego)) {
      stop("Please provide numeric vertex id's for 'ego'")
    }
    alter <- lapply(ego, function(z) {
      j <- igraph::neighbors(object, v = z, mode = "out") |> as.vector()
      if (length(j) > 0) {
        data.frame(i = z, j = j)
      } else {
        NULL
      }
    })
    edgelist <- do.call("rbind", alter) # turn the dataframes in the list to one dataframe
  }
  
  if (!check_ego & check_alter) {
    if (!is.numeric(alter)) {
      stop("Please provide numeric vertex id's for 'alter'")
    }
    ego <- lapply(alter, function(z) {
      i <- igraph::neighbors(object, v = z, mode = "in") |> as.vector()
      if (length(i) > 0) {
        data.frame(i = i, j = z)
      } else {
        NULL
      }
    })
    edgelist <- do.call("rbind", ego) # turn the dataframes in the list to one dataframe
  }
  
  if (is.null(edgelist)) {
    return(NULL)
  }
  
  edgelist_is_numeric <- sapply(edgelist, is.numeric)
  if (!all(edgelist_is_numeric)) {
    stop("Provide only numeric id's for the requested vertices")
  }
  
  colnames(edgelist) <- c("ego", "alter")
  vp <- t(edgelist[, 1:2]) |> matrix(nrow = 1, byrow = TRUE) |> as.vector()
  eids <- igraph::get.edge.ids(object, vp = vp, directed = directed, error = FALSE)
  out <- cbind(edgelist, eid = eids)
  out <- out[!duplicated(out), ]
  out <- out[order(out[, 1], out[, 2]), ]
  out
}






#' Extract attributes from the graph object
#'
#' @param x graph object of class \code{igraph} or \code{network}
#' @param name name of the attribute to be accessed
#'
#' @return the values of the requested attributes (if any)
#' @name extract
NULL


#' @export
#' @rdname extract
extract_vertex_attribute <- function(x, name) {
  UseMethod("extract_vertex_attribute")
}

#' @export
extract_vertex_attribute.default <- function(x, name) {
  txt <- methods_error_message("x", "extract_vertex_attribute")
  stop(txt)
}

#' @export
extract_vertex_attribute.igraph <- function(x, name) {
  igraph::get.vertex.attribute(x, name = name)
}


#' @export
extract_vertex_attribute.network <- function(x, name) {
  network::get.vertex.attribute(x, attrname = name)
}


#' @export
#' @rdname extract
extract_vertex_names <- function(x) {
  UseMethod("extract_vertex_names")
}

#' @export
extract_vertex_names.default <- function(x) {
  txt <- methods_error_message("x", "extract_vertex_names")
  stop(txt)
}

#' @export
extract_vertex_names.igraph <- function(x) {
  igraph::get.vertex.attribute(x, name = "name")
}


#' @export
extract_vertex_names.network <- function(x) {
  network::network.vertex.names(x)
}


#' @export
#' @rdname extract
extract_edge_attribute <- function(x, name) {
  UseMethod("extract_edge_attribute")
}

#' @export
extract_edge_attribute.default <- function(x, name) {
  txt <- methods_error_message("x", "extract_edge_attribute")
  stop(txt)
}

#' @export
extract_edge_attribute.igraph <- function(x, name) {
  igraph::get.edge.attribute(x, name = name)
}


#' @export
extract_edge_attribute.network <- function(x, name) {
  network::get.edge.attribute(x, attrname = name)
}



#' @export
#' @rdname extract
extract_graph_attribute <- function(x, name) {
  UseMethod("extract_graph_attribute")
}

#' @export
extract_graph_attribute.default <- function(x, name) {
  txt <- methods_error_message("x", "extract_graph_attribute")
  stop(txt)
}

#' @export
extract_graph_attribute.igraph <- function(x, name) {
  igraph::get.graph.attribute(x, name = name)
}


#' @export
extract_graph_attribute.network <- function(x, name) {
  network::get.network.attribute(x, attrname = name)
}


### extract_subgraph -----------------------------------------------



#' Extract a subgraph
#' 
#' Extract a subgraph
#' 
#' Extract a subgraph from an input graph object by \emph{either} choosing which 
#' vertices to keep \emph{or} which edges to keep.
#' 
#' When both \code{v_to_keep} and \code{e_to_keep} are provided, only 
#' \code{v_to_keep} is used. Of course, the resulting graph can subsequently 
#' be used as input to this function while providing \code{e_to_keep} to further 
#' prune the network. 
#' 
#' When vertices are removed (through \code{v_to_keep}), exactly the specified 
#' vertices and all the already existing edges between them will be kept in the 
#' resulting graph. Edges that lose their sender and/or receiver are removed  
#' from the resulting graph.
#' 
#' Both \code{v_to_keep} and \code{e_to_keep} are numeric vectors and do not 
#' take names.
#' 
#' The function works on both \code{igraph} and \code{network} and returns a subgraph 
#' of that same class.
#' 
#' The underlying functions that perform the actual extraction are 
#' \code{\link[igraph]{induced_subgraph}}, 
#' \code{\link[igraph]{subgraph.edges}}, and 
#' \code{\link[network]{get.inducedSubgraph}}.
#' Consult these functions for more detail and, in some cases, some more 
#' functionality.
#'
#' @param x input graph
#' @param v_to_keep Numeric vector, the vertices of the original graph which 
#' will form the subgraph.
#' @param e_to_keep The edge ids of the edges that will be kept in the resulting graph.
#'
#' @return graph of same class as the input graph
#' @export
#'
#' @examples
#' g <- igraph::make_ring(10)
#' extract_subgraph(g, v_to_keep = 3:8)
#' extract_subgraph(g, e_to_keep = 4:8)
#' extract_subgraph(g, v_to_keep = 3:8, e_to_keep = 4:8)
#' 
#' g <- to_network(g)
#' extract_subgraph(g, v_to_keep = 3:8)
#' extract_subgraph(g, e_to_keep = 4:8)
#' extract_subgraph(g, v_to_keep = 3:8, e_to_keep = 4:8)
#' 
#' data(emon, package = "network")
#' g <- emon$MtStHelens
#' extract_subgraph(g, e_to_keep = which(extract_edge_attribute(g, 'Frequency') == 2))
#' 
#' g_i <- to_igraph(g)
#' extract_subgraph(g_i, e_to_keep = which(extract_edge_attribute(g_i, 'weight') == 2))
extract_subgraph <- function(x, v_to_keep, e_to_keep) {
  UseMethod("extract_subgraph")
}

#' @export
extract_subgraph.default <- function(x, v_to_keep, e_to_keep) {
  txt <- methods_error_message("x", "extract_subgraph")
  stop(txt)
}

#' @export
extract_subgraph.igraph <- function(x, v_to_keep, e_to_keep) {
  if (!missing(v_to_keep)) {
    if (!missing(e_to_keep)) {
      warning("When both 'v_to_keep' and 'e_to_keep' are specified, only 'v_to_keep' is used, to ensure result integrity.")
    }
    return(igraph::induced_subgraph(x, vids = v_to_keep, impl = "auto"))
  }
  if (!missing(e_to_keep)) {
    x <- igraph::subgraph.edges(x, eids = e_to_keep)
  }
  return(x)
}


#' @export
extract_subgraph.network <- function(x, v_to_keep, e_to_keep) {
  if (!missing(v_to_keep)) {
    if (!missing(e_to_keep)) {
      warning("When both 'v_to_keep' and 'e_to_keep' are specified, only 'v_to_keep' is used, to ensure result integrity.")
    }
    return(network::get.inducedSubgraph(x, v = v_to_keep))
  }
  if (!missing(e_to_keep)) {
    x <- network::get.inducedSubgraph(x, eid = e_to_keep)
  }
  return(x)
}



## neighbors -------------------------------------------------------------------


#' Extract the neighbors of a vertex
#' 
#' Get the names or ID's of all vertices with whom the focal vertex is connected
#' 
#' When \code{mode == "out"} the function returns all vertices that receive an 
#' edge from \code{vertex}.
#' 
#' When \code{mode == "in"} the function returns all vertices that send an 
#' edge to \code{vertex}.
#' 
#' When \code{mode == "all"} all vertices with an edge to or from \code{vertex} 
#' are returned.
#' 
#' In an undirected graph, \code{mode} will always be taken to be "all".
#' 
#' Note that \code{vertex} needs to be a \emph{single} vertex, not a vector.
#' 
#' @param x graph object if class \code{igraph} or \code{network}
#' @param vertex number or name (if the graph has vertex names) of a single vertex
#' @param type the type of neighborhood to be returned
#'
#' @return vector with vertex ID's or names of the neighbors
#' @export
#'
#' @examples
#' m <- matrix(0, 3, 3)
#' m[1, 2] <- 1; m[2, 3] <- 1; m[3, 1] <- 1
#' g_i <- snafun::to_igraph(m)
#' extract_neighbors(g_i, 1, "out")
#' extract_neighbors(g_i, 1, "in")
#' extract_neighbors(g_i, 1, "all")
#' 
#' g_n <- snafun::to_network(m)
#' extract_neighbors(g_n, 1, "out")
#' extract_neighbors(g_n, 1, "in")
#' extract_neighbors(g_n, 1, "all")
extract_neighbors <- function(x, vertex, type = c("out", "in", "all")) {
  UseMethod("extract_neighbors")
}


#' @export
extract_neighbors.default <- function(x, vertex, type = c("out", "in", "all")) {
  txt <- methods_error_message("x", "extract_neighbors")
  stop(txt)
}


#' @export
extract_neighbors.igraph <- function(x, vertex, type = c("out", "in", "all")) {
  type <- snafun.match.arg(type)
  if (length(vertex) != 1) stop("You need to specify exactly 1 vertex")
  
  nb <- tryCatch(igraph::neighbors(graph = x, v = vertex, mode = type), error = function(e) e)
  if (inherits(nb, "simpleError")) {
    if (nb$message == "Invalid vertex names") {
      stop("This vertex is in not the graph")
    } else if (grepl("Given vertex is not in the graph", nb$message)) {
      stop("This vertex is in not the graph")
    } else {
      stop(nb$message)
    }
  } else {
    nb <- as.vector(nb)
  }
  
  if (is.character(vertex) & has_vertexnames(x)) {
    nb <- igraph::V(x)$name[nb]
  }
  sort(nb)
}


#' @export
extract_neighbors.network <- function(x, vertex, type = c("out", "in", "all")) {
  type <- snafun.match.arg(type)
  if (type == "all") type <- "combined"
  if (length(vertex) != 1) stop("You need to specify exactly 1 vertex")
  if (is.character(vertex)) {
    is_naam <- TRUE
    vertex <- which(network::network.vertex.names(x) == vertex)
    if (length(vertex) == 0) {
      stop("This vertex is in not the graph")
    }
  } else {
    is_naam <- FALSE
  }
  nb <- network::get.neighborhood(x = x, v = vertex, type = type)
  if (is_naam) {
    nb <- network::network.vertex.names(x)[nb]
  }
  sort(nb)
}














# extract_egonet ---------------------------------------------------------------


#' Extract ego networks
#' 
#' Extract one of more ego network from an input graph
#' 
#' For each vertex in \code{vertices}, the ego network is returned. This includes 
#' the vertices that can be reached in \code{order} number of steps. 
#' The \code{type} argument determines whether edges should be followed as 
#' incoming, outgoing, or undirected (and are considered undirected anyway for 
#' an undirected graph.)
#' 
#' The output is a list of egonetworks, one for each ego vertex.
#'
#' @param x input graph of class \code{igraph} or \code{network}
#' @param vertices if \code{NULL} the egonetworks for each vertex in the graph 
#' are returned. If \code{vertices} is a single vertex or a vector, the ego networks 
#' for these specific vertices are returned. This is generally what you want.
#' @param order the order of the resulting graph
#' @param type Character constant, it specifies how to use the direction of the 
#' edges if a directed graph is analyzed. For ‘out’ only the outgoing edges are 
#' followed, so all vertices reachable from the source vertex in at most 
#' \code{order} steps are counted. For ‘"in"’ all vertices from which the 
#' source vertex is reachable in at most \code{order} steps are counted. 
#' ‘"all"’ ignores the direction of the edges. This argument is ignored for undirected graphs.
#'
#' @return a list of graphs of the same class as the input graph
#' @export
#'
#' @examples
#' g <- igraph::graph_from_literal(One --+ Two +-+ Three +-- Four --+ Five +-- Six +-- Seven +-+ Eight +-+ One +-+ Five)
#' snafun::to_matrix(g)
#' extract_egonet(g, vertices = 1, order = 1, type = "in")[[1]] |> snafun::to_matrix()
#' extract_egonet(g, vertices = 1, order = 1, type = "out")[[1]] |> snafun::to_matrix()
#' extract_egonet(g, vertices = 1, order = 1, type = "all")[[1]] |> snafun::to_matrix()
#' extract_egonet(g, vertices = 1, order = 2, type = "in")[[1]] |> snafun::to_matrix()
#' extract_egonet(g, vertices = 1, order = 2, type = "out")[[1]] |> snafun::to_matrix()
#' extract_egonet(g, vertices = 1, order = 2, type = "all")[[1]] |> snafun::to_matrix()
#' 
#' extract_egonet(g, vertices = c("One"), order = 2, type = "in")[[1]] |> snafun::to_matrix()
#' extract_egonet(g, vertices = c("One", "Three"), order = 2, type = "in")
#' 
#' g_n <- snafun::to_network(g)
#' extract_egonet(g_n, vertices = 1, order = 1, type = "in")[[1]] |> snafun::to_matrix()
#' extract_egonet(g_n, vertices = 1, order = 1, type = "out")[[1]] |> snafun::to_matrix()
#' extract_egonet(g_n, vertices = 1, order = 1, type = "all")[[1]] |> snafun::to_matrix()
#' extract_egonet(g_n, vertices = 1, order = 2, type = "in")[[1]] |> snafun::to_matrix()
#' extract_egonet(g_n, vertices = 1, order = 2, type = "out")[[1]] |> snafun::to_matrix()
#' extract_egonet(g_n, vertices = 1, order = 2, type = "all")[[1]] |> snafun::to_matrix()
#' extract_egonet(g_n, vertices = c("One"), order = 2, type = "in")[[1]] |> snafun::to_matrix()
#' extract_egonet(g_n, vertices = c("One", "Three"), order = 2, type = "in")
extract_egonet <- function(x, vertices = NULL, order = 1, type = c("all", "out", "in")) {
  UseMethod("extract_egonet")
}

#' @export
extract_egonet.default <- function(x, vertices = NULL, order = 1, type = c("all", "out", "in")) {
  txt <- methods_error_message("x", "extract_egonet")
  stop(txt)
}

#' @export
extract_egonet.igraph <- function(x, vertices = NULL, order = 1, type = c("all", "out", "in")) {
  type <- snafun.match.arg(type)
  if (is.null(vertices)) {
    vertices <- igraph::V(x)
  }
  igraph::make_ego_graph(
    graph = x,
    order = order,
    nodes = vertices,
    mode = type,
    mindist = 0
  )
}


#' @export
extract_egonet.network <- function(x, vertices = NULL, order = 1, type = c("all", "out", "in")) {
  type = snafun.match.arg(type) 
  if (type == "all") type <- "combined"
  if (is.character(vertices)) {  # turn name into vertex id
    vertices <- which(network::network.vertex.names(x) %in% vertices)
  }
  if (is.null(vertices)) vertices <- network::valid.eids(x)
  
  lijst <- lapply(vertices, function(z) {
    alters <- get_neigbors_network(x = x, vertices = z, type = type, steps = order)
    extract_subgraph(x = x, v_to_keep = alters)
  })
  names(lijst) <- vertices
  lijst
}



get_neigbors_network <- function(x, vertices, type, steps = 1) {
  all_alters <- new_alters <- c(vertices)
  
  while(steps > 0) {
  step <- lapply(new_alters, function(z) {
    network::get.neighborhood(x = x, v = z, type = type)
  }) |> 
    unlist() |> unique()
  new_alters <- setdiff(step, all_alters)
  all_alters <- c(all_alters, new_alters)
  steps <- steps - 1
  }
  
  sort(all_alters)
}


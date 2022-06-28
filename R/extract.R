
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
#' When only \code{ego} is specified, *all* edges are included that have an 'ego' 
#' node as the sender (in case of a directed graph) or as a sender or receiver 
#' (in case of an undirected graph).
#' 
#' When only \code{alter} is specified, *all* edges are included that have an 'alter' 
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
  txt <- methods_error_message("x", "extract_edge_id")
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







#' Make a matrix from a graph
#' 
#' Make an matrix from various input graph types.
#' 
#' Create the appropriate matrix from various inputs:
#' 
#' \describe{
#' \item{igraph}{a bipartite network is turned into an incidence matrix, 
#' any other network becomes an adjacency matrix. If the original graph is 
#' weighted (ie. it has an edge attribute called 'weight'), the resulting 
#' matrix is weighted as well.}
#' \item{network}{a bipartite network is turned into an incidence matrix, 
#' any other network becomes an adjacency matrix (non-sparse).
#' If the original graph is 
#' weighted (ie. it has an edge attribute called 'weight'), the resulting 
#' matrix is weighted as well.}
#' \item{data.frame}{the first two columns are taken as, respectively, denoting 
#' 'from' and 'to'. If it exists, the third column is used for the values of the 
#' cells in the matrix. If there are only two columns in the edgelist, 
#' the matrix is weighted in case an edge occurs multiple times.
#' Edgelists created by \code{snafun::to_edgelist()} carry hidden metadata that
#' preserve isolates and the original directedness. For plain external
#' edgelists, \code{snafun} infers an undirected one-mode matrix when every row
#' has an exact reciprocal counterpart; otherwise the safest default is a
#' directed interpretation. In case the senders and receivers have no overlap in
#' the edgelist, the graph is treated as bipartite and an incidence matrix
#' (potentially weighted) is returned. Use \code{directed = FALSE} when a plain
#' external edgelist lists each undirected edge only once.}
#' }
#' 
#' @param x graph object
#' @param directed Optional logical override that is only used for
#' \code{data.frame} edgelists. Leave \code{NULL} to let \code{snafun} infer
#' the most plausible interpretation from hidden metadata and the visible rows.
#' Set \code{FALSE} to force an undirected one-mode matrix when a plain
#' external edgelist only lists each undirected edge once.
#'
#' @return a \code{matrix} object
#' @export
#'
#' @examples
#' actors <- data.frame(name=c("Alice", "Bob", "Cecil", "David", "Esmeralda"), 
#'    age=c(48,33,45,34,21),
#'    gender=c("F","M","F","M","F"))
#' relations <- data.frame(from=c("Bob", "Cecil", "Cecil", "David", 
#'    "David", "Esmeralda"),
#'    to = c("Alice", "Bob", "Alice", "Alice", "Bob", "Alice"), 
#'    same.dept = c(FALSE, FALSE, TRUE, FALSE, FALSE, TRUE), 
#'    friendship = c(4, 5, 5, 2, 1, 1), 
#'    advice = c(4, 5, 5, 4, 2, 3))
to_matrix <- function(x, directed = NULL) {
  UseMethod("to_matrix")
}


#' @export
to_matrix.default <- function(x, directed = NULL) {
  txt <- methods_error_message("x", "to_matrix")
  stop(txt)
}



#' @export
to_matrix.matrix <- function(x, directed = NULL) {
  x
}


#' @export
to_matrix.igraph <- function(x, directed = NULL) {
  if (is_bipartite(x)) {
    if (is_weighted(x) | is_signed(x)) {
      mat <- igraph::as_biadjacency_matrix(x, sparse = FALSE,
                                         attr = igraph::edge_attr_names(x)[[1]])
    } else {
      mat <- igraph::as_biadjacency_matrix(x, sparse = FALSE, attr = NULL)
    }
  } else {
    if (is_weighted(x) | is_signed(x)) {
      mat <- igraph::as_adjacency_matrix(x, type = "both", sparse = FALSE, attr = "weight")
    } else {
      mat <- igraph::as_adjacency_matrix(x, type = "both", sparse = FALSE, attr = NULL)
      # igraph omits loop entries in this unweighted branch. Reinsert them via
      # explicit row/column coordinates so the result stays a proper matrix even
      # when there is only a single loop.
      which_loops <- which(igraph::which_loop(x))
      if (length(which_loops) > 0) {
        these_loops <- igraph::ends(
          x,
          es = igraph::E(x)[which_loops],
          names = FALSE
        )
        mat[cbind(these_loops[, 1], these_loops[, 2])] <- 1
      }
    }
  }
  mat
}



#' @export
to_matrix.network <- function(x, directed = NULL) {
  if (network::is.bipartite(x)) {
    if ("weight" %in% network::list.edge.attributes(x)) {
      network::as.matrix.network(x, attrname = "weight", expand.bipartite = FALSE)
    } else {
      network::as.matrix.network(x, expand.bipartite = FALSE)
    }
  } else {
    if ("weight" %in% network::list.edge.attributes(x)) {
      network::as.matrix.network(x, attrname = "weight")
    } else {
      network::as.matrix.network(x)
    }
  }
}




#' @export
to_matrix.data.frame <- function(x, directed = NULL){
  if (inherits(x, "tbl_df")) x <- as.data.frame(x)
  if (ncol(x) == 1) stop("'x' should have at least two columns")
  
  semantics <- resolve_edgelist_conversion(x, directed = directed)
  stored_vertices <- semantics$vertices
  is_bipartite_edgelist <- semantics$bipartite
  is_directed_edgelist <- semantics$directed
  x <- semantics$x
  
  # Normalize the edgelist to a three-column representation. This keeps the
  # rest of the logic simple and allows us to reconstruct full matrices with
  # isolated vertices when hidden vertex metadata are available.
  if (ncol(x) == 2) {
    x <- data.frame(
      from = x[, 1],
      to = x[, 2],
      weight = rep(1, nrow(x)),
      stringsAsFactors = FALSE
    )
  }

  if (ncol(x) > 3) {
    warning("There are more than 3 columns, 
            note that only column 3 will be used to fill the elements of the matrix")
    x <- x[, 1:3]
  }
  if (ncol(x) == 3) {
    colnames(x)[1:3] <- c("from", "to", "weight")
    if (nrow(x) > 0) {
      x <- stats::aggregate(
        x = list(weight = as.numeric(x[, 3])),
        by = list(
          from = as.character(x[, 1]),
          to = as.character(x[, 2])
        ),
        FUN = sum
      )
    } else {
      x <- data.frame(
        from = character(0),
        to = character(0),
        weight = numeric(0),
        stringsAsFactors = FALSE
      )
    }
    
    if (!is.null(stored_vertices)) {
      vertex_ids <- as.character(stored_vertices[[1]])
      if (is_bipartite_edgelist && "type" %in% colnames(stored_vertices)) {
        type_indicator <- as.logical(stored_vertices$type)
        sender <- vertex_ids[!type_indicator]
        receiver <- vertex_ids[type_indicator]
      } else {
        sender <- vertex_ids
        receiver <- if (is_bipartite_edgelist) {
          as.character(unique(x[, 2])) |> sort()
        } else {
          vertex_ids
        }
      }
    } else {
      if (is_bipartite_edgelist) {
        sender <- as.character(unique(x[,1])) |> sort()
        receiver <- as.character(unique(x[,2])) |> sort()
      } else {
        all_vertices <- sort(unique(c(as.character(x[, 1]), as.character(x[, 2]))))
        sender <- receiver <- all_vertices
      }
    }
    
    out <- matrix(
      0,
      nrow = length(sender),
      ncol = length(receiver),
      dimnames = list(sender, receiver)
    )
    
    row_index <- match(x$from, sender)
    col_index <- match(x$to, receiver)
    if (any(is.na(row_index)) || any(is.na(col_index))) {
      stop("The edgelist contains vertices that do not occur in the stored vertex metadata.")
    }
    
    if (is_bipartite_edgelist) {
      out[cbind(row_index, col_index)] <- x$weight
    } else if (is_directed_edgelist) {
      out[cbind(row_index, col_index)] <- x$weight
    } else {
      # Build undirected adjacency matrices from canonical dyads so each
      # undirected tie is mirrored exactly once. This avoids double-counting
      # when the edgelist already contains reciprocal rows.
      if (nrow(x) == 0) {
        x_undirected <- data.frame(
          from = character(0),
          to = character(0),
          weight = numeric(0),
          stringsAsFactors = FALSE
        )
      } else {
        endpoint_1 <- as.character(x$from)
        endpoint_2 <- as.character(x$to)
        canonical_from <- ifelse(endpoint_1 <= endpoint_2, endpoint_1, endpoint_2)
        canonical_to <- ifelse(endpoint_1 <= endpoint_2, endpoint_2, endpoint_1)
        x_undirected <- stats::aggregate(
          x = list(weight = x$weight),
          by = list(from = canonical_from, to = canonical_to),
          FUN = sum
        )
      }
      undirected_row <- match(x_undirected$from, sender)
      undirected_col <- match(x_undirected$to, receiver)
      loop_rows <- x_undirected$from == x_undirected$to
      if (any(!loop_rows)) {
        out[cbind(undirected_row[!loop_rows], undirected_col[!loop_rows])] <- x_undirected$weight[!loop_rows]
        out[cbind(undirected_col[!loop_rows], undirected_row[!loop_rows])] <- x_undirected$weight[!loop_rows]
      }
      if (any(loop_rows)) {
        out[cbind(undirected_row[loop_rows], undirected_col[loop_rows])] <- x_undirected$weight[loop_rows]
      }
    }
  }
  out
}

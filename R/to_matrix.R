


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
#' The graph is assumed to be directed, so one needs to symmetrize the matrix 
#' afterwards if the graph is undirected. In case the senders and receivers have 
#' no overlap in the edgelist, the graph is assumed to be bipartite and an 
#' incidence matrix (potentially weighted) is returned}
#' }
#' 
#' @param x graph object
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
to_matrix <- function(x) {
  UseMethod("to_matrix")
}


#' @export
to_matrix.default <- function(x) {
  txt <- methods_error_message("x", "to_matrix")
  stop(txt)
}



#' @export
to_matrix.matrix <- function(x) {
  x
}


#' @export
to_matrix.igraph <- function(x) {
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
      # the igraph::as_adjacency_matrix removes the diagonal, put it back
      which_loops <- which(igraph::which_loop(x))
      if (length(which_loops) > 0) {
        these_loops <- igraph::as_edgelist(x)[which_loops, ]
        mat[these_loops] <- 1
      }
    }
  }
  mat
}



#' @export
to_matrix.network <- function(x) {
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
to_matrix.data.frame <- function(x){
  if (inherits(x, "tbl_df")) x <- as.data.frame(x)
  if (ncol(x) == 1) stop("'x' should have at least two columns")
  if (ncol(x) == 2) {
    x <- as.data.frame(table(c(x[,1]), c(x[,2])))
    names(x) <- c("from","to","weight")
  }

  if (ncol(x) > 3) {
    warning("There are more than 3 columns, 
            note that only column 3 will be used to fill the elements of the matrix")
    x <- x[, 1:3]
  }
  if (ncol(x) == 3) {
    x <- x[order(x[,1], x[,2]),]
    sender <- as.character(unique(x[,1])) |> sort()
    receiver <- as.character(unique(x[,2])) |> sort()
    # make sender and receiver sets the same if there is some overlap between 
    # senders and receivers
    if (length(intersect(sender, receiver)) > 0 & !setequal(sender, receiver)) {
      sender <- receiver <- sort(unique(c(sender,receiver)))
    }
    # expand the edgelist to include all possible dyads (incl loops)
    if (nrow(x) != length(sender)*length(receiver)) {
      allcombs <- expand.grid(sender, receiver, stringsAsFactors = FALSE)
      allcombs <- subset(allcombs, !duplicated(allcombs))
      names(allcombs) <- c("from","to")
      x <- merge(allcombs, x, all.x = TRUE)
      x <- x[order(x[,2], x[,1]),]
      x[is.na(x)] <- 0
    }

    x <- x[order(as.character(x[,2]), as.character(x[,1])), ]

    out <- structure(as.numeric(x[,3]),
                     .Dim = c(as.integer(length(sender)),
                              as.integer(length(receiver))),
                     .Dimnames = list(sender, receiver))
  }
  out
}

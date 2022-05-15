


#' Make a matrix from a graph
#' 
#' Make an matrix from various input graph types.
#' 
#' Create the appropriate matrix from various inputs:
#' 
#' \describe{
#' \item{igraph}{a bipartite network is turned into an incidence matrix, 
#' any other network becomes an adjacency matrix}
#' \item{network}{a bipartite network is turned into an incidence matrix, 
#' any other network becomes an adjacency matrix (non-sparse)}
#' \item{data.frame}{the first two columns are taken as, respectively, denoting 
#' 'from' and 'to'. The third column is then used for the values of the cells 
#' in the matrix.}
#' }
#' 
#' If the input graph is bipartite, the resulting matrix will be an incidence 
#' matrix, otherwise it will be an adjacency natrix (weighted if the original 
#' graph is weighted).
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
to_matrix.igraph <- function(x) {
  if (is_bipartite(x)) {
    if (is_weighted(x) | is_signed(x)) {
      mat <- igraph::as_incidence_matrix(x, sparse = FALSE,
                                         attr = igraph::edge_attr_names(x)[[1]])
    } else {
      mat <- igraph::as_incidence_matrix(x, sparse = FALSE, attr = NULL)
    }
  } else {
    if (is_weighted(x) | is_signed(x)) {
      mat <- igraph::as_adjacency_matrix(x, sparse = FALSE,
                                         attr = igraph::edge_attr_names(x)[[1]])
    } else {
      mat <- igraph::as_adjacency_matrix(x, sparse = FALSE,
                                         attr = NULL)
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



#' 
#' #' @export
#' to_matrix.data.frame <- function(x){
#'   if (inherits(x, "tbl_df")) x <- as.data.frame(x)
#'   
#'   if (ncol(x) == 1) stop("'x' should have at least two columns")
#'   
#'   if (ncol(x) == 2 | !is_weighted(x)) {
#'     x <- data.frame(x) # in case it's a tibble
#'     x <- as.data.frame(table(c(x[,1]), c(x[,2])))
#'     names(x) <- c("from","to","weight")
#'   }
#'   
#'   if (ncol(x) > 3) {
#'     warning("There are more than 3 columns, note that only column 3 will be used to fill the elements of the matrix")
#'     x <- x[, 1:3]
#'   } ############################################################################# BIPARTITE????
#'   if (ncol(x) == 3) {
#'     # Adds a third (weight) column to a two-column edgelist
#'     # x <- x[order(x[,1], x[,2]),]
#'     nodes1 <- as.character(unique(x[,1]))
#'     nodes1 <- sort(nodes1)
#'     nodes2 <- as.character(unique(x[,2]))
#'     nodes2 <- sort(nodes2)
#'     if(length(intersect(nodes1, nodes2)) > 0 &
#'        !setequal(nodes1, nodes2))
#'       nodes1 <- nodes2 <- sort(unique(c(nodes1,nodes2)))
#'     if (nrow(x) != length(nodes1)*length(nodes2)) {
#'       allcombs <- expand.grid(nodes1, nodes2, stringsAsFactors = FALSE)
#'       allcombs <- subset(allcombs, !duplicated(allcombs))
#'       names(allcombs) <- c("from","to")
#'       x <- merge(allcombs, x, all.x = TRUE)
#'       x <- x[order(x[,2], x[,1]),]
#'       x[is.na(x)] <- 0
#'     }
#'     
#'     
#'     
#'     x <- dplyr::arrange(x, as.character(.data$to), 
#'                              as.character(.data$from))
#'     out <- structure(as.numeric(x[,3]),
#'                      .Dim = c(as.integer(length(nodes1)),
#'                               as.integer(length(nodes2))),
#'                      .Dimnames = list(nodes1, nodes2))
#'   }
#'   out
#' }

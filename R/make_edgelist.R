
#' Make edgelist
#' 
#' Make an edgelist from a set of input objects. This function doesn't do 
#' anything that \code{igraph} or \code{network}/\code{sna} can't also already 
#' do. The usefulness of this function is its consistent API.
#' 
#' Currently, input can be a matrix, \code{igraph} object or \code{network} object.
#' 
#' @param x input object
#' @param sort character, name of the column to sort on. Defaults to "from".
#'
#' @return data frame
#' @export
#' @rdname make_edgelist
#' @examples
#' ## from an igraph object
#' g_i <- igraph::sample_gnp(10, 2/10)
#' make_edgelist(g_i)
#' 
#' # add vertex names
#' g1_i <- g_i
#' igraph::V(g1_i)$name <- LETTERS[seq_len(igraph::gorder(g1_i))]
#' make_edgelist(g1_i)
#' # add weights
#' igraph::E(g1_i)$weight <- seq_len(igraph::ecount(g1_i))
#' make_edgelist(g1_i)
#' # add further edge attribute
#' g2_i <- g1_i |> 
#'     igraph::set_edge_attr("color", value = "red")
#' make_edgelist(g2_i)
#' 
#' ## from a matrix
#' g_n <- sapply(runif(10, 0, 1), rep, 10)
#' g_n <- sna::rgraph(10, tprob = g_n)
#' make_edgelist(g_n)
#' g_b <- igraph::make_bipartite_graph(c(rep(0, 5), rep(1, 3)), c(1,6,2,7,3,6,3,7,4,8,5,8))
#' make_edgelist(g_b)
#' 
#' ## from a network object
#' g2_n <- network::as.network.matrix(g_n)
#' make_edgelist(g2_n)
make_edgelist <- function(x, sort = "from") {
  UseMethod("make_edgelist")
}


#' @export
make_edgelist.default <- function(x, sort = "from") {
  stop("'x' needs to be an 'igraph' object, a 'network' object, or a network 'matrix'")
}


#' @export
#' @describeIn make_edgelist
make_edgelist.igraph <- function(x, sort = "from") {
  el <- igraph::get.data.frame(x)
  if (!sort %in% colnames(el)) {
    stop("You did not provide an appropriate name for the 'sort' column")
  }
  el[order(el[, sort]), ]
}


#' @export
#' @describeIn make_edgelist
make_edgelist.network <- function(x, sort = "from") {
  out <- sna::as.edgelist.sna(x)
  edges <- as.data.frame(out)
  x1 <- as.matrix(x)
  if (dim(x1)[1] != dim(x1)[2]) {
    edges <- edges[((nrow(edges)/2) + 1):nrow(edges),]
  }
  names(edges) <- c("from", "to", "weight")
  # Handle node names
  if (!is.null(dimnames(x))) {
    names <- attr(out, "vnames")
    edges[,1] <- names[edges[,1]]
    edges[,2] <- names[edges[,2]]
  }
  # Handle edge weights
  if ("weight" %in% network::list.edge.attributes(x)) {
    edges[,3] <- network::get.edge.attribute(x, "weight")
  }
  # Remove weight column if only unity weights.
  if (all(edges$weight == 1)) edges <- edges[, -3]
  el <- as.data.frame(edges)
  if (!sort %in% colnames(edges)) {
    stop("You did not provide an appropriate name for the 'sort' column")
  }
  el[order(el[, sort]), ]
}



#' @export
#' @describeIn make_edgelist
make_edgelist.matrix <- function(x, sort = "from"){
  el <- make_edgelist.igraph(make_igraph(x))
  if (!sort %in% colnames(el)) {
    stop("You did not provide an appropriate name for the 'sort' column")
  }
  el[order(el[, sort]), ]
}







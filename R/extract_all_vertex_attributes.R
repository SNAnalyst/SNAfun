

#' Extract all vertex attributes
#'
#' Extract all vertex attributes from the network
#'
#' This function extract all vertex attributes from a \code{network} object
#' or from a \code{igraph} object and returns it as a data.frame.
#'
#' The columns of the data.frame have the corresponding attribute names.
#'
#' No attempt is made to assign a specific class to the attributes. The user
#' will have to set them appropriately for the analysis at hand.
#'
#' Note that \code{network} objects tend to have a vertex attribute called
#' \code{na}. This will, correctly, be included, but is likely irrelevant.
#'
#' If there are no vertex attributes, \code{NULL} is returned.
#'
#' @param g name of the input network
#'
#' @return data.frame in case of vertex attributes, otherwise NULL
#' @export
#'
#' @examples
#' \dontrun{
#' data(florentine, package = "SNA4DSData")
#' flomar <- florentine$flomarriage
#' extract_all_vertex_attributes(flomar)
#'
#' # NULL
#' extract_all_vertex_attributes(igraph::erdos.renyi.game(10, 4, type = "gnm"))
#'
#' flomar_nw <- to_network(flomar)
#' extract_all_vertex_attributes(flomar_nw)
#' }
extract_all_vertex_attributes <- function(g) {
  if (inherits(g, "network")) {
    attrs <- network::list.vertex.attributes(g)
    if (length(attrs) > 0) {
      mat <- data.frame(matrix(ncol = length(attrs), nrow = network::network.size(g)))
      for (att in 1:length(attrs)) {
        mat[, att] <- network::get.vertex.attribute(g, attrs[att])
      }
    } else {
      return(NULL)
    }
  } else if (inherits(g, "igraph")) {
    attrs <- igraph::vertex_attr_names(g)
    if (length(attrs) > 0) {
      mat <- data.frame(matrix(ncol = length(attrs), nrow = igraph::gorder(g)))
      for (att in 1:length(attrs)) {
        atje <- igraph::vertex_attr(g, attrs[att]) |> 
          fix_list_attribute()
        mat[, att] <- atje
      }
    } else {
      return(NULL)
    }
  } else {
    stop("'g' needs to be a 'network' object or an 'igraph' object")
  }
  colnames(mat) <- attrs
  mat
}

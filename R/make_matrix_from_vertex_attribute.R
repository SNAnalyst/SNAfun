
#' Make a matrix from a vertex attribute
#'
#' Create a matrix based on a vertex attribute
#'
#' This function creates a matrix on the basis of a vertex attribute in an
#' object of class \code{igraph} or \code{network}. In this case, the vertex
#' attribute provided in the \code{name} argument will be extracted.
#'
#' Alternatively, \code{x} can be a numeric vector provided by the user.
#'
#' This vector/attribute is then used to construct a valued matrix, which can be
#' fed into \code{igraph} or \code{network} for further use.
#'
#' The function hosts several options to create this valued matrix with:
#'
#' \itemize{
#' \item absdiff the absolute difference between values for two vertices
#' \item diff the difference is returned, so the value of the vertex with the
#' lower vertex index minus the value of the vertex with the higher vertex
#' index. This value can be negative.
#' \item sum the sum of the values for both vertices
#' \item max the highest value between the two vertices
#' \item min the highest value between the two vertices
#' \item mean the mean between the two vertices
#' \item sender the value of the sender's attribute in the entire row
#' \item receiver the value of the receiver's attribute in the entire column
#' \item equal 1 if both vertices have the same on the attribute, 0 otherwise
#' }
#'
#' Most of the options speak for themselves.
#' The \code{sender} option is used to model a sender effect. For example,
#' if it is to be tested whether the sender's age drives edges,
#' the sender's age is the edge attribute for every edge this sender is
#' involved in. Mathematically, this means that the sender's age
#' should be in all of the cells of that sender's row.
#'
#' Similarly, the \code{receiver} option places the receiver's attribute in
#' each column. For the attribute 'age', this makes the edge attribute equal
#' to the receiver's age, for every edge the vertex is the receiver.
#'
#' If \code{diag == FALSE} (the default), the diagonal of the returned matrix
#' will contain zeroes. If \code{diag} is any other value, then that will be
#' inserted into the diagonal. Using recycling rules, any object of fitting
#' length is accepted.
#' If \code{diag == TRUE}, the \code{measure} will also be applied to the
#' diagonal elements.
#'
#' @param x input, a vector or an object of class \code{igraph} or
#' \code{network}
#' @param name the name of the attribute to be extracted. This will only be used
#' if \code{x} is of class \code{igraph} or \code{network}
#' @param measure character, currently a choice between "absdiff", "diff",
#' "sum", "max", "min". The default value is \code{"absdiff"}
#' @param diag if \code{FALSE} (the default), the diagonal will be zero. If
#' anything else, the diagonal is filled with that (if it fits)
#'
#' @return matrix
#' @export
#'
#' @examples
#' make_matrix_from_vertex_attribute(1:5)
#' make_matrix_from_vertex_attribute(1:5, measure = "sum", diag = 99)
#' make_matrix_from_vertex_attribute(1:5, measure = "max", diag = NA)
#' make_matrix_from_vertex_attribute(1:5, measure = "sum", diag = TRUE)
#' make_matrix_from_vertex_attribute(1:5, measure = "min", diag = 11:15)
#' make_matrix_from_vertex_attribute(1:5, measure = "sender")
#' make_matrix_from_vertex_attribute(1:5, measure = "receiver")
#' \dontrun{
#' data(florentine, package = "SNA4DSData")
#' # absdiff, with zeroes on the diagonal
#' make_matrix_from_vertex_attribute(florentine$floattrs$Wealth)
#' }
#'
make_matrix_from_vertex_attribute <- function (x, name,
                                                measure = c("absdiff", "diff",
                                                            "sum", "max", "min",
                                                            "mean", "sender",
                                                            "receiver",
                                                            "equal"),
                                                diag = FALSE) {

  kall <- as.list(match.call())

  if (inherits(x, "igraph")) {
    all_att <- igraph::vertex_attr_names(x)
    if (!name %in% all_att) {
      stop("The attribute ", name, " does not occur in the graph")
    }
    att <- igraph::vertex_attr(graph = x, name = name)
    att <- fix_list_attribute(att)
  } else if (inherits(x, "network")) {
    all_att <- network::list.vertex.attributes(x)
    if (!name %in% all_att) {
      stop("The attribute ", name, " does not occur in the graph")
    }
      att <- network::get.vertex.attribute(x, name)
  } else if (is.null(dim(x)) || max(dim(x)) == 1) {
      att <- x
  } else (
    stop ("'x' has to be an igraph object, a network object, or a vector")
  )
  if (any(is.na(att))) stop("The attribute contains missing data, which can not
                          logically be handled by this function.")

  dX <- length(att)
  Y <- att
  FUN <- match.arg(measure)
  Y <- rep(Y, rep.int(length(att), length(Y)))
  X <- rep(att, times = ceiling(length(Y)/length(att)))

  XY <- cbind(att, Y)
  if (FUN == "diff") {
    if (!is.numeric(att)) stop("The data should be numeric for this measure.")
    robj <- X - Y
  } else if (FUN == "absdiff") {
    if (!is.numeric(X)) stop("The data should be numeric for this measure.")
    robj <- abs(X - Y)
  } else if (FUN == "sum") {
    if (!is.numeric(X)) stop("The data should be numeric for this measure.")
    robj <- X + Y
  } else if (FUN == "max") {
    if (!is.numeric(X)) stop("The data should be numeric for this measure.")
    robj <- apply(XY, 1, max)
  } else if (FUN == "min") {
    if (!is.numeric(X)) stop("The data should be numeric for this measure.")
    robj <- apply(XY, 1, min)
  } else if (FUN == "mean") {
    if (!is.numeric(X)) stop("The data should be numeric for this measure.")
    robj <- apply(XY, 1, mean)
  } else if (FUN == "sender") {
    robj <- X
  } else if (FUN == "receiver") {
    robj <- Y
  } else if (FUN == "equal") {
    robj <- (X == Y)
  }

  dim(robj) <- c(dX, dX)
  if (is.logical(diag)) { # logical or NA
    if (is.na(diag)) { # if NA, then fill that on the diagonal
      diag(robj) <- diag
    } else if (!diag) { # if FALSE, put zeroes
      diag(robj) <- 0
    } # if TRUE leave as is
  } else {
    diag(robj) <- diag   # if anything else, put that on the diagonal
  }

  robj
}



# if an attribute is really a list, this is turned into a vector
# if multiple occur for the attribute for some vertex/edge, the first value is used 
# and a warning is triggered.
fix_list_attribute <- function(att) {
  if (is.list(att)) {
    islistlist <- any(sapply(att, is.list))
    if (islistlist) {
      stop("The attribute may be a list of lists, please fix before progressing.")
    }
    lens <- sapply(att, length)
    multiple <- which(lens > 1)
    if (length(multiple) > 0) {
      warning("Vertex/vertices ", paste(multiple, collapse = ","), " have more than one value for this attribute: only their first value will be used!")
      for (at in multiple) {
        att[[at]] <- att[[at]][1]
      }
      att <- unlist(att)
    }
  }
  att
}




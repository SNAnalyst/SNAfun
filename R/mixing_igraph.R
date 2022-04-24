
#' igraph mixing matrix
#'
#' Create a network mixing matrix for an igraph object.
#'
#' Network mixing matrix is, traditionally, a two-dimensional
#' cross-classification of edges depending on the values of a specified vertex
#' attribute for tie sender and tie receiver. It is an important tool
#' for assessing network homophily or segregation.
#'
#' Let \eqn{G} be the number of distinct values of the vertex attribute in
#' question.  We may say that we have \eqn{G} mutually exclusive groups in the
#' network.  The mixing matrix is a \eqn{G \times G}{GxG} matrix such that
#' \eqn{m_{ij}}{m[ij]} is the number of ties send by vertices in group \eqn{i}
#' to vertices in group \eqn{j}. The diagonal of that matrix is of special
#' interest as, say, \eqn{m_{ii}}{m[ii]} is the number of ties \emph{within}
#' group \eqn{i}.
#'
#' A full mixing matrix is a list that cross-classifies
#' \emph{all} network \emph{dyads} depending on:
#' \enumerate{
#' \item{the value of the vertex attribute for tie sender}
#' \item{the value of the vertex attribute for tie receiver}
#' \item{the status of the dyad, i.e. whether it is connected or not}
#' }
#' The two-dimensional version is a so-called "contact layer"
#' of the list version.
#' 
#' The mixing matrix is created for the \code{igraph} network in 
#' \code{object} based on vertex attributes supplied in arguments
#' \code{rattr} and, optionally, \code{cattr}.
#'
#' If only \code{rattr} is specified (or, equivalently, \code{rattr} and
#' \code{cattr} are identical), the result will be a mixing matrix \eqn{G
#' \times G} if \code{full} is \code{FALSE} or \eqn{G \times G \times 2}{GxGx2}
#' if \code{full} is \code{TRUE}. Where \eqn{G} is the number of categories of
#' vertex attribute specified by \code{rattr}.
#'
#' If \code{rattr} and \code{cattr} can be used to specify different vertex
#' attributes for tie sender and tie receiver.
#' 
#' It should be possible to convert the attribute for which the mixing matrix 
#' is to be determined into a factor, otherwise a mixing matrix is irrelevant. 
#' If this is not possible, the function will throw an appropriate error 
#' stating this.
#' 
#' @param object \code{igraph} object
#' 
#' @param rattr name of the vertex attribute or an attribute itself as a
#' vector. If \code{cattr} is not NULL, \code{rattr} is used for rows of the
#' resulting mixing matrix.
#'
#' @param cattr name of the vertex attribute or an attribute itself as a
#' vector. If supplied, used for columns in the mixing matrix.
#'
#' @param full logical, whether two- or three-dimensional mixing matrix
#' should be returned.
#'
#' @param directed logical, whether the network is directed. By default,
#' directedness of the network is determined with
#' \code{\link[igraph]{is.directed}}.
#'
#' @param loops logical, whether loops are allowed. By default it is TRUE
#' whenever there is at least one loop in \code{object}.
#' 
#' @source This is a slightly modified version of the \code{mixingm.igraph} method 
#' from the \code{isnar} package from \href{https://github.com/mbojan/isnar/blob/master/R/mixingm.R}{isnar}.
#' 
#' @return
#' Depending on \code{full} argument a table or a list
#' crossclassifying connected or all dyads in \code{object}.
#'
#' @export
#' 
#' @examples 
#' \dontrun{
#' data(judge_net, package = "SNA4DSData")
#' 
#' # only for connected dyads
#' mixing_igraph(judge_net, "JudgeSex")
#' mixing_igraph(judge_net, "DivisionCode")
#' 
#' # mixing matrices for connected and unconnected
#' # dyads seperately
#' mixing_igraph(judge_net, "JudgeSex", full = TRUE)
#' }
mixing_igraph <- function (object, rattr, 
                           cattr = rattr, full = FALSE, 
                           directed = igraph::is.directed(object), 
                           loops = any(igraph::is.loop(object))) {
  if (is.character(rattr) && length(rattr) == 1) {
    ra <- igraph::get.vertex.attribute(object, rattr)
  }
  else {
    stopifnot(length(rattr) == igraph::vcount(object))
    ra <- rattr
  }
  if (is.character(cattr) && length(cattr) == 1) {
    ca <- igraph::get.vertex.attribute(object, cattr)
  }
  else {
    stopifnot(length(cattr) == igraph::vcount(object))
    ca <- cattr
  }
  el <- igraph::get.edgelist(object, names = FALSE)
  
  ego <- try(factor(ra[el[, 1]], levels = sort(unique(ra))), silent = TRUE)
  if (inherits(ego, "try-error")) stop("This attribute can not be turned into a factor, so a mixing matrix cannot be determined")
  alter <- try(factor(ca[el[, 2]], levels = sort(unique(ca))), silent = TRUE)
  if (inherits(alter, "try-error")) stop("This attribute can not be turned into a factor, so a mixing matrix cannot be determined")
  
  con <- table(ego = ego, alter = alter)

  if (!directed) {
    con <- fold(con, "upper")
    # con[lower.tri(con)] <- con[upper.tri(con)]
  }
  
  if (full) {
    tussen <- full_mm(con, gsizes = table(ra, ca), directed = directed, 
                   loops = loops)
    
    namen <- dimnames(tussen)
    welke_tie <- which(names(namen) == "tie")
    if (length(welke_tie) == 0) stop("There is no dimension called 'tie'")
    if (welke_tie != 3) stop("The object has an unexpected structure")
    waar_TRUE <- which(unlist(namen[welke_tie]) == TRUE)
    if (length(waar_TRUE) == 0) stop("There is no TRUE dimension for 'tie'")
    waar_FALSE <- which(unlist(namen[welke_tie]) == FALSE)
    
    TIE <- tussen[, , waar_TRUE]
    TIE[lower.tri(TIE)] <- TIE[upper.tri(TIE)]
    NO_TIE <- tussen[, , waar_FALSE]
    NO_TIE[lower.tri(NO_TIE)] <- NO_TIE[upper.tri(NO_TIE)]
    
    return(list(tie_present = TIE, no_tie_present = NO_TIE))
  }
  else {
    con[lower.tri(con)] <- con[upper.tri(con)]
    return(con)
  }
}




fold <- function (x, direction = c("upper", "lower")) {
  stopifnot(is.matrix(x))
  stopifnot(dim(x)[1] == dim(x)[2])
  m <- t(x) + x
  diag(m) <- diag(x)
  d <- match.arg(direction)
  if (d == "upper") 
    m[lower.tri(m)] <- 0
  else m[upper.tri(m)] <- 0
  m
}


full_mm <- function (cl, gsizes, directed = TRUE, loops = FALSE) {
  if (length(dim(cl)) == 3) {
    stopifnot(dim(cl)[3] == 2)
    return(cl)
  }
  gsizes <- as.table(gsizes)
  ndims <- length(dim(gsizes))
  stopifnot(ndims %in% 1:2)
  if (ndims == 1) {
    gs <- gsizes
  }
  else {
    dtab <- as.data.frame(as.table(gsizes))
    gs <- dtab$Freq
  }
  o <- outer(gs, gs, "*")
  if (directed) {
    mar <- o
    if (!loops) {
      diag(mar) <- diag(o) - gs
    }
  }
  else {
    mar <- o
    mar[lower.tri(mar)] <- 0
    if (loops) {
      diag(mar) <- (diag(o) + gs)/2
    }
    else {
      diag(mar) <- (diag(o) - gs)/2
    }
  }
  if (ndims == 2) {
    a1 <- apply(mar, 1, function(r) tapply(r, dtab[, 2], 
                                           sum))
    mar <- apply(a1, 1, function(k) tapply(k, dtab[, 1], 
                                           sum))
  }
  rval <- array(NA, dim = c(dim(cl), 2))
  rval[, , 1] <- mar - cl
  rval[, , 2] <- cl
  if (is.null(dimnames(cl))) {
    dimnames(rval) <- list(NULL, NULL, tie = c(FALSE, TRUE))
  }
  else {
    dimnames(rval) <- c(dimnames(cl)[1:2], list(tie = c(FALSE, 
                                                        TRUE)))
  }
  rval
}

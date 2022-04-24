#' Symmetrize an adjacency matrix
#' 
#' Function to symmetrize an adjacency matrix in several ways
#' 
#' Symmetrizes a square matrix. There is a choice of several methods 
#' to do this.
#' 
#' @param g a suqare numeric matrix, need not be binary
#' @param rule the rule used to determine how to symmetrize
#' \itemize{
#'  \item \strong{out} outgoing ties (upper triangle of the adjacency matrix) are copied exactly. What is sent is reciprocated. Technically: the upper triangle is copied over the lower triangle.
#'  \item \strong{in} ingoing ties (lower triangle of the adjacency matrix) are copied exactly. What is received is reciprocated. Technically: the lower triangle is copied over the upper triangle.
#'  \item \strong{weak} if i -> j AND/OR j -> i, the tie becomes reciprocated
#'  \item \strong{mutual} only reciprocated ties are maintained
#'  \item \strong{average} the average value of ij and ji is returned (\code{na.rm} is used)
#'  \item \strong{max} the max value of ij and ji is returned (\code{na.rm} is used)
#'  \item \strong{min} the min value of ij and ji is returned (\code{na.rm} is used)
#' }
#' @param na.rm how to deal with missings, default is \code{TRUE}
#' 
#' @return the symmetrized matrix
#' @export symMat
#' @examples
#' g = matrix(sample(c(0,1), 25, replace = TRUE), nrow = 5)
#' symMat(g, "weak")
#' symMat(g, "mutual")
#' g = matrix(runif(25), nrow = 5)
#' diag(g) <- 0
#' symMat(g, "average")
#' 

symMat <- function (g, rule = "weak", na.rm = TRUE) {

      n <- dim(g)[1]
      o <- dim(g)[2]
      if (n != o) {stop("Matrix is not square but it should be")}
      d <- g

      if (rule == "out") {
        d[lower.tri(d)] <- t(d[upper.tri(d)])
      }
      else if (rule == "in") {
        d[upper.tri(d)] <- t(d[lower.tri(d)])
      }
      else if (rule == "weak") {
        d <- matrix(as.numeric(d | t(d)), nrow = n, ncol = o)
      }
      else if (rule == "mutual") {
        d <- matrix(as.numeric(d & t(d)), nrow = n, ncol = o)
      }  

      else if (rule == "average") {
        d <- matrix(colMeans(rbind(as.vector(g), as.vector(t(g))), 
                             na.rm = na.rm), dim(g))
      }

      else if (rule == "max") {
        d <- pmax(g, t(g), na.rm = na.rm)
      }
      
      else if (rule == "min") {
        d <- pmin(g, t(g), na.rm = na.rm)
      }
      
      return(d)
}


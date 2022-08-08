
#' Symmetrize an adjacency matrix
#' 
#' Symmetrize an adjacency matrix
#' 
#' Turn a (adjacency) matrix into a symmetric one, according to several 
#' potential rules.
#' 
#' The diagonal of the matrix remains unaffected.
#' 
#' Rule \code{out}: \code{g} is made symmetric by copying the upper triangle 
#' onto the lower triangle. 
#' 
#' Rule \code{in}: \code{g} is made symmetric by copying the lower triangle 
#' onto the upper triangle. 
#' 
#' Rule \code{weak}: every edge is assumed to be reciprocated. Hence, an edge 
#' from $i$ to $j$ also becomes an edge from $j$ to $i$,
#' Mathematically: i<->j iff i->j or i<-j (OR rule)
#' 
#' Rule \code{mutual}: every edge is only maintained if it is already reciprocated
#' in \code{g}. Hence, if an edge from $i$ to $j$ exist and also from $j$ to $i$, 
#' these edges are maintained in the resulting matrix. Otherwise (for unreciprocated 
#' edges or null dyads), the resulting matrix will neither contain an edge 
#' from $i$ to $j$ nor from $j$ to $i$.
#' Mathematically: i<->j iff i->j and i<-j (AND rule)
#' 
#' Rule \code{average}: the values for the edges ($i$, $j$) and ($j$, $i$) are 
#' averaged and assigned to both edges in the resulting matrix. This will 
#' typically result in a non-binary matrix.
#' 
#' Rule \code{max}: edges ($i$, $j$) and ($j$, $i$) are both assigned the highest value 
#' of the two edges. If input matrix \code{g} is binary, this will yield the 
#' same result as rule \code{weak}.
#' 
#' Rule \code{min}: edges ($i$, $j$) and ($j$, $i$) are both assigned the lowest value 
#' of the two edges. If input matrix \code{g} is binary, this will yield the 
#' same result as rule \code{mutual}.
#' 
#' @param g input matrix
#' @param rule character, which rule to follow when deciding how to symmetrize
#' @param na.rm logical, should missing values for the cells be ignored? This is 
#' used for the rules "average", "max", or "min". For rules "out" and "in", the 
#' \code{NA} is copied over if it is in a cell that is copied by the rule. 
#' Under "weak", a \code{NA} is copied if it is in either i->j or j->i and the 
#' other edges is not 1. Under "mutual", a \code{NA} is copied if it is in 
#' either i->j or j->i and the other edges is 1. 
#'
#' @return symmatrized matrix
#' @export
#'
#' @examples
#' m <- matrix(c(0,1,1,0,1,0,0,1,0,0,0,1,0,1,1,1,0,1,0,1,1,0,0,1,0), byrow = TRUE, ncol = 5)#' 
#' m
#' to_symmetric_matrix(m, "out")
#' to_symmetric_matrix(m, "in")
#' to_symmetric_matrix(m, "weak")
#' to_symmetric_matrix(m, "mutual")
#' to_symmetric_matrix(m, "average")
#' to_symmetric_matrix(m, "max")
#' to_symmetric_matrix(m, "min")
#' m[1, 2] <- m[3, 1] <- NA
#' m
#' to_symmetric_matrix(m, "out")
#' to_symmetric_matrix(m, "in")
#' to_symmetric_matrix(m, "weak")
#' to_symmetric_matrix(m, "mutual")
#' to_symmetric_matrix(m, "average")
#' to_symmetric_matrix(m, "max")
#' to_symmetric_matrix(m, "min")
to_symmetric_matrix <- function (g, 
                    rule = c("weak", "mutual", "out", "in", 
                             "average", "max", "min"), 
                    na.rm = TRUE) {
  if (!inherits(g, "matrix")) stop("Input 'g' should be a matrix")
  n <- dim(g)[1]
  o <- dim(g)[2]
  if (n != o) {
    stop("Matrix is not square but it should be")
  }
  d <- g
  
  switch(rule, 
        "out" = {
          d[lower.tri(d, diag = FALSE)] <- t(d)[lower.tri(d, diag = FALSE)]
          d
        },
        "in" = {
          d[upper.tri(d, diag = FALSE)] <- t(d)[upper.tri(d, diag = FALSE)]
          d
        },
        "weak" = {
          d <- matrix(as.numeric(d | t(d)), nrow = n, ncol = o)
          d
        },
        "mutual" = {
          d <- matrix(as.numeric(d & t(d)), nrow = n, ncol = o)
          d
        },
        "average" = {
          d <- matrix(colMeans(rbind(as.vector(g), as.vector(t(g))),
                               na.rm = na.rm), dim(g))
          d
        },
        "max" = {
          d <- pmax(g, t(g), na.rm = na.rm)
          d
        },
        "min" = {
          pmin(g, t(g), na.rm = na.rm)
          d
        },
        stop("unknown value for 'rule'")
  )
}




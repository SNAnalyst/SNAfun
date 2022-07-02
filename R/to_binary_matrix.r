
#' Dichotomize a matrix
#' 
#' Make a binary matrix out of a weighted matrix
#' 
#' If a cell has value smaller than a minimum value, 
#' the cell becomes 0. If the cell has a value of at least 
#' the minimum value, it becomes 1.
#' The procedure will work for any matrix, not necessarily a square one.
#' 
#' @param mat a numeric matrix
#' @param min a threshold. All values in \code{mat} of at least this minimum value 
#' will become 1, all values smaller than this minimum will become 0.
#' @return the dichotomized matrix
#' @export to_binary_matrix
#' @examples
#' M <- matrix(round(runif(20),1), ncol = 4)
#' to_binary_matrix(M, min = .45)
to_binary_matrix <- function(mat, min) {
  mat <- ifelse(mat < min, 0, 1)
  return(mat)
}




#' makeEdgeList
#'
#' Makes an edge list from row data.
#'
#' This function processes row network data producing a list of edges. 
#' The function returns a data frame. 
#' If only the argument `names` is specified, the function returns a data frame
#' with two columns, indicating the node from which the link comes from in column 
#' one and the node to which the link is directed in column two. 
#' 
#' If the argument `attribute` is also specified, the function returns a data frame
#' with three columns. The first two columns are the same as above, the third 
#' column contain information about one node attribute
#' 
#' It is not possible to process more than one attribute at the time.
#' 
#' The function takes only data frames as input and the first column in the
#' input data frame needs to express the nodes originating the edges. 
#' 
#' The user should provide the values of 
#' names and attributes in the same order
#'
#' @param names A data frame with N columns where the first one has the survey 
#' respondents (edge origin), and the other columns express the destination of 
#' the edge  
#' @param attribute A data frame with the same dimensions as the data frame in 
#' names, where each column has the corresponding attribute for the structure 
#' in the names data frame.
#'
#' @return a data frame
#' @export makeEdgeList
#'
#' @examples
#' \dontrun{
#' namedf <- data.frame(respondent = c('A', 'B', 'C'), 
#'                       alter1 = c('D', 'C', 'A'), 
#'                       alter2 = c('B', 'A', 'A') )
#' attributedf <- data.frame(attribute1 = 1:3, attribute2 = 4:6, attribute3 = 2:4)
#' 
#' edgelist <-  makeEdgeList(names =  namedf, attribute = attributedf)  
#' }
makeEdgeList <- function(names = NULL , attribute = NULL) {

if (inherits(names, "data.frame")) {
  
  colNodes <- names[!is.na(names[ , 1]), ]
  
  edge <- data.frame()
  temp <- data.frame()
  for (i in 2:ncol(colNodes)) {
    temp <- cbind(colNodes[, 1], colNodes[, i])
    edge <- rbind(edge, temp)
  }
  
  if (!is.null(attribute)) {
    
    if((nrow(attribute) == nrow(names)) == TRUE) { 
      
      colweight <- attribute[!is.na(attribute[ , 1]), ]
      
      edgeW <- data.frame()
      temp <- data.frame()
      for (i in 2:ncol(colweight)) {
        temp <- cbind(colweight[ , 1], colweight[ , i])
        edgeW <- rbind(edgeW, temp)
        
      }
      
      edge <- cbind(edge, edgeW[ , 2])
      colnames(edge) <- c("from", "to", 'attribute')
      
    } else {
      
      cat('ERROR: names and attributes have to be data frames with the same dimensions')
      
    }
    
  } else {
    
    colnames(edge) <- c("from", "to")
    
  } 
  
  return(edge) 
  
  } else {
    
    cat("ERROR: The argument 'names' needs to be a data frame")
  }
}


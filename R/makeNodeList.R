#' makeNodeList
#'
#' Makes a node list from row data.
#'
#' This function processes row network data producing a list of nodes.
#' The function returns a vector if only `nodes` argument is specified and a
#' data frame if the `attribute` argument is specified too.
#' If only the argument `names` is specified, the function returns a vector with
#' the complete list of nodes named in the row data (respondents and alters).
#' If both the arguments `names` and `attribute` are specified, the function returns
#' a data frame with the complete list of nodes named in the survey, in column one,
#' and the attributes of these nodes in column two. If the attribute information
#' about the alter is unknown the value is reported as a missing data.
#'
#' It is not possible to process more than one attribute at the time.
#'
#' The function takes only data frames as input for the argument `names` and
#' vectors for the argument `attribute`. The user should provide the values of
#' names and attributes in the same order
#'
#' @param names A data frame with N columns where the first one has the survey
#' respondents (edge origin), and the other columns express the destination of
#' the edge
#' @param attribute A vector with information about the node attribute provided
#' in the same order as column one of the names data frame
#'
#' @return a vector or a data frame
#' @export makeNodeList
#'
#' @examples
#' \dontrun{
#' namedf <- data.frame(respondent = c('A', 'B', 'C'),
#'                       alter1 = c('D', 'C', 'A'),
#'                       alter2 = c('B', 'A', 'A') )
#' attributeV <- c(1:3)
#'
#' nodelist <-  makeNodeList(names =  namedf, attribute = attributeV)
#' }
makeNodeList <- function(names = NULL, attribute = NULL ) {

if (inherits(names, "data.frame")) {

  namelist <- sort(stats::na.omit(unique(as.vector(t(names)))))

  if (is.null(attribute)) {

    return(namelist)


  } else if (!is.null(attribute)) {

    if (is.vector(attribute)) {


      # attribute is a vector

      nodes <- stats::na.omit(data.frame(ID = names[ , 1], attribute = attribute))
      nodes <- nodes[order(nodes$ID), ]
      missing <- namelist  %in% nodes[ , 1]

      index <- data.frame(namelist, missing)
      add <- index[missing == FALSE,]
      colnames(add) <- c('ID', 'attribute')
      nodes <- rbind(nodes, add)

    } else {

      cat('ERROR: The argument attribute needs to be a vector')

    }
  }

} else {

  cat('ERROR: The argument names needs to be a data frame')

}

}

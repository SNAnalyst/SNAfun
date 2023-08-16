#' Perturb a binary graph
#' 
#' Perturb a binary graph probabilistically
#' 
#' Given a binary (ie. non-weighted) graph, change some proportion of its edges 
#' from 0 to 1 or 1 to 0. There are several ways of doing this:
#' 
#' When \code{prob_tot} is specified and \code{combined} is set to \code{FALSE}, 
#' all edges in the graph have the same probability of changing. In this case, 
#' \code{prob_0_to 1} and \code{prob_1_to_0} are ignored and, hence, no distinction 
#' is made between edges with value 0 or with value 1. 
#' This will ignore any choice of \code{prob_0_to 1} and \code{prob_1_to_0}.
#' 
#' When \code{prob_tot} is \code{NULL}, edges with value 0 will change to a 1 
#' with probability \code{prob_0_to 1} and edges with value 1 will change to a 0 
#' with probability \code{prob_1_to 0}.
#' 
#' When \code{prob_tot} is specified and \code{combined} is set to \code{TRUE},
#' first edges with value 0 will change to a 1 with probability \code{prob_0_to 1} 
#' and edges with value 1 will change to a 0 with probability \code{prob_1_to 0}.
#' After that, the edges in the resulting graph will be perturbated with 
#' probability \code{prob_tot} (potentially reversing some of the previous changes).
#' 
#' The \code{replace} argument allows for edges to be drawn multiple times. 
#' This means that fewer edges will change, since the same edge might change from 
#' a 0 to a 1 multiple times (or from a 1 to a 0). 
#' This is likely rarely useful.
#' 
#' The resulting graph will have the same class as has \code{x}.
#'
#' @param x graph of class \code{igraph}, \code{network}, or \code{matrix}
#' @param prob_tot probability of any edge to change (from 1 to 0 or 0 to 1)
#' @param prob_0_to_1 probability of any 0 edge to change to a 1
#' @param prob_1_to_0 probability of any 0 edge to change to a 1
#' @param combined logical, see details
#' @param replace logical, whether to draw the cells to be changed with replacement
#' @param diag logical, should the diagonal (self-loops) be allowed to change?
#'
#' @return the perturbed graph 
#' @export
#'
#' @examples
#' g <- snafun::create_random_graph(20, "gnm", m = 100)
#' make_perturbed_graph(g, prob_tot = .3)
#' # prob_0_to_1 is ignored
#' make_perturbed_graph(g, prob_tot = .3, prob_0_to_1 = 1)
#' # prob_0_to_1 is used first, then prob_tot
#' make_perturbed_graph(g, prob_tot = .3, prob_0_to_1 = 1, combined = TRUE)
#' make_perturbed_graph(snafun::to_matrix(g), prob_tot = .3, prob_0_to_1 = 1)
#' make_perturbed_graph(snafun::to_network(g), prob_tot = .3, prob_0_to_1 = 1)
make_perturbed_graph <- function(x, prob_tot = NULL, prob_0_to_1 = 0,
                            prob_1_to_0 = 0,
                            combined = FALSE,
                            replace = FALSE,
                            diag = FALSE
                            ) {
  
  klasse <- NULL
  if (inherits(x, "matrix")) {
    klasse <- "matrix"
  } else if (inherits(x, "network")) {
    klasse <- "network"
    x <- snafun::to_matrix(x)
  } else if (inherits(x, "igraph")) {
    klasse <- "igraph"
    x <- snafun::to_matrix(x)
  }
  if (is.null(klasse)) stop("Please input a graph of class igraph, network, or matrix")
  
  if (snafun::is_weighted(x)) stop("The graph needs to be binary, but you supplied a weighted graph")
  
  if (!is.null(prob_tot) && (prob_tot < 0 || prob_tot > 1)) {
    stop("'prob_tot' should be a probability")
  }
  if (prob_0_to_1 < 0 || prob_0_to_1 > 1) {
    stop("'prob_0_to_1' should be a probability")
  }
  if (prob_1_to_0 < 0 || prob_1_to_0 > 1) {
    stop("'prob_1_to_0' should be a probability")
  }
  
  if (!is.logical(replace)) stop("'replace' should be logical")
  if (!is.logical(diag)) stop("'diag' should be logical")
  if (!is.logical(combined)) stop("'combined' should be logical")
  
  len <- nrow(x)
  if (!diag) {
    diag_cells <- (0:(len-1))*len + (1:len)
  } else {
    diag_cells <- NULL
  }

  if (!is.null(prob_tot) && !combined){
    cellen <- setdiff(1:(len*len), diag_cells)
    cellen <- sample(cellen, round(prob_tot*len*(len-1)), replace = replace)
    x[cellen] <- abs(x[cellen] - 1)
  } else {
    cellen_0 <- which(x == 0)
    cellen_1 <- which(x == 1)
    if (!diag) {
      cellen_0 <- setdiff(cellen_0, diag_cells)
      cellen_1 <- setdiff(cellen_1, diag_cells)
    }
    
    # 0 naar 1
    cellen <- sample(cellen_0, size = round(prob_0_to_1*length(cellen_0)), replace = replace)
    if (length(cellen) > 0) x[cellen] <- x[cellen] + 1
    # 1 naar 0
    cellen <- sample(cellen_1, size = round(prob_1_to_0*length(cellen_1)), replace = replace)
    if (length(cellen) > 0) x[cellen] <- x[cellen] - 1
  }
  
  if (combined) {
    x <- make_perturbed_graph(x, 
                              prob_tot = prob_tot, 
                              prob_0_to_1 = 0,
                              prob_1_to_0 = 0,
                              replace = replace,
                              diag = diag,
                              combined = FALSE)
  }
  
  # return the correct class
  if (klasse == "igraph") {
    snafun::to_igraph(x)
  } else if (klasse == "network") {
    snafun::to_network(x)
  } else {   # matrix
    x
  }

}


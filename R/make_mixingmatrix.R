
#' Construct a mixing matrix
#' 
#' Construct a mixing matrix for a graph, based on a specific vertex attribute
#' 
#' A network mixing matrix is, traditionally, a two-dimensional cross-classification 
#' of edges by the values of a specific vertex attribute. This is an important 
#' tool for assessing network homophily or seggregation and often very useful 
#' for the subsequent construction of explanatory statistical models of the network. 
#' 
#' Each cell (*i*, *j*) in the mixing matrix reports the number of edges in the 
#' graph where the sender has value *i* on the vertex attribute and the receiver 
#' has value *j* on that vertex attribute. In case of an undirected graph, 
#' each edge counts twice, when *j* != *j*, since an undirected edge from MALE to 
#' FEMALE means that there is also an undirected edge from FEMALE to MALE.
#' 
#' If the argument \code{by_edge} is set to \code{TRUE}, a list of two mixing 
#' matrices is returned: the first contains the traditional mixing matrix and the 
#' second contains the mixing matrix of edges that do *no* occur in the graph.
#' The two matrices are appropriately called "edge_present" and "no_edge_present".
#' 
#' The mixing matrix includes row and column margins. Note that this can be somewhat 
#' misleading when a mixing matrix is constructed for an undirected graph, as 
#' the off-diagonal entries of the mixing matrix will occur twice. Therefore, the 
#' overall sum of edges corrects for this and will hence not equal the grand 
#' total one would expect by somply adding up the row margins or the column 
#' margins. In fact, in case of an undirected graph,. the correct grand total 
#' should be equal to the sum of the elements in the upper (or lower) triangle + 
#' the summed diagonal.
#' 
#' The argument \code{loops} can be set to \code{TRUE} if edges from a vertex 
#' to itself should be included. The default is to only include loops if the 
#' graph already includes loops itself. Otherwise, it generally makes little sense. 
#' 
#' The \code{network} package has a \code{\link[network]{mixingmatrix}} 
#' function that works only on \code{network} 
#' objects, but has specific functionality for bipartite networks. 
#' 
#' @param x graph of class \code{network} or \code{igraph}
#' @param attrname character, name of the attribute name
#' @param by_edge logical, if \code{TRUE} a full mixing matrix is calculated
#' @param loops logical, are loops allowed? By default, this is \code{TRUE} if 
#' the graph itself already has at least one loop.
#'
#' @return a table (if \code{by_edge} is \code{FALSE}) or a list with two 
#' tables (if \code{by_edge} is \code{TRUE})
#' @export
#' @examples
#' data(emon, package = "network")
#' is_directed(emon$LakePomona)   # TRUE
#' network::mixingmatrix(emon$LakePomona, "Sponsorship")
#' g <- emon$LakePomona
#' make_mixingmatrix(g, attrname = "Sponsorship")
#' make_mixingmatrix(g, attrname = "Sponsorship", by_edge = TRUE)
#' g <- snafun::to_igraph(emon$LakePomona)
#' make_mixingmatrix(g, attrname = "Sponsorship")
#' make_mixingmatrix(g, attrname = "Sponsorship", by_edge = TRUE)
#' 
#' data("judge_net", package = "snafun")
#' is_directed(judge_net)   # FALSE
#' make_mixingmatrix(judge_net, attrname = "color")
#' make_mixingmatrix(judge_net, attrname = "JudgeSex")
#' g <- suppressWarnings(snafun::to_network(judge_net))
#' make_mixingmatrix(g, attrname = "color")
#' make_mixingmatrix(g, attrname = "JudgeSex")
#' make_mixingmatrix(judge_net, attrname = "color", by_edge = TRUE)
#' make_mixingmatrix(judge_net, attrname = "JudgeSex", by_edge = TRUE)
make_mixingmatrix <- function(x, 
                              attrname, 
                              by_edge = FALSE, 
                              loops = has_loops(x)) {
  UseMethod("make_mixingmatrix")
}



#' @export
make_mixingmatrix.default <- function(x, 
                                      attrname, 
                                      by_edge = FALSE, 
                                      loops = has_loops(x)) {
  txt <- methods_error_message("x", "make_mixingmatrix")
  stop(txt)
}



#' @export
make_mixingmatrix.network <- function (x, 
                                      attrname, 
                                      by_edge = FALSE, 
                                      loops = has_loops(x)) {
  make_mixingmatrix_R(graph = x,
                      attr_name = attrname, 
                      by_edge = by_edge, 
                      loops = loops)
}



#' @export
make_mixingmatrix.igraph <- function (x, 
                                       attrname, 
                                       by_edge = FALSE, 
                                       loops = has_loops(x)) {
  make_mixingmatrix_R(graph = x, 
                      attr_name = attrname, 
                      by_edge = by_edge, 
                      loops = loops)
}








# the exact same code below works for both an igraph object and a network object
make_mixingmatrix_R <- function (graph, 
                                      attr_name, 
                                      by_edge = FALSE, 
                                      loops = has_loops(graph)) {
  # remove nanme, as the code does not work with names
  if (snafun::has_vertexnames(graph)) {
    graph <- remove_vertex_names(graph)
  }
  if (!has_vertex_attribute(graph, attrname = attr_name)) {
    stop("This vertex attribute does not occur on the graph object")
  }
  
  attrs <- snafun::extract_vertex_attribute(graph, name = attr_name)
  directed = is_directed(graph)
  if (length(attrs) != snafun::count_vertices(graph)) {
    stop("The number of vertices does not equal the attribute length")
  }
  if (!loops) {
    graph <- snafun::remove_loops(graph)
  }
  el <- snafun::to_edgelist(graph)
  if (nrow(el) != snafun::count_edges(graph)) {
    stop("The number of edges in the edgelist does not equal the number of edges on the graph object")
  }
  from <- try(factor(attrs[as.integer(el$from)], levels = sort(unique(attrs))), silent = TRUE)
  to <- try(factor(attrs[as.integer(el$to)], levels = sort(unique(attrs))), silent = TRUE)
  if (inherits(from, "try-error") | inherits(to, "try-error")) {
    stop("This attribute can not be correctly turned into a factor, so a mixing matrix can not be determined")
  }
  
  is_na <- any(is.na(attrs))
  
  if (!by_edge) {    ### make no distinction by_edge
    tab <- table(from = from, to = to, useNA = "ifany")
    if (!directed) {  # not directed
      tab <- fix_undirected_total(tab)
    } else {   # directed
      tab <- stats::addmargins(tab, quiet = TRUE)
    }
  } else {     # by_edge == TRUE, make distinction by_edge
    # force use of NA's if there is an NA anywhere 
    edge_present <- table(from = from, to = to, useNA = ifelse(is_na, "always", "no"))
    if (!directed) {  # not directed
      edge_present <- fix_undirected_total(edge_present)
    } else {   # directed
      edge_present <- stats::addmargins(edge_present, quiet = TRUE)
    }
    
    grid <- expand.grid(attrs, attrs)
    vcount <- length(attrs)
    directed_ids <- which(upper.tri(matrix(ncol = vcount, nrow = vcount)))
    loop_ids <- 1 + seq(from = 0, by = vcount + 1, length.out = vcount)
    
    if (!loops & !directed) {
      grid <- grid[-c(loop_ids, directed_ids),  ]
    } else if (!loops & directed) {
      grid <- grid[-loop_ids,  ]
    } else if (loops & !directed) {
      grid <- grid[-directed_ids,  ]
    }
    from <- try(factor(grid[, 1], levels = sort(unique(attrs))), silent = TRUE)
    to <- try(factor(grid[, 2], levels = sort(unique(attrs))), silent = TRUE)
    if (inherits(from, "try-error") | inherits(to, "try-error")) {
      stop("This attribute can not be correctly turned into a factor, so a mixing matrix can not be determined")
    }
    no_edge_present <- table(from = from, to = to, useNA = ifelse(is_na, "always", "no"))
    if (!directed) {
      no_edge_present <- fix_undirected_total(no_edge_present)
    } else {
      no_edge_present <- stats::addmargins(no_edge_present)
    }
    no_edge_present <- no_edge_present - edge_present
    tab <- list(edge_present = edge_present, no_edge_present = no_edge_present)
  }
  if (!directed) {
    message("\nNote:  Marginal totals can be misleading for undirected mixing matrices.")
  }
  return(tab)
}





fix_undirected_total <- function(x) {
  tap <- diag(x)
  x <- t(x) + x
  diag(x) <- tap
  tot <- sum(x[upper.tri(x)]) + sum(diag(x))
  x <- stats::addmargins(x, quiet = TRUE)
  x[nrow(x), ncol(x)] <- tot
  x
}
  

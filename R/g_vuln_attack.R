
#' Vulnerability: attack
#' 
#' Check the vulnerability of a graph against an attack against its vertices
#' 
#' Many complex systems display a surprising degree of tolerance against errors. 
#' However, error tolerance frequently comes at the high price that these 
#' networks become quite vulnerable to attacks (that is, to the removal of a 
#' few nodes that play a vital role in maintaining the network's connectivity). 
#' 
#' This function drops vertices from the graph according to four different regimes.
#' Every time, one vertex is removed, then the next one as well, et cetera, until 
#' the network has become empty. 
#' 
#' The network performance measure implemented here is the total number of vertices 
#' that can be reached by all of the vertices in the network. 
#' Mathematically, this is equivalent to the number of geodesics that have finite length.
#' In fact, this number is normalized, such that each value shows the fraction of 
#' "the total number of vertices that can be reached by all of the vertices in the 
#' original network" that is lost after removal of the specific vertex--this 
#' number is cumulative.
#' 
#' After the first vertex has been removed, the total number of vertices 
#' that can be reached by all of the vertices in the network is recalculated.
#' Of course, the number of vertices that can be reached is reduced already by 
#' the fact that there now is one vertex less that can be reached and that can 
#' be the start of a geodesic.
#' Once the network becomes emptier, vertices may have become isolates, so there
#' no longer is a reduction in the total number of vertices that can be reached 
#' when a given vertex is removed.
#' 
#' This algorithm is useful to show which (groups of vertices) are most critical 
#' to the functioning of the graph and which attack approach yields the best results.
#' 
#' Scenario 1: vertices are removed based on their betweenness score in the 
#' original graph. 
#' First the vertex is removed with the highest betweenness, then the one with 
#' the second highest betweenness, etc. This the column \code{Betw.-based}.
#' Say, the first vertex here has score 0.40. This means that 40 percent of 
#' all vertex pairs (directed, including the pairs that include this vertex itself) 
#' that could reach each other before this vertex was removed can no longer 
#' reach each other. 
#' Hence, only 60 percent of the reachable paths remain.
#' When the second vertex then has score 0.45, this means that removing the
#' second vertex removed an additional 5 percent of the original reachable paths.
#' 
#' Scenario 2: vertices are removed based on their degree in the 
#' original graph. 
#' First the vertex is removed with the highest degree, then the one with 
#' the second highest degree, etc. This the column \code{Degree-based}.
#' 
#' Scenario 3: vertices are removed based on their betweenness score in the 
#' active graph. 
#' First the vertex is removed with the highest betweenness. 
#' Then, betweenness scores are recalculated for the new, smaller graph.
#' This mimicks the case where a graph resettles itself after an attack/failure, 
#' redistributing the load across the remaining vertices.
#' Then, the vertex with the highest betweenness in this new graph is removed.
#' Then, betweenness scores are recalculated for the new, smaller graph and 
#' the vertex with the highest betweenness in this new graph is removed. Etc.
#' This the column \code{Cascade}.
#' 
#' Scenario 4: vertices are removed at random. 
#' This is done \code{k} times and then the average effect is determined of 
#' removing 1 random vertex, 2 random vertices, etc. 
#' This is useful to check how vulnerable a network is against a random attack 
#' or random drop-out. This is the \code{Random} column.
#' 
#' The \code{PropRemoved} column details the proportion of vertices that have 
#' been removed for that row. 
#' 
#' The \code{Expected} shows the score that one would expect to see for a 
#' connected network with this density. This is useful to see whether the network is affected 
#' faster or slower than you would expect in a random connected network. 
#' Note that this number does not correct for potential isolates in the network.
#' 
#'
#' @param g graph, as an object of class \code{igraph}, \code{network}, 
#' or \code{matrix}
#' @param mode Character constant, gives whether the shortest paths to or from 
#' the vertices should be calculated for directed graphs. If \code{out} then the 
#' shortest paths in the direction of the edges is taken, if \code{in} then 
#' paths will go against the direction of the edge. 
#' If \code{all}, the default, then the corresponding undirected graph will be 
#' used, ie. edge direction is not taken into account. 
#' This argument is ignored for undirected graphs.
#' @param weight Possibly a numeric vector giving edge weights. 
#' If this is \code{NA}--which is the default--
#' then no weights are used 
#' (even if the graph has a \code{weight} attribute).
#' If set to \code{NULL} and the graph 
#' has a \code{weight} edge attribute (with that exact name!), 
#' then this edge attribute is used in the calculation of the distances. 
#' @param k the number of simulations for the random scenario.
#' @param digits the number of decimals in the output
#' 
#' @family vulnerability measures
#' @return numeric matrix with appropriately named columns
#' @export
g_vuln_attack <- function(g, 
                        mode = c("all", "out", "in"),
                        weight = NA,
                        k = 10,
                        digits = 4) {
  
  if (inherits(g, "network")) {
    g <- snafun::to_igraph(g)
  }
  
  if (inherits(g, "matrix")) {
    g <- snafun::to_igraph(g)
  }
  
  if (!inherits(g, "igraph")) {
    stop("'g' should be an igraph object or be convertible to one")
  }
  # let hier op het gebruik van &&, 
  # dat is nodig omdat is.na(NULL) gelijk is aan logical(0) ipv FALSE
  if (!is.null(weight) && !is.matrix(weight) && !is.na(weight)) {
    stop("'weight' can only be NA, NULL, or a numeric matrix.")
  }
  
  if (is.matrix(weight) & !is.numeric(weight)) {
    stop("If you specify a matrix for 'weight', it needs to be numeric.")
  }
  
  if (is.null(weight)) {
    attrs <- igraph::edge_attr(g)
    if (!"weight" %in% names(attrs)) {
      stop("You specified NULL for 'weight', but there is no 'weight' edge attribute.")
    }
  }

  n <- length(igraph::V(g))
  noeud <- igraph::V(g)
  arc <- igraph::E(g)
  mode <- mode[1]
  dist <- igraph::distances(g, mode = mode, weights = weight)
  dist[dist == Inf] <- 0
  dist[dist > 0] <- 1
  tot <- sum(dist)
  fin <- matrix(ncol = 6, nrow = n, 0)
  # set empty colnames, so they can be set properly below for each method
  colnames(fin) <- rep("", ncol(fin))
  
  # betweenness-based attack
  mat <- matrix(ncol = 2, nrow = n, 0)
  mat[, 1] <- 1:n
  mat[, 2] <- igraph::betweenness(g, weights = weight)
  matri <- mat[order(mat[, 2]), ]
  g2 <- g
  # remove vertices, starting from the vertex with the highest betweenness
  # then the second highest one, etc.
  colnames(fin)[1] <- "Prop.Removed"
  colnames(fin)[2] <- "Betw.-basematrid"
  for (i in 1:n) {
    v = n + 1 - i
    g2 <- igraph::delete_vertices(g2, matri[v, 1])
    dist2 <- igraph::distances(g2, mode = mode, weights = weight)
    dist2[dist2 == Inf] <- 0
    dist2[dist2 > 0] <- 1
    tot2 <- sum(dist2)
    fin[i, 1] <- i / n
    fin[i, 2] <- tot - tot2
    matri[matri[, 1] > matri[v, 1], 1] <-
      matri[matri[, 1] > matri[v, 1], 1] - 1 #bluff
  }
  # proportion only 2 decimals is enough
  fin[, 1] <- round(fin[, 1], digits = 2)
  rm(g2, matri, mat, dist2, tot2, v, i)
  # degree attack
  mat <- matrix(ncol = 2, nrow = n, 0)
  mat[, 1] <- 1:n
  deg <- igraph::degree(g, mode = mode)
  mat[, 2] <- deg
  matri <- mat[order(mat[, 2]), ]
  # remove vertices, starting from the vertex with the highest degree
  # then the second highest one, etc.
  colnames(fin)[3] <- "Degree-based"
  g2 <- g
  for (i in 1:n) {
    v = n + 1 - i
    g2 <- igraph::delete_vertices(g2, matri[v, 1])
    dist2 <- igraph::distances(g2, mode = mode, weights = weight)
    dist2[dist2 == Inf] <- 0
    dist2[dist2 > 0] <- 1
    tot2 <- sum(dist2)
    fin[i, 3] <- tot - tot2
    matri[matri[, 1] > matri[v, 1], 1] <-
      matri[matri[, 1] > matri[v, 1], 1] - 1 #bluff
  }
  rm(g2, matri, mat, dist2, tot2, v, i)

  # cascading attack
  g2 <- g
  npro <- n
  lim <- n - 1
  # remove vertices, starting from the vertex with the highest degree
  # then recalculate bwness and remove the one that then has the highest bwness
  # then recalculate bwness and remove the one that then has the highest bwness, etc.
  colnames(fin)[4] <- "Cascade"
  for (i in 1:lim) {
    mat <- matrix(ncol = 2, nrow = npro, 0)
    mat[, 1] <- 1:npro
    mat[, 2] <- igraph::betweenness(g2, weights = weight)
    matri <- mat[order(mat[, 2]), ]  # sorted according to bwness (lo to hi)
    g2 <- igraph::delete_vertices(g2, matri[npro, 1]) # highest bw removed
    dist2 <- igraph::distances(g2, mode = mode, weights = weight)
    dist2[dist2 == Inf] <- 0
    dist2[dist2 > 0] <- 1
    tot2 <- sum(dist2)
    fin[i, 4] <- tot - tot2
    npro <- npro - 1
  }
  fin[n, 4] <- tot
  rm(g2, matri, mat, dist2, tot2, i, npro, lim)

  # random
  # remove the vertices in random order and everytime calculate tot2 
  # after each removal
  # Do this again from the beginning. And again, a total of k times.
  # Then average how much tot changed after removal of 1 node, 2 nodes, etc.
  colnames(fin)[5] <- "Random"
  for (l in 1:k) {
    al <- sample(1:n, n)  # vertices in willekeurige volgorde zetten
    g2 <- g
    for (i in 1:n) {
      g2 <- igraph::delete_vertices(g2, al[i])
      dist2 <- igraph::distances(g2, mode = mode, weights = weight)
      dist2[dist2 == Inf] <- 0
      dist2[dist2 > 0] <- 1
      tot2 <- sum(dist2)
      fin[i, 5] <- fin[i, 5] + (tot - tot2)
      al[al > al[i]] <- al[al > al[i]] - 1 #bluff
    }
  }
  # make proportions
  fin[, 2:4] <- fin[, 2:4] / tot
  fin[, 5] <- fin[, 5] / tot / k    # average out
  colnames(fin)[6] <- "Expected"
  fin[, 6] <- cumsum(2*(n - (1:n)))
  fin[, 6] <- fin[, 6]/max(fin[, 6])
  
  return(round(fin, digits = digits))
}


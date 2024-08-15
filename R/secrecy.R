
#' secrecy index
#' 
#' Calculates the graph level secrecy index of a graph and its vertices
#' 
#' The \code{secrecy} measure is useful for covert networks where its
#' members want to remain undetected to the law enforcement agencies (LEA's), 
#' even when some of their peers are detected. 
#' 
#' The \code{secrecy} measure is defined as the fraction of the network that 
#' remains unexposed if a single member of the network is detected. 
#' Hence, the score runs between 0 (= everybody gets exposed as soon as 1 
#' person is exposed) to 1 (= nobody gets exposed when 1 person is exposed). 
#' In real cases, the boundary values of 0 and 1 will only occur in pathological 
#' networks.
#' 
#' This relies on two things: 1. the probability of an individual becoming 
#' exposed when the LEA conducts a surveillance; 2. the fraction of 
#' the network that is exposed when a member of the network becomes detected 
#' in that surveillance.
#' 
#' There are several ways of computing the network's \code{secrecy}.
#' 
#' \code{S1} assumes that every actor has the same probability of being 
#' detected under a surveillance (with p = 1/number_of_vertices) and 
#' defines the secrecy that individual i 'contributes' to the
#' network as the fraction of individuals that remain unexposed when
#' upon monitoring individual i all his links with his neighbors are
#' detected. 
#' 
#' \code{S2} assumes that whenever an individual in
#' the network is being monitored, communication between him and
#' one of his neighbors is detected independently with probability p.
#' The case where p = 1 therefore corresponds to measure \code{S1}. 
#' If individual i has di neighbors the 
#' number of neighbors that will be detected is binomially distributed.
#' As with \code{S1}, it is assumed that every actor has the same probability 
#' of being detected under a surveillance (with p = 1/number_of_vertices).
#' 
#' The calculation of \code{S2} requires the user to specify a reasonable 
#' value for \code{p}, which may not be obvious.
#' 
#' \code{S3} no longer assumes that i = 1/n for all i in V (as in \code{S1} 
#' and \code{S2}). 
#' It can be argued that i = 1/n is a fair assumption when a covert operation 
#' is in its initial phase.
#' However, if an operation passed its initial stage the probability of
#' exposure will vary among network members. 
#' This happens because certain individuals, due to a more central position 
#' in the network, are more likely to be discovered. 
#' In \code{S3} this is captured by the equilibrium distribution of a random 
#' walk on the graph. 
#' This random walk chooses its next vertex at random from the neighbors
#' of the current vertex including itself.
#' 
#' NOTE: measure \code{S3} can break down in certain graphs and is no longer
#' bound to \[0,1\]. Discard of \code{S3} in those situations.
#' 
#' This function either returns one of the three secrecy measures (specified 
#' by \code{type}) or a data.frame containing all three. 
#' When \code{type == 0} or \code{type == 2}, a reasonable value for \code{p} 
#' should be specified.
#' 
#' The function \code{g_secrecy} returns the overall secrecy score of the 
#' graph: this is the fraction of vertices in the graph expected to remain 
#' unexposed under a single surveillance when a single vertex is exposed by the 
#' LEA.
#' Here, the default scenario is \code{type = 0}.
#' 
#' The function \code{v_secrecy} returns the secrecy score per vertex, 
#' this is the fraction of vertices in the graph that are to remain 
#' unexposed under a surveillance when that vertex is exposed multiplied by 
#' the probability of that vertex being exposed to begin with. 
#' In other words: if 1 vertex in the graph is exposed, what is the 
#' fraction of vertices that that is likely to remain
#' unexposed, due to vertex \code{i}? This is 
#' indeed determined by the number of neighbors of 
#' \code{i} and the risk of \code{i} being the one 
#' to be exposed.
#' 
#' 
#' In scenarios 1 and 2, all vertices have the same probability of being exposed,
#' so this secrecy score is proportional to the fraction that they leave unexposed 
#' when captured. 
#' For scenario 3, more central vertices are more likely to be exposed themselves, 
#' so more central vertices will have lower secrecy than other vertices (with 
#' the same number of neighbors): these central vertices are more easily exposed 
#' and therefore threaten the secrecy of the graph more.
#' Here, the default scenario is \code{type = 1}.
#' 
#' Note that the graph-level secrecy score is equal (barring rounding differences) 
#' to the sum of the vertex-level secrecy scores.
#' 
#' @param g graph of class \code{igraph}, \code{network}, \code{matrix}
#' @param type numeric, which secrecy type needs to be returned: 0, 1, 2, or 3.
#' @param p probability, needed when \code{type == 0} or \code{type == 2}
#' @param digits number of decimals used
#'
#' @return data.frame
#' @export
#' 
#' @references The formulas come from Lindelauf, R., Borm, P., & Hamers, H. (2009).
#' The influence of secrecy on the communication structure of covert networks. 
#' Social Networks, 31(2), 126-137. 
#' @name g_secrecy
#' @examples
#' \dontrun{
#' data(Madrid_bombing, package = "snafun")
#' g <- Madrid_bombing[[25]]
#' g_secrecy(g) # all three measures, p = .25
#' g_secrecy(g, p = .1) # all three measures, p = .1
#' g_secrecy(g, type = 1)  # only S1
#' 
#' v_secrecy(g, type = 0)
#' # almost the same, difference only due to rounding of each score
#' v_secrecy(g, type = 0) |> colSums()
#' }
NULL



#' @export
#' @rdname g_secrecy
g_secrecy <- function(g, type = 0, p = .25, digits = 3) {

  if (inherits(g, "network")) {
    g <- snafun::to_igraph(g)
  }
  
  if (inherits(g, "matrix")) {
    g <- snafun::to_igraph(g)
  }
  
  if (!inherits(g, "igraph")) {
    stop("'g' should be an igraph, network, or matrix object")
  }
  
  if (p < 0 || p > 1) {
    stop("'p' needs to be between 0 and 1 (inclusive)")
  }
  
  if (!type %in% 0:3) {
    stop("'type' can only be 0, 1, 2, or 3")
  }
  
  n <- igraph::vcount(g)
  m <- igraph::ecount(g)
  
  if (type == 1) {
    S1 <- (n^2 - n - 2*m)/(n^2)
    secrecy <- data.frame(S1 = round(S1, digits = digits))
  }
  if (type == 2) {
    S2 <- (n^2 - n - 2*p*m)/(n^2)
    secrecy <- data.frame(S2 = round(S2, digits = digits))
  }
  if (type == 3) {
    di <- igraph::degree(g)
    S3 <- ((2*m*(n - 2)) + (n*(n - 1)) - sum(di^2))/((2*m + n)*n)
    secrecy <- data.frame(S3 = round(S3, digits = digits))
  }
  if (type == 0) {
    S1 <- (n^2 - n - 2*m)/(n^2)
    S2 <- (n^2 - n - 2*p*m)/(n^2)
    di <- igraph::degree(g, mode = "all", loops = FALSE)
    S3 <- ((2*m*(n - 2)) + (n*(n - 1)) - sum(di^2))/((2*m + n)*n)
    secrecy <- data.frame(S1 = round(S1, digits = digits), 
                          S2 = round(S2, digits = digits), 
                          S3 = round(S3, digits = digits))
  }

  return(secrecy)
}



#' @export
#' @rdname g_secrecy
v_secrecy <- function(g, type = 1, p = .25, digits = 3) {

  if (inherits(g, "network")) {
    g <- snafun::to_igraph(g)
  }
  
  if (inherits(g, "matrix")) {
    g <- snafun::to_igraph(g)
  }
  
  if (!inherits(g, "igraph")) {
    stop("'g' should be an igraph, network, or matrix object")
  }
  
  if (p < 0 || p > 1) {
    stop("'p' needs to be between 0 and 1 (inclusive)")
  }
  
  if (!type %in% 0:3) {
    stop("'type' can only be 0, 1, 2, or 3")
  }
  
  n <- igraph::vcount(g)
  
  if (type == 1) {
    frac_exposed <- 1 - (igraph::degree(g) + 1)/n
    secrecy <- frac_exposed/n
  } else if (type == 2) {
    frac_exposed <- 1 - (p*igraph::degree(g) + 1)/n
    secrecy <- frac_exposed/n
  } else if (type == 3) {
    m <- igraph::ecount(g)
    deg <- igraph::degree(g)
    frac_exposed <- 1 - (deg + 1)/n
    alpha <- (deg + 1)/(2*m + n)
    secrecy <- alpha * frac_exposed
  } else { # type == 0
    m <- igraph::ecount(g)
    deg <- igraph::degree(g)
    S1 <- (1 - (deg + 1)/n)/n
    S2 <- (1 - (p*deg + 1)/n)/n
    frac_exposed <- 1 - (deg + 1)/n
    alpha <- (deg + 1)/(2*m + n)
    S3 <- alpha * frac_exposed
    secrecy <- cbind(S1, S2, S3)
  }
  round(secrecy, digits = digits)
}


% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/g.R
\name{gli}
\alias{gli}
\alias{g_summary}
\alias{g_density}
\alias{g_mean_distance}
\alias{g_correlation}
\alias{g_reciprocity}
\alias{g_transitivity}
\alias{g_diameter}
\alias{g_radius}
\alias{g_compactness}
\alias{g_degree_distribution}
\title{Graph level indices}
\usage{
g_summary(x, directed = TRUE)

g_density(x, loops = FALSE)

g_mean_distance(x)

g_correlation(g1, g2, diag = FALSE, weight = TRUE, wf = NULL)

g_reciprocity(x)

g_transitivity(x)

g_diameter(x, directed = is_directed(x), unconnected = TRUE)

g_radius(x, mode = c("all", "out", "in"))

g_compactness(x, mode = c("out", "in", "all"))

g_degree_distribution(
  x,
  mode = c("out", "in", "all"),
  type = c("density", "count"),
  cumulative = FALSE,
  loops = FALSE,
  digits = 3
)
}
\arguments{
\item{x}{graph object}

\item{directed}{Logical, whether directed or undirected paths are to be 
considered. This is ignored for undirected graphs. The default is \code{TRUE} 
is \code{x} is directed and \code{FALSE} otherwise}

\item{loops}{logical, should loops/diagonal be included? Default is \code{FALSE}.
This is almost always what you want.}

\item{g1}{an input graph of class \code{igraph}, \code{network}, \code{matrix}}

\item{g2}{an input graph of class \code{igraph}, \code{network}, \code{matrix}}

\item{diag}{logical, indicating whether or not the diagonal should be treated 
as valid data. Set this \code{TRUE} if and only if the data can contain 
loops. Is \code{FALSE} by default.}

\item{weight}{local, should weight be included? Is \code{TRUE} by default.}

\item{wf}{not to be set by the user, for internal use only}

\item{unconnected}{Logical, what to do if the graph is unconnected. 
If \code{FALSE}, the function will return a number that is one larger the 
largest possible diameter, which is always the number of vertices. 
If \code{TRUE}, the diameters of the connected components will be calculated 
and the largest one will be returned.}

\item{mode}{Character constant, gives whether the shortest paths to or from 
the given vertices should be calculated for directed graphs. 
If \code{out} then the shortest paths from the vertex, if \code{in} then to 
it will be considered. If \code{all}, the default, then the corresponding 
undirected graph will be used, edge directions will be ignored. 
This argument is ignored for undirected graphs.}

\item{type}{character, either "density" or "count"}

\item{cumulative}{logical, should the raw densities be returned or their 
cumulative sum}

\item{digits}{the maximal number of decimals in the result}
}
\description{
Graph level indices
}
\details{
Calculate several graph level indices.
}
\section{Functions}{
\itemize{
\item \code{g_summary()}: Summary indices of a graph.

This function provides a few very basic summary indices of a graph.
Only the default settings for the underlying functions (from this package)
are used. For further investigation, consider those settings.

If \code{directed == TRUE} (the default), the graph will be treated as
directed if it is (and as undirected if it is not). When
\code{directed == FALSE} a directed graph will be forced to become an undirected
graph using \link{to_symmetric_matrix} with \code{rule = "weak"}.

It can sometimes be informative to compare the summary stats for a directed
graph with its undirected equivalent.

\item \code{g_density()}: Density of a graph. Weights are discarded. 
Use \code{\link[sna]{gden}} if edge weights are to be used and \code{code} is 
a graph of class \code{network}.
Note that this function computes the density of a bipartite network without 
symmetrizing, so the density that is reported is half that of the density 
reported by the \code{\link[sna]{gden}} in this specific case.
Also note that \code{\link[igraph]{edge_density}} does not correctly deal with 
loops, so the current function reports correct density values whereas 
\code{\link[igraph]{edge_density}} reports incorrect values.

\item \code{g_mean_distance()}: Mean path distance

\item \code{g_correlation()}: Product-moment correlation between two networks. 
Missing values are permitted. Takes into account whether the 
graph is (un)directed (judged from \code{g1}). 

NOTE: The input graphs should be of class 
\code{igraph},\code{network} or 
\code{matrix}. It is possible to mix graph classes, so correlations can 
be calculated between graphs of class \code{network} and \code{igraph}, for 
example.

If a weight attribute is included in the graphs, these are used by default.

It is possible and the most common to provide two graphs directly, 
but the function also accepts a list of two graphs or an array 
of two graph (so, an array of size 2 x n x n). 
In this case, provide the list or array as the \code{g1} argument. 
The \code{g2} is ignored if \code{g1} is a list or array.

Internally, the graphs are converted to matrices before correlation is 
calculated.

\item \code{g_reciprocity()}: Reciprocity

\item \code{g_transitivity()}: Transitivity Transitivity is a triadic, algebraic structural 
constraint. In its weak form (which is the common form), the transitive constraint 
corresponds to a -> b -> c implying a -> c. This measure returns  the fraction 
of potentially intransitive triads obeying the weak condition. 
In other words, we count the number of triplets in which i -> j, 
j -> k, and i -> k, and divide by the number of triplets in which 
i -> j and j -> k (regardless of whether there is an i -> k edge).

Weights are discarded. Specific functions that can alternatively be used (and are 
called by this function) include \code{\link[sna]{gtrans}} (for objects of 
class \code{network}) and \code{\link[igraph]{transitivity}} (for objects of 
class \code{igraph}).

The \code{network} and \code{igraph} implementations differ and can give 
somewhat different results.

\item \code{g_diameter()}: Diameter of a graph. Weights are discarded. The diameter is 
equal to the maximum eccentricity scores across all vertices. 
Substantively, this measures the maximum number of steps that are needed to 
connect any two vertices in the graph (given that they are connected).
Use \code{\link[igraph]{diameter}} if edge weights are to be used or specific 
options are needed.

\item \code{g_radius()}: Radius of a graph. Weights are discarded. The diameter is 
equal to the minimum eccentricity scores across all vertices. 
Substantively, this measures the minimum number of steps that are needed to 
connect any two vertices in the graph (given that they are connected). 
In other words, regardless of which vertex is considered, it is not possible 
to reach all other vertices from it within fewer than this number of steps.

\item \code{g_compactness()}: Compactness of a graph. Weights are discarded. 

The compactness of a graph is the average of the inverse distances across 
all dyads in the graph. Mathematically:

\deqn{\frac{\sum_{i,j : i \neq j} (\frac{1}{d_{ij}})}{n(n-1)}}

where the denominator is altered according to the number of dyads in the graph.

If all vertices are directly tied to each other, compactness is 1. 
The shorter the paths between vertices, the larger the inverse distances become. 
Hence, the shorter the paths, the higher compactness. As such, it is often 
seen as a measure of cohesion.

This measure works well for disconnected graphs. Inverse distances between 
disconnected vertices are 0, so the more disconnected the graph, the lower its 
compactness. 

Thus, the measure runs between 0 (abolutely "uncompact") to 1 (maximally compact).

\item \code{g_degree_distribution()}: Degree distribution. 

The distribution of the degrees of the vertices in the graph.

Returns a matrix with the counts or densities for each degree and the 
degrees themselves as column names.
The result can be raw or cumulative (starting from degree 0.

}}
\examples{
m <- matrix(rbinom(25, 1, 0.5), 5, 5)
diag(m) <- 0
g_density(snafun::to_network(m))
g_density(snafun::to_igraph(m))
# when loops matter
g <- igraph::graph( c(1,2, 2,2, 2,3) )
g_density(g, loops = FALSE)   # this is wrong, if loops matter
g_density(g, loops = TRUE)   # this is correct, if loops matter

g <- sna::rgraph(10, mode = "digraph")
g_n <- snafun::to_network(g)
g_mean_distance(g_n)
g_i <- snafun::to_igraph(g)
g_mean_distance(g_i)

g <- sna::rgraph(10, mode = "graph")
g_n <- snafun::to_network(g)
g_mean_distance(g_n)
g_i <- snafun::to_igraph(g)
g_mean_distance(g_i)
#
# correlation
# matrices
# 
g1 <- sna::rgraph(10,1,tprob=c(0.2,0.2,0.5,0.5,0.8,0.8))
g2 <- sna::rgraph(10,1,tprob=c(0.2,0.2,0.5,0.5,0.8,0.8))
g_correlation(g1, g2)
g1 <- to_network(g1); g2 <- to_network(g2)
g_correlation(g1, g2)
g1 <- to_igraph(g1); g2 <- to_igraph(g2)
g_correlation(g1, g2)              
# 
# reciprocity
g <- igraph::erdos.renyi.game(10, .3, type = "gnp", directed = TRUE)
g_reciprocity(g)
g_reciprocity(snafun::to_network(g))

g <- snafun::create_random_graph(10, strategy = "gnm", m = 2, directed = FALSE, graph = "igraph")
g_reciprocity(g)
g_reciprocity(snafun::to_network(g))
#
# transitivity
data("emon", package = "network")
g <- emon$Cheyenne
is_directed(g)  # TRUE
g_transitivity(g)
g_transitivity(to_network(g))

data(florentine, package = "snafun")
g <- florentine$flobusiness
is_directed(g)  # FALSE
g_transitivity(g)
g_transitivity(to_network(g))
#
# diameter
g <- igraph::make_ring(10)
g2 <- igraph::delete_edges(g, c(1,2,1,10))
igraph::diameter(g2, unconnected=TRUE)
igraph::diameter(g2, unconnected=FALSE)
g_diameter(g2)  # 7
g_diameter(g2, unconnected = FALSE)  # Inf

g_diameter(to_network(g2)) # 7
g_diameter(to_network(g2), unconnected = FALSE) # Inf
#
# radius
g_i <- snafun::create_random_graph(10, strategy = "gnp", p = .2, 
   directed = TRUE, graph = "igraph")
# add isolate
g_i_iso <- igraph::add_vertices(g_i, nv = 1)
igraph::radius(g_i)
snafun::v_eccentricity(g_i)
snafun::extract_isolates(g_i_iso)
snafun::v_eccentricity(g_i_iso)  # the isolate has eccentricity 0
igraph::radius(g_i_iso)  # also 0, should raise a flag about isolates
g_radius(g_i, mode = "all")
g_radius(g_i, mode = "in")
g_radius(g_i, mode = "out")

g_n <- snafun::to_network(g_i)
g_radius(g_n, mode = "all")
g_radius(g_n, mode = "in")
g_radius(g_n, mode = "out")
#
# compactness
g_i <- snafun::create_random_graph(10, strategy = "gnp", p = .2, 
   directed = TRUE, graph = "igraph")
# add isolate
g_i_iso <- igraph::add_vertices(g_i, nv = 1)
g_n <- snafun::to_network(g_i)
g_n_iso <- snafun::to_network(g_i_iso)
g_compactness(g_i)
g_compactness(g_n)
g_compactness(g_i, mode = "all")   # if direction is irrelevant
g_compactness(g_i_iso)
g_compactness(g_n_iso)

g1 <- igraph::graph_from_literal(A-B-C-D-E-F)
g2 <- igraph::delete_edges(g1, 5)
g3 <- igraph::delete.edges(g1, 3)
g4 <- igraph::delete_edges(g1, c(4, 5))
g5 <- igraph::delete_edges(g1, c(2, 4))
g_compactness(g1)
g_compactness(g2)
g_compactness(g3)
g_compactness(g4)
g_compactness(g5)
#
# degree distribution
g <- snafun::create_random_graph(1000, "gnp", p = .01)
g_degree_distribution(g)
}

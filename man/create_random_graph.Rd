% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create.R
\name{create_random_graph}
\alias{create_random_graph}
\title{Create a random bipartite network}
\usage{
create_random_graph(
  n_vertices,
  strategy = c("gnp", "gnm"),
  p,
  m,
  directed = TRUE,
  graph = c("igraph", "network")
)
}
\arguments{
\item{n_vertices}{integer, number of vertices to include in the graph}

\item{strategy}{Character scalar, the type of the graph, ‘gnp’ creates a 
$G(n,p)$ graph, ‘gnm’ creates a $G(n,m)$ graph. See details below.}

\item{p}{probability for $G(n,p)$ graphs. Should not be given for $G(n,m)$ graphs.}

\item{m}{integer, the number of edges for $G(n,p)$ graphs. Should not be given for $G(n,p)$ graphs.}

\item{directed}{logical, whether to create a directed graph}

\item{graph}{character, the type of graph to be generated: \code{igraph} or 
\code{network}}
}
\value{
a graph of class \code{network} or \code{igraph}
}
\description{
Generate a random graph
}
\details{
Generate a random network, either as a \code{igraph} or 
\code{network} object.

When using strategy 'gnp', a bipartite network is generated where each edge
has probability 'p' of occuring. This means that consecutive runs of the 
algorithm will usually result in graphs with different numbers of edges. 

When using strategy 'gnm', a bipartite network is generated with exactly 'm' 
edges. This means that consecutive runs of the 
algorithm will result in graphs with the same number of edges (but will occur 
between different vertices).
}
\examples{
create_random_graph(n_vertices = 10, "gnp", p = .25, graph = "igraph")
create_random_graph(n_vertices = 10, "gnp", p = .25, graph = "network")
create_random_graph(n_vertices = 10, "gnm", m = 15, graph = "igraph")
create_random_graph(n_vertices = 10, "gnm", m = 15, graph = "network")
}

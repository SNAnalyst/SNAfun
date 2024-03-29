% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract.R
\name{extract_neighbors}
\alias{extract_neighbors}
\title{Extract the neighbors of a vertex}
\usage{
extract_neighbors(x, vertex, type = c("out", "in", "all"))
}
\arguments{
\item{x}{graph object if class \code{igraph} or \code{network}}

\item{vertex}{number or name (if the graph has vertex names) of a single vertex}

\item{type}{the type of neighborhood to be returned}
}
\value{
vector with vertex ID's or names of the neighbors
}
\description{
Get the names or ID's of all vertices with whom the focal vertex is connected
}
\details{
When \code{mode == "out"} the function returns all vertices that receive an 
edge from \code{vertex}.

When \code{mode == "in"} the function returns all vertices that send an 
edge to \code{vertex}.

When \code{mode == "all"} all vertices with an edge to or from \code{vertex} 
are returned.

In an undirected graph, \code{mode} will always be taken to be "all".

Note that \code{vertex} needs to be a \emph{single} vertex, not a vector.
}
\examples{
m <- matrix(0, 3, 3)
m[1, 2] <- 1; m[2, 3] <- 1; m[3, 1] <- 1
g_i <- snafun::to_igraph(m)
extract_neighbors(g_i, 1, "out")
extract_neighbors(g_i, 1, "in")
extract_neighbors(g_i, 1, "all")

g_n <- snafun::to_network(m)
extract_neighbors(g_n, 1, "out")
extract_neighbors(g_n, 1, "in")
extract_neighbors(g_n, 1, "all")
}

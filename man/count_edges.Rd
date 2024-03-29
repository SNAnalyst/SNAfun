% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/count.R
\name{count_edges}
\alias{count_edges}
\title{Number of edges in the graph}
\usage{
count_edges(x)
}
\arguments{
\item{x}{graph}
}
\value{
integer
}
\description{
Count the number of edges in the graph
}
\details{
Returns the number of edges in an object of class \code{network} 
or \code{igraph}.
}
\examples{
net <- igraph::random.graph.game(10, p.or.m = .15, type = "gnp")
count_edges(net)

net <- sna::rgraph(n = 10, tprob = 0.15) |> network::as.network()
count_edges(net)
}

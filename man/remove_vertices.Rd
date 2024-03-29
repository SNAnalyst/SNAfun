% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/remove.R
\name{remove_vertices}
\alias{remove_vertices}
\title{Remove vertices}
\usage{
remove_vertices(x, vertices)
}
\arguments{
\item{x}{graph object of class \code{network} or \code{igraph}}

\item{vertices}{vector with either vertex names or vertex id's.}
}
\value{
graph without the removed vertices
}
\description{
Remove specific vertices from the graph
}
\details{
Removes specific vertices from a graph (potentially bipartite) of 
class \code{network} or \code{igraph}.

The vertices to be removed can either be their id's or their names (if they 
exist in the graph object). The algorithm will first check if the 
\code{vertices} coincide with vertex names that are present in the object 
(if they exist) and, if they match, the vertices are removed according to 
their names. If they do not match, or if there are no vertex names in the 
graph object, \code{vertices} is assumed to refer to vertex id's.
}

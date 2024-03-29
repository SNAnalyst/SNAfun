% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract.R
\name{extract_edge_id}
\alias{extract_edge_id}
\title{Extract edge id's}
\usage{
extract_edge_id(object, ego, alter, edgelist, ordered = FALSE)
}
\arguments{
\item{object}{graph of class \code{network} or \code{igraph}}

\item{ego}{numeric vector with the vertex id's}

\item{alter}{numeric vector with the vertex id's}

\item{edgelist}{matrix or data.frame with two numeric columns, 
containing the vertex id's. Is ignored when \code{ego} and/or \code{alter} 
are (is) specified.}

\item{ordered}{logical, should the output be ordered according to the edgelist}
}
\value{
data.frame with three columns: ego (=sender), alter (=receiver), 
eid (= edge id)
}
\description{
Extract the edge id's for given sets of ego(s) and alter(s)
}
\details{
This is a utility function that extracts the id of specific edges. 
This is mainly useful when working with the same graph, both as a \code{network} 
and \code{igraph} object, because these objects are constructed differently 
and internally order the edges differently.

There are three arguments that help determine for which edges the id's are extracted.

When only \code{ego} is specified, \emph{all} edges are included that have an 'ego' 
node as the sender (in case of a directed graph) or as a sender or receiver 
(in case of an undirected graph).

When only \code{alter} is specified, \emph{all} edges are included that have an 'alter' 
node as the receiver (in case of a directed graph) or as a sender or receiver 
(in case of an undirected graph).

When both \code{ego} and \code{alter} are specified, edges are included that 
have an 'ego' as the sender and an 'alter' as receiver (or vice versa, in 
case of an undirected graph).

When only \code{edgelist} is specified, the exact edges specified in that 
edgelist are used.

In case an id is requested for an edge that does not occur in the graph, 
zero is returned for that non-existent edge(s).

Note that \code{ego}, \code{vertex}, and \code{edgelist} should only contain 
numeric vertex id's and will not accept vertex names in the current implementation 
of this function.
}

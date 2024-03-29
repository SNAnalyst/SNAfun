% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/is.R
\name{is_something}
\alias{is_something}
\alias{is_network}
\alias{is_igraph}
\title{Is something?}
\usage{
is_network(x)

is_igraph(x)
}
\arguments{
\item{x}{graph object}
}
\value{
logical, \code{TRUE} or \code{FALSE}
}
\description{
Check if the network is something specific.
}
\details{
Check for various "identities" of the input object
}
\section{Functions}{
\itemize{
\item \code{is_network()}: Check whether a graph is of class \code{network}

\item \code{is_igraph()}: Check whether a graph is of class \code{igraph}

}}
\examples{
data(florentine, package = "snafun")
is_network(florentine$flobusiness)   # FALSE
is_igraph(florentine$flobusiness)   # TRUE
data(emon, package = "network")
is_network(emon$Cheyenne)   # TRUE
is_igraph(emon$Cheyenne)   # FALSE
}

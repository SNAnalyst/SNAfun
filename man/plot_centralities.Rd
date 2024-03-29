% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_centralities.R
\name{plot_centralities}
\alias{plot_centralities}
\title{Plot one or more centrality scores}
\usage{
plot_centralities(
  net,
  measures = c("betweenness", "closeness", "degree", "eccentricity"),
  directed = TRUE,
  mode = c("all", "out", "in"),
  k = 3,
  rescaled = FALSE,
  ...
)
}
\arguments{
\item{net}{a network of class \code{igraph} or \code{network}}

\item{measures}{character vector of one or more centrality measures}

\item{directed}{logical, should direction be taken into account? This argument
is optional, if it is not chosen the directionality of the network will be used.}

\item{mode}{Character. The default is "all".}

\item{k}{The k parameter for the "geokpath" measure. The default is 3.}

\item{rescaled}{Logical scalar, whether to rescale the centrality scores to 
they add up to 1.}

\item{...}{other arguments passed to \code{\link{plot}}.}
}
\value{
a plot
}
\description{
Plot one or more centrality scores of all vertices in the network
}
\details{
This function creates of grid of plots containing the values a set of 
centrality scores, selected by the user. The function will calculate the 
requested scores and then plot them. When a large network is used and 
many or complex measures are requested, the calculations can take some time. 
The plotting itself will be fast.

Since the function calculates the centrality scores itself, the choice 
is currently limited to the centralities implemented in this package.

At the moment, these are "betweenness", "closeness", "degree", "eccentricity", 
"eigenvector", "geokpath", "harmonic", "shapley", and "stress". The default is to 
plot "betweenness", "closeness", "degree", and "eccentricity".

The various arguments relate to these measures. The \code{mode}, 
\code{directed}, and \code{rescaled} arguments will be appled to all chosen 
measures

The function takes an \code{igraph} object or a \code{network} object as input.
}
\examples{
\dontrun{
g <- igraph::erdos.renyi.game(20, 1/20)
plot_centralities(g, measures = c("degree", "betweenness"))
# note that closeness is not well-defined for disconnected graphs
plot_centralities(g)

data(florentine, package = "snafun")
business <- florentine$flobusiness
plot_centralities(business)
plot_centralities(business, rescaled = TRUE)
plot_centralities(business, measures = c("betweenness", "closeness", "degree", 
   "eccentricity", "eigenvector", "geokpath", "harmonic", "stress"))
bus_n <- snafun::to_network(business)
plot_centralities(bus_n, c("degree", "betweenness"), mode = "out", use_vertexnames = TRUE)
}
}

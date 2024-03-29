% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_perturbed_graph.R
\name{make_perturbed_graph}
\alias{make_perturbed_graph}
\title{Perturb a binary graph}
\usage{
make_perturbed_graph(
  x,
  prob_tot = NULL,
  prob_0_to_1 = 0,
  prob_1_to_0 = 0,
  combined = FALSE,
  replace = FALSE,
  diag = FALSE
)
}
\arguments{
\item{x}{graph of class \code{igraph}, \code{network}, or \code{matrix}}

\item{prob_tot}{probability of any edge to change (from 1 to 0 or 0 to 1)}

\item{prob_0_to_1}{probability of any 0 edge to change to a 1}

\item{prob_1_to_0}{probability of any 0 edge to change to a 1}

\item{combined}{logical, see details}

\item{replace}{logical, whether to draw the cells to be changed with replacement}

\item{diag}{logical, should the diagonal (self-loops) be allowed to change?}
}
\value{
the perturbed graph
}
\description{
Perturb a binary graph probabilistically
}
\details{
Given a binary (ie. non-weighted) graph, change some proportion of its edges 
from 0 to 1 or 1 to 0. There are several ways of doing this:

When \code{prob_tot} is specified and \code{combined} is set to \code{FALSE}, 
all edges in the graph have the same probability of changing. In this case, 
\code{prob_0_to 1} and \code{prob_1_to_0} are ignored and, hence, no distinction 
is made between edges with value 0 or with value 1. 
This will ignore any choice of \code{prob_0_to 1} and \code{prob_1_to_0}.

When \code{prob_tot} is \code{NULL}, edges with value 0 will change to a 1 
with probability \code{prob_0_to 1} and edges with value 1 will change to a 0 
with probability \code{prob_1_to 0}.

When \code{prob_tot} is specified and \code{combined} is set to \code{TRUE},
first edges with value 0 will change to a 1 with probability \code{prob_0_to 1} 
and edges with value 1 will change to a 0 with probability \code{prob_1_to 0}.
After that, the edges in the resulting graph will be perturbated with 
probability \code{prob_tot} (potentially reversing some of the previous changes).

The \code{replace} argument allows for edges to be drawn multiple times. 
This means that fewer edges will change, since the same edge might change from 
a 0 to a 1 multiple times (or from a 1 to a 0). 
This is likely rarely useful.

The resulting graph will have the same class as has \code{x}.
}
\examples{
g <- snafun::create_random_graph(20, "gnm", m = 100)
make_perturbed_graph(g, prob_tot = .3)
# prob_0_to_1 is ignored
make_perturbed_graph(g, prob_tot = .3, prob_0_to_1 = 1)
# prob_0_to_1 is used first, then prob_tot
make_perturbed_graph(g, prob_tot = .3, prob_0_to_1 = 1, combined = TRUE)
make_perturbed_graph(snafun::to_matrix(g), prob_tot = .3, prob_0_to_1 = 1)
make_perturbed_graph(snafun::to_network(g), prob_tot = .3, prob_0_to_1 = 1)
}

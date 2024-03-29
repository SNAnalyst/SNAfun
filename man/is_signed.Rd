% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/is.R
\name{is_signed}
\alias{is_signed}
\title{Is the network signed?}
\usage{
is_signed(x)
}
\arguments{
\item{x}{graph object}
}
\value{
logical, \code{TRUE} or \code{FALSE}
}
\description{
Check if the network is signed
}
\details{
A signed \code{igraph} or \code{network} object has a "sign" attribute.

For a matrix or data.frame, a signed network is recognized by the presence 
of positive and negative ties, where values are integers.
A signed graph that only has positive integer values will not be recognized 
as such (but is pathologial anyway).

Specifially, for a \code{data.frame}, this check is conducted on its 
third column, which is the most logical column to hold the signs.
}
\examples{
data(florentine, package = "snafun")
is_signed(florentine$flobusiness)   # FALSE
}
\references{
functions from the excellent \code{migraph} package. 
These methods are licensed through the 
\href{https://choosealicense.com/licenses/mit/}{MIT license}
in combination with copyright for James Hollway.
}

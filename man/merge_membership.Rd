% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/merge_community_membership.R
\name{merge_membership}
\alias{merge_membership}
\title{Merge community membership}
\usage{
merge_membership(coms, merges)
}
\arguments{
\item{coms}{object of class \code{communities} from one of the appropriate 
\code{igraph} cluster / communities functions.}

\item{merges}{list of the merged that should be performed (see 'details' section)}
}
\value{
new object of class \code{communities}
}
\description{
Merge communities together for nicer plotting.
}
\details{
Community detection algorithms, such as
\code{\link[igraph]{cluster_edge_betweenness}},
\code{\link[igraph]{cluster_fast_greedy}},
\code{\link[igraph]{cluster_louvain}},
\code{\link[igraph]{cluster_leiden}},
\code{\link[igraph]{cluster_walktrap}}, 
et cetera,
sometimes yield a bunch of small communities, in addition to larger, 
meaningful communities.
This is always the case when there are isolates, as they might each get 
a community of their own.

Then, it can be useful to merge (some of) the small communities together for 
more informative plotting. 

The input of this function is the output of one of the algorithms above: 
an object of class \code{communities}. 
The output is also of class \code{communities} and can thus be fed into 
igraph's plot function.

The \code{merges} argument specifies which communities should be merged. 
It is a list, where each element contains the numbers of the communities 
(as specified in the \code{coms} object) that need to be merged into a new 
community. 
The list can contain multiple elements, each will become their own new community.
See the examples for, well, examples of this.

For clarity, the new communities will be numbered from n + 1, where 
'n' is the number of communities in \code{coms}.

NOTE: this is only useful for plotting!! 
The only thing the function does is change the membership of the vertices 
into the new membership structure. 
However, it leaves the modularity values intact and also does not change the 
'merges' element from the \code{coms} object. 
Hence, running \code{\link[igraph]{modularity}} on the output of this function 
will return the modularity of the \strong{original} community memberships, not the 
new merged ones. 
However, it is easy to calculate the modularity of the new 
communities: see example section below.
Applying \code{\link[igraph]{sizes}} works fine on the output of 
this function.
}
\examples{
\dontrun{
data(judge_net, package = "SNA4DSData")
# original plot, with 5 communities
coms <- igraph::cluster_fast_greedy(judge_net)
plot(coms, judge_net)
igraph::sizes(coms)
# for illustration of the function, let's join node 40 with community 2
# it is best to merge into a new object, so the original result is not lost
com2 <- merge_membership(coms, merges = list(c(2, 5)))
plot(com2, judge_net)
igraph::sizes(com2)

# Compute modularity for the new grouping
# (Note: it is OK that the communities are not numbered consecutively)
igraph::modularity(judge_net, com2$membership)
# igraph::cluster_fast_greedy uses the edge weight by default.
# To include that here as well, use:
igraph::modularity(judge_net, com2$membership, 
       weights = igraph::E(judge_net)$weight)
# Note: \code{igraph::modularity} is only intended for undirected 
# graphs with non-negative weights. In case of negative weights, 
# use \code{wsyn::modularity}. 
# In case of directed graphs, I currently am not aware of a good 
# implementation of modularity.
}
}

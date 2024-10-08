% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stat_gof.R
\name{stat_plot_gof_as_btergm}
\alias{stat_plot_gof_as_btergm}
\title{gof and plot (btergm style)}
\usage{
stat_plot_gof_as_btergm(
  m,
  ...,
  btergm_statistics = c(btergm::esp, btergm::geodesic, btergm::deg, btergm::rocpr),
  silent = FALSE,
  verbose = TRUE
)
}
\arguments{
\item{m}{a fitted \code{ergm} or \code{btergm} model}

\item{...}{optional arguments, see description}

\item{btergm_statistics}{vector of statistics to be used in the gof}

\item{silent}{logical, if \code{TRUE} fewer messages will be printed during the 
calculation of the gof (but the function will often still print some output)}

\item{verbose}{logical, if \code{FALSE}, the printing of most details 
by the gof function is suppressed (but some warnings may still appear and 
errors are always shown)}
}
\value{
the goodness of fit is silently returned
}
\description{
Determine the gof and plot the gof, using the btergm approach
}
\details{
For a fitted \code{ergm} or \code{btergm}, this function calculates 
the goodness of fit (\code{gof}) and plots it. 
This is done using the \code{btergm} engine, which also works for 
fitted \code{ergm} models.

For calculating the gof, btergm provides the option to check the fit of the 
model against a range of statistics. 
See \link[btergm]{gof-statistics} for an overview of the available statistics 
to check against. Note that these partly overlap with statistics available 
in the \code{ergm} package, but not exactly. 
If a statistic is required that is not inluded in the \code{btergm} package 
for use in the \code{gof} function (such as "nodecov"), you need to use the 
\code{ergm} package for this and can not use this function.

The \code{...} argument allows one to pass arguments to the 
\link[btergm]{gof} function, such as \code{target}, \code{formula}, 
\code{parallel}, \code{ncpus}. 
See the help of \link[btergm]{gof} for the options.

Many messages that are returned during the whole process can be suppressed 
by setting \code{silent = TRUE} and \code{verbose = FALSE}.

NOTE: the goodness of fit is silently returned. The plot is 
always plotted.

NOTE: \code{ergm} and \code{btergm} are packages made by separate research 
groups. As a result, the objects and functions are not 100 percent 
compatible. If applying this function to an \code{ergm}-model returns 
weird errors, it is better to calculate the gof using the 
\code{ergm} package and then plot that (using \link{stat_plot_gof} or 
using the \code{ergm} package functions).

NOTE2: because the gof statistics for the models are determined inside the 
plotting functions (so they can be used for the plotting), they can take some
time to run. This is why it is useful that the gof results themselves are not 
only calculated by this function, but also returned for further inspection.
}
\examples{
\dontrun{
### ergm model
data(florentine, package = "snafun")
flom <- to_network(florentine$flomarriage)
m_ergm <- ergm::ergm(flom ~ edges + nodecov("Wealth"))
gof_ergm <- stat_plot_gof_as_btergm(m_ergm)
gof_ergm <- stat_plot_gof_as_btergm(m_ergm, 
        btergm_statistics = c(btergm::esp, btergm::dsp, btergm::deg))
gof_ergm

### btergm model
data(alliances, package = "SNA4DSData")
m_btergm <- btergm::btergm(allianceNets ~ edges + 
  gwesp(0, fixed = TRUE) +
  edgecov(lastYearsSharedPartners) + 
  edgecov(militaryDisputes) + 
  nodecov("polity") +
  nodecov("Composite_Index_National_Capability") + 
  absdiff("polity") + 
  absdiff("Composite_Index_National_Capability") +
  edgecov(sharedBorder), 
  R = 100, # number of bootstraps, perhaps set lower to just try the example
  parallel = "snow", ncpus = 16  # optional line
)

# gof with only 10 simulations to keep time for the example low
gof_btergm <- stat_plot_gof_as_btergm(m_btergm, nsim = 10)
gof_btergm <- stat_plot_gof_as_btergm(m_btergm, 
  parallel = "snow", ncpus = 12)
gof_btergm
gof_btergm <- stat_plot_gof_as_btergm(m_btergm, 
  parallel = "snow", ncpus = 12, silent = TRUE)
gof_btergm <- stat_plot_gof_as_btergm(m_btergm, 
  parallel = "snow", ncpus = 12, verbose = FALSE, silent = TRUE)
}
}
\seealso{
Other statistics functions: 
\code{\link{stat_ef_int}()},
\code{\link{stat_plot_gof}()}
}
\concept{statistics functions}

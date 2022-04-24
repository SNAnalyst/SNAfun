



#' Florentine families
#'
#' Network data of 16 Florentine families, in \code{igraph} format.
#'
#' A list containing two networks and a data.frame with attributes.
#'
#' This is a data set of 1520 marriage and 15 business ties among Renaissance Florentine
#' families. The data is originally from Padgett (1994) via UCINET and stored
#' as an \code{igraph} object.
#'
#' This dataset is a subset of the original data and contains data on sixteen
#' families.
#'
#' The two relations are business ties (\code{flobusiness}, recorded financial
#' ties such as loans, credits and joint partnerships) and
#' marriage alliances (\code{flomarriage}).
#'
#' In addition, there is a data.frame with attributes:
#' \itemize{
#' \item wealth each family's net wealth in 1427 (in thousands of lira)
#' \item priorates the number of priorates (seats on the civic council) held
#' between 1282- 1344
#' \item totalties the total number of business or marriage ties in the total
#' dataset of 116 families.
#' }
#'
#' The data are symmetrically coded. This makes sense for marital ties,
#' but is unfortunate for the financial ties (which are almost certainly
#' directed in real life).
#'
#' Substantively, the data include families who were engaged in a struggle
#' for political control of the city of Florence in around 1430.
#' Two factions were dominant in this struggle: one revolved around the
#' infamous Medicis (9), the other around the powerful Strozzis (15).
#' 
#' @name florentine
#' @docType data
#' @keywords internal
#' @format A list containing two \code{igraph} networks and a data.frame with
#' attributes.
#' @source \url{http://networkdata.ics.uci.edu/netdata/html/florentine.html}
NULL




#' Judges network
#'
#' Network of judges from one of the Polish regional courts. Relation indicates
#' which judges have ruled in at least one case together. 
#'
#' Node attributes include gender and code of division.
#'
#' @docType data
#' @name judge_net
#'
#' @format
#' Object of class igraph of size 40, undirected, with predefined layout.
#'
#' @keywords internal
#'
#' @source
#' Own calculation by Michal Bojanowski, based on \href{https://saos-test.icm.edu.pl}{SAOS}.
#' Originally published in the \code{isnar} package at \href{https://github.com/mbojan/isnar}{isnar}.
#'
NULL




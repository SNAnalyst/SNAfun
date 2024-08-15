



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
#' @usage data(florentine, package = "snafun")
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
#' @usage data(judge_net, package = "snafun")
#' @keywords internal
#'
#' @source
#' Own calculation by Michal Bojanowski, based on \href{https://saos-test.icm.edu.pl}{SAOS}.
#' Originally published in the \code{isnar} package at \href{https://github.com/mbojan/isnar}{isnar}.
#'
NULL




#' Madrid Train Bombing 2004 (dataset)
#' @description This is a time series that treat specific attacks as endpoints 
#' and depict the evolution of relations between individuals indirectly and directly 
#' associated with the Madrid train bombing. http://en.wikipedia.org/wiki/2004_Madrid_train_bombings
#' 
#' 1-mode stacked matrices 55 x 55 person by person, data on 20 time periods plus kinship data and tie extinguished data.
#' 
#' Codebook available here http://doitapps.jjay.cuny.edu/jjatt/files/Relations_Codebook_Public_Version2.pdf
#' @source Available from Manchester (https://sites.google.com/site/ucinetsoftware/datasets/covert-networks) 
#' or here http://doitapps.jjay.cuny.edu/jjatt/data.php. Converted to R format 
#' in the \code{networkdata} package from David Schoch.
#' @format list of igraph objects
#' @usage data(Madrid_bombing, package = "snafun")
#' @keywords internal
#' @name Madrid_bombing
#' @docType data
NULL



#' Soccer98 network
#'
#' The network is based on the 32 soccer teams that participated in the 
#' World Championship soccer in Paris, 1998. 
#' Players of the national team often have contracts in other countries. 
#' This constitutes a players market where national teams "export" players to other 
#' countries. Members of the 32 teams had contracts in 36 countries.
#' 
#' The vertices are of the network are the 36 countries that players of these 
#' national teams play in. The senders are the countries that export 
#' players, the receivers are the countries that import players.
#' 
#' The edge weights *i, j* represent the number of players of nationality *i* 
#' that play in country *j* at the time of the World Championship.
#' 
#' There exist countries that are only exporters: Yugoslavia, Chile, Cameroon, Nigeria, ...
#' Some other countries are only importers: Spain, France, Turkey, Greece, GBR...
#'
#' @docType data
#' @name soccer98
#' @usage data(soccer98, package = "snafun")
#' @format
#' Object of class network of size 36, directed, weighted, named.
#'
#' @keywords internal
#'
#' @source
#' Data collected by Lothar Krempel, October 5, 1999.
#' Transformed in Pajek format by V. Batagelj, February 9, 2001.
#' Raw data are available at \href{http://vlado.fmf.uni-lj.si/pub/networks/data/sport/football.htm}{Pajek datasets}.
#'
NULL



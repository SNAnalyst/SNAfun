

#' Plot network slices
#'
#' Plot slices of a \code{networkDynamic} network
#'
#' This function is a variation of \link[ndtv]{filmstrip}.
#' That function has the odd behavior of only plotting edges that occur
#' at specific times, rather than over intervals.
#' The current function plots the edges that occur within time intervals.
#'
#' This function splits the time interval between the starting time
#' (\code{start}) and the end time (\code{end}),
#' into \code{number} equal-sized intervals.
#' The plots include all edges that occur in that interval.
#'
#' By default, the overall interval covers the period from where the first
#' edges starts to where the last one ends.
#'
#' Each interval is left-closed and right-open.
#' This means that it can happen that the very last edge is not included, if
#' it starts and ends at the very last time point.
#' This can be solved by setting the \code{end} time (a little) higher than
#' the actual end time.
#'
#' @param x object of class \code{networkDynamic}
#' @param start start time for the calculation
#' @param end end time for the calculation
#' @param number number of intervals in between \code{start} and \code{end}
#' @param digits number of decimals, used to show the time slices in the plot
#'
#'
#' @importFrom graphics par
#' @return plot
#' @export
plot_network_slices <- function (x, number = 9, start = NULL, end = NULL,
                                 digits = 3) {

  if (!networkDynamic::is.networkDynamic(x)) {
    stop("filmstrip plots require a networkDynamic object as the first argument")
  }

  df <- networkDynamic::as.data.frame.networkDynamic(x)
  if (is.null(start)) {start <- min(c(df$onset))}
  if (is.null(end)) {end <- max(c(df$terminus))}
  times <- seq(from = start, to = end, length.out = number + 1)

  sliced_nets <- lapply(1:(length(times) - 1), function(z) {
    networkDynamic::network.collapse(x, onset = times[z],
                                     terminus = times[z + 1] - .00001)
  })

  opar <- par()$mfrow
  mf <- ceiling(sqrt(number))
  par(mfrow = c(mf, mf))

  coords <- network.layout.animate.kamadakawai(x)

  lapply(1:length(sliced_nets), function(z) {
    xx <- paste0("[", round(times[z], digits = digits), " - ",
                 round(times[z + 1], digits = digits), ")")

    n_edges <- network::network.edgecount(sliced_nets[[z]])
    if (n_edges > 0) {
          sna::gplot(sliced_nets[[z]], gmode = network::is.directed(sliced_nets[[z]]),
                     label = NULL, coord = coords, xlab = xx)
    } else {
        num <- 1
        while (num <= number) {
          nn <- sliced_nets[[num]]
          n_edges <- network::network.edgecount(nn)
          if (n_edges > 0) {
            sna::gplot(nn, gmode = network::is.directed(nn),
                       label = NULL, coord = coords, xlab = xx,
                       edge.lwd	= 0, edge.col = "white")
            num <- number + 1
          }
          num <- num + 1
        }
      }
    })

  on.exit(par(mfrow = opar))
  invisible(NULL)
}





# original code from the ndtv package
# integreated here to reduce package dependencies
network.layout.animate.kamadakawai <- function (net, 
                                                dist.mat = NULL, 
                                                default.dist = NULL, 
                                                seed.coords = NULL, 
                                                layout.par = list(), 
                                                verbose = FALSE) {
  if (is.null(dist.mat)) {
    dist.mat <- layout.distance(net, default.dist = default.dist)
  }
  layout.par$seed.coord <- seed.coords
  layout.par$elen <- dist.mat
  coords <- network::network.layout.kamadakawai(net, layout.par = layout.par)
  return(coords)
}



layout.distance <- function (net, 
                             default.dist = NULL, 
                             weight.attr = NULL, 
                             weight.dist = FALSE) {
  if (is.null(default.dist)) {
    default.dist = sqrt(network::network.size(net))
  }
  else {
    if (!is.numeric(default.dist) | length(default.dist) > 
        1) {
      stop("default.dist must be a numeric value of length 1")
    }
  }
  if (network::network.edgecount(net) < 1) {
    weight.attr = NULL
  }
  raw <- network::as.matrix.network.adjacency(net, attrname = weight.attr, 
                                              expand.bipartite = TRUE)
  if (!is.null(weight.attr) & !weight.dist) {
    matmax <- max(raw)
    matmin <- min(raw[raw > 0])
    raw[] <- vapply(raw, function(x) {
      ifelse(x > 0, (matmax - x) + matmin, 0)
    }, numeric(1))
  }
  if (network::is.directed(net)) {
    if (weight.dist) {
      raw[raw == 0] <- t(raw)[raw == 0]
      raw <- pmin(raw, t(raw))
    }
    else {
      raw <- pmax(raw, t(raw))
    }
  }
  dg <- sna::geodist(raw, inf.replace = default.dist, ignore.eval = is.null(weight.attr))$gdist
  return(dg)
}






#' Count edges in time interval
#'
#' Count number of edges in time interval
#'
#' Counts how many edges happen (ie. have their start within an interval)
#' over the time period {start, end}. The toime period is split into
#' \code{number} intervals, each of equal time length. So, if \code{start = 0},
#' \code{end = 10}, and \code{number = 2}, then the time period is divided
#' into the time intervals from 0 (inclusive) to 5 (exclusive) and from 5
#' (inclusive) to 10 (exclusive).
#'
#' \code{count_edges_in_interval} counts the number of edges that start in
#' each interval.
#'
#' \code{count_unique_edges_in_interval} counts the number of unique edges
#' that start in each interval, so edges that occur multiple times are only
#' counted once.
#'
#' @param x object of class \code{networkDynamic}
#' @param start start time for the calculation
#' @param end end time for the calculation
#' @param number number of intervals in between \code{start} and \code{end}
#'
#' @return vector with counts
#'
#' @export
count_edges_in_interval <- function(x, start = NULL, end = NULL, number = 30) {
  if (!networkDynamic::is.networkDynamic(x)) stop("'x' has to be a 'networkDynamic' object")
  df <- networkDynamic::as.data.frame.networkDynamic(x)
  if (is.null(start)) {start <- min(c(df$onset))}
  if (is.null(end)) {end <- max(c(df$terminus))}
  times <- seq(from = start, to = end, length.out = number + 1)
  n_edges <- sapply(times, function(z) {sum(df$onset >= z, na.rm = TRUE)})
  count <- n_edges[1:(length(n_edges) - 1)] - n_edges[2:length(n_edges)]
  count <- stats::as.ts(count)
  count
}





#' Count unique edges in time interval
#'
#' Count number of unique edges in time interval
#'
#' Counts how many edges happen (ie. have their start within an interval)
#' over the time period {start, end}. The toime period is split into
#' \code{number} intervals, each of equal time length. So, if \code{start = 0},
#' \code{end = 10}, and \code{number = 2}, then the time period is divided
#' into the time intervals from 0 (inclusive) to 5 (exclusive) and from 5
#' (inclusive) to 10 (exclusive).
#'
#' \code{count_edges_in_interval} counts the number of edges that start in
#' each interval.
#'
#' \code{count_unique_edges_in_interval} counts the number of unique edges
#' that start in each interval, so edges that occur multiple times are only
#' counted once.
#'
#' @param x object of class \code{networkDynamic}
#' @param start start time for the calculation
#' @param end end time for the calculation
#' @param number number of intervals in between \code{start} and \code{end}
#' @param directed logical, should the graph be treated as directed? Defaults
#' to the value of \code{directed} encoded inside \code{x} itself.
#'
#' @return vector with counts
#'
#' @export
count_unique_edges_in_interval <- function(x, start = NULL, end = NULL, number = 30,
                                           directed = network::is.directed(x)) {
  if (!networkDynamic::is.networkDynamic(x)) stop("'x' has to be a 'networkDynamic' object")
  df <- networkDynamic::as.data.frame.networkDynamic(x)
  if (is.null(start)) {start <- min(c(df$onset))}
  if (is.null(end)) {end <- max(c(df$terminus))}
  times <- seq(from = start, to = end, length.out = number + 1)
  count <- sapply(1:length(times), function(z) {
    df_sub <- df[df$onset >= times[z] & df$onset < times[z + 1], c("head", "tail")]
    if (directed) {
      not_unique <- duplicated(t(apply(df_sub, 1, sort)))
      df_sub <- df_sub[-not_unique, ]
    } else {  # undirected network
      df_sub <- unique(df_sub)
    }
    return(nrow(df_sub))
  })
  count <- stats::as.ts(count)
  count
}




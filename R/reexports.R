

# snafun provides S3 methods for four generics it does not own: plot, print,
# summary, and as.data.frame. The generics are documented and exported here (and
# in plot.R and print.R) so that users never have to attach the package: a
# qualified snafun::summary(x) has to work just as well as a bare summary(x).
#
# The mechanism is subtle enough to be worth stating once, in full.
#
# An @export tag on a documentation-only block exports the *name* without
# creating an object of that name inside the snafun namespace. R then resolves
# the export through the namespace's parent chain (imports:snafun ->
# namespace:base), so snafun::summary is literally base::summary.
#
# The absence of a local object is not incidental, it is load-bearing.
# loadNamespace() treats a generic as *local* as soon as an object of that name
# exists in the package namespace, and then registers the package's methods in
# snafun's own S3 method table rather than in the one belonging to base. Define
# `summary <- base::summary` here and a bare summary(<stat_cug>) would silently
# stop finding summary.stat_cug and fall through to summary.default.
#
# roxygen2 warns that these names are "listed as exports, but not present in
# namespace". That warning is a false alarm. Removing the tags to silence it is
# what broke snafun::plot() and snafun::print() between 2026-04 and 2026-07.
#
# inst/tinytest/test_generic_reexports.R guards both halves of this.


#' Summarize a snafun object
#'
#' Summarize an object produced by \code{snafun}.
#'
#' The \code{summary} generic is re-exported from \code{base} so that
#' \code{snafun::summary(x)} works without \code{snafun} ever being attached.
#' It is the same object as \code{base::summary}; the summary methods that
#' \code{snafun} provides are documented with the functions that create the
#' objects they summarize, such as \link{stat_cug} and \link{stat_qap_cor}.
#'
#' @param object an object summarized by one of \code{snafun}'s
#' \code{summary} methods
#' @param ... Additional arguments, passed on to the method.
#'
#' @return a summary object, whose class depends on the class of \code{object}
#' @name summary
#' @export
#' @examples
#' \dontrun{
#' cug <- snafun::stat_cug(snafun::create_random_graph(10, "gnm", m = 20),
#'                         FUN = snafun::g_transitivity, cmode = "edges")
#' snafun::summary(cug)
#' }
NULL


#' Coerce a snafun object to a data frame
#'
#' Coerce an object produced by \code{snafun} to a \code{data.frame}.
#'
#' The \code{as.data.frame} generic is re-exported from \code{base} so that
#' \code{snafun::as.data.frame(x)} works without \code{snafun} ever being
#' attached. It is the same object as \code{base::as.data.frame}; the method
#' that \code{snafun} provides is documented with \link{stat_cug}.
#'
#' @param x an object coerced by one of \code{snafun}'s \code{as.data.frame}
#' methods
#' @param row.names \code{NULL} or a character vector of row names.
#' @param optional logical; if \code{TRUE}, setting row and column names is
#' optional.
#' @param ... Additional arguments, passed on to the method.
#'
#' @return a \code{data.frame}
#' @name as.data.frame
#' @export
#' @examples
#' \dontrun{
#' cug <- snafun::stat_cug(snafun::create_random_graph(10, "gnm", m = 20),
#'                         FUN = snafun::g_transitivity, cmode = "edges")
#' snafun::as.data.frame(cug)
#' }
NULL

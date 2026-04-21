#' Browse installed vignettes
#'
#' Open installed vignettes from a simple chooser.
#'
#' By default, this function shows a small chooser for the installed vignettes
#' of a package and opens the selected vignette in the browser. On Windows,
#' \code{utils::menu(..., graphics = TRUE)} usually gives a clickable dialog.
#' On other platforms, or when a graphical menu is unavailable, R falls back to
#' its usual menu interface.
#'
#' The default \code{package = "snafun"} makes it easy for users to discover
#' the package vignettes without first typing \code{utils::vignette(package =
#' "snafun")}. When the package was installed without vignettes, the function
#' stops with a more informative message explaining that a reinstall with
#' \code{build_vignettes = TRUE} is needed.
#'
#' @param package Character scalar, package whose installed vignettes should be
#' browsed. Defaults to \code{"snafun"}.
#' @param open Logical scalar, should a vignette be opened? Set to
#' \code{FALSE} if you only want the vignette index returned invisibly.
#' @param interface Character scalar, either \code{"menu"} for a chooser or
#' \code{"index"} for the base R vignette index page.
#' @param lib.loc Optional library path to search. This is mainly useful for
#' advanced use cases and testing.
#' @param selection Optional preselected vignette. This is mainly useful for
#' programming and testing. It can be a row number, an \code{Item}, or a
#' \code{Title} from the vignette index.
#'
#' @return Invisibly returns a data frame with the installed vignette index.
#' @export
#'
#' @examples
#' \dontrun{
#' browse_vignettes()
#' browse_vignettes(interface = "index")
#' browse_vignettes(package = "stats")
#' }
browse_vignettes <- function(package = "snafun",
                             open = TRUE,
                             interface = c("menu", "index"),
                             lib.loc = NULL,
                             selection = NULL) {
  interface <- snafun.match.arg(interface)
  vignettes <- installed_package_vignettes(package = package, lib.loc = lib.loc)
  if (nrow(vignettes) == 0) {
    stop(
      paste0(
        "No installed vignettes found for package '", package, "'. ",
        "If you installed it with remotes, reinstall with ",
        "'build_vignettes = TRUE'."
      ),
      call. = FALSE
    )
  }
  
  if (isTRUE(open)) {
    if (interface == "index") {
      utils::browseVignettes(package = package, lib.loc = lib.loc)
    } else {
      selected_row <- select_installed_vignette(
        vignettes = vignettes,
        selection = selection
      )
      if (!is.na(selected_row)) {
        open_installed_vignette(
          vignette_row = vignettes[selected_row, , drop = FALSE],
          package = package,
          lib.loc = lib.loc
        )
      }
    }
  }
  
  invisible(vignettes)
}


#' Collect the installed vignette index for a package
#'
#' @param package Character scalar naming the package
#' @param lib.loc Optional library path
#'
#' @return Data frame with columns \code{Package}, \code{LibPath},
#' \code{Item}, and \code{Title}.
#' @keywords internal
#' @noRd
installed_package_vignettes <- function(package, lib.loc = NULL) {
  results <- tryCatch(
    utils::vignette(package = package, lib.loc = lib.loc)[["results"]],
    error = function(e) NULL
  )
  if (is.null(results) || length(results) == 0) {
    return(data.frame(
      Package = character(0),
      LibPath = character(0),
      Item = character(0),
      Title = character(0),
      stringsAsFactors = FALSE
    ))
  }
  
  data.frame(results, check.names = FALSE, stringsAsFactors = FALSE)
}


#' Resolve a vignette selection
#'
#' @param vignettes data frame returned by \code{installed_package_vignettes()}
#' @param selection Optional user or programmatic selection
#'
#' @return Integer row number or \code{NA_integer_} if nothing was selected.
#' @keywords internal
#' @noRd
select_installed_vignette <- function(vignettes, selection = NULL) {
  if (!is.null(selection)) {
    if (is.numeric(selection) && length(selection) == 1 && !is.na(selection)) {
      selection <- as.integer(selection)
      if (selection >= 1 && selection <= nrow(vignettes)) {
        return(selection)
      }
      stop("'selection' is outside the vignette index.", call. = FALSE)
    }
    
    if (is.character(selection) && length(selection) == 1) {
      item_match <- which(vignettes$Item == selection)
      if (length(item_match) == 1) {
        return(item_match)
      }
      title_match <- which(vignettes$Title == selection)
      if (length(title_match) == 1) {
        return(title_match)
      }
      stop("'selection' does not match a vignette Item or Title.", call. = FALSE)
    }
    
    stop("'selection' should be NULL, a row number, or a vignette name.", call. = FALSE)
  }
  
  choice_labels <- sprintf("%s: %s", vignettes$Item, vignettes$Title)
  choice <- utils::menu(
    choices = choice_labels,
    graphics = identical(.Platform$OS.type, "windows"),
    title = paste0("Choose a vignette from package '", vignettes$Package[[1]], "'")
  )
  
  if (identical(choice, 0L)) {
    return(NA_integer_)
  }
  as.integer(choice)
}


#' Open one installed vignette
#'
#' @param vignette_row one-row data frame from the installed vignette index
#' @param package package name
#' @param lib.loc optional library path
#'
#' @param browse Logical scalar, should the html path be opened in a browser?
#'
#' @return Invisibly returns the path to the vignette html file.
#' @keywords internal
#' @noRd
open_installed_vignette <- function(vignette_row, package, lib.loc = NULL, browse = TRUE) {
  vignette_object <- utils::vignette(
    topic = vignette_row$Item[[1]],
    package = package,
    lib.loc = lib.loc
  )
  html_path <- file.path(vignette_object$Dir, "doc", vignette_object$PDF)
  if (isTRUE(browse)) {
    utils::browseURL(html_path)
  }
  invisible(html_path)
}

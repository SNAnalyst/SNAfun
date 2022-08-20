
#' Open a SNA4DS cheatsheet in the browser
#'
#' Open a SNA4DS cheatsheer in your browser
#'
#' Shows a list of currently available online vignettes for the 
#' \code{SNA4DS} course.
#' 
#' The user can pick the preferred vignette by entering the number that corresponds
#' to the preferred vignette. The vignette will then open in the user's default
#' web browser.
#'
#' When \code{graphics} is \code{TRUE}, a graphical choice menu is shown. If that
#' is not preferred, or if the user's machine lacks the graphical tools needed,
#' setting \code{graphics} to \code{FALSE} will show the list of vignettes in the
#' R console.
#'
#' @param graphics logical, should the list of options be shown as a clickable
#' graphical menu?
#'
#' @return nothing
#' @export
#' @examples
#' \dontrun{
#' open_SNA4DS_cheatsheet()
#' }
open_SNA4DS_cheatsheet <- function(graphics = TRUE) {
  paths <- find.package("snafun", lib.loc = NULL, quiet = TRUE)
  
  if (dir.exists(file.path(paths, "doc"))) {
    paths <- file.path(paths, "doc")
  } else if (dir.exists(file.path(paths, "inst", "doc"))) {
    paths <- file.path(paths, "inst", "doc")
  } else {
    stop("The folder containing the vignettes does not exist")
  }
  
  INDEX <- suppressWarnings(
    utils::read.table(
    file = file.path(paths, "INDEX"), 
    header = TRUE, sep = ",", strip.white = TRUE)
  )
  
  INDEX$DOC <- file.path(paths, INDEX$DOC)
  
  perform <- paste0("browseURL('", INDEX$DOC, "')")
  cat("\n\nPlease pick which vignette you want to open, it will show in your default browser.\n")
  cat("The following vignettes are currently available to pick from:\n")
  pick <- utils::menu(INDEX$TITLE, graphics = graphics, title = "Select your favorite vignette")
  eval(parse(text = perform[pick], keep.source = FALSE), envir = .GlobalEnv)
}

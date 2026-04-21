

.onLoad <- function(libname, pkgname) {
  op <- options()
  invisible()
}


.onAttach <- function(lib, pkg,...){
  print_message <-  
    paste("\n",
        "Welcome to snafun version ", utils::packageDescription("snafun")$Version, "\n\n",
        "This package enhances the FUN in the SNA4DS course by providing you\n",
        "with a consistent API to the main SNA packages and by giving you access\n",
        "to a bunch of new cool and extraordinarily useful functions that will\n",
        "make you life as a network data analyst a lot easier and--you guessed it--\n",
        "more fun!\n\n",
        "If you experience any issues or have a request for new or improved\n",
        "functions, please contact Roger and Claudia or add an issue to\n",
        "the package github repo at https://github.com/SNAnalyst/SNAfun/issues.\n\n\n",
        "Type help(package = 'snafun') to access the package documentation.\n\n",
        "To suppress this message use:\n",
        "\tsuppressPackageStartupMessages(library(snafun))\n\n",
        sep = "")
  packageStartupMessage(print_message)
}

# Optional dependencies such as 'ergm' and 'btergm' should not prevent
# 'snafun' from loading. For that reason we never resolve them at file scope.
# Instead, the functions that need them ask for the namespace only when they
# are actually called.

#' Safely retrieve an object from an optional namespace
#'
#' This helper wraps \code{utils::getFromNamespace()} in a way that keeps
#' optional dependencies optional at package load time. If the requested
#' package is not installed, the function returns \code{NULL} instead of
#' triggering an error while \code{snafun} is loading.
#'
#' The intended use is to call this helper inside functions that rely on
#' packages listed in \code{Suggests}. That way, users can still load and use
#' the rest of \code{snafun} on systems where those optional packages are not
#' available.
#'
#' @param name Character scalar. Name of the object to retrieve from the
#' namespace.
#' @param package Character scalar. Name of the package namespace.
#'
#' @return The requested object if the namespace is available; otherwise
#' \code{NULL}.
#'
#' @keywords internal
get_optional_namespace_object <- function(name, package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    return(NULL)
  }
  utils::getFromNamespace(name, package)
}


#' Retrieve an object from an optional namespace or fail with a clear message
#'
#' This helper is the strict companion of
#' \code{get_optional_namespace_object()}. It is meant for code
#' paths where an optional package becomes mandatory because the user calls a
#' function that genuinely depends on it.
#'
#' We keep this logic in one place so the error messages stay consistent across
#' the package and so the dependency remains lazy: the check happens only when
#' the relevant functionality is requested.
#'
#' @param name Character scalar. Name of the object to retrieve from the
#' namespace.
#' @param package Character scalar. Name of the package namespace.
#' @param caller Character scalar. Name of the calling function, used to build
#' a targeted error message.
#'
#' @return The requested object.
#'
#' @keywords internal
require_optional_namespace_object <- function(name, package, caller) {
  object <- get_optional_namespace_object(name = name, package = package)
  if (is.null(object)) {
    stop(
      paste0(
        "Package '", package, "' is required for '", caller,
        "()'. Please install '", package, "' first."
      ),
      call. = FALSE
    )
  }
  object
}

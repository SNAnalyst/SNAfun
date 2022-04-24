

.onLoad <- function(libname, pkgname) {
  op <- options()
  invisible()
}


.onAttach <- function(lib, pkg,...){
  print_message <-  paste("\n",
                          "Welcome to SNAfun version ", utils::packageDescription("SNAfun")$Version,
                          "\n",
                          "Type ?SNAfun to access the package documentation\n\n",
                          "To suppress this message use:\n",
                          "\tsuppressPackageStartupMessages(library(SNAfun))\n\n",
                          sep = "")
  packageStartupMessage(print_message)
}



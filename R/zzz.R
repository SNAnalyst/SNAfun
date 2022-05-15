

.onLoad <- function(libname, pkgname) {
  op <- options()
  invisible()
}


.onAttach <- function(lib, pkg,...){
  print_message <-  paste("\n",
                          "Welcome to snafun version ", utils::packageDescription("snafun")$Version,
                          "\n",
                          "Type help(package = 'snafun') to access the package documentation\n\n",
                          "To suppress this message use:\n",
                          "\tsuppressPackageStartupMessages(library(snafun))\n\n",
                          sep = "")
  packageStartupMessage(print_message)
}




gof_btergm <- utils::getFromNamespace("gof.btergm", "btergm")
plot_gof_btergm <- utils::getFromNamespace("plot.gof", "btergm")
plot_gof_ergm <- utils::getFromNamespace("plot.gof", "ergm")


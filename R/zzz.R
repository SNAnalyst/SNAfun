

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




gof_btergm <- utils::getFromNamespace("gof.btergm", "btergm")
plot_gof_btergm <- utils::getFromNamespace("plot.gof", "btergm")
plot_gof_ergm <- utils::getFromNamespace("plot.gof", "ergm")


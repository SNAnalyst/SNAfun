
#' Save internal data
#'
#' Save and add to internal data
#'
#' Saves internal data. There are arguments whether or not existing objects
#' can be safely overwritten and whether the data object can be overwritten.
#'
#' NOTE: this is experimental, warranty until the doorstep at best.
#'
#' @param ... the names of the objects to be saved (as symbols or character strings).
#' @param overwrite_vars logical, is it OK to replace/overwrite existing objects
#' with new values?
#' @param overwrite By default the function will not overwrite existing files.
#' If you really want to do so, set this to \code{TRUE.}
#' @param compress Choose the type of compression used by save(). Should be one
#' of "gzip", "bzip2", or "xz".
#' @param version The serialization format version to use. The default, 2, was
#' the default format from R 1.4.0 to 3.5.3. Version 3 became the default from
#' R 3.6.0 and can only be read by R versions 3.5.0 and higher.
#'
#' @return nothing
#' @keywords internal
save_internal_data <- function(..., overwrite_vars = FALSE, overwrite = FALSE,
                                compress = "bzip2", version = 2) {
  check_is_package("use_data()")
  objs <- get_objs_from_dots(dots(...))
  use_dependency("R", "depends", "2.10")

  usethis::use_directory("R")
  paths <- file.path("R", "sysdata.rda")
  # paths <- fs::path("R", "sysdata.rda")
  objs <- list(objs)

  check_files_absent(usethis::proj_path(paths), overwrite = overwrite)

  usethis::ui_done("Saving {usethis::ui_value(unlist(objs))} to {usethis::ui_value(paths)}")

  envir <- parent.frame()
  temp_env <- new.env()

  # copy the original objects from the sysdata file to this new environment
  load(usethis::proj_path(paths), envir = temp_env)
  # copy new objects to this new environment
  overwritten <- NULL
  lapply(objs[[1]], function(z) {
    bestaat_al_in_oude_file <- z %in% ls(envir = temp_env)

    if (bestaat_al_in_oude_file) {
      overwritten <<- c(overwritten, z)
    }

    if (bestaat_al_in_oude_file & !overwrite_vars) {
      msg <- paste0("An object called ", z, "already exists in sysdata.rda, please set overwrite = TRUE")
      stop(msg)
    } else {
      assign(z, get(z, envir = envir), envir = temp_env)
    }
  })

  save(list = ls(envir = temp_env), file = usethis::proj_path(paths),
       envir = temp_env, compress = compress, version = version)

    if (length(overwritten) > 0) {
    message("The following object have been overwritten: ", paste(overwritten, collapse = ", "), ".\n")
  }
  invisible()
}





#' @keywords internal
check_is_package <- function(whos_asking = NULL) {
  if (is_package()) {
    return(invisible())
  }
  message <- "Project {ui_value(project_name())} is not an R package."
  if (!is.null(whos_asking)) {
    message <- c("{usethis::ui_code(whos_asking)} is designed to work with packages.",
                 message)
  }
  usethis::ui_stop(message)
}


#' @keywords internal
get_objs_from_dots <- function(.dots) {
  if (length(.dots) == 0L) {
    usethis::ui_stop("Nothing to save.")
  }
  is_name <- vapply(.dots, is.symbol, logical(1))
  if (any(!is_name)) {
    usethis::ui_stop("Can only save existing named objects.")
  }
  objs <- vapply(.dots, as.character, character(1))
  duplicated_objs <- which(stats::setNames(duplicated(objs),
                                           objs))
  if (length(duplicated_objs) > 0L) {
    objs <- unique(objs)
    usethis::ui_warn("Saving duplicates only once: {ui_value(names(duplicated_objs))}")
  }
  objs
}


#' @keywords internal
use_dependency <- function(package, type, min_version = NULL) {
  # stopifnot(rlang::is_string(package))
  # stopifnot(rlang::is_string(type))
  stopifnot(is.character(package))
  stopifnot(is.character(type))
  
  # if (package != "R" && !rlang::is_installed(package)) {
  if (package != "R" && !check_if_installed(package)) {
    usethis::ui_stop(c("{usethis::ui_value(package)} must be installed before you can ",
              "take a dependency on it."))
  }
  if (isTRUE(min_version)) {
    min_version <- utils::packageVersion(package)
  }
  version <- if (is.null(min_version))
    "*"
  else paste0(">= ", min_version)
  types <- c("Depends", "Imports", "Suggests",
             "Enhances", "LinkingTo")
  names(types) <- tolower(types)
  type <- types[[match.arg(tolower(type), names(types))]]
  deps <- desc::desc_get_deps(usethis::proj_get())
  existing_dep <- deps$package == package
  existing_type <- deps$type[existing_dep]
  existing_ver <- deps$version[existing_dep]
  is_linking_to <- (existing_type != "LinkingTo" & type ==
                      "LinkingTo") | (existing_type == "LinkingTo" &
                                        type != "LinkingTo")
  if (!any(existing_dep) || any(is_linking_to)) {
    usethis::ui_done("Adding {usethis::ui_value(package)} to {usethis::ui_field(type)} field in DESCRIPTION")
    desc::desc_set_dep(package, type, version = version,
                       file = usethis::proj_get())
    return(invisible())
  }
  existing_type <- setdiff(existing_type, "LinkingTo")
  delta <- sign(match(existing_type, types) - match(type, types))
  if (delta < 0) {
    usethis::ui_warn("Package {usethis::ui_value(package)} is already listed
                     in \\\n      {usethis::ui_value(existing_type)} in DESCRIPTION, no change made.")
  }
  else if (delta == 0 && !is.null(min_version)) {
    upgrade <- existing_ver == "*" || numeric_version(min_version) >
      version_spec(existing_ver)
    if (upgrade) {
      usethis::ui_done("Increasing {ui_value(package)} version to {ui_value(version)} in DESCRIPTION")
      desc::desc_set_dep(package, type, version = version,
                         file = usethis::proj_get())
    }
  }
  else if (delta > 0) {
    if (existing_type != "LinkingTo") {
      usethis::ui_done("\n        Moving {usethis::ui_value(package)}
                       from {usethis::ui_field(existing_type)} to {usethis::ui_field(type)}
                       \\\n        field in DESCRIPTION\n        ")
      desc::desc_del_dep(package, existing_type, file = usethis::proj_get())
      desc::desc_set_dep(package, type, version = version,
                         file = usethis::proj_get())
    }
  }
  invisible()
}

#' @keywords internal
check_files_absent <- function(paths, overwrite) {
  if (overwrite) {
    return()
  }
  # ok <- !fs::file_exists(paths)
  ok <- !file.exists(paths)
  if (all(ok)) {
    return()
  }
  usethis::ui_stop("\n    {usethis::ui_path(paths[!ok])} already exist.,\n
                   Use {usethis::ui_code('overwrite = TRUE')} to overwrite.\n    ")
}




#' @keywords internal
dots <- function(...) {
  eval(substitute(alist(...)))
}

#' @keywords internal
version_spec <- function(x) {
  x <- gsub("(<=|<|>=|>|==)\\s*", "", x)
  numeric_version(x)
}


#' @keywords internal
is_package <- function(base_path = usethis::proj_get()) {
  # res <- tryCatch(rprojroot::find_package_root_file(path = base_path),
  #                 error = function(e) NULL)
  res <- tryCatch(find_package_root_file(path = base_path),
                  error = function(e) NULL)
  !is.null(res)
}




###############################################################################
# very short replacement for rlang::is_installed
check_if_installed <- function (package) {
  is_installed <- sapply(package, requireNamespace, quietly = TRUE)
  invisible(is_installed)
}



###### replacement for rprojroot function
is_absolute_path <- function (x) {
  grepl("^[/\\\\~]|^[a-zA-Z]:[/\\\\]", x)
}


match_contents <- function (f, contents, n, fixed) {
  if (is.null(contents)) {
    return(TRUE)
  }
  fc <- readLines(f, n)
  any(grepl(contents, fc, fixed = fixed))
}







find_root_file <- function (..., criterion, path = ".") {
  if (!missing(..1)) {
    abs <- is_absolute_path(..1)
    if (all(abs)) {
      return(path(...))
    }
    if (any(abs)) {
      stop("Combination of absolute and relative paths not supported.", 
           call. = FALSE)
    }
  }
  root <- find_root(criterion = criterion, path = path)
  path(root, ...)
}



is_root <- function (path) {
  identical(normalizePath(path, winslash = "/"), normalizePath(dirname(path), 
                                                               winslash = "/"))
}


find_root <- function (criterion, path = ".") {
  # KLASSE criterion LIJKT "root_criterion", dan kan de regel weg
  # criterion <- as_root_criterion(criterion)  # KLASSE LIJKT "root_criterion", 
  start_path <- get_start_path(path, criterion$subdir)
  path <- start_path
  # eigen toevoeging, lijkt overeen te komen met wat rpropjroot gebruikt
  .MAX_DEPTH <- 100
  
  for (i in seq_len(.MAX_DEPTH)) {
    for (f in criterion$testfun) {
      if (f(path)) {
        return(path)
      }
    }
    if (is_root(path)) {
      stop("No root directory found in ", start_path, 
           " or its parent directories. ", paste(format(criterion), 
                                                 collapse = "\n"), call. = FALSE)
    }
    path <- dirname(path)
  }
  stop("Maximum search of ", .MAX_DEPTH, " exceeded. Last path: ", 
       path, call. = FALSE)
}



get_start_path <- function (path, subdirs) {
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  for (subdir in subdirs) {
    subdir_path <- file.path(path, subdir)
    if (dir.exists(subdir_path)) {
      return(subdir_path)
    }
  }
  path
}






check_relative <- function (filepath) {
  if (is_absolute_path(filepath)) {
    stop("filepath must be a file or a relative path, not an absolute path.", 
         call. = FALSE)
  }
}















format_lines <- function (n) {
  if (n == 1) 
    "line"
  else paste0(n, " lines")
}





make_find_root_file <- function (criterion) {
  force(criterion)
  eval(bquote(function(..., path = ".") {
    find_root_file(..., criterion = criterion, path = path)
  }))
}



make_fix_root_file <- function (criterion, path, subdir = NULL) {
  root <- find_root(criterion = criterion, path = path)
  if (!is.null(subdir)) {
    root <- file.path(root, subdir)
  }
  eval(bquote(function(...) {
    if (!missing(..1)) {
      abs <- is_absolute_path(..1)
      if (all(abs)) {
        return(path(...))
      }
      if (any(abs)) {
        stop("Combination of absolute and relative paths not supported.", 
             call. = FALSE)
      }
    }
    path(.(root), ...)
  }))
}








path <- function (...) {
  dots <- list(...)
  if (!is.null(names(dots)) && any(names(dots) != "")) {
    warning("Arguments must be unnamed", call. = FALSE)
  }
  lengths <- lengths(dots)
  if (any(lengths == 0) && all(lengths %in% 0:1)) {
    return(character())
  }
  component_df <- as.data.frame(dots, stringsAsFactors = FALSE)
  missing <- apply(is.na(component_df), 1, any)
  components <-
    lapply(component_df, function(x)
      enc2utf8(as.character(x)))
  out <- do.call(file.path, components)
  out[missing] <- NA_character_
  Encoding(out) <- "UTF-8"
  out
}








check_relative <- function (filepath) {
  if (is_absolute_path(filepath)) {
    stop("filepath must be a file or a relative path, not an absolute path.", 
         call. = FALSE)
  }
}


check_testfun <- function (testfun) {
  if (is.function(testfun)) {
    testfun <- list(testfun)
  }
  for (f in testfun) {
    if (!isTRUE(all.equal(names(formals(f)), "path"))) {
      stop("All functions in testfun must have exactly one argument 'path'", 
           call. = FALSE)
    }
  }
  testfun
}



root_criterion <- function (testfun, desc, subdir = NULL) {
  testfun <- check_testfun(testfun)
  stopifnot(length(desc) == length(testfun))
  full_desc <- paste0(desc, if (!is.null(subdir)) {
    paste0(" (also look in subdirectories: ", 
           paste0("`", subdir, "`", collapse = ", "), ")")
  })
  criterion <- structure(list(testfun = testfun, desc = full_desc, 
                              subdir = subdir), class = "root_criterion")
  criterion$find_file <- make_find_root_file(criterion)
  criterion$make_fix_file <- function(path = getwd(), subdir = NULL) {
    make_fix_root_file(criterion, path, subdir)
  }
  criterion
}





has_file <- function (filepath, contents = NULL, n = -1L, fixed = FALSE) {
  force(filepath)
  stopifnot(is.character(filepath), length(filepath) == 1)
  force(contents)
  if (!is.null(contents)) {
    stopifnot(is.character(contents), length(contents) == 1)
  }
  force(n)
  stopifnot(length(n) == 1)
  check_relative(filepath)
  testfun <- eval(bquote(function(path) {
    testfile <- file.path(path, .(filepath))
    if (!file.exists(testfile)) {
      return(FALSE)
    }
    if (dir.exists(testfile)) {
      return(FALSE)
    }
    match_contents(testfile, .(contents), .(n), .(fixed))
  }))
  desc <- paste0("contains a file \"", filepath, "\"", 
                 if (!is.null(contents)) {
                   paste0(" with contents ", if (!fixed) 
                     "matching ", "\"", contents, "\"", 
                     if (n >= 0L) 
                       paste0(" in the first ", format_lines(n)))
                 })
  root_criterion(testfun, desc)
}



# is_r_package <- has_file("DESCRIPTION", contents = "^Package: ")




find_package_root_file <- function (..., path = ".") {
  # eigen toevoeging, dit lijkt me de correcte definitie van het criterion
  criterion <- has_file("DESCRIPTION", contents = "^Package: ")
  find_root_file(..., criterion = criterion, path = path)
}










which_methods <- function(name) {
  methods <- format(utils::.S3methods(name, envir = getNamespace("snafun")))
  methods <- sub(paste0(name, "."), "", methods)
  methods <- sub("\\*", "", methods)
  if ("default" %in% methods) {
    methods <- methods[-which(methods == "default")]
  }
  methods
}


methods_error_message <- function(what, method) {
  implemented <- paste0("`", which_methods(method), "`") |> 
    paste0(collapse = ", ")
  txt <- paste0("'", what, "' should be of class ", implemented)
}


# return a list with two elements: 
# 'value' has the result from the evaluation of the expression
# 'warnings' holds any warnings that resulted from the expression
# 
# the warnings are not returned to the console



#' Catch output and warnings
#' 
#' Catch the output and any warnings from evaluating an expression
#' 
#' Some expressions generate a warning, besides some output. 
#' In some cases, it is profitable to not print the warning to the console, 
#' but still capture the warning. You can not use \code{\link{suppressWarnings}},
#' because that suppresses the warning altogether and it can't be checked 
#' programatically whether a warning was triggered (and which warning).
#' 
#' This little function evaluates the expression and stores the result in 
#' a list with two entries: 
#' 
#' \describe{
#' \item{value}{the object that is returned by 
#' evaluating the expression}
#' \item{warnings}{a list of lists that contains 
#' the warnings; each warning is a separate list within this overall list. 
#' \code{warnings} is NULL is no warnings were generated.}
#' }
#' 
#' No warning is printed to the console, but errors and messages are.
#' 
#' This function is intended for internal use and is not openly exposed to 
#' the user (but it is exported for potential use elsewhere).
#'
#' @param expr a valied R expression
#'
#' @return list, see above for the contents
#' @export
#' @keywords internal
#'
#' @examples
#' uit <- withWarnings(sum(c(1:3)))
#' uit$value   # 6
#' class(uit$value)  # integer
#' class(uit$warnings)  # NULL
#' is.null(uit$warnings)   # TRUE
#' 
#' testit <- function(x) {
#'     warning("testit_warning")
#'     sum(x)
#' }
#' uit <- withWarnings(testit(1:3))
#' uit$value   # 6
#' is.list(uit$warnings)  # TRUE
#' uit$warnings[[1]]   # <simpleWarning in testit(1:3): testit_warning>
#' inherits(uit$warnings[[1]], "simpleWarning")  # TRUE
#' # check for this specific warning
#' grepl("testit_warning", uit$warnings[[1]])  # TRUE
withWarnings <- function(expr) {
  myWarnings <- NULL
  wHandler <- function(w) {
    myWarnings <<- c(myWarnings, list(w))
    invokeRestart("muffleWarning")
  }
  val <- withCallingHandlers(expr, warning = wHandler)
  list(value = val, warnings = myWarnings)
}


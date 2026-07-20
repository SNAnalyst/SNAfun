#!/usr/bin/env Rscript

# Audit which package functions are exported, and flag anything that looks like a
# forgotten export. Written after snafun::plot() silently lost its export in
# April 2026 (an @export tag was removed, roxygenise() dropped it from NAMESPACE,
# and nothing noticed for three months). Run this before a release:
#
#   Rscript tools/audit-exports.R
#
# It is dev-only tooling and is .Rbuildignore'd, so it never ships or runs during
# R CMD check. It has no dependencies beyond base R; the optional roxygen drift
# check is skipped when roxygen2 is not installed.
#
# Exit status is 1 when it finds a hard problem (an @export tag that did not reach
# NAMESPACE, or a documented non-internal function that is not exported), so it
# can gate a release in a script. The prefix-based review list is advisory only.

# --- locate the package root (walk up until DESCRIPTION is found) ------------
find_pkg_root <- function(start) {
  d <- normalizePath(start, winslash = "/", mustWork = FALSE)
  repeat {
    if (file.exists(file.path(d, "DESCRIPTION")) &&
        dir.exists(file.path(d, "R"))) return(d)
    parent <- dirname(d)
    if (identical(parent, d)) stop("Could not find the package root (no DESCRIPTION).")
    d <- parent
  }
}

# script may be run from anywhere; find its own location, then the root above it
this_file <- local({
  a <- commandArgs(FALSE)
  f <- sub("^--file=", "", a[grep("^--file=", a)])
  if (length(f)) f[[1]] else getwd()
})
root <- find_pkg_root(dirname(this_file))

# public-API name prefixes, from README.md
prefixes <- c("add_", "count_", "create_", "extract_", "is_", "has_", "list_",
              "make_", "plot_", "plot", "print", "summary", "remove_", "stat_",
              "to_", "e_", "g_", "v_", "d_")
has_prefix <- function(nm) any(vapply(prefixes, function(p) startsWith(nm, p), logical(1)))

# --- read NAMESPACE ----------------------------------------------------------
ns <- readLines(file.path(root, "NAMESPACE"), warn = FALSE)
exports <- gsub('"', '', sub("^export\\((.*)\\)$", "\\1", grep("^export\\(", ns, value = TRUE)))
s3 <- regmatches(ns, regexec("^S3method\\(([^,]+),(.+)\\)$", ns))
s3generics <- unique(vapply(s3[lengths(s3) > 0], function(m) m[2], character(1)))
is_s3_method <- function(nm) any(startsWith(nm, paste0(s3generics, ".")))

# --- parse every top-level function definition + its roxygen block -----------
rfiles <- list.files(file.path(root, "R"), pattern = "[.]R$", full.names = TRUE)
rows <- list()
for (f in rfiles) {
  lines <- readLines(f, warn = FALSE)
  for (h in grep("^([A-Za-z._][A-Za-z0-9._]*)\\s*(<-|=)\\s*function", lines)) {
    name <- sub("^([A-Za-z0-9._]+)\\s*(<-|=).*$", "\\1", lines[h])
    j <- h - 1L; block <- character()
    while (j >= 1 && grepl("^#'", lines[j])) { block <- c(lines[j], block); j <- j - 1L }
    tag <- paste(block, collapse = "\n")
    rows[[length(rows) + 1L]] <- data.frame(
      name = name, file = basename(f),
      has_export = grepl("@export", tag),
      internal   = grepl("@noRd|@keywords internal", tag),
      # "documented" = a real doc block (has a title/@param/@return), not just @noRd
      documented = length(block) > 0 && grepl("@param|@return|@rdname|@describeIn", tag),
      stringsAsFactors = FALSE)
  }
}
d <- do.call(rbind, rows)
d$exported <- d$name %in% exports
d$s3method <- vapply(d$name, is_s3_method, logical(1))
d$hook     <- grepl("^\\.on", d$name)
d$prefix   <- vapply(d$name, has_prefix, logical(1))

problems <- 0L

# --- 1. HARD: @export in source but not in NAMESPACE -------------------------
bug <- d[d$has_export & !d$exported & !d$s3method, ]
cat("== [1] @export in source but missing from NAMESPACE ==\n")
if (nrow(bug) == 0) cat("   OK: none\n") else {
  problems <- problems + nrow(bug)
  for (i in seq_len(nrow(bug))) cat(sprintf("   FAIL: %s (%s)\n", bug$name[i], bug$file[i]))
  cat("   -> NAMESPACE is out of sync with the sources; run roxygen2::roxygenise().\n")
}

# --- 2. HARD: documented, non-internal, but not exported ---------------------
undoc <- d[d$documented & !d$has_export & !d$exported & !d$s3method & !d$internal & !d$hook, ]
cat("\n== [2] documented (title/@param/@return) but not exported ==\n")
if (nrow(undoc) == 0) cat("   OK: none\n") else {
  problems <- problems + nrow(undoc)
  for (i in seq_len(nrow(undoc)))
    cat(sprintf("   FAIL: %s (%s) -- has user docs, add @export or mark @keywords internal\n",
                undoc$name[i], undoc$file[i]))
}

# --- 3. ADVISORY: public-prefixed, not exported, not a method, not internal --
review <- d[d$prefix & !d$exported & !d$s3method & !d$internal & !d$hook, ]
cat("\n== [3] public-API-named but not exported (review; usually internal helpers) ==\n")
if (nrow(review) == 0) cat("   none\n") else
  for (i in seq_len(nrow(review)))
    cat(sprintf("   review: %-40s (%s)\n", review$name[i], review$file[i]))

# --- 4. INFO: exported functions without a standard prefix -------------------
noncat <- sort(unique(d$name[d$exported & !d$prefix & !d$hook]))
cat("\n== [4] exported without a standard prefix (the 'uncategorized' public API) ==\n")
for (n in noncat) cat("   ", n, "\n")

# --- 5. OPTIONAL: roxygen drift (only if roxygen2 is available) --------------
cat("\n== [5] roxygen drift (NAMESPACE vs regenerated) ==\n")
if (requireNamespace("roxygen2", quietly = TRUE)) {
  tmp <- file.path(tempdir(), "audit_roxy")
  unlink(tmp, recursive = TRUE); dir.create(tmp)
  file.copy(c(file.path(root, "DESCRIPTION"), file.path(root, "NAMESPACE")), tmp)
  file.copy(file.path(root, c("R", "man")), tmp, recursive = TRUE)
  suppressMessages(try(roxygen2::roxygenise(tmp, roclets = c("rd", "namespace")), silent = TRUE))
  a <- readLines(file.path(root, "NAMESPACE")); b <- readLines(file.path(tmp, "NAMESPACE"))
  if (identical(sort(a), sort(b))) cat("   OK: NAMESPACE matches the sources\n") else {
    problems <- problems + 1L
    cat("   FAIL: NAMESPACE differs from a fresh roxygenise():\n")
    for (x in setdiff(b, a)) cat("     + ", x, "\n")
    for (x in setdiff(a, b)) cat("     - ", x, "\n")
  }
} else cat("   skipped (roxygen2 not installed)\n")

# --- summary -----------------------------------------------------------------
cat(sprintf("\n== summary: %d function(s), %d exported, %d S3 methods, %d hard problem(s) ==\n",
            nrow(d), sum(d$exported), sum(d$s3method & !d$exported), problems))
if (problems > 0) quit(status = 1L)

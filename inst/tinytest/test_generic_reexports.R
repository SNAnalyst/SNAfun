report_side_effects()

# snafun supplies S3 methods for a handful of generics it does not own. Two
# properties have to hold simultaneously, and they pull in opposite directions:
#
#   (a) the generic's name is exported, so snafun::plot(g) resolves and users
#       never need library(snafun);
#   (b) no object of that name exists inside the snafun namespace, because
#       loadNamespace() would then treat the generic as local and register the
#       methods in snafun's own S3 table instead of in the one owned by the
#       package that defines the generic. A bare plot(<stat_cug>) would quietly
#       fall through to plot.default.
#
# Satisfying (a) by writing `plot <- base::plot` breaks (b) without any error,
# and dropping the roxygen export tag to silence roxygen2's "listed as exports,
# but not present in namespace" warning breaks (a). Both have happened.
#
# See R/reexports.R for the full explanation.

ns <- asNamespace("snafun")
declared <- getNamespaceInfo(ns, "S3methods")
exported <- getNamespaceExports("snafun")

is_local <- function(name) exists(name, envir = ns, inherits = FALSE)

generics <- sort(unique(declared[, 1L]))

# The generics snafun means to borrow. Everything below is asserted against this
# list rather than against whatever the package currently happens to do, so that
# a broken build fails loudly instead of quietly re-deriving its own brokenness.
foreign <- c("as.data.frame", "plot", "print", "summary")
own <- setdiff(generics, foreign)

# snafun addresses igraph and network as igraph:: and network:: rather than
# importing them, so their namespaces load lazily, on first use. Once they do,
# they re-register their own plot.igraph / print.igraph / plot.network /
# print.network on top of snafun's wrappers. That is harmless -- snafun's
# wrappers only forward to those very functions -- but it means the *owner* of
# those entries depends on whether anything has touched igraph:: yet. Force the
# steady state here so the assertions below do not depend on execution order.
invisible(lapply(c("igraph", "network"), requireNamespace, quietly = TRUE))

# Classes belonging to another package, which that package may reclaim as
# described above. For these we assert registration, but not ownership.
reclaimable <- c("igraph", "network", "communities")

# pkgload::setup_ns_exports() keeps only those exports it can find in the
# namespace env or the imports env; it does not follow the parent chain up to
# base. Our four generics live in base, so under devtools::load_all() pkgload
# warns about them and then *drops them from the exports*. snafun::plot() really
# does not exist under load_all(), fix or no fix -- which is very probably how
# the @export tags came to be deleted in the first place.
#
# The export assertions below therefore only mean something against an installed
# package, which is what R CMD check (and therefore CI) uses. Under load_all()
# they would fail for a reason that has nothing to do with snafun.
loaded_by_pkgload <- !is.null(get0(".__DEVTOOLS__", envir = ns, inherits = FALSE))
if (loaded_by_pkgload) {
  message("test_generic_reexports.R: package loaded by pkgload; ",
          "skipping the export assertions, which pkgload cannot satisfy. ",
          "Run them against an installed package (R CMD check).")
}


# --- the set of foreign generics is a deliberate choice, not an accident ------
# If this fails, snafun started providing methods for a generic it does not own
# (or stopped). Decide whether snafun::<generic> should work, then update the
# `foreign` vector above.
expect_equal(sort(generics[!vapply(generics, is_local, logical(1))]), foreign,
             info = "unexpected set of foreign generics")


# --- (a) every foreign generic is reachable as snafun::<generic> -------------
for (g in if (loaded_by_pkgload) character(0) else foreign) {
  expect_true(g %in% exported,
              info = paste0("snafun::", g, "() is not exported; a qualified call ",
                            "fails with \"'", g, "' is not an exported object\""))

  reexported <- try(getExportedValue("snafun", g), silent = TRUE)
  expect_false(inherits(reexported, "try-error"),
               info = paste0("getExportedValue(\"snafun\", \"", g, "\") failed"))
  if (!inherits(reexported, "try-error")) {
    expect_identical(reexported, get(g, envir = baseenv()),
                     info = paste0("snafun::", g, " is not the generic from base"))
  }
}


# --- (b) no foreign generic has a namesake object in the namespace -----------
for (g in foreign) {
  expect_false(is_local(g),
               info = paste0("an object named '", g, "' exists in the snafun ",
                             "namespace; loadNamespace() now treats it as a local ",
                             "generic and its methods are no longer registered ",
                             "against the generic in base"))
}


# --- (b), stated directly: the methods went to base, not to snafun -----------
# This is the load-order-independent form of the invariant. If a `plot` object
# ever reappears in the namespace, the methods show up here instead.
snafun_tbl <- get(".__S3MethodsTable__.", envir = ns)
stray <- grep(paste0("^(", paste(foreign, collapse = "|"), ")\\."),
              ls(snafun_tbl), value = TRUE)
expect_equal(stray, character(0),
             info = paste0("methods for foreign generics were registered in ",
                           "snafun's own S3 table: ", paste(stray, collapse = ", ")))

for (g in foreign) {
  defenv <- environment(get(g, envir = baseenv()))
  base_tbl <- get(".__S3MethodsTable__.", envir = defenv)
  methods <- declared[declared[, 1L] == g, , drop = FALSE]

  for (i in seq_len(nrow(methods))) {
    cls <- methods[i, 2L]
    method_name <- paste(g, cls, sep = ".")

    registered <- exists(method_name, envir = base_tbl, inherits = FALSE)
    expect_true(registered,
                info = paste0(method_name, " is absent from the S3 method table of ",
                              environmentName(defenv), "; a bare ", g,
                              "(<", cls, ">) does not dispatch to it"))

    if (registered && !cls %in% reclaimable) {
      owner <- environmentName(environment(get(method_name, envir = base_tbl)))
      expect_identical(owner, "snafun",
                       info = paste0(method_name, " in ", environmentName(defenv),
                                     "'s method table is owned by '", owner,
                                     "', not by snafun"))
    }
  }
}


# --- the other direction: snafun's own generics must all be exported ---------
for (g in own) {
  expect_true(g %in% exported,
              info = paste0("snafun defines generic ", g, "() but does not export it"))
}


# --- behaviour, not just bookkeeping -----------------------------------------
# stat_cug is the case that regressed silently: plot.default happily accepts a
# list and complains about missing 'x' and 'y' components rather than erroring
# inside snafun's method.
grDevices::pdf(NULL)
on.exit(grDevices::dev.off(), add = TRUE)
empty_cug <- structure(list(), class = "stat_cug")

# Bare dispatch is what property (b) protects, and it holds under load_all() too.
expect_error(plot(empty_cug), pattern = "no valid replicate statistics",
             info = "bare plot(<stat_cug>) does not reach snafun's method")

# The qualified calls that started all this. Only meaningful when the package is
# installed; see the note on pkgload above.
if (!loaded_by_pkgload) {
  expect_error(snafun::plot(empty_cug), pattern = "no valid replicate statistics",
               info = "snafun::plot(<stat_cug>) does not reach snafun's method")

  g_i <- snafun::create_random_graph(10, "gnm", m = 20, graph = "igraph")
  g_n <- snafun::create_random_graph(10, "gnm", m = 20, graph = "network")

  expect_stdout(snafun::print(g_i), pattern = "IGRAPH")
  expect_stdout(snafun::print(g_n), pattern = "Network attributes")
  expect_silent(snafun::plot(g_i))
  expect_silent(snafun::plot(g_n))
}

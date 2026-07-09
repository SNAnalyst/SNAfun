# load_all() drops exports of generics re-exported from base

> Filed upstream as [r-lib/pkgload#339](https://github.com/r-lib/pkgload/issues/339).
> This is the reference copy of what was submitted.

### Summary

A package that provides S3 methods for a base generic can export the generic's
*name* so that `pkg::plot(x)` works without the package being attached. This is
`export(plot)` with no object named `plot` in the namespace; R resolves the
export through the namespace's parent chain, and `pkg::plot` becomes `base::plot`.

When the package is **installed**, this works exactly as intended. Under
`load_all()`, `setup_ns_exports()` removes such names from the package's exports,
so `pkg::plot` does not exist. `load_all()` and an installed package therefore
disagree about the package's API.

This is not the same as #31. I'm not arguing the warning is wrong â€” I understand
the position there that `plot` is genuinely "exported but not in the package
namespace". The point here is the second half: after warning, `setup_ns_exports()`
*drops the export*, and that is what makes `load_all()` diverge from an installed
package. A developer working under `load_all()` sees `pkg::plot()` fail with an
error that does not occur for their users.

### Reprex

No roxygen2 involved; hand-written `NAMESPACE`.

```
basegen/
  DESCRIPTION      # Package: basegen, plus the usual required fields
  NAMESPACE
  R/plot.R
```

```
# NAMESPACE
S3method(plot,widget)
export(plot)
```

```r
# R/plot.R
plot.widget <- function(x, ...) invisible("widget method ran")
```

Installed, this behaves as intended:

```r
"plot" %in% getNamespaceExports("basegen")
#> TRUE
identical(basegen::plot, base::plot)
#> TRUE
basegen::plot(structure(1, class = "widget"))
#> "widget method ran"
```

Under `load_all()`:

```r
pkgload::load_all("basegen", export_all = FALSE)
#> Warning: Objects listed as exports, but not present in namespace:
#> â€¢ plot

"plot" %in% getNamespaceExports("basegen")
#> FALSE

basegen::plot(structure(1, class = "widget"))
#> Error: 'plot' is not an exported object from 'namespace:basegen'
```

### Mechanism

`setup_ns_exports()` (R/namespace-env.R, current dev) keeps only names it finds
in the namespace env or the imports env, and does not follow the parent chain to
`base`:

```r
ns_and_imports <- c(env_names(nsenv), env_names(imports_env(package)))
extra_exports  <- setdiff(exports, ns_and_imports)
...
exports <- intersect(ns_and_imports, exports)   # base-inherited exports dropped here
```

Consistently, a re-export from a *normal* package is fine, because the object
lands in the imports env:

```r
# NAMESPACE:  export(head)  +  importFrom(utils, head)
pkgload::load_all("basegen", export_all = FALSE)
"head" %in% getNamespaceExports("basegen")   #> TRUE
"plot" %in% getNamespaceExports("basegen")   #> FALSE
```

So `base` is the special case: the object it re-exports is reachable through the
parent chain but is neither in the namespace env nor the imports env.

### No workaround

The obvious one, importing from base, is rejected by R itself:

```r
# NAMESPACE:  export(plot)  +  importFrom(base, plot)
pkgload::load_all("basegen")
#> Error: operation not allowed on base namespace
install.packages("basegen", repos = NULL, type = "source")
#> Error in asNamespace(ns, base.OK = FALSE) : operation not allowed on base namespace
```

`@rawNamespace export(plot)` produces the same NAMESPACE line and the same
outcome. So a package in this position cannot make `load_all()` match an
installed build.

### Suggestion

Would `setup_ns_exports()` be willing to keep exports that resolve in the
namespace's parent chain â€” e.g. `exists(name, envir = nsenv, inherits = TRUE)`,
or at minimum names that resolve in `baseenv()` â€” rather than dropping them? That
would make `load_all()` agree with an installed package for re-exported base
generics, independently of whether the warning is kept.

Happy to send a PR if that direction is acceptable.

### Context

This bit us in practice: working under `load_all()`, a contributor saw
`pkg::plot()` fail and removed the `@export` tag to "fix" it, which then broke the
installed package for every user. Reduced example above.

pkgload 1.5.3 (current CRAN); dev `setup_ns_exports()` is unchanged. R 4.6.1.

# load_all() drops exports of generics re-exported from base, and warns about them

pkgload 1.5.3, R 4.6.1.

A package that supplies S3 methods for a base generic can export the generic's
name so that `pkg::plot(x)` works without the package being attached. `@export`
on a documentation-only roxygen block (or a hand-written `export(plot)`) exports
the *name* without creating an object called `plot` in the namespace. R resolves
it through the namespace's parent chain, and `pkg::plot` becomes `base::plot`.

Under `load_all()` this breaks. `setup_ns_exports()` warns about the name and
then removes it from the package's exports, so `pkg::plot` does not exist.

## Reprex

No roxygen2 involved; the NAMESPACE is hand-written.

```
basegen/
  DESCRIPTION      # Package: basegen, plus the usual required fields
  NAMESPACE
  R/plot.R
```

```r
# NAMESPACE
S3method(plot,widget)
export(plot)

# R/plot.R
plot.widget <- function(x, ...) invisible("widget method ran")
```

Installed, this behaves exactly as intended:

```r
"plot" %in% getNamespaceExports("basegen")
#> TRUE
identical(basegen::plot, base::plot)
#> TRUE
basegen::plot(structure(1, class = "widget"))
#> "widget method ran"
exists("plot.widget", envir = get(".__S3MethodsTable__.", envir = baseenv()))
#> TRUE
```

Under `load_all()`:

```r
pkgload::load_all("basegen", export_all = FALSE)
#> ℹ Loading basegen
#> Warning message:
#> Objects listed as exports, but not present in namespace:
#> • plot

"plot" %in% getNamespaceExports("basegen")
#> FALSE

basegen::plot(structure(1, class = "widget"))
#> Error: 'plot' is not an exported object from 'namespace:basegen'
```

## Cause

`setup_ns_exports()` looks in the namespace environment and the imports
environment, but does not follow the parent chain to `base`:

```r
ns_and_imports <- c(env_names(nsenv), env_names(imports_env(package)))
extra_exports <- setdiff(exports, ns_and_imports)
if (length(extra_exports) > 0) {
    cli::cli_warn(c("Objects listed as exports, but not present in namespace: ",
        set_names(extra_exports, "*")))
    exports <- intersect(ns_and_imports, exports)
}
```

Consistent with that, a re-export from a normal package is fine, because the
object lands in the imports environment:

```r
# NAMESPACE
export(head)
importFrom(utils,head)
export(plot)
```

```r
pkgload::load_all("basegen", export_all = FALSE)
e <- getNamespaceExports("basegen")
"head" %in% e   #> TRUE   (importFrom, in imports env)
"plot" %in% e   #> FALSE  (base generic, only via parent chain)
```

## There is no workaround

The obvious one, importing from base, is not allowed by R:

```r
# NAMESPACE
export(plot)
importFrom(base,plot)
```

```r
pkgload::load_all("basegen")
#> Error: operation not allowed on base namespace

install.packages("basegen", repos = NULL, type = "source")
#> Error in asNamespace(ns, base.OK = FALSE) : operation not allowed on base namespace
#> ERROR: lazy loading failed for package 'basegen'
```

`@rawNamespace export(plot)` produces the same NAMESPACE line and the same
warning. So a package in this position cannot make `load_all()` behave, and
cannot silence the warning either.

Note also that replacing the name-only export with a real object is not an
equivalent spelling. `loadNamespace()` treats a generic as *local* as soon as an
object of that name exists in the package namespace, and then registers the
package's methods in the package's own S3 method table rather than in base's.
Adding `plot <- base::plot` therefore satisfies `load_all()` while silently
stopping a bare `plot(<widget>)` from dispatching.

## Why this matters

This cost our package (`snafun`) a three-month regression. Working under
`load_all()`, a contributor saw the warning naming `plot` and `print`, saw
`snafun::plot(g)` fail, and removed the `@export` tags. The next `roxygenise()`
dropped `export(plot)` and `export(print)` from NAMESPACE, and the *installed*
package then failed too:

```
Error: 'plot' is not an exported object from 'namespace:snafun'
```

Both signals pointed at the tags. Both were artefacts of `load_all()`. Following
them broke the package for every user.

## Suggested fix

In `setup_ns_exports()`, treat a name as present when it resolves anywhere in
the namespace's parent chain — `exists(name, envir = nsenv, inherits = TRUE)` —
or at minimum when it resolves in `baseenv()`. Keeping such names in `exports`
would make `load_all()` agree with an installed package.

Failing that, dropping the warning for names found in `baseenv()` would already
remove the trap, since the warning is what invites the fatal "fix".

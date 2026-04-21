report_side_effects()


# The helper should quietly return NULL when an optional package is absent.
expect_true(
  is.null(
    snafun:::get_optional_namespace_object(
      name = "whatever",
      package = "definitelyMissingPackageForSnafun"
    )
  )
)


# When the namespace is available, the helper should return the requested
# object unchanged.
expect_identical(
  snafun:::get_optional_namespace_object(name = "head", package = "utils"),
  utils::head
)


# The modeling helpers should fail only when the optional package is actually
# needed, and then with a targeted error message.
if (!requireNamespace("btergm", quietly = TRUE)) {
  fake_ergm <- structure(list(), class = "ergm")
  expect_error(
    snafun::stat_plot_gof_as_btergm(fake_ergm),
    "Package 'btergm' is required for 'stat_plot_gof_as_btergm\\(\\)'"
  )

  fake_btergm_gof <- structure(list(), class = "gof")
  expect_error(
    snafun::stat_plot_gof(fake_btergm_gof),
    "Package 'btergm' is required for 'stat_plot_gof\\(\\)'"
  )
}


if (!requireNamespace("ergm", quietly = TRUE)) {
  fake_ergm_gof <- structure(list(), class = "gof.ergm")
  expect_error(
    snafun::stat_plot_gof(fake_ergm_gof),
    "Package 'ergm' is required for 'stat_plot_gof\\(\\)'"
  )
}

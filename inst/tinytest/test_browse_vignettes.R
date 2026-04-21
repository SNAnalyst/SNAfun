report_side_effects()


missing_index <- snafun:::installed_package_vignettes("definitelynotapackage")
expect_true(is.data.frame(missing_index))
expect_identical(
  colnames(missing_index),
  c("Package", "LibPath", "Item", "Title")
)
expect_identical(nrow(missing_index), 0L)

expect_error(
  snafun::browse_vignettes(package = "definitelynotapackage", open = FALSE),
  "No installed vignettes found"
)


all_packages <- rownames(utils::installed.packages())
first_with_vignettes <- NULL
for (one_package in all_packages) {
  one_index <- snafun:::installed_package_vignettes(one_package)
  if (nrow(one_index) > 0) {
    first_with_vignettes <- one_package
    break
  }
}

expect_true(!is.null(first_with_vignettes))

one_index <- snafun:::installed_package_vignettes(first_with_vignettes)
expect_true(nrow(one_index) > 0)
expect_true(all(c("Package", "LibPath", "Item", "Title") %in% colnames(one_index)))

browsed <- snafun::browse_vignettes(package = first_with_vignettes, open = FALSE)
expect_true(is.data.frame(browsed))
expect_equal(browsed, one_index)

expect_identical(snafun:::select_installed_vignette(one_index, selection = 1), 1L)
expect_identical(
  snafun:::select_installed_vignette(one_index, selection = one_index$Item[[1]]),
  1L
)
expect_identical(
  snafun:::select_installed_vignette(one_index, selection = one_index$Title[[1]]),
  1L
)

expect_error(
  snafun:::select_installed_vignette(one_index, selection = 999),
  "outside the vignette index"
)
expect_error(
  snafun:::select_installed_vignette(one_index, selection = "this does not exist"),
  "does not match"
)

html_path <- snafun:::open_installed_vignette(
  vignette_row = one_index[1, , drop = FALSE],
  package = first_with_vignettes,
  browse = FALSE
)
expect_true(file.exists(html_path))

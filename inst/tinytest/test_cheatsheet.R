report_side_effects()


if (all(vapply(
  c("gt", "htmltools", "magrittr", "knitr"),
  FUN = requireNamespace,
  FUN.VALUE = logical(1),
  quietly = TRUE
))) {
  tmp_html <- tempfile(fileext = ".html")
  out_path <- snafun::create_cheatsheet_html(file = tmp_html)

  expect_true(file.exists(tmp_html))
  expect_equal(
    normalizePath(out_path, winslash = "/", mustWork = TRUE),
    normalizePath(tmp_html, winslash = "/", mustWork = TRUE)
  )

  html_txt <- paste(readLines(tmp_html, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

  expect_true(grepl("SNA4DS cheatsheet", html_txt, fixed = TRUE))
  expect_true(grepl(">Contents<", html_txt, fixed = TRUE))
  expect_true(grepl("href=\"#main_packages\"", html_txt, fixed = TRUE))
  expect_true(grepl("Main packages in the SNA4DS course", html_txt, fixed = TRUE))
  expect_true(grepl("The snafun package", html_txt, fixed = TRUE))
  expect_true(grepl("Automatically generated from the current snafun vignette tables and figures.", html_txt, fixed = TRUE))
  expect_true(grepl("snafun::stat_qap_logit", html_txt, fixed = TRUE))
  expect_true(grepl("snafun::evaluate_communities", html_txt, fixed = TRUE))
  expect_true(grepl("Plot silhouette widths for communities", html_txt, fixed = TRUE))
  expect_true(grepl("Example output from snafun::plot_centralities().", html_txt, fixed = TRUE))
  expect_true(grepl("Participation-shift categories used in the temporal networks part of the course.", html_txt, fixed = TRUE))
  expect_true(grepl("data:image/png;base64", html_txt, fixed = TRUE))
  expect_true(grepl("data:image/jpeg;base64", html_txt, fixed = TRUE))

  bundled_path <- snafun::browse_cheatsheet(browse = FALSE)
  expect_true(file.exists(bundled_path))
  expect_true(grepl("cheatsheet\\.html$", bundled_path))
  expect_true(file.exists(snafun:::cheatsheet_table_script_path()))
  expect_true(file.exists(snafun:::cheatsheet_asset_path("triad_census.png")))

  browser_path <- snafun:::headless_browser_path()
  if (!is.null(browser_path)) {
    expect_true(file.exists(browser_path))
  }

  expect_error(
    snafun::create_cheatsheet_html(file = "", browse = FALSE),
    "non-empty path"
  )
  expect_error(
    snafun::create_cheatsheet_pdf(file = "", browse = FALSE),
    "non-empty path"
  )
  expect_error(
    snafun::create_cheatsheet_pdf(
      file = tempfile(fileext = ".pdf"),
      html = "",
      browse = FALSE
    ),
    "non-empty path"
  )
  expect_error(
    snafun::create_cheatsheet_html(file = tempfile(fileext = ".html"), browse = NA),
    "TRUE or FALSE"
  )
  expect_error(
    snafun::create_cheatsheet_pdf(file = tempfile(fileext = ".pdf"), browse = NA),
    "TRUE or FALSE"
  )
  expect_error(
    snafun::browse_cheatsheet(browse = NA),
    "TRUE or FALSE"
  )
}

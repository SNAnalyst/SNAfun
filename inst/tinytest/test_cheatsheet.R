report_side_effects()


if (all(vapply(
  c("gt", "htmltools", "knitr"),
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

  # --- New "Components and communities" chapter (components + moved community
  #     block), rendered right before the vertex-level indices. -----------------
  expect_true(grepl("Components and communities", html_txt, fixed = TRUE))
  expect_true(grepl("href=\"#components-and-communities\"", html_txt, fixed = TRUE))
  expect_true(grepl("snafun::count_components", html_txt, fixed = TRUE))
  expect_true(grepl("snafun::extract_bridges", html_txt, fixed = TRUE))
  expect_true(grepl("snafun::extract_cut_vertices", html_txt, fixed = TRUE))
  # community material moved here (still present in the document)
  expect_true(grepl("snafun::extract_comm_louvain", html_txt, fixed = TRUE))

  # --- New graph-, vertex-, creation-, conversion- and manipulation functions --
  expect_true(grepl("snafun::g_efficiency", html_txt, fixed = TRUE))
  expect_true(grepl("snafun::g_secrecy", html_txt, fixed = TRUE))
  expect_true(grepl("snafun::v_distance", html_txt, fixed = TRUE))
  expect_true(grepl("snafun::v_transitivity", html_txt, fixed = TRUE))
  expect_true(grepl("snafun::read_ucinet", html_txt, fixed = TRUE))
  expect_true(grepl("snafun::to_binary_matrix", html_txt, fixed = TRUE))
  expect_true(grepl("snafun::make_matrix_from_vertex_attribute", html_txt, fixed = TRUE))
  expect_true(grepl("snafun::extract_vertex_ids", html_txt, fixed = TRUE))

  # --- Statistical-models prose now lists extra stat_* functions (key args) -----
  expect_true(grepl("snafun::stat_nam_summary", html_txt, fixed = TRUE))
  expect_true(grepl("snafun::plot_nam", html_txt, fixed = TRUE))
  expect_true(grepl("snafun::stat_plot_gof", html_txt, fixed = TRUE))
  expect_true(grepl("snafun::stat_ef_int", html_txt, fixed = TRUE))

  # --- Temporal chapter renamed + new snafun-only descriptives table -----------
  expect_true(grepl("snafun::plot_network_slices", html_txt, fixed = TRUE))
  expect_true(grepl("snafun::count_edges_in_interval", html_txt, fixed = TRUE))

  # --- The bundled table script must build the two new table objects -----------
  tables_env <- new.env(parent = baseenv())
  sys.source(snafun:::cheatsheet_table_script_path(), envir = tables_env)
  expect_true(exists("table_components_communities", envir = tables_env, inherits = FALSE))
  expect_true(exists("table_temporal", envir = tables_env, inherits = FALSE))

  # --- Regression guard for the actuality audit -------------------------------
  # Deprecated / renamed / non-existent functions must not creep back into the
  # cheatsheet source. Each pattern is a full "pkg::fn(" call, so it does NOT
  # match same-named arguments (e.g. the `edge.betweenness =` argument of
  # cluster_edge_betweenness()). btergm::memory()/delrecip() are intentionally
  # NOT listed: they appear on purpose in the explanatory source note that warns
  # against calling them that way.
  tables_src <- paste(
    readLines(snafun:::cheatsheet_table_script_path(), warn = FALSE,
              encoding = "UTF-8"),
    collapse = "\n"
  )
  forbidden <- c(
    "igraph::as.directed(",                 # -> igraph::as_directed(
    "igraph::as.undirected(",               # -> igraph::as_undirected(
    "igraph::graph_from_incidence_matrix(", # -> graph_from_biadjacency_matrix(
    "igraph::subgraph.edges(",              # -> igraph::subgraph_from_edges(
    "igraph::edge.betweenness(",            # -> igraph::edge_betweenness(
    "igraph::sample_bipartite(",            # -> igraph::sample_bipartite_gnp(
    "network::set.vertex.attr(",            # -> network::set.vertex.attribute(
    "network::network_density(",            # -> network::network.density(
    "edge.attrib.comb"                      # -> edge.attr.comb
  )
  for (bad in forbidden) {
    expect_false(
      grepl(bad, tables_src, fixed = TRUE),
      info = paste("deprecated/incorrect reference reappeared in cheatsheet:", bad)
    )
  }

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

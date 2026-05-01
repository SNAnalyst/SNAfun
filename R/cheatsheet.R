#' Create the HTML cheatsheet
#'
#' Build a standalone HTML cheatsheet from the table definitions used in the
#' \pkg{snafun} vignettes.
#'
#' The generated cheatsheet includes:
#'
#' - a title and short introduction inspired by the \code{Introduction}
#'   vignette;
#' - a table of contents linking to the main sections; and
#' - the current comparison tables and static figures used in the course
#'   materials.
#'
#' This is useful when you want an up-to-date course handout without manually
#' rendering multiple vignette files.
#'
#' @param file output path for the HTML file
#' @param title title shown at the top of the HTML cheatsheet
#' @param browse logical; should the generated file be opened in a browser?
#'
#' @return Invisibly returns the path to the generated HTML file.
#' @export
#'
#' @examples
#' \dontrun{
#' path <- snafun::create_cheatsheet_html(
#'   file = tempfile(fileext = ".html")
#' )
#' path
#' }
create_cheatsheet_html <- function(file = "cheatsheet.html",
                                   title = "SNA4DS cheatsheet",
                                   browse = FALSE) {
  if (length(file) != 1L || is.na(file) || !nzchar(file)) {
    stop("'file' should be a single non-empty path")
  }
  if (length(title) != 1L || is.na(title) || !nzchar(title)) {
    stop("'title' should be a single non-empty string")
  }
  if (!is.logical(browse) || length(browse) != 1L || is.na(browse)) {
    stop("'browse' should be either TRUE or FALSE")
  }

  required_pkgs <- c("gt", "htmltools", "magrittr", "knitr")
  missing_pkgs <- required_pkgs[!vapply(
    required_pkgs,
    FUN = requireNamespace,
    FUN.VALUE = logical(1),
    quietly = TRUE
  )]
  if (length(missing_pkgs) > 0L) {
    stop(
      "The following packages are required to build the cheatsheet: ",
      paste(missing_pkgs, collapse = ", "),
      "."
    )
  }

  tables_env <- new.env(parent = baseenv())
  sys.source(
    cheatsheet_table_script_path(),
    envir = tables_env
  )

  table_order <- c(
    "table_create",
    "table_convert",
    "table_to",
    "table_manipulate",
    "table_graph",
    "table_vertices",
    "table_dyads",
    "table_stats",
    "table_models",
    "table_btergm_terms"
  )

  missing_tables <- table_order[!vapply(
    table_order,
    FUN = exists,
    FUN.VALUE = logical(1),
    envir = tables_env,
    inherits = FALSE
  )]
  if (length(missing_tables) > 0L) {
    stop(
      "The bundled cheatsheet source is missing the following table objects: ",
      paste(missing_tables, collapse = ", "),
      "."
    )
  }

  tables <- mget(table_order, envir = tables_env, inherits = FALSE)
  sections <- cheatsheet_sections()

  document <- htmltools::tags$html(
    htmltools::tags$head(
      htmltools::tags$title(title),
      htmltools::tags$meta(charset = "utf-8"),
      htmltools::tags$meta(
        name = "viewport",
        content = "width=device-width, initial-scale=1"
      ),
      htmltools::tags$style(htmltools::HTML(cheatsheet_css()))
    ),
    htmltools::tags$body(
      htmltools::tags$div(
        class = "page-shell",
        htmltools::tags$header(
          class = "page-header",
          htmltools::tags$h1(class = "page-title", title),
          htmltools::tags$p(
            class = "page-subtitle",
            "Automatically generated from the current snafun vignette tables and figures."
          )
        ),
        htmltools::tags$div(
          class = "page-layout",
          htmltools::tags$aside(
            class = "page-toc",
            htmltools::tags$h2("Contents"),
            cheatsheet_toc_node(sections)
          ),
          htmltools::tags$main(
            class = "page-content",
            cheatsheet_intro_node(),
            htmltools::tagList(
              lapply(
                sections,
                FUN = function(one_section) {
                  cheatsheet_section_node(one_section, tables)
                }
              )
            )
          )
        )
      )
    )
  )

  file <- normalizePath(file, winslash = "/", mustWork = FALSE)
  htmltools::save_html(
    html = document,
    file = file,
    background = "white"
  )

  if (isTRUE(browse)) {
    utils::browseURL(file)
  }

  invisible(file)
}


#' Open the bundled cheatsheet
#'
#' Open the prebuilt HTML cheatsheet that ships with \pkg{snafun}. This is the
#' recommended way for students and other end users to access the cheatsheet,
#' because it avoids rebuilding the HTML locally.
#'
#' If the bundled HTML file is not available (for example during development
#' before it has been generated), the function falls back to creating a
#' temporary cheatsheet with [create_cheatsheet_html()].
#'
#' @param browse logical; should the cheatsheet be opened in a browser?
#'
#' @return Invisibly returns the path to the cheatsheet HTML file.
#' @export
#'
#' @examples
#' \dontrun{
#' snafun::browse_cheatsheet()
#' }
browse_cheatsheet <- function(browse = TRUE) {
  if (!is.logical(browse) || length(browse) != 1L || is.na(browse)) {
    stop("'browse' should be either TRUE or FALSE")
  }

  path <- bundled_cheatsheet_html_path()

  if (is.null(path)) {
    path <- tempfile(fileext = ".html")
    create_cheatsheet_html(file = path, browse = FALSE)
  }

  path <- normalizePath(path, winslash = "/", mustWork = TRUE)

  if (isTRUE(browse)) {
    utils::browseURL(path)
  }

  invisible(path)
}


#' Create a PDF version of the cheatsheet
#'
#' Build a PDF version of the complete \pkg{snafun} cheatsheet by rendering the
#' standalone HTML cheatsheet in a headless Chromium-based browser such as
#' Google Chrome or Microsoft Edge.
#'
#' This is useful when you want a printable handout with the same contents as
#' the bundled HTML cheatsheet: the introduction, table of contents, figures,
#' and all current comparison tables.
#'
#' @param file output path for the PDF file
#' @param html optional path to an existing cheatsheet HTML file. If
#'   \code{NULL}, [create_cheatsheet_html()] is used to build a temporary HTML
#'   file first.
#' @param title title passed to [create_cheatsheet_html()] when a temporary HTML
#'   file is built internally
#' @param browse logical; should the generated PDF be opened afterwards?
#'
#' @return Invisibly returns the path to the generated PDF file.
#' @export
#'
#' @examples
#' \dontrun{
#' path <- snafun::create_cheatsheet_pdf(
#'   file = tempfile(fileext = ".pdf")
#' )
#' path
#' }
create_cheatsheet_pdf <- function(file = "cheatsheet.pdf",
                                  html = NULL,
                                  title = "SNA4DS cheatsheet",
                                  browse = FALSE) {
  if (length(file) != 1L || is.na(file) || !nzchar(file)) {
    stop("'file' should be a single non-empty path")
  }
  if (!is.null(html) && (length(html) != 1L || is.na(html) || !nzchar(html))) {
    stop("'html' should be NULL or a single non-empty path")
  }
  if (length(title) != 1L || is.na(title) || !nzchar(title)) {
    stop("'title' should be a single non-empty string")
  }
  if (!is.logical(browse) || length(browse) != 1L || is.na(browse)) {
    stop("'browse' should be either TRUE or FALSE")
  }

  browser <- headless_browser_path()
  if (is.null(browser)) {
    stop(
      "Could not locate a supported headless browser. ",
      "Please install Google Chrome or Microsoft Edge."
    )
  }

  html_is_temp <- is.null(html)
  if (isTRUE(html_is_temp)) {
    html <- tempfile(fileext = ".html")
    create_cheatsheet_html(
      file = html,
      title = title,
      browse = FALSE
    )
  } else {
    html <- normalizePath(html, winslash = "/", mustWork = TRUE)
  }

  file <- normalizePath(file, winslash = "/", mustWork = FALSE)
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  if (file.exists(file)) {
    file.remove(file)
  }

  user_data_dir <- tempfile(pattern = "cheatsheet-browser-")
  dir.create(user_data_dir, recursive = TRUE, showWarnings = FALSE)

  args <- c(
    paste0("--user-data-dir=", normalizePath(
      user_data_dir,
      winslash = "/",
      mustWork = FALSE
    )),
    "--headless",
    "--disable-gpu",
    "--run-all-compositor-stages-before-draw",
    "--no-first-run",
    "--no-default-browser-check",
    paste0("--print-to-pdf=", file),
    "--print-to-pdf-no-header",
    html_path_to_uri(html)
  )

  on.exit(
    unlink(user_data_dir, recursive = TRUE, force = TRUE),
    add = TRUE
  )
  if (isTRUE(html_is_temp)) {
    on.exit(
      unlink(html, force = TRUE),
      add = TRUE
    )
  }

  status <- suppressWarnings(
    system2(
      command = browser,
      args = args,
      stdout = TRUE,
      stderr = TRUE
    )
  )

  for (i in seq_len(20L)) {
    if (file.exists(file)) {
      break
    }
    Sys.sleep(0.5)
  }

  if (!file.exists(file)) {
    stop(
      "PDF export failed. Browser output was:\n",
      paste(status, collapse = "\n")
    )
  }

  if (isTRUE(browse)) {
    utils::browseURL(file)
  }

  invisible(file)
}


#' Locate the bundled cheatsheet table definitions
#'
#' @keywords internal
#' @noRd
cheatsheet_table_script_path <- function() {
  installed_path <- system.file("extdata", "create_tables.R", package = "snafun")
  if (nzchar(installed_path) && file.exists(installed_path)) {
    return(installed_path)
  }

  source_path <- file.path(getwd(), "inst", "extdata", "create_tables.R")
  if (file.exists(source_path)) {
    return(source_path)
  }

  stop(
    "Could not locate the bundled cheatsheet table definitions. ",
    "Please reinstall 'snafun' or run the function from the package source tree."
  )
}


#' Locate the bundled cheatsheet HTML file
#'
#' @keywords internal
#' @noRd
bundled_cheatsheet_html_path <- function() {
  installed_path <- system.file("extdata", "cheatsheet.html", package = "snafun")
  if (nzchar(installed_path) && file.exists(installed_path)) {
    return(installed_path)
  }

  source_path <- file.path(getwd(), "inst", "extdata", "cheatsheet.html")
  if (file.exists(source_path)) {
    return(source_path)
  }

  NULL
}


#' Locate bundled cheatsheet assets
#'
#' @keywords internal
#' @noRd
cheatsheet_asset_path <- function(filename) {
  installed_path <- system.file(
    "extdata",
    "cheatsheet_assets",
    filename,
    package = "snafun"
  )
  if (nzchar(installed_path) && file.exists(installed_path)) {
    return(installed_path)
  }

  source_path <- file.path(getwd(), "inst", "extdata", "cheatsheet_assets", filename)
  if (file.exists(source_path)) {
    return(source_path)
  }

  stop(
    "Could not locate the cheatsheet asset '",
    filename,
    "'."
  )
}


#' Find a supported headless browser
#'
#' @keywords internal
#' @noRd
headless_browser_path <- function() {
  candidates <- c(
    Sys.which("chrome"),
    Sys.which("msedge"),
    "C:/Program Files/Google/Chrome/Application/chrome.exe",
    "C:/Program Files (x86)/Google/Chrome/Application/chrome.exe",
    "C:/Program Files/Microsoft/Edge/Application/msedge.exe",
    "C:/Program Files (x86)/Microsoft/Edge/Application/msedge.exe"
  )

  candidates <- unique(candidates[nzchar(candidates)])
  candidates <- candidates[file.exists(candidates)]

  if (length(candidates) < 1L) {
    return(NULL)
  }

  candidates[[1L]]
}


#' Convert a local HTML path to a file URI
#'
#' @keywords internal
#' @noRd
html_path_to_uri <- function(path) {
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  path <- gsub("%", "%25", path, fixed = TRUE)
  path <- gsub(" ", "%20", path, fixed = TRUE)
  path <- gsub("#", "%23", path, fixed = TRUE)
  paste0("file:///", path)
}


#' Cheatsheet section metadata
#'
#' @keywords internal
#' @noRd
cheatsheet_sections <- function() {
  list(
    list(
      id = "main_packages",
      title = "Main packages in the SNA4DS course",
      children = list(
        list(
          id = "overview-of-igraph-and-network-graph-objects",
          title = "Overview of igraph and network graph objects"
        ),
        list(
          id = "snafun-intro",
          title = "The snafun package"
        )
      )
    ),
    list(
      id = "generate",
      title = "Creating a graph object",
      children = list(
        list(
          id = "additional-graph-creation-functions",
          title = "Additional graph creation functions"
        )
      )
    ),
    list(
      id = "converting-between-graph-classes",
      title = "Converting between graph classes"
    ),
    list(
      id = "manipulate",
      title = "Manipulating the graph object"
    ),
    list(
      id = "graph-level-indices",
      title = "Graph level indices",
      children = list(
        list(
          id = "communities-and-other-subgroups",
          title = "Communities and other subgroups"
        )
      )
    ),
    list(
      id = "vertex-level-indices",
      title = "Vertex-level indices"
    ),
    list(
      id = "dyad-level-indices",
      title = "Dyad-level indices"
    ),
    list(
      id = "plotting",
      title = "Plotting",
      children = list(
        list(
          id = "plotting-in-snafun",
          title = "Plotting in snafun"
        )
      )
    ),
    list(
      id = "statistical-models",
      title = "Statistical models",
      children = list(
        list(id = "overview-table", title = "Overview table"),
        list(
          id = "network-autocorrelation-models",
          title = "Network autocorrelation models"
        ),
        list(
          id = "conditional-uniform-graphs-cug",
          title = "Conditional Uniform graphs (CUG)"
        ),
        list(id = "qap-test", title = "QAP test"),
        list(
          id = "qap-linear-regression",
          title = "QAP linear regression"
        ),
        list(
          id = "qap-logistic-regression",
          title = "QAP logistic regression"
        ),
        list(
          id = "terms-classification-for-every-exponential-random-graph-model-ergm",
          title = "Terms classification for every Exponential Random Graph Model (ERGM)"
        ),
        list(
          id = "bipartite-ergms",
          title = "Bipartite ERGMs"
        ),
        list(
          id = "ergm-for-temporal-networks",
          title = "ERGM for temporal networks"
        )
      )
    ),
    list(
      id = "temporal-networks-exploration-and-description",
      title = "Temporal networks (exploration and description)",
      children = list(
        list(
          id = "network-generation-and-manipulation",
          title = "Network generation and manipulation"
        ),
        list(
          id = "network-measures-and-descriptives",
          title = "Network measures and descriptives"
        ),
        list(
          id = "participation-shifts",
          title = "Participation shifts"
        )
      )
    )
  )
}


#' Cheatsheet CSS
#'
#' @keywords internal
#' @noRd
cheatsheet_css <- function() {
  paste(
    "html { scroll-behavior: smooth; }",
    "body {",
    "  font-family: Helvetica, Arial, sans-serif;",
    "  margin: 0;",
    "  color: #222;",
    "  background: #fff;",
    "  line-height: 1.45;",
    "}",
    ".page-shell { max-width: 1680px; margin: 0 auto; padding: 1.5rem 1.5rem 4rem 1.5rem; }",
    ".page-header { margin-bottom: 1.5rem; }",
    ".page-title { margin: 0 0 0.35rem 0; font-size: 2.2rem; }",
    ".page-subtitle { margin: 0; color: #555; }",
    ".page-layout { display: grid; grid-template-columns: minmax(220px, 280px) minmax(0, 1fr); gap: 2rem; align-items: start; }",
    ".page-toc { position: sticky; top: 1rem; max-height: calc(100vh - 2rem); overflow: auto; padding: 1rem 1.1rem; border: 1px solid #ddd; border-radius: 10px; background: #fafafa; }",
    ".page-toc h2 { font-size: 1.1rem; margin: 0 0 0.8rem 0; }",
    ".page-toc ul { margin: 0; padding-left: 1.15rem; }",
    ".page-toc li { margin: 0.25rem 0; }",
    ".page-toc a { color: #1f4b99; text-decoration: none; }",
    ".page-toc a:hover { text-decoration: underline; }",
    ".page-content { min-width: 0; }",
    ".intro-block { margin-bottom: 2rem; }",
    ".intro-note { margin: 1rem 0 0 0; padding: 1rem 1.1rem; border-left: 4px solid #1f4b99; background: #f5f8ff; }",
    ".cheatsheet-section { margin-bottom: 3rem; }",
    ".cheatsheet-section > h1, .cheatsheet-section > h2, .cheatsheet-section > h3 { scroll-margin-top: 1rem; }",
    ".cheatsheet-section h1 { margin-bottom: 0.7rem; font-size: 1.8rem; }",
    ".cheatsheet-section h2 { margin-top: 1.4rem; margin-bottom: 0.55rem; font-size: 1.3rem; }",
    ".cheatsheet-section p { margin: 0 0 0.8rem 0; }",
    ".cheatsheet-table { margin: 1rem 0 1.6rem 0; overflow-x: auto; }",
    ".cheatsheet-figure { margin: 1rem 0 1.8rem 0; }",
    ".cheatsheet-figure img { display: block; max-width: 100%; height: auto; border: 1px solid #ddd; border-radius: 8px; background: #fff; }",
    ".cheatsheet-figure figcaption { margin-top: 0.45rem; color: #555; font-size: 0.95rem; }",
    "@media (max-width: 980px) {",
    "  .page-layout { grid-template-columns: 1fr; }",
    "  .page-toc { position: static; max-height: none; }",
    "}",
    sep = "\n"
  )
}


#' Intro block for the cheatsheet
#'
#' @keywords internal
#' @noRd
cheatsheet_intro_node <- function() {
  htmltools::tags$div(
    class = "intro-block",
    htmltools::tags$p(
      htmltools::HTML("<br><br> In this <em>cheatsheet</em> we summarize the main "),
      htmltools::tags$code("R"),
      " functions that are used in the SNA4DS course. We will not explain ",
      "the underlying concepts here, but refer you to the lectures, labs, and ",
      "slides of the course for that."
    ),
    htmltools::tags$p(
      "The aim of this cheatsheet is that it provides you with an overview ",
      "of the main functions you will need throughout the course. We hope that ",
      "it can provide a useful reference for you, as you develop and apply your ",
      "network analysis skills."
    ),
    htmltools::tags$blockquote(
      class = "intro-note",
      htmltools::tags$p(htmltools::tags$strong("NOTE:")),
      htmltools::tags$p(
        "Most functions have multiple arguments. Our aim is in this cheatsheet ",
        "not to show and discuss the various arguments that exist, because that ",
        "would yield an unwieldy and very long document. Rather, we recommend ",
        "you use your ", htmltools::tags$code("R"), " skills and use the help ",
        "function ", htmltools::tags$code("?"), " and ", htmltools::tags$code("help"),
        " and other approaches we teach you in this course to learn about the ",
        "details of a specific function. If you still can't figure it out, ",
        "contact us and we'll assist you."
      )
    )
  )
}


#' Build TOC recursively
#'
#' @keywords internal
#' @noRd
cheatsheet_toc_node <- function(sections) {
  htmltools::tags$ul(
    lapply(
      sections,
      FUN = function(one_section) {
        children <- one_section$children
        htmltools::tags$li(
          htmltools::tags$a(href = paste0("#", one_section$id), one_section$title),
          if (!is.null(children) && length(children) > 0L) {
            cheatsheet_toc_node(children)
          }
        )
      }
    )
  )
}


#' Render one major cheatsheet section
#'
#' @keywords internal
#' @noRd
cheatsheet_section_node <- function(section, tables) {
  switch(
    section$id,
    "main_packages" = cheatsheet_main_packages_node(section),
    "generate" = cheatsheet_generate_node(section, tables),
    "converting-between-graph-classes" = cheatsheet_simple_table_section(
      section = section,
      table_name = "table_convert",
      tables = tables,
      extra_nodes = list(
        make_heading(2L, "additional-graph-creation-functions", "Additional graph creation functions"),
        htmltools::tags$p(
          "The cheatsheet below focuses on the most common creation routes. ",
          "The package also contains helper functions for adding vertices and edges ",
          "after creation."
        ),
        gt_table_div(tables$table_to)
      )
    ),
    "manipulate" = cheatsheet_simple_table_section(
      section = section,
      table_name = "table_manipulate",
      tables = tables
    ),
    "graph-level-indices" = cheatsheet_graph_level_node(section, tables),
    "vertex-level-indices" = cheatsheet_simple_table_section(
      section = section,
      table_name = "table_vertices",
      tables = tables
    ),
    "dyad-level-indices" = cheatsheet_simple_table_section(
      section = section,
      table_name = "table_dyads",
      tables = tables
    ),
    "plotting" = cheatsheet_plotting_node(section),
    "statistical-models" = cheatsheet_stat_models_node(section, tables),
    "temporal-networks-exploration-and-description" = cheatsheet_temporal_node(section),
    stop("Unknown cheatsheet section id: ", section$id)
  )
}


#' Heading helper
#'
#' @keywords internal
#' @noRd
make_heading <- function(level, id, text) {
  heading_fun <- htmltools::tags[[paste0("h", level)]]
  heading_fun(id = id, text)
}


#' Render gt table as htmltools div
#'
#' @keywords internal
#' @noRd
gt_table_div <- function(table_obj) {
  htmltools::tags$div(
    class = "cheatsheet-table",
    htmltools::HTML(gt::as_raw_html(table_obj))
  )
}


#' Embedded figure helper
#'
#' @keywords internal
#' @noRd
cheatsheet_figure_node <- function(filename, alt, caption) {
  htmltools::tags$figure(
    class = "cheatsheet-figure",
    htmltools::tags$img(
      src = knitr::image_uri(cheatsheet_asset_path(filename)),
      alt = alt
    ),
    htmltools::tags$figcaption(caption)
  )
}


#' Main packages section
#'
#' @keywords internal
#' @noRd
cheatsheet_main_packages_node <- function(section) {
  htmltools::tags$section(
    class = "cheatsheet-section",
    make_heading(1L, section$id, section$title),
    make_heading(
      2L,
      "overview-of-igraph-and-network-graph-objects",
      "Overview of igraph and network graph objects"
    ),
    htmltools::tags$p(
      "There are two main packages for basic graph generation and manipulation: ",
      "the ", htmltools::tags$code("igraph"), " package and the ",
      htmltools::tags$code("statnet"), " package. Actually, ",
      htmltools::tags$code("statnet"), " is a suite of packages that work ",
      "together. In this course, we will make use of several packages from ",
      "the ", htmltools::tags$code("statnet"), " suite."
    ),
    htmltools::tags$p(
      "The ", htmltools::tags$code("igraph"), " package creates a graph object ",
      "of type ", htmltools::tags$code("igraph"), ". The ",
      htmltools::tags$code("statnet"), " suite creates a graph object of type ",
      htmltools::tags$code("network"), ". Both can generate graphs and do basic ",
      "manipulation. The ", htmltools::tags$code("igraph"), " package provides ",
      "more mathematical graph functions, while the ", htmltools::tags$code("statnet"),
      " suite provides many statistical models."
    ),
    make_heading(2L, "snafun-intro", "The snafun package"),
    htmltools::tags$p(
      "The ", htmltools::tags$code("igraph"), " package and ",
      htmltools::tags$code("statnet"), " suite are jointly very powerful, ",
      "but they each require graph objects with specific structures. If you want ",
      "to combine functions from ", htmltools::tags$code("igraph"), ", ",
      htmltools::tags$code("network"), ", and ", htmltools::tags$code("sna"),
      ", you often end up converting between ", htmltools::tags$code("igraph"),
      ", ", htmltools::tags$code("network"), ", ", htmltools::tags$code("matrix"),
      ", and edgelist representations."
    ),
    htmltools::tags$p("Believe it or not, this is a pain and quite annoying."),
    htmltools::tags$p(
      htmltools::tags$strong("THE "),
      htmltools::tags$code("snafun"),
      htmltools::tags$strong(" PACKAGE TO THE RESCUE!")
    ),
    htmltools::tags$ul(
      htmltools::tags$li(
        "It provides a fairly consistent API, so you don't have to constantly ",
        "re-learn different argument conventions."
      ),
      htmltools::tags$li(
        "Most functions work on ", htmltools::tags$code("igraph"), ", ",
        htmltools::tags$code("network"), ", matrices, and edgelists/data frames."
      ),
      htmltools::tags$li(
        "It lets you focus on the fun of network analysis rather than the friction ",
        "of conversions and mismatched interfaces."
      )
    )
  )
}


#' Graph creation section
#'
#' @keywords internal
#' @noRd
cheatsheet_generate_node <- function(section, tables) {
  htmltools::tags$section(
    class = "cheatsheet-section",
    make_heading(1L, section$id, section$title),
    gt_table_div(tables$table_create),
    make_heading(
      2L,
      "additional-graph-creation-functions",
      "Additional graph creation functions"
    ),
    htmltools::tags$p(
      "After creating a graph, you can also extend it with helper functions such as ",
      htmltools::tags$code("snafun::add_vertices()"), " and ",
      htmltools::tags$code("snafun::add_edges()"), "."
    )
  )
}


#' Generic single-table section
#'
#' @keywords internal
#' @noRd
cheatsheet_simple_table_section <- function(section,
                                            table_name,
                                            tables,
                                            extra_nodes = list()) {
  htmltools::tags$section(
    class = "cheatsheet-section",
    make_heading(1L, section$id, section$title),
    gt_table_div(tables[[table_name]]),
    htmltools::tagList(extra_nodes)
  )
}


#' Graph-level section
#'
#' @keywords internal
#' @noRd
cheatsheet_graph_level_node <- function(section, tables) {
  htmltools::tags$section(
    class = "cheatsheet-section",
    make_heading(1L, section$id, section$title),
    gt_table_div(tables$table_graph),
    make_heading(2L, "communities-and-other-subgroups", "Communities and other subgroups"),
    htmltools::tags$p(
      "Community results returned by ", htmltools::tags$code("snafun::extract_comm_*()"),
      " are compatible with the wider ", htmltools::tags$code("igraph"),
      " community API. You can inspect memberships, sizes, crossings, ",
      "modularity, and evaluate the detected communities with ",
      htmltools::tags$code("snafun::evaluate_communities()"), "."
    )
  )
}


#' Plotting section
#'
#' @keywords internal
#' @noRd
cheatsheet_plotting_node <- function(section) {
  htmltools::tags$section(
    class = "cheatsheet-section",
    make_heading(1L, section$id, section$title),
    make_heading(2L, "plotting-in-snafun", "Plotting in snafun"),
    htmltools::tags$p(
      "The ", htmltools::tags$code("snafun"), " package provides S3 plotting methods, ",
      "so a plain call to ", htmltools::tags$code("plot(x)"), " gives a quick plot ",
      "regardless of whether ", htmltools::tags$code("x"), " is an ",
      htmltools::tags$code("igraph"), " or ", htmltools::tags$code("network"),
      " object. The package also includes a dedicated centrality comparison plot."
    ),
    cheatsheet_figure_node(
      filename = "plot_centralities.png",
      alt = "Example output from plot_centralities",
      caption = "Example output from snafun::plot_centralities()."
    )
  )
}


#' Statistical models section
#'
#' @keywords internal
#' @noRd
cheatsheet_stat_models_node <- function(section, tables) {
  htmltools::tags$section(
    class = "cheatsheet-section",
    make_heading(1L, section$id, section$title),
    make_heading(2L, "overview-table", "Overview table"),
    gt_table_div(tables$table_models),
    make_heading(
      2L,
      "network-autocorrelation-models",
      "Network autocorrelation models"
    ),
    htmltools::tags$p(
      "Use ", htmltools::tags$code("snafun::stat_nam()"),
      " for network autocorrelation models with row-normalized weight matrices."
    ),
    make_heading(
      2L,
      "conditional-uniform-graphs-cug",
      "Conditional Uniform graphs (CUG)"
    ),
    htmltools::tags$p(
      "Use ", htmltools::tags$code("snafun::stat_cug()"),
      " for conditional uniform graph tests on supported graph classes."
    ),
    make_heading(2L, "qap-test", "QAP test"),
    htmltools::tags$p(
      "Use ", htmltools::tags$code("snafun::stat_qap_cor()"),
      " to test the association between two networks."
    ),
    make_heading(2L, "qap-linear-regression", "QAP linear regression"),
    htmltools::tags$p(
      "Use ", htmltools::tags$code("snafun::stat_qap_lm()"),
      " for valued dependent networks explained by one or more predictor networks."
    ),
    make_heading(2L, "qap-logistic-regression", "QAP logistic regression"),
    htmltools::tags$p(
      "Use ", htmltools::tags$code("snafun::stat_qap_logit()"),
      " when the dependent network is binary."
    ),
    make_heading(
      2L,
      "terms-classification-for-every-exponential-random-graph-model-ergm",
      "Terms classification for every Exponential Random Graph Model (ERGM)"
    ),
    htmltools::tags$p(
      "The static figures below are used in the course to explain common ERGM term families."
    ),
    cheatsheet_figure_node(
      filename = "triad_census.png",
      alt = "Triad census categories",
      caption = "Triad census categories used in ERGM teaching."
    ),
    make_heading(2L, "bipartite-ergms", "Bipartite ERGMs"),
    cheatsheet_figure_node(
      filename = "bipartite_terms.png",
      alt = "Bipartite ERGM terms overview",
      caption = "Overview of common bipartite ERGM terms."
    ),
    make_heading(2L, "ergm-for-temporal-networks", "ERGM for temporal networks"),
    cheatsheet_figure_node(
      filename = "tergm_terms.jpg",
      alt = "Temporal ERGM terms overview",
      caption = "Overview of common temporal ERGM terms."
    ),
    gt_table_div(tables$table_btergm_terms)
  )
}


#' Temporal section
#'
#' @keywords internal
#' @noRd
cheatsheet_temporal_node <- function(section) {
  htmltools::tags$section(
    class = "cheatsheet-section",
    make_heading(1L, section$id, section$title),
    make_heading(
      2L,
      "network-generation-and-manipulation",
      "Network generation and manipulation"
    ),
    htmltools::tags$p(
      "In this course, temporal network work mostly relies on ",
      htmltools::tags$code("networkDynamic"), ", ", htmltools::tags$code("tsna"),
      ", and ", htmltools::tags$code("ndtv"), ". The ", htmltools::tags$code("snafun"),
      " API now also supports several active-attribute accessors for ",
      htmltools::tags$code("networkDynamic"), " objects."
    ),
    make_heading(
      2L,
      "network-measures-and-descriptives",
      "Network measures and descriptives"
    ),
    htmltools::tags$p(
      "Temporal descriptives often focus on durations, edge formations, dissolutions, ",
      "and time-varying ERGM terms."
    ),
    make_heading(2L, "participation-shifts", "Participation shifts"),
    cheatsheet_figure_node(
      filename = "pshifts.png",
      alt = "Participation shift categories",
      caption = "Participation-shift categories used in the temporal networks part of the course."
    )
  )
}

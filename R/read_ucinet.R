#' Read UCINET data
#'
#' Read a UCINET file and convert it to a graph object.
#'
#' This function currently supports the two UCINET input families that are most
#' commonly encountered in practice:
#'
#' \itemize{
#'   \item DL text files such as \code{.dl}, including
#'   \code{format = fullmatrix} and \code{format = edgelist1};
#'   \item recent UCINET system files, read from the header file
#'   \code{.##h} together with the matching \code{.##d} file.
#' }
#'
#' The default return value is an \code{igraph} object. The same imported data
#' can also be returned as a \code{network}, \code{matrix}, or \code{edgelist}
#' through the \code{graph} argument.
#'
#' Vertex labels are read when the UCINET file supplies them. For one-mode
#' edgelists these labels are also used to preserve isolates that occur in the
#' label list but not in any observed tie. Recent UCINET \code{.##h}/\code{.##d}
#' files are interpreted as weighted matrices when the stored values are not all
#' binary.
#'
#' A few UCINET variants are intentionally still out of scope for now:
#' packed files with multiple matrices, and DL files that use
#' \code{labels embedded:} for \code{format = fullmatrix}. In those cases
#' \code{read_ucinet()} gives an explicit error so it is clear why the import
#' stops.
#'
#' @param file Path to a UCINET file. This can be a DL text file such as
#'   \code{.dl}, or a UCINET header file \code{.##h}. When a header file is
#'   supplied, the matching \code{.##d} file is read automatically from the same
#'   folder.
#' @param graph Output graph class. One of \code{"igraph"}, \code{"network"},
#'   \code{"matrix"}, or \code{"edgelist"}.
#' @param directed Character scalar controlling how one-mode data should be
#'   interpreted. \code{"auto"} infers directedness from the imported data,
#'   \code{"directed"} forces a directed interpretation, and
#'   \code{"undirected"} forces an undirected interpretation. Rectangular
#'   full-matrix data are always treated as bipartite incidence data.
#'
#' @return A graph object in the requested class.
#' @export
#'
#' @examples
#' tmp <- tempfile(fileext = ".dl")
#' writeLines(
#'   c(
#'     "dl n = 3 format = fullmatrix",
#'     "labels:",
#'     "Alice, Bob, Carol",
#'     "data:",
#'     "0 1 0",
#'     "1 0 1",
#'     "0 1 0"
#'   ),
#'   con = tmp
#' )
#' g <- read_ucinet(tmp)
#' snafun::count_vertices(g)
#' unlink(tmp)
read_ucinet <- function(file,
                        graph = c("igraph", "network", "matrix", "edgelist"),
                        directed = c("auto", "directed", "undirected")) {
  graph <- match.arg(graph)
  directed <- match.arg(directed)

  if (length(file) != 1L || is.na(file) || !file.exists(file)) {
    stop("'file' should be the path to an existing UCINET file")
  }

  if (grepl("\\.##h$", tolower(file))) {
    parsed <- parse_ucinet_binary_file(file)
  } else {
    parsed <- parse_ucinet_dl_file(file)
  }

  imported_graph <- build_ucinet_igraph(parsed = parsed, directed = directed)
  imported_graph <- add_ucinet_graph_metadata(imported_graph, parsed)

  if (identical(graph, "igraph")) {
    return(imported_graph)
  }
  if (identical(graph, "network")) {
    return(snafun::to_network(imported_graph))
  }
  if (identical(graph, "matrix")) {
    return(snafun::to_matrix(imported_graph))
  }
  if (identical(graph, "edgelist")) {
    return(snafun::to_edgelist(imported_graph))
  }

  stop("Unsupported 'graph' value in 'read_ucinet()'")
}


#' Parse a UCINET binary header/data pair
#'
#' Read a UCINET \code{.##h}/\code{.##d} pair into the same intermediate format
#' used by the DL parser.
#'
#' @param file Path to the UCINET header file \code{.##h}.
#'
#' @return A parsed representation of the binary UCINET content.
#' @keywords internal
#' @noRd
parse_ucinet_binary_file <- function(file) {
  if (!grepl("\\.##h$", tolower(file))) {
    stop("Binary UCINET input should point to the '.##h' header file")
  }

  data_file <- sub("h$", "d", file)
  if (!file.exists(data_file)) {
    stop(
      "The matching UCINET data file '.##d' was not found next to the supplied '.##h' file"
    )
  }

  header <- read_ucinet_binary_header(file)
  if (!(header$ndim == 2L || (header$ndim == 3L && header$dims[[3]] == 1L))) {
    stop(
      "UCINET files with multiple packed matrices are not yet supported.\n",
      "Please unpack or export the desired matrix separately first."
    )
  }

  data_connection <- file(data_file, "rb")
  on.exit(close(data_connection))
  values <- readBin(
    con = data_connection,
    what = numeric(),
    n = header$dims[[1]] * header$dims[[2]],
    size = 4,
    endian = "little"
  )

  if (length(values) != header$dims[[1]] * header$dims[[2]]) {
    stop(
      "The UCINET '.##d' file ended before the expected number of values was read."
    )
  }

  matrix_data <- matrix(
    data = values,
    nrow = header$dims[[2]],
    ncol = header$dims[[1]],
    byrow = TRUE,
    dimnames = header$dim_labels[c(2, 1)]
  )

  list(
    source = "binary",
    format = "fullmatrix",
    n = NULL,
    nr = nrow(matrix_data),
    nc = ncol(matrix_data),
    labels = NULL,
    row_labels = rownames(matrix_data),
    column_labels = colnames(matrix_data),
    labels_embedded = FALSE,
    diagonal = NULL,
    has_data_section = TRUE,
    data_lines = character(0),
    matrix = matrix_data,
    title = header$title,
    date = header$date
  )
}


#' Read a UCINET binary header
#'
#' Parse the metadata stored in a UCINET \code{.##h} file.
#'
#' @param file Path to a UCINET header file.
#'
#' @return A list with header metadata.
#' @keywords internal
#' @noRd
read_ucinet_binary_header <- function(file) {
  connection <- file(file, "rb")
  on.exit(close(connection))

  # UCINET binary headers start with one leading byte followed by a short
  # version marker such as "V6404" or, for some older files, "DATE:".
  readBin(connection, what = "raw", n = 1, size = 1)
  header_version <- ucinet_raw_to_string(
    readBin(connection, what = "raw", n = 5, size = 1)
  )
  if (!(header_version %in% c("DATE:", "V6404", "6404"))) {
    stop(
      "Unsupported UCINET header type '", header_version,
      "'. Please export a recent UCINET file type first."
    )
  }

  year <- 2000 + readBin(connection, what = integer(), n = 1, size = 2)
  month <- c(
    "Jan", "Feb", "Mar", "Apr", "May", "Jun",
    "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
  )[readBin(connection, what = integer(), n = 1, size = 2)]
  day <- readBin(connection, what = integer(), n = 1, size = 2)
  weekday <- c(
    "Monday", "Tuesday", "Wednesday", "Thursday",
    "Friday", "Saturday", "Sunday"
  )[readBin(connection, what = integer(), n = 1, size = 2)]

  label_type <- readBin(connection, what = integer(), n = 1, size = 2)
  input_datatype <- readBin(connection, what = integer(), n = 1, size = 1)
  ndim <- readBin(connection, what = integer(), n = 1, size = 2)

  dimension_size <- if (grepl("6404$", header_version)) 4 else 2
  dims <- integer(ndim)
  for (dimension_index in seq_len(ndim)) {
    dims[[dimension_index]] <- readBin(
      connection,
      what = integer(),
      n = 1,
      size = dimension_size
    )
  }

  title_length <- readBin(connection, what = integer(), n = 1, size = 1)
  title <- if (title_length > 0L) {
    ucinet_raw_to_string(readBin(connection, what = "raw", n = title_length, size = 1))
  } else {
    ""
  }

  has_labels <- logical(ndim)
  for (dimension_index in seq_len(ndim)) {
    has_labels[[dimension_index]] <- as.logical(
      readBin(connection, what = integer(), n = 1, size = 1)
    )
  }

  dim_labels <- vector("list", length = ndim)
  for (dimension_index in seq_len(ndim)) {
    if (isTRUE(has_labels[[dimension_index]])) {
      labels <- character(dims[[dimension_index]])
      for (label_index in seq_len(dims[[dimension_index]])) {
        label_length <- readBin(connection, what = integer(), n = 1, size = 2)
        labels[[label_index]] <- if (label_length > 0L) {
          ucinet_raw_to_string(
            readBin(connection, what = "raw", n = label_length, size = 1)
          )
        } else {
          ""
        }
      }
      dim_labels[[dimension_index]] <- labels
    } else {
      dim_labels[[dimension_index]] <- as.character(seq_len(dims[[dimension_index]]))
    }
  }

  if (ndim == 3L && dims[[3]] == 1L) {
    ndim <- 2L
    dims <- dims[1:2]
    has_labels <- has_labels[1:2]
    dim_labels <- dim_labels[1:2]
  }

  list(
    header_version = header_version,
    date = paste(weekday, paste(day, month, year, sep = "-")),
    label_type = label_type,
    input_datatype = input_datatype,
    ndim = ndim,
    dims = dims,
    title = title,
    has_labels = has_labels,
    dim_labels = dim_labels
  )
}


#' Convert raw bytes to a UCINET header string
#'
#' @param x Raw vector.
#'
#' @return Character scalar.
#' @keywords internal
#' @noRd
ucinet_raw_to_string <- function(x) {
  if (length(x) == 0L) {
    return("")
  }
  x <- x[x != as.raw(0)]
  if (length(x) == 0L) {
    return("")
  }
  paste(rawToChar(x, multiple = TRUE), collapse = "")
}


#' Parse a UCINET DL file
#'
#' Read and parse a UCINET DL file into an intermediate representation that can
#' subsequently be converted to an \code{igraph} object.
#'
#' @param file Path to a UCINET DL file.
#'
#' @return A parsed representation of the DL content.
#' @keywords internal
#' @noRd
parse_ucinet_dl_file <- function(file) {
  lines <- readLines(file, warn = FALSE, encoding = "UTF-8")
  lines <- trimws(lines)
  lines <- lines[nzchar(lines)]

  if (length(lines) == 0L) {
    stop("The UCINET file appears to be empty")
  }

  section_pattern <- "^(labels( embedded)?|row labels|column labels|col labels|data)\\s*:"
  section_start <- which(grepl(section_pattern, tolower(lines)))[1]
  if (is.na(section_start)) {
    section_start <- length(lines) + 1L
  }

  header_text <- paste(lines[seq_len(section_start - 1L)], collapse = " ")
  if (!grepl("^dl\\b", tolower(header_text))) {
    stop("This does not look like a UCINET DL file: the header should start with 'dl'")
  }

  parsed <- list(
    source = "dl",
    format = parse_ucinet_header_value(header_text, "format"),
    n = parse_ucinet_header_value(header_text, "n"),
    nr = parse_ucinet_header_value(header_text, "nr"),
    nc = parse_ucinet_header_value(header_text, "nc"),
    labels = NULL,
    row_labels = NULL,
    column_labels = NULL,
    labels_embedded = FALSE,
    diagonal = parse_ucinet_header_value(header_text, "diagonal"),
    has_data_section = FALSE,
    data_lines = character(0),
    matrix = NULL,
    title = NULL,
    date = NULL
  )

  if (!is.null(parsed$n)) {
    parsed$nr <- parsed$n
    parsed$nc <- parsed$n
  }

  i <- section_start
  while (i <= length(lines)) {
    current_line <- lines[[i]]
    current_lower <- tolower(current_line)

    if (grepl("^labels embedded\\s*:", current_lower)) {
      parsed$labels_embedded <- TRUE
      remainder <- trimws(sub("^[^:]*:", "", current_line))
      if (nzchar(remainder)) {
        parsed$labels <- c(parsed$labels, parse_ucinet_label_tokens(remainder))
      }
      i <- i + 1L
      next
    }

    if (grepl("^labels\\s*:", current_lower)) {
      section <- collect_ucinet_section(lines = lines, start = i)
      parsed$labels <- parse_ucinet_label_tokens(section$content)
      i <- section[["next_index"]]
      next
    }

    if (grepl("^row labels\\s*:", current_lower)) {
      section <- collect_ucinet_section(lines = lines, start = i)
      parsed$row_labels <- parse_ucinet_label_tokens(section$content)
      i <- section[["next_index"]]
      next
    }

    if (grepl("^(column labels|col labels)\\s*:", current_lower)) {
      section <- collect_ucinet_section(lines = lines, start = i)
      parsed$column_labels <- parse_ucinet_label_tokens(section$content)
      i <- section[["next_index"]]
      next
    }

    if (grepl("^data\\s*:", current_lower)) {
      section <- collect_ucinet_section(lines = lines, start = i)
      parsed$has_data_section <- TRUE
      parsed$data_lines <- section$content
      i <- section[["next_index"]]
      next
    }

    i <- i + 1L
  }

  if (!parsed$has_data_section) {
    stop("Could not find a 'data:' section in the UCINET DL file")
  }
  if (is.null(parsed$format)) {
    stop("Could not determine the UCINET DL 'format=' value from the header")
  }

  parsed$format <- tolower(parsed$format)
  parsed
}


#' Extract a single value from the UCINET DL header
#'
#' @param header_text Collapsed DL header.
#' @param key Header key to extract.
#'
#' @return Character scalar, integer scalar, or \code{NULL}.
#' @keywords internal
#' @noRd
parse_ucinet_header_value <- function(header_text, key) {
  lower_header <- tolower(header_text)
  match <- regexec(
    pattern = paste0("\\b", key, "[[:space:]]*=[[:space:]]*([^[:space:],]+)"),
    text = lower_header
  )
  values <- regmatches(lower_header, match)[[1]]
  if (length(values) < 2L) {
    return(NULL)
  }
  value <- values[[2]]
  if (grepl("^[0-9]+$", value)) {
    return(as.integer(value))
  }
  value
}


#' Collect one named section from a UCINET DL file
#'
#' Gather all lines that belong to one DL section, such as \code{labels:} or
#' \code{data:}, until the next section starts.
#'
#' @param lines Character vector of trimmed, non-empty lines.
#' @param start Index of the section header line.
#'
#' @return A list with the section content and the next unread line index.
#' @keywords internal
#' @noRd
collect_ucinet_section <- function(lines, start) {
  section_pattern <- "^(labels( embedded)?|row labels|column labels|col labels|data)\\s*:"
  header_line <- lines[[start]]
  remainder <- trimws(sub("^[^:]*:", "", header_line))
  content <- character(0)
  if (nzchar(remainder)) {
    content <- c(content, remainder)
  }

  next_line <- start + 1L
  while (next_line <= length(lines) &&
         !grepl(section_pattern, tolower(lines[[next_line]]))) {
    content <- c(content, lines[[next_line]])
    next_line <- next_line + 1L
  }

  list(content = content, next_index = next_line)
}


#' Parse label tokens from a UCINET DL section
#'
#' @param content Character vector with the content of a label section.
#'
#' @return Character vector of labels.
#' @keywords internal
#' @noRd
parse_ucinet_label_tokens <- function(content) {
  content <- trimws(content)
  content <- content[nzchar(content)]
  if (length(content) == 0L) {
    return(character(0))
  }

  if (any(grepl(",", content, fixed = TRUE))) {
    joined <- paste(content, collapse = ",")
    scanner <- textConnection(joined)
    on.exit(close(scanner))
    return(scan(
      file = scanner,
      what = character(),
      sep = ",",
      quiet = TRUE,
      strip.white = TRUE,
      quote = "\"'"
    ))
  }

  if (length(content) > 1L) {
    return(content)
  }

  scanner <- textConnection(content)
  on.exit(close(scanner))
  scan(
    file = scanner,
    what = character(),
    quiet = TRUE,
    strip.white = TRUE,
    quote = "\"'"
  )
}


#' Build an igraph object from parsed UCINET content
#'
#' @param parsed Parsed content from either the DL or binary reader.
#' @param directed Directedness override from \code{read_ucinet()}.
#'
#' @return An \code{igraph} object.
#' @keywords internal
#' @noRd
build_ucinet_igraph <- function(parsed, directed) {
  if (identical(parsed$format, "fullmatrix")) {
    return(build_ucinet_igraph_from_matrix(parsed = parsed, directed = directed))
  }

  if (startsWith(parsed$format, "edgelist")) {
    return(build_ucinet_igraph_from_edgelist(parsed = parsed, directed = directed))
  }

  stop(
    "Unsupported UCINET format '", parsed$format, "'.\n",
    "Currently supported formats are 'fullmatrix' and 'edgelist*'."
  )
}


#' Add UCINET metadata as graph attributes
#'
#' @param graph igraph object
#' @param parsed Parsed UCINET content
#'
#' @return igraph object with metadata attached
#' @keywords internal
#' @noRd
add_ucinet_graph_metadata <- function(graph, parsed) {
  if (!inherits(graph, "igraph")) {
    return(graph)
  }

  igraph::graph_attr(graph, "ucinet_source") <- parsed$source
  igraph::graph_attr(graph, "ucinet_format") <- parsed$format
  if (!is.null(parsed$title) && nzchar(parsed$title)) {
    igraph::graph_attr(graph, "title") <- parsed$title
  }
  if (!is.null(parsed$date) && nzchar(parsed$date)) {
    igraph::graph_attr(graph, "ucinet_date") <- parsed$date
  }
  graph
}


#' Build an igraph object from a UCINET fullmatrix section
#'
#' @param parsed Parsed UCINET content.
#' @param directed Directedness override.
#'
#' @return An \code{igraph} object.
#' @keywords internal
#' @noRd
build_ucinet_igraph_from_matrix <- function(parsed, directed) {
  if (isTRUE(parsed$labels_embedded)) {
    stop("UCINET 'labels embedded:' is not yet supported for 'format = fullmatrix'")
  }

  matrix_data <- extract_ucinet_fullmatrix(parsed)

  if (nrow(matrix_data) != ncol(matrix_data)) {
    return(snafun::to_igraph(matrix_data, bipartite = TRUE))
  }

  if (identical(directed, "undirected")) {
    # For binary data the UCINET-style weak interpretation is appropriate. For
    # weighted data we preserve magnitudes by taking the strongest of the two
    # directed values instead of coercing everything to 0/1.
    if (all(matrix_data %in% c(0, 1))) {
      matrix_data <- snafun::to_symmetric_matrix(matrix_data, rule = "weak")
    } else {
      matrix_data <- snafun::to_symmetric_matrix(matrix_data, rule = "max")
    }
    return(snafun::to_igraph(matrix_data))
  }

  if (identical(directed, "directed")) {
    if (all(matrix_data %in% c(0, 1))) {
      return(igraph::graph_from_adjacency_matrix(
        adjmatrix = matrix_data,
        mode = "directed",
        weighted = NULL,
        diag = TRUE
      ))
    }
    return(igraph::graph_from_adjacency_matrix(
      adjmatrix = matrix_data,
      mode = "directed",
      weighted = TRUE,
      diag = TRUE
    ))
  }

  snafun::to_igraph(matrix_data)
}


#' Extract a full matrix from parsed UCINET content
#'
#' @param parsed Parsed UCINET content.
#'
#' @return Numeric matrix with dimnames when available.
#' @keywords internal
#' @noRd
extract_ucinet_fullmatrix <- function(parsed) {
  if (!is.null(parsed$matrix)) {
    return(parsed$matrix)
  }

  if (is.null(parsed$nr) || is.null(parsed$nc)) {
    stop("UCINET fullmatrix files should specify 'n=' or both 'nr=' and 'nc='")
  }

  if (length(parsed$data_lines) == 0L) {
    stop("The UCINET fullmatrix does not contain any numeric data")
  }

  values <- scan(
    text = paste(parsed$data_lines, collapse = " "),
    what = numeric(),
    quiet = TRUE
  )

  matrix_data <- NULL
  expected_values <- parsed$nr * parsed$nc
  if (!is.null(parsed$diagonal) &&
      identical(tolower(parsed$diagonal), "absent") &&
      parsed$nr == parsed$nc &&
      length(values) == parsed$nr * (parsed$nc - 1L)) {
    matrix_data <- matrix(0, nrow = parsed$nr, ncol = parsed$nc)
    matrix_data[row(matrix_data) != col(matrix_data)] <- values
  } else {
    if (length(values) != expected_values) {
      stop(
        "The UCINET fullmatrix data contain ", length(values),
        " numeric values, but ", expected_values, " were expected from the header"
      )
    }
    matrix_data <- matrix(values, nrow = parsed$nr, ncol = parsed$nc, byrow = TRUE)
  }

  if (is.null(parsed$row_labels) && is.null(parsed$column_labels) && !is.null(parsed$labels)) {
    if (parsed$nr == parsed$nc && length(parsed$labels) == parsed$nr) {
      parsed$row_labels <- parsed$labels
      parsed$column_labels <- parsed$labels
    } else if (length(parsed$labels) == parsed$nr + parsed$nc) {
      parsed$row_labels <- parsed$labels[seq_len(parsed$nr)]
      parsed$column_labels <- parsed$labels[parsed$nr + seq_len(parsed$nc)]
    }
  }

  if (is.null(parsed$row_labels)) {
    parsed$row_labels <- as.character(seq_len(parsed$nr))
  }
  if (is.null(parsed$column_labels)) {
    parsed$column_labels <- as.character(seq_len(parsed$nc))
  }

  if (length(parsed$row_labels) != parsed$nr) {
    stop("The UCINET file supplies a row label vector with the wrong length")
  }
  if (length(parsed$column_labels) != parsed$nc) {
    stop("The UCINET file supplies a column label vector with the wrong length")
  }

  rownames(matrix_data) <- parsed$row_labels
  colnames(matrix_data) <- parsed$column_labels
  matrix_data
}


#' Build an igraph object from a UCINET edgelist section
#'
#' @param parsed Parsed UCINET content.
#' @param directed Directedness override.
#'
#' @return An \code{igraph} object.
#' @keywords internal
#' @noRd
build_ucinet_igraph_from_edgelist <- function(parsed, directed) {
  if (!is.null(parsed$row_labels) || !is.null(parsed$column_labels)) {
    stop("Separate row/column labels are not yet supported for UCINET edgelist formats")
  }

  if (length(parsed$data_lines) == 0L) {
    vertices <- NULL
    if (!is.null(parsed$labels) && length(parsed$labels) > 0L) {
      vertices <- data.frame(name = parsed$labels, stringsAsFactors = FALSE)
    }
    return(snafun::to_igraph(
      x = data.frame(from = character(0), to = character(0), stringsAsFactors = FALSE),
      vertices = vertices,
      directed = if (identical(directed, "auto")) NULL else identical(directed, "directed")
    ))
  }

  normalized_lines <- gsub(",", " ", parsed$data_lines, fixed = TRUE)
  edgelist <- utils::read.table(
    text = paste(normalized_lines, collapse = "\n"),
    header = FALSE,
    stringsAsFactors = FALSE,
    fill = TRUE,
    quote = "\"'"
  )

  if (ncol(edgelist) < 2L) {
    stop("UCINET edgelist data should contain at least two columns")
  }

  edgelist[] <- lapply(edgelist, utils::type.convert, as.is = TRUE)
  colnames(edgelist)[1:2] <- c("from", "to")
  if (ncol(edgelist) >= 3L && is.numeric(edgelist[[3]])) {
    colnames(edgelist)[3] <- "weight"
  }

  vertices <- NULL
  if (!is.null(parsed$labels) && length(parsed$labels) > 0L) {
    vertices <- data.frame(name = parsed$labels, stringsAsFactors = FALSE)
    edgelist <- remap_ucinet_edgelist_ids(edgelist, vertices)
  }

  directed_argument <- if (identical(directed, "auto")) NULL else identical(directed, "directed")
  snafun::to_igraph(
    x = edgelist,
    vertices = vertices,
    directed = directed_argument
  )
}


#' Remap numeric UCINET edge ids to vertex labels
#'
#' @param edgelist data.frame with at least two columns
#' @param vertices data.frame with a first column named \code{name}
#'
#' @return data.frame with remapped endpoint columns when necessary
#' @keywords internal
#' @noRd
remap_ucinet_edgelist_ids <- function(edgelist, vertices) {
  if (nrow(edgelist) == 0L) {
    return(edgelist)
  }

  first_two_numeric <- vapply(
    edgelist[1:2],
    function(one_column) is.numeric(one_column),
    FUN.VALUE = logical(1)
  )
  if (!all(first_two_numeric)) {
    return(edgelist)
  }

  from_id <- as.integer(edgelist$from)
  to_id <- as.integer(edgelist$to)
  if (any(is.na(from_id)) || any(is.na(to_id))) {
    return(edgelist)
  }
  if (any(from_id < 1L | from_id > nrow(vertices)) ||
      any(to_id < 1L | to_id > nrow(vertices))) {
    stop("The UCINET edgelist refers to vertex ids outside the supplied label list")
  }

  edgelist$from <- vertices$name[from_id]
  edgelist$to <- vertices$name[to_id]
  edgelist
}

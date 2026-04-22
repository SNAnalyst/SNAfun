report_side_effects()


canonical_membership_partition <- function(x) {
  x <- data.frame(x, stringsAsFactors = FALSE)
  x$vertex <- as.character(x$vertex)
  x$component <- as.integer(x$component)
  x <- x[order(x$vertex), , drop = FALSE]
  parts <- split(x$vertex, x$component)
  parts <- lapply(parts, sort)
  ordering <- order(vapply(parts, paste, character(1), collapse = "\r"))
  unname(parts[ordering])
}


canonical_bridge_pairs <- function(x) {
  if (is.null(x) || nrow(x) == 0) {
    return(character(0))
  }
  x <- data.frame(x, stringsAsFactors = FALSE)
  from <- as.character(x[[1]])
  to <- as.character(x[[2]])
  sort(paste(from, to, sep = "->"))
}


component_reference_from_matrix <- function(mat, directed, type) {
  graph <- igraph::graph_from_adjacency_matrix(
    adjmatrix = mat,
    mode = if (directed) "directed" else "undirected",
    weighted = NULL,
    diag = TRUE
  )
  membership <- igraph::components(graph, mode = type)$membership
  data.frame(
    vertex = rownames(mat),
    component = membership,
    stringsAsFactors = FALSE
  )
}


make_component_audit_graphs <- function(mat, directed) {
  ig <- igraph::graph_from_adjacency_matrix(
    adjmatrix = mat,
    mode = if (directed) "directed" else "undirected",
    weighted = NULL,
    diag = TRUE
  )
  nw <- network::as.network.matrix(
    x = mat,
    directed = directed,
    matrix.type = "adjacency",
    ignore.eval = FALSE,
    names.eval = "weight"
  )
  el <- snafun::to_edgelist(ig)
  list(igraph = ig, network = nw, matrix = mat, edgelist = el)
}


set.seed(20260421)
for (directed in c(FALSE, TRUE)) {
  for (allow_loops in c(FALSE, TRUE)) {
    for (weighted in c(FALSE, TRUE)) {
      for (sim in seq_len(60)) {
        n <- sample(4:10, size = 1)
        mat <- matrix(
          stats::rbinom(n * n, size = 1, prob = stats::runif(1, 0.05, 0.35)),
          nrow = n
        )
        
        if (!directed) {
          mat[lower.tri(mat)] <- t(mat)[lower.tri(mat)]
        }
        
        if (!allow_loops) {
          diag(mat) <- 0
        }
        
        if (weighted) {
          nonzero <- which(mat != 0)
          if (length(nonzero) > 0) {
            mat[nonzero] <- sample(2:9, size = length(nonzero), replace = TRUE)
          }
        }
        
        dimnames(mat) <- list(paste0("v", seq_len(n)), paste0("v", seq_len(n)))
        binary_mat <- mat
        binary_mat[binary_mat != 0] <- 1
        
        graphs <- make_component_audit_graphs(binary_mat, directed = directed)
        
        types <- if (directed) c("weak", "strong") else "weak"
        for (one_type in types) {
          expected_membership <- component_reference_from_matrix(
            mat = binary_mat,
            directed = directed,
            type = one_type
          )
          expected_partition <- canonical_membership_partition(expected_membership)
          expected_count <- length(expected_partition)
          
          for (graph in graphs) {
            actual_membership <- snafun::extract_component_membership(graph, type = one_type)
            actual_partition <- canonical_membership_partition(actual_membership)
            expect_identical(actual_partition, expected_partition)
            expect_equal(snafun::count_components(graph, type = one_type), expected_count)
            
            extracted_components <- snafun::extract_components(graph, type = one_type)
            expect_equal(length(extracted_components), expected_count)
            
            actual_sizes <- sort(vapply(extracted_components, function(one_component) {
              if (is.matrix(one_component)) {
                return(nrow(one_component))
              }
              if (is.data.frame(one_component)) {
                return(nrow(attr(one_component, "snafun_vertices", exact = TRUE)))
              }
              snafun::count_vertices(one_component)
            }, integer(1)))
            expected_sizes <- sort(vapply(expected_partition, length, integer(1)))
            expect_identical(unname(actual_sizes), unname(expected_sizes))
          }
        }
        
        expected_cut <- sort(as.character(rownames(binary_mat)[as.integer(igraph::articulation_points(graphs$igraph))]))
        expected_bridges <- canonical_bridge_pairs(igraph::as_data_frame(graphs$igraph, what = "edges")[igraph::bridges(graphs$igraph), 1:2, drop = FALSE])
        
        for (graph in graphs) {
          actual_cut <- snafun::extract_cut_vertices(graph)
          if (length(expected_cut) == 0) {
            expect_null(actual_cut)
          } else {
            expect_identical(sort(as.character(actual_cut)), expected_cut)
          }
          
          actual_bridges <- snafun::extract_bridges(graph)
          if (length(expected_bridges) == 0) {
            expect_null(actual_bridges)
          } else {
            expect_identical(canonical_bridge_pairs(actual_bridges), expected_bridges)
          }
        }
      }
    }
  }
}

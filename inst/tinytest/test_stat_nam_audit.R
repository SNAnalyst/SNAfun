report_side_effects()


standardize_rows_stat_nam <- function(W) {
  row_sums <- base::rowSums(W)
  out <- W
  positive_rows <- row_sums > 0
  out[positive_rows, ] <- out[positive_rows, , drop = FALSE] / row_sums[positive_rows]
  out
}


scale_non_zero_rows_stat_nam <- function(W) {
  out <- W
  positive_rows <- base::rowSums(W) > 0
  if (!any(positive_rows)) {
    return(out)
  }

  # Different row multipliers make sure the preprocessing really performs its
  # own normalization instead of accidentally depending on an already
  # standardized matrix.
  row_multipliers <- seq(1.5, by = 0.25, length.out = sum(positive_rows))
  out[positive_rows, ] <- out[positive_rows, , drop = FALSE] * row_multipliers
  out
}


make_weight_input_stat_nam <- function(W, input_type) {
  switch(
    input_type,
    matrix = W,
    igraph = snafun::to_igraph(W),
    network = snafun::to_network(W),
    data.frame = snafun::to_edgelist(W)
  )
}


fit_spatialreg_reference_stat_nam <- function(data, W, W2 = NULL, model) {
  listw_1 <- suppressWarnings(
    spdep::mat2listw(W, style = "W", zero.policy = TRUE)
  )
  listw_2 <- if (!is.null(W2)) {
    suppressWarnings(
      spdep::mat2listw(W2, style = "W", zero.policy = TRUE)
    )
  } else {
    NULL
  }

  suppressWarnings(
    if (identical(model, "lag")) {
      spatialreg::lagsarlm(y ~ x1 + grp,
                           data = data,
                           listw = listw_1,
                           quiet = TRUE,
                           zero.policy = TRUE)
    } else if (identical(model, "error")) {
      spatialreg::errorsarlm(y ~ x1 + grp,
                             data = data,
                             listw = listw_1,
                             quiet = TRUE,
                             zero.policy = TRUE)
    } else {
      spatialreg::sacsarlm(y ~ x1 + grp,
                           data = data,
                           listw = listw_1,
                           listw2 = listw_2,
                           quiet = TRUE,
                           zero.policy = TRUE)
    }
  )
}


fit_stat_nam_audit <- function(data, W, W2 = NULL, model, input_type) {
  W_input <- make_weight_input_stat_nam(W, input_type = input_type)
  W2_input <- if (is.null(W2)) NULL else make_weight_input_stat_nam(W2, input_type = input_type)

  suppressMessages(
    suppressWarnings(
      if (identical(model, "combined")) {
        snafun::stat_nam(y ~ x1 + grp,
                         data = data,
                         W = W_input,
                         W2 = W2_input,
                         model = model,
                         quiet = TRUE,
                         zero.policy = TRUE)
      } else {
        snafun::stat_nam(y ~ x1 + grp,
                         data = data,
                         W = W_input,
                         model = model,
                         quiet = TRUE,
                         zero.policy = TRUE)
      }
    )
  )
}


expect_spatialreg_equivalent_stat_nam <- function(actual, reference, model,
                                                  tolerance_beta = 1e-5,
                                                  tolerance_dependence = 1e-5) {
  expect_true(
    max(abs(as.numeric(actual$coefficients) -
              as.numeric(reference$coefficients))) <= tolerance_beta
  )

  if (identical(model, "lag")) {
    expect_true(
      abs(as.numeric(actual$rho) - as.numeric(reference$rho)) <=
        tolerance_dependence
    )
  } else if (identical(model, "error")) {
    expect_true(
      abs(as.numeric(actual$lambda) - as.numeric(reference$lambda)) <=
        tolerance_dependence
    )
  } else {
    expect_true(
      abs(as.numeric(actual$rho) - as.numeric(reference$rho)) <=
        tolerance_dependence
    )
    expect_true(
      abs(as.numeric(actual$lambda) - as.numeric(reference$lambda)) <=
        tolerance_dependence
    )
  }
}


make_stat_nam_audit_dataset <- function(W, W2, model, seed) {
  n <- nrow(W)
  set.seed(seed)
  data <- data.frame(
    x1 = stats::rnorm(n),
    grp = factor(rep(c("a", "b"), length.out = n))
  )
  design_matrix <- stats::model.matrix(~ x1 + grp, data = data)
  beta <- c(0.4, -0.2, 0.3)

  if (identical(model, "lag")) {
    disturbances <- stats::rnorm(n, sd = 0.15)
    data$y <- as.numeric(
      solve(diag(n) - 0.12 * W,
            design_matrix %*% beta + disturbances)
    )
  } else if (identical(model, "error")) {
    disturbances <- stats::rnorm(n, sd = 0.15)
    data$y <- as.numeric(
      design_matrix %*% beta +
        solve(diag(n) - 0.08 * W, disturbances)
    )
  } else {
    disturbances <- stats::rnorm(n, sd = 0.15)
    data$y <- as.numeric(
      solve(
        diag(n) - 0.12 * W,
        design_matrix %*% beta +
          solve(diag(n) - 0.08 * W2, disturbances)
      )
    )
  }

  data
}


weight_cases_stat_nam <- list(
  list(
    name = "undirected_binary",
    W = matrix(
      c(
        0, 1, 1, 0, 0, 0,
        1, 0, 1, 1, 0, 0,
        1, 1, 0, 0, 1, 0,
        0, 1, 0, 0, 1, 1,
        0, 0, 1, 1, 0, 1,
        0, 0, 0, 1, 1, 0
      ),
      nrow = 6,
      byrow = TRUE
    )
  ),
  list(
    name = "undirected_weighted",
    W = matrix(
      c(
        0.0, 2.0, 1.0, 0.0, 0.0, 0.0,
        2.0, 0.0, 0.5, 1.5, 0.0, 0.0,
        1.0, 0.5, 0.0, 0.0, 1.5, 0.0,
        0.0, 1.5, 0.0, 0.0, 1.0, 2.0,
        0.0, 0.0, 1.5, 1.0, 0.0, 0.5,
        0.0, 0.0, 0.0, 2.0, 0.5, 0.0
      ),
      nrow = 6,
      byrow = TRUE
    )
  ),
  list(
    name = "directed_binary",
    W = matrix(
      c(
        0, 1, 1, 0, 0, 0,
        0, 0, 1, 1, 0, 0,
        1, 0, 0, 1, 1, 0,
        0, 0, 0, 0, 1, 1,
        1, 0, 0, 0, 0, 1,
        0, 1, 0, 0, 0, 0
      ),
      nrow = 6,
      byrow = TRUE
    )
  ),
  list(
    name = "directed_weighted",
    W = matrix(
      c(
        0.0, 2.0, 1.0, 0.0, 0.0, 0.0,
        0.0, 0.0, 1.5, 0.5, 0.0, 0.0,
        1.0, 0.0, 0.0, 1.0, 2.0, 0.0,
        0.0, 0.0, 0.0, 0.0, 1.5, 1.0,
        0.5, 0.0, 0.0, 0.0, 0.0, 2.0,
        0.0, 1.0, 0.0, 0.0, 0.0, 0.0
      ),
      nrow = 6,
      byrow = TRUE
    )
  ),
  list(
    name = "directed_weighted_loops",
    W = matrix(
      c(
        0.4, 2.0, 1.0, 0.0, 0.0, 0.0,
        0.0, 0.2, 1.5, 0.5, 0.0, 0.0,
        1.0, 0.0, 0.3, 1.0, 2.0, 0.0,
        0.0, 0.0, 0.0, 0.1, 1.5, 1.0,
        0.5, 0.0, 0.0, 0.0, 0.6, 2.0,
        0.0, 1.0, 0.0, 0.0, 0.0, 0.5
      ),
      nrow = 6,
      byrow = TRUE
    )
  ),
  list(
    name = "undirected_weighted_isolate",
    W = matrix(
      c(
        0.0, 1.5, 1.0, 0.0, 0.0, 0.0,
        1.5, 0.0, 0.5, 1.0, 0.0, 0.0,
        1.0, 0.5, 0.0, 0.0, 1.5, 0.0,
        0.0, 1.0, 0.0, 0.0, 2.0, 0.0,
        0.0, 0.0, 1.5, 2.0, 0.0, 0.0,
        0.0, 0.0, 0.0, 0.0, 0.0, 0.0
      ),
      nrow = 6,
      byrow = TRUE
    )
  )
)


weight_case_two_stat_nam <- lapply(weight_cases_stat_nam, function(one_case) {
  W <- one_case$W
  one_case$W2 <- t(W) + diag(diag(W)) * 0.5
  one_case$W2 <- standardize_rows_stat_nam(one_case$W2)
  one_case
})


input_types_stat_nam <- c("matrix", "igraph", "network", "data.frame")


for (case_index in seq_along(weight_case_two_stat_nam)) {
  one_case <- weight_case_two_stat_nam[[case_index]]

  for (one_model in c("lag", "error", "combined")) {
    audit_data <- make_stat_nam_audit_dataset(
      W = standardize_rows_stat_nam(one_case$W),
      W2 = standardize_rows_stat_nam(one_case$W2),
      model = one_model,
      seed = 200 + case_index
    )

    reference_fit <- fit_spatialreg_reference_stat_nam(
      data = audit_data,
      W = standardize_rows_stat_nam(one_case$W),
      W2 = standardize_rows_stat_nam(one_case$W2),
      model = one_model
    )

    for (one_input_type in input_types_stat_nam) {
      actual_fit <- fit_stat_nam_audit(
        data = audit_data,
        W = scale_non_zero_rows_stat_nam(one_case$W),
        W2 = scale_non_zero_rows_stat_nam(one_case$W2),
        model = one_model,
        input_type = one_input_type
      )

      expect_spatialreg_equivalent_stat_nam(
        actual = actual_fit,
        reference = reference_fit,
        model = one_model
      )
    }
  }
}

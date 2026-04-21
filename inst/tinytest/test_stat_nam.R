report_side_effects()

data(huk, package = "SNA4DSData")


manual_row_standardize <- function(W) {
  row_sums <- base::rowSums(W)
  out <- W
  positive_rows <- row_sums > 0
  out[positive_rows, ] <- out[positive_rows, , drop = FALSE] / row_sums[positive_rows]
  out
}


make_weight_input <- function(W, input_type) {
  switch(
    input_type,
    matrix = W,
    igraph = snafun::to_igraph(W),
    network = snafun::to_network(W),
    data.frame = snafun::to_edgelist(W)
  )
}


make_handcrafted_nam_fixture <- function() {
  # These matrices exercise the tricky preprocessing path on purpose: they are
  # weighted, directed, include diagonal values, and contain one isolate row.
  # They are nevertheless well-behaved enough for spatialreg and sna::lnam to
  # agree closely, so they make good regression fixtures.
  W1 <- matrix(
    c(
      0.3, 0.7, 0.0, 0.0, 0.0, 0.0,
      0.4, 0.0, 0.6, 0.0, 0.0, 0.0,
      0.0, 0.5, 0.2, 0.3, 0.0, 0.0,
      0.0, 0.0, 0.5, 0.0, 0.5, 0.0,
      0.0, 0.0, 0.0, 0.8, 0.2, 0.0,
      0.0, 0.0, 0.0, 0.0, 0.0, 0.0
    ),
    nrow = 6,
    byrow = TRUE
  )
  W2 <- matrix(
    c(
      0.2, 0.8, 0.0, 0.0, 0.0, 0.0,
      0.3, 0.1, 0.6, 0.0, 0.0, 0.0,
      0.0, 0.4, 0.3, 0.3, 0.0, 0.0,
      0.0, 0.0, 0.6, 0.1, 0.3, 0.0,
      0.0, 0.0, 0.0, 0.7, 0.3, 0.0,
      0.0, 0.0, 0.0, 0.0, 0.0, 0.0
    ),
    nrow = 6,
    byrow = TRUE
  )

  set.seed(123)
  data_frame <- data.frame(
    x1 = stats::rnorm(6),
    grp = factor(c("a", "b", "a", "b", "a", "b"))
  )
  design_matrix <- stats::model.matrix(~ x1 + grp, data = data_frame)

  lag_errors <- stats::rnorm(6, sd = 0.2)
  error_errors <- stats::rnorm(6, sd = 0.2)
  combined_errors <- stats::rnorm(6, sd = 0.2)

  y_lag <- as.numeric(
    solve(diag(6) - 0.15 * W1,
          design_matrix %*% c(0.5, 0.2, -0.3) + lag_errors)
  )
  y_error <- as.numeric(
    design_matrix %*% c(0.5, -0.1, 0.3) +
      solve(diag(6) - 0.1 * W1, error_errors)
  )
  y_combined <- as.numeric(
    solve(
      diag(6) - 0.15 * W1,
      design_matrix %*% c(0.5, 0.2, -0.3) +
        solve(diag(6) - 0.1 * W2, combined_errors)
    )
  )

  list(
    W1 = W1,
    W2 = W2,
    lag = transform(data_frame, y = y_lag),
    error = transform(data_frame, y = y_error),
    combined = transform(data_frame, y = y_combined),
    design_matrix = design_matrix
  )
}


fit_lnam_reference <- function(data, W, W2 = NULL, model) {
  design_matrix <- stats::model.matrix(y ~ x1 + grp, data = data)

  suppressWarnings(
    if (identical(model, "lag")) {
      sna::lnam(y = data$y,
                x = design_matrix,
                W1 = manual_row_standardize(W))
    } else if (identical(model, "error")) {
      sna::lnam(y = data$y,
                x = design_matrix,
                W2 = manual_row_standardize(W))
    } else {
      sna::lnam(y = data$y,
                x = design_matrix,
                W1 = manual_row_standardize(W),
                W2 = manual_row_standardize(W2))
    }
  )
}


fit_spatialreg_reference <- function(data, W, W2 = NULL, model) {
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


fit_stat_nam <- function(data, W, W2 = NULL, model, input_type = "matrix",
                         check_vars = TRUE) {
  W_input <- make_weight_input(W, input_type = input_type)
  W2_input <- if (is.null(W2)) NULL else make_weight_input(W2, input_type = input_type)

  suppressMessages(
    suppressWarnings(
      if (identical(model, "combined")) {
        snafun::stat_nam(y ~ x1 + grp,
                         data = data,
                         W = W_input,
                         W2 = W2_input,
                         model = model,
                         quiet = TRUE,
                         zero.policy = TRUE,
                         check_vars = check_vars)
      } else {
        snafun::stat_nam(y ~ x1 + grp,
                         data = data,
                         W = W_input,
                         model = model,
                         quiet = TRUE,
                         zero.policy = TRUE,
                         check_vars = check_vars)
      }
    )
  )
}


expect_nam_close <- function(actual, reference, model,
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
    expect_null(actual$lambda)
  } else if (identical(model, "error")) {
    expect_true(
      abs(as.numeric(actual$lambda) - as.numeric(reference$lambda)) <=
        tolerance_dependence
    )
    expect_null(actual$rho)
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


expect_lnam_close <- function(actual, reference, model,
                              tolerance_beta = 1e-5,
                              tolerance_dependence = 5e-5) {
  expect_true(
    max(abs(as.numeric(actual$coefficients) -
              as.numeric(reference$beta))) <= tolerance_beta
  )

  if (identical(model, "lag")) {
    expect_true(
      abs(as.numeric(actual$rho) - as.numeric(reference$rho1)) <=
        tolerance_dependence
    )
  } else if (identical(model, "error")) {
    expect_true(
      abs(as.numeric(actual$lambda) - as.numeric(reference$rho2)) <=
        tolerance_dependence
    )
  } else {
    expect_true(
      abs(as.numeric(actual$rho) - as.numeric(reference$rho1)) <=
        tolerance_dependence
    )
    expect_true(
      abs(as.numeric(actual$lambda) - as.numeric(reference$rho2)) <=
        tolerance_dependence
    )
  }
}


fixture <- make_handcrafted_nam_fixture()
input_types <- c("matrix", "igraph", "network", "data.frame")


# huk remains a useful sanity check because it is the historical example that
# motivated the wrapper in the first place.
nam_lag_huk <- suppressMessages(
  snafun::stat_nam(y ~ ., data = hukYX, W = hukWstd, model = "lag")
)
nam_error_huk <- suppressMessages(
  snafun::stat_nam(y ~ ., data = hukYX, W = hukWstd, model = "error")
)
nam_combined_huk <- suppressMessages(
  snafun::stat_nam(y ~ ., data = hukYX, W = hukWstd, W2 = hukWstd, model = "combined")
)

expect_equivalent(nam_lag_huk$rho, 0.5581997, tolerance = 0.001)
expect_null(nam_lag_huk$lambda)

expect_equivalent(nam_error_huk$lambda, 0.6593457, tolerance = 0.001)
expect_null(nam_error_huk$rho)

expect_equivalent(nam_combined_huk$rho, 0.6168094, tolerance = 0.001)
expect_equivalent(nam_combined_huk$lambda, -0.453029, tolerance = 0.001)

expect_equal(names(nam_lag_huk$coefficients),
             c("(Intercept)", "PFMP", "POWN", "PSGR", "PMNT", "PSWP"))
expect_equal(names(nam_error_huk$coefficients),
             c("(Intercept)", "PFMP", "POWN", "PSGR", "PMNT", "PSWP"))
expect_equal(names(nam_combined_huk$coefficients),
             c("(Intercept)", "PFMP", "POWN", "PSGR", "PMNT", "PSWP"))


huk_inputs <- lapply(input_types, function(one_input_type) {
  list(
    lag = suppressMessages(
      snafun::stat_nam(
        y ~ .,
        data = hukYX,
        W = make_weight_input(hukWstd, one_input_type),
        model = "lag"
      )
    ),
    error = suppressMessages(
      snafun::stat_nam(
        y ~ .,
        data = hukYX,
        W = make_weight_input(hukWstd, one_input_type),
        model = "error"
      )
    ),
    combined = suppressMessages(
      snafun::stat_nam(
        y ~ .,
        data = hukYX,
        W = make_weight_input(hukWstd, one_input_type),
        W2 = make_weight_input(hukWstd, one_input_type),
        model = "combined"
      )
    )
  )
})
names(huk_inputs) <- input_types

for (one_input_type in input_types) {
  expect_equal(as.numeric(huk_inputs[[one_input_type]]$lag$coefficients),
               as.numeric(nam_lag_huk$coefficients),
               tolerance = 1e-5)
  expect_equal(as.numeric(huk_inputs[[one_input_type]]$lag$rho),
               as.numeric(nam_lag_huk$rho),
               tolerance = 1e-5)

  expect_equal(as.numeric(huk_inputs[[one_input_type]]$error$coefficients),
               as.numeric(nam_error_huk$coefficients),
               tolerance = 1e-5)
  expect_equal(as.numeric(huk_inputs[[one_input_type]]$error$lambda),
               as.numeric(nam_error_huk$lambda),
               tolerance = 1e-5)

  expect_equal(as.numeric(huk_inputs[[one_input_type]]$combined$coefficients),
               as.numeric(nam_combined_huk$coefficients),
               tolerance = 1e-5)
  expect_equal(as.numeric(huk_inputs[[one_input_type]]$combined$rho),
               as.numeric(nam_combined_huk$rho),
               tolerance = 1e-5)
  expect_equal(as.numeric(huk_inputs[[one_input_type]]$combined$lambda),
               as.numeric(nam_combined_huk$lambda),
               tolerance = 1e-5)
}


# The handcrafted fixture exercises weighted, directed matrices with a diagonal,
# an isolate, and a factor predictor on the rhs. For these stable cases,
# stat_nam should agree with both direct spatialreg calls and sna::lnam.
for (one_model in c("lag", "error", "combined")) {
  reference_data <- fixture[[one_model]]
  reference_lnam <- fit_lnam_reference(reference_data,
                                       W = fixture$W1,
                                       W2 = fixture$W2,
                                       model = one_model)
  reference_spatialreg <- fit_spatialreg_reference(reference_data,
                                                   W = fixture$W1,
                                                   W2 = fixture$W2,
                                                   model = one_model)

  for (one_input_type in input_types) {
    fitted_model <- fit_stat_nam(reference_data,
                                 W = fixture$W1,
                                 W2 = fixture$W2,
                                 model = one_model,
                                 input_type = one_input_type)
    expect_nam_close(fitted_model, reference_spatialreg, one_model)
    expect_lnam_close(fitted_model, reference_lnam, one_model)
  }
}


# Row-standardization messages should only appear when a non-zero row is
# genuinely off scale. Zero rows are legitimate when zero.policy = TRUE.
under_normalized_W1 <- fixture$W1
positive_rows_W1 <- base::rowSums(under_normalized_W1) > 0
under_normalized_W1[positive_rows_W1, ] <- under_normalized_W1[positive_rows_W1, , drop = FALSE] * 0.5

expect_message(
  suppressWarnings(
    snafun::stat_nam(y ~ x1 + grp,
                     data = fixture$lag,
                     W = under_normalized_W1,
                     model = "lag",
                     quiet = TRUE,
                     zero.policy = TRUE)
  ),
  pattern = "not row-normalized"
)


# zero.policy now reaches mat2listw(), so isolate rows should no longer trigger
# the old "no-neighbour observations found" warning.
lag_with_zero_row_warnings <- snafun::withWarnings(
  suppressMessages(
    snafun::stat_nam(y ~ x1 + grp,
                     data = fixture$lag,
                     W = fixture$W1,
                     model = "lag",
                     quiet = TRUE,
                     zero.policy = TRUE)
  )
)
warning_messages <- if (is.null(lag_with_zero_row_warnings$warnings)) {
  character(0)
} else {
  vapply(lag_with_zero_row_warnings$warnings,
         conditionMessage,
         character(1))
}
expect_false(any(grepl("no-neighbour observations found", warning_messages, fixed = TRUE)))


# check_vars should be robust for factors and warn on actual constant design
# columns rather than erroring on factor predictors.
constant_fixture <- fixture$lag
constant_fixture$constant_x <- 1

expect_warning(
  suppressMessages(
    snafun::stat_nam(y ~ x1 + grp + constant_x,
                     data = constant_fixture,
                     W = fixture$W1,
                     model = "lag",
                     quiet = TRUE,
                     zero.policy = TRUE)
  ),
  pattern = "constant design-matrix column"
)


expect_error(
  snafun::stat_nam(y ~ x1 + grp,
                   data = fixture$lag,
                   W = fixture$W1,
                   model = "combined",
                   quiet = TRUE,
                   zero.policy = TRUE),
  pattern = "need to supply 'W2'"
)

expect_error(
  snafun::stat_nam(y ~ x1 + grp,
                   data = fixture$lag,
                   W = fixture$W1[-1, -1],
                   model = "lag",
                   quiet = TRUE,
                   zero.policy = TRUE),
  pattern = "matching the number of observations"
)

na_weight_matrix <- fixture$W1
na_weight_matrix[1, 1] <- NA_real_

expect_error(
  snafun::stat_nam(y ~ x1 + grp,
                   data = fixture$lag,
                   W = na_weight_matrix,
                   model = "lag",
                   quiet = TRUE,
                   zero.policy = TRUE),
  pattern = "should not contain missing values"
)

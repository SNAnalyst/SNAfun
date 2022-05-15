
report_side_effects()



### withWarnings ----
## no warnings generated
uit <- withWarnings(sum(c(1:3)))
expect_null(uit$warnings)
expect_inherits(uit$warnings, "NULL")
expect_inherits(uit$value, "integer")
expect_equal(uit$value, 6L)
expect_equal(grepl("testit_warning", uit$warnings[[1]]), logical(0))
expect_true(length(grepl("testit_warning", uit$warnings[[1]])) == 0)

uit <- withWarnings(sum(c(1:3, NA)))
expect_null(uit$warnings)
expect_inherits(uit$warnings, "NULL")
expect_inherits(uit$value, "integer")
expect_equal(uit$value, NA_integer_)

## warning generated
testit <- function(x) {
    warning("testit_warning")
    sum(x)
}

expect_silent(withWarnings(testit(c(1:3)))) # indeed, no warning shown 
uit <- withWarnings(testit(1:3))
expect_equal(length(uit$warnings), 1)
expect_inherits(uit$warnings, "list")
expect_inherits(uit$warnings[[1]], "simpleWarning")
expect_inherits(uit$warnings[[1]], "warning")
expect_inherits(uit$warnings[[1]], "condition")
expect_inherits(uit$value, "integer")
expect_equal(uit$value, 6L)
# check whether that specific warning was triggered
expect_true(grepl("testit_warning", uit$warnings[[1]]))

uit <- withWarnings(testit(c(1:3, NA)))
expect_equal(length(uit$warnings), 1)
expect_inherits(uit$warnings, "list")
expect_inherits(uit$warnings[[1]], "simpleWarning")
expect_inherits(uit$warnings[[1]], "warning")
expect_inherits(uit$warnings[[1]], "condition")
expect_inherits(uit$value, "integer")
expect_equal(uit$value, NA_integer_)
expect_true(grepl("testit_warning", uit$warnings[[1]]))

# now generate two warnings
testit2 <- function(x) {
  warning("testit_warning_2")
  testit(x)
  sum(x)
}

uit <- withWarnings(testit2(c(1:3)))
expect_silent(withWarnings(testit2(c(1:3))))
expect_equal(length(uit$warnings), 2) #2 warnings
expect_inherits(uit$warnings, "list")
expect_inherits(uit$warnings[[1]], "simpleWarning")
expect_inherits(uit$warnings[[1]], "warning")
expect_inherits(uit$warnings[[1]], "condition")
expect_inherits(uit$warnings[[2]], "simpleWarning")
expect_inherits(uit$warnings[[2]], "warning")
expect_inherits(uit$warnings[[2]], "condition")
expect_inherits(uit$value, "integer")
expect_equal(uit$value, 6)
expect_true(grepl("testit_warning_2", uit$warnings[[1]])) # the first one triggered
expect_true(grepl("testit_warning", uit$warnings[[2]]))

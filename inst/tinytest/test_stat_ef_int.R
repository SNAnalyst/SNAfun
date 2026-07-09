data(florentine, package = "SNA4DSData")
fflom <- florentine$flomarriage
flom <- snafun::to_network(fflom)
m <- ergm::ergm(flom ~ edges + nodecov("Wealth"))

probtest <- snafun::stat_ef_int(m, "prob")



expect_equal(class(probtest), "data.frame")
expect_equal(probtest[,1], c(-2.595, 0.011))
expect_equal(probtest[,2], c(0.069, 0.503))
expect_equal(probtest[,3], c(0.536, 0.005))
expect_equal(probtest[,4], c(0.000, 0.024))
expect_equal(rownames(probtest), c("edges", "nodecov.Wealth"))
expect_equal(colnames(probtest), c("Estimate", "Prob", "Std.Error", "Pval"))


################################################################################
oddstest <- snafun::stat_ef_int(m, "odds")


expect_equal(class(oddstest), "data.frame")
expect_equal(oddstest[,1], c(-2.595, 0.011))
expect_equal(oddstest[,2], c(0.075, 1.011))
expect_equal(oddstest[,3], c(0.536, 0.005))
expect_equal(oddstest[,4], c(0.000, 0.024))
expect_equal(rownames(oddstest), c("edges", "nodecov.Wealth"))
expect_equal(colnames(oddstest), c("Estimate", "Odds", "Std.Error", "Pval"))


################################################################################
# The input check uses inherits(), not class(m) != "ergm". A class attribute may
# have more than one element, and `!=` then compares element-wise, so if() dies
# with "the condition has length > 1" instead of saying what is actually wrong.

# a single-class non-ergm object: the intended message
expect_error(snafun::stat_ef_int(structure(list(), class = "lm")),
             pattern = "not class ergm")

# a multi-class non-ergm object: the same message, not a condition-length error
err <- tryCatch(snafun::stat_ef_int(structure(list(), class = c("lm", "glm"))),
                error = conditionMessage)
expect_true(grepl("not class ergm", err, fixed = TRUE))
expect_false(grepl("condition has length", err, fixed = TRUE))

# an object that inherits from ergm must get past the guard, whatever it does
# afterwards. It must not be rejected, and it must not trip the length check.
err <- tryCatch(snafun::stat_ef_int(structure(list(), class = c("btergm", "ergm"))),
                error = conditionMessage)
expect_false(grepl("not class ergm", err, fixed = TRUE))
expect_false(grepl("condition has length", err, fixed = TRUE))

# and a real ergm object still passes, of course
expect_silent(invisible(snafun::stat_ef_int(m, "odds")))

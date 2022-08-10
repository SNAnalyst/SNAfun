data(florentine, package = "snafun")
fflom <- florentine$flomarriage
flom <- snafun::to_network(fflom)
m <- ergm::ergm(flom ~ edges + nodecov("Wealth"))

probtest <- snafun::stat_ef_int(m, "prob")



expect_equal(class(probtest), "data.frame")
expect_equal(probtest[,1], c(-2.595, 0.011))
expect_equal(probtest[,2], c(0.069, 0.503))
expect_equal(probtest[,3], c(0.536, 0.005))
expect_equal(probtest[,4], c(-4.841, 2.256))
expect_equal(rownames(probtest), c("edges", "nodecov.Wealth"))
expect_equal(colnames(probtest), c("Estimate", "Prob", "Std.Error", "Pval"))


################################################################################
oddstest <- snafun::stat_ef_int(m, "odds")


expect_equal(class(oddstest), "data.frame")
expect_equal(oddstest[,1], c(-2.595, 0.011))
expect_equal(oddstest[,2], c(0.075, 1.011))
expect_equal(oddstest[,3], c(0.536, 0.005))
expect_equal(oddstest[,4], c(-4.841, 2.256))
expect_equal(rownames(oddstest), c("edges", "nodecov.Wealth"))
expect_equal(colnames(oddstest), c("Estimate", "Odds", "Std.Error", "Pval"))

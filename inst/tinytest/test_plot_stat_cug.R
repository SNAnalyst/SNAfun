# Tests for plot.stat_cug().
#
# REGRESSION (2026-09-14): the observed-value reference line was drawn with
# abline(v = obs), but the histogram's x-axis only spanned the replicate range.
# In a CUG test the observed statistic usually lies far in the tail of the null
# distribution, so the line fell outside the plot region and was invisible
# (unlike sna::plot.cug.test()). plot.stat_cug() now widens xlim to include the
# observed value. These tests check that the observed statistic falls within the
# plotted x-range (so the line is visible) and that an explicit xlim is honoured.

report_side_effects(FALSE)

# helper: plot to an off-screen device and return the plotted x-range (usr[1:2])
plot_xrange <- function(obj, ...) {
  f <- tempfile(fileext = ".pdf")
  grDevices::pdf(f)
  on.exit({ grDevices::dev.off(); unlink(f) }, add = TRUE)
  graphics::plot(obj, ...)
  graphics::par("usr")[1:2]
}

set.seed(20260914)

# A clustered (high-transitivity) graph so the observed transitivity sits far in
# the upper tail of the edge-conditioned null distribution.
g <- igraph::simplify(igraph::sample_smallworld(1, 40, 4, 0.05))

res <- snafun::stat_cug(g, FUN = snafun::g_transitivity, mode = "graph",
                        cmode = "edges", reps = 100, graph = "igraph")

# The observed value must be well outside the replicate range (otherwise this
# test would not exercise the bug).
expect_true(res$obs.stat > max(res$rep.stat),
            info = "observed transitivity is in the tail (test is meaningful)")

# The plotted x-range must include the observed statistic, so the reference line
# is visible.
xr <- plot_xrange(res)
expect_true(res$obs.stat >= xr[1] && res$obs.stat <= xr[2],
            info = "observed statistic lies within the plotted x-range (line visible)")

# The replicates must also still be visible (range not collapsed).
expect_true(min(res$rep.stat) >= xr[1] && max(res$rep.stat) <= xr[2],
            info = "replicate range is also within the plotted x-range")

# An explicit xlim supplied by the caller must be respected (not overridden).
xr2 <- plot_xrange(res, xlim = c(0, 0.2))
expect_true(xr2[1] <= 0 + 1e-8 && xr2[2] >= 0.2 - 1e-8 && xr2[2] < 0.3,
            info = "explicit xlim is respected")

# plot() returns its input invisibly.
ret <- withVisible(plot_xrange)  # sanity: helper exists
f <- tempfile(fileext = ".pdf"); grDevices::pdf(f)
out <- plot(res)
grDevices::dev.off(); unlink(f)
expect_equal(out$obs.stat, res$obs.stat, info = "plot.stat_cug returns the object invisibly")

# A result whose observed value falls inside the replicate range still plots
# without error and includes the observed value.
res2 <- res
res2$obs.stat <- stats::median(res$rep.stat)
xr3 <- plot_xrange(res2)
expect_true(res2$obs.stat >= xr3[1] && res2$obs.stat <= xr3[2],
            info = "observed value inside the null range is also within the plot")

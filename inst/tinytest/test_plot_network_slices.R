report_side_effects()



data("McFarland_cls33_10_16_96", package = "networkDynamic")
classroom <- cls33_10_16_96



# The plotting code should leave the user's original dynamic edge attributes
# intact, even though they are ignored internally while constructing slices.
attrs_before <- network::list.edge.attributes(classroom)

expect_true("interaction_type.active" %in% attrs_before)
expect_true("weight.active" %in% attrs_before)



# This dataset contains edges with multiple active attribute values within a
# slice. plot_network_slices() should now ignore those attributes and therefore
# avoid the repeated networkDynamic warnings during collapse.
outfile <- tempfile(fileext = ".pdf")
grDevices::pdf(outfile)
on.exit(grDevices::dev.off(), add = TRUE)

uit <- snafun::withWarnings(
  snafun::plot_network_slices(classroom, number = 9, start = 0, end = 45, digits = 3)
)

warning_messages <- character()
if (!is.null(uit$warnings)) {
  warning_messages <- vapply(uit$warnings, conditionMessage, character(1))
}

expect_false(any(grepl("Multiple attribute values matched query spell",
                       warning_messages, fixed = TRUE)))
expect_true(file.exists(outfile))

attrs_after <- network::list.edge.attributes(classroom)
expect_equal(sort(attrs_after), sort(attrs_before))

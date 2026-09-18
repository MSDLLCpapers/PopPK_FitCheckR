### Reproduces the full analysis path a user follows, from uploaded file to plots ###

test_that("a NONMEM table runs end to end from file to diagnostic plots", {
  path <- write_nonmem_table(make_gof_data(), tempfile())
  on.exit(unlink(path), add = TRUE)

  data    <- read_uploaded_table(path, "sdtab001")
  mapping <- gof_mapping()

  expect_s3_class(draw_quietly(gof_observed_plots(data, mapping)), "gtable")
  expect_s3_class(draw_quietly(gof_cwres_plots(data, mapping)), "gtable")

  for (gof_type in gof_types()) {
    p <- gof_interactive_plot(data, mapping, gof_type,
                              notify = function(message, type, duration = 5) invisible(NULL))
    expect_s3_class(p, "ggplot")
    built <- suppressMessages(suppressWarnings(ggplot2::ggplot_build(p)))
    expect_gt(nrow(built$data[[1]]), 0)
  }
})

test_that("log scaling drops non-positive observations and warns the user", {
  reference <- make_gof_data()
  # NONMEM tables routinely carry DV = 0 rows, which cannot be shown on a log axis
  reference$DV[c(1, 5, 9)] <- 0

  path <- write_nonmem_table(reference, tempfile())
  on.exit(unlink(path), add = TRUE)

  data     <- read_uploaded_table(path, "sdtab001")
  recorder <- notification_recorder()

  p <- gof_interactive_plot(data, gof_mapping(), "Observed vs. Predicted",
                            axis_scale = "Log", notify = recorder$notify)

  expect_s3_class(p, "ggplot")
  expect_true(all(p$data$DV > 0))
  expect_equal(nrow(p$data), sum(data$DV > 0 & data$PRED > 0))
  expect_equal(recorder$records()[[1]]$type, "warning")
})

test_that("correlation plots run on an uploaded parameter table", {
  path <- write_nonmem_table(make_param_data(), tempfile())
  on.exit(unlink(path), add = TRUE)

  data <- read_uploaded_table(path, "patab001")

  expect_s3_class(
    draw_quietly(correlation_plots(data, c("CL", "V1"), c("ETA1", "ETA2"),
                                   "Line plot", "lm", FALSE)),
    "gtable"
  )
})

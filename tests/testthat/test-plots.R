test_that("facet_ncol grows with the panel count at the documented breakpoints", {
  expect_equal(facet_ncol(1), 2)
  expect_equal(facet_ncol(4), 2)
  expect_equal(facet_ncol(5), 3)
  expect_equal(facet_ncol(9), 3)
  expect_equal(facet_ncol(10), 4)
  expect_equal(facet_ncol(16), 4)
  expect_equal(facet_ncol(17), 5)
  expect_equal(facet_ncol(200), 5)
})

test_that("bin_continuous_facets bins continuous columns and leaves others alone", {
  data     <- make_gof_data()
  var_type <- function(column) if (column == "WT") "Continuous" else "Categorical"

  binned <- bin_continuous_facets(data, c("WT", "SEX"), data, var_type)

  expect_s3_class(binned$WT, "factor")
  expect_lte(nlevels(binned$WT), 5)
  expect_false(anyNA(binned$WT))
  expect_identical(binned$SEX, data$SEX)
  expect_identical(nrow(binned), nrow(data))
})

test_that("bin_continuous_facets never creates more bins than unique values", {
  data     <- make_gof_data()
  data$DOSE <- rep(c(10, 20), length.out = nrow(data))

  binned <- bin_continuous_facets(data, "DOSE", data, function(column) "Continuous")

  expect_equal(nlevels(binned$DOSE), 2)
})

test_that("bin_continuous_facets ignores stratification columns absent from the data", {
  data <- make_gof_data()

  expect_identical(
    bin_continuous_facets(data, "NOT_A_COLUMN", data, function(column) "Continuous"),
    data
  )
})

test_that("add_gof_points colours points according to the variable type", {
  data <- make_gof_data()
  base <- ggplot2::ggplot(data, ggplot2::aes(x = PRED, y = DV))

  plain <- add_gof_points(base, NULL, function(column) "Categorical")
  expect_length(plain$layers, 1)
  expect_null(plain$layers[[1]]$mapping$colour)

  categorical <- add_gof_points(base, "SEX", function(column) "Categorical")
  expect_true("colour" %in% names(categorical$layers[[1]]$mapping))
  expect_equal(categorical$labels$colour, "SEX")

  continuous <- add_gof_points(base, "WT", function(column) "Continuous")
  expect_true("colour" %in% names(continuous$layers[[1]]$mapping))
})

test_that("static GOF panels are produced for both observed and CWRES pairs", {
  data <- make_gof_data()

  observed <- draw_quietly(gof_observed_plots(data, gof_mapping()))
  cwres    <- draw_quietly(gof_cwres_plots(data, gof_mapping()))

  expect_s3_class(observed, "gtable")
  expect_s3_class(cwres, "gtable")
})

test_that("every interactive GOF type builds without error", {
  data <- make_gof_data()

  for (gof_type in gof_types()) {
    p <- gof_interactive_plot(data, gof_mapping(), gof_type)
    expect_s3_class(p, "ggplot")
    built <- suppressMessages(suppressWarnings(ggplot2::ggplot_build(p)))
    expect_equal(nrow(built$data[[1]]), nrow(data))
  }
})

test_that("interactive GOF plots map the requested columns to the axes", {
  data <- make_gof_data()

  expectations <- list(
    "Observed vs. Predicted"            = c("PRED", "DV"),
    "Observed vs. Individual Predicted" = c("IPRED", "DV"),
    "CWRES vs. Predicted"               = c("PRED", "CWRES"),
    "CWRES vs. Time"                    = c("TIME", "CWRES")
  )

  for (gof_type in names(expectations)) {
    p      <- gof_interactive_plot(data, gof_mapping(), gof_type)
    points <- suppressMessages(suppressWarnings(ggplot2::ggplot_build(p)))$data[[1]]
    expect_equal(points$x, data[[expectations[[gof_type]][1]]], tolerance = 1e-8)
    expect_equal(points$y, data[[expectations[[gof_type]][2]]], tolerance = 1e-8)
  }
})

test_that("default and custom axis and plot titles are applied", {
  data <- make_gof_data()

  default <- gof_interactive_plot(data, gof_mapping(), "CWRES vs. Time")
  expect_equal(default$labels$x, "Time")
  expect_equal(default$labels$y, "CWRES")
  expect_equal(default$labels$title, "CWRES vs. Time")

  custom <- gof_interactive_plot(data, gof_mapping(), "CWRES vs. Time",
                                 x_axis_title = "Time after dose (h)",
                                 y_axis_title = "Conditional weighted residuals",
                                 plot_title   = "Model 309")
  expect_equal(custom$labels$x, "Time after dose (h)")
  expect_equal(custom$labels$y, "Conditional weighted residuals")
  expect_equal(custom$labels$title, "Model 309")
})

test_that("user supplied axis limits are honoured on the linear scale", {
  data <- make_gof_data()

  p <- gof_interactive_plot(data, gof_mapping(), "CWRES vs. Time",
                            x_start = 2, x_end = 6, y_start = -3, y_end = 3)

  expect_equal(p$coordinates$limits$x, c(2, 6))
  expect_equal(p$coordinates$limits$y, c(-3, 3))
})

test_that("stratification adds one panel per level of the grouping variable", {
  data <- make_gof_data()

  p <- gof_interactive_plot(data, gof_mapping(), "CWRES vs. Time",
                            stratify = "SEX",
                            var_type = function(column) "Categorical")

  expect_s3_class(p$facet, "FacetWrap")
  expect_equal(nrow(suppressMessages(suppressWarnings(ggplot2::ggplot_build(p)))$layout$layout),
               length(unique(data$SEX)))
})

test_that("stratifying on a continuous variable bins it before faceting", {
  data <- make_gof_data()

  p      <- gof_interactive_plot(data, gof_mapping(), "CWRES vs. Time",
                                 stratify = "WT",
                                 var_type = function(column) "Continuous")
  layout <- suppressMessages(suppressWarnings(ggplot2::ggplot_build(p)))$layout$layout

  expect_lte(nrow(layout), 5)
})

test_that("log scaling excludes non-positive values and reports how many", {
  data <- make_gof_data()
  data$DV[1:10] <- 0
  recorder <- notification_recorder()

  p <- gof_interactive_plot(data, gof_mapping(), "Observed vs. Predicted",
                            axis_scale = "Log", notify = recorder$notify)

  expect_s3_class(p, "ggplot")
  expect_equal(nrow(p$data), nrow(data) - 10)
  expect_length(recorder$records(), 1)
  expect_equal(recorder$records()[[1]]$type, "warning")
  expect_match(recorder$records()[[1]]$message, "10 of 96 observations")
  # The exclusion must also travel with the downloaded figure
  expect_match(p$labels$subtitle, "10 of 96 observations excluded")
})

test_that("log scaling on the CWRES plots only filters the x axis", {
  data <- make_gof_data()
  data$TIME[data$TIME == 1] <- 0
  recorder <- notification_recorder()

  p <- gof_interactive_plot(data, gof_mapping(), "CWRES vs. Time",
                            x_axis_scale = "Log", notify = recorder$notify)

  expect_equal(nrow(p$data), sum(data$TIME > 0))
  expect_true(any(data$CWRES < 0))
  expect_match(recorder$records()[[1]]$message, "X-axis values")
})

test_that("a log request with no positive data returns NULL and an error notification", {
  data     <- make_gof_data()
  data$DV  <- 0
  recorder <- notification_recorder()

  p <- gof_interactive_plot(data, gof_mapping(), "Observed vs. Predicted",
                            axis_scale = "Log", notify = recorder$notify)

  expect_null(p)
  types <- vapply(recorder$records(), function(record) record$type, character(1))
  expect_true("error" %in% types)
})

test_that("a plot without a log request emits no notification", {
  recorder <- notification_recorder()

  gof_interactive_plot(make_gof_data(), gof_mapping(), "Observed vs. Predicted",
                       notify = recorder$notify)

  expect_length(recorder$records(), 0)
})

test_that("interactive GOF plots can be written to PNG", {
  skip_if_not(capabilities("png"))
  p    <- gof_interactive_plot(make_gof_data(), gof_mapping(), "Observed vs. Predicted")
  file <- tempfile(fileext = ".png")
  on.exit(unlink(file), add = TRUE)

  suppressWarnings(suppressMessages(
    ggplot2::ggsave(file, plot = p, width = 8, height = 8, dpi = 150)
  ))

  expect_true(file.exists(file))
  expect_gt(file.info(file)$size, 0)
})

test_that("correlation plots are produced for every requested pair", {
  data <- make_gof_data()

  scatter <- draw_quietly(correlation_plots(data, c("WT", "TIME"), c("CWRES"),
                                            "Line plot", "lm", FALSE))
  expect_s3_class(scatter, "gtable")

  loess <- draw_quietly(correlation_plots(data, "WT", "CWRES",
                                          "Line plot", "loess", TRUE))
  expect_s3_class(loess, "gtable")

  boxes <- draw_quietly(correlation_plots(data, "SEX", c("CWRES", "WT"),
                                          "Box plot", "lm", FALSE))
  expect_s3_class(boxes, "gtable")
})

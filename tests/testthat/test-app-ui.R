test_that("app_ui builds and exposes every input the server reads", {
  ui   <- app_ui()
  html <- paste(as.character(ui), collapse = "")

  expect_s3_class(ui, "shiny.tag.list")

  input_ids <- c("table_file", "select_column", "select_stratify", "select_color",
                 "gof_type", "x_start", "x_end", "y_start", "y_end",
                 "x_axis_title", "y_axis_title", "plot_title",
                 "cov_file", "select_cov_x", "select_cov_y",
                 "plot_type", "regression_type", "display_ci", "dedup_by_id")

  for (id in input_ids) {
    expect_true(grepl(id, html, fixed = TRUE), info = paste("missing input:", id))
  }
})

test_that("app_ui offers the four documented GOF plot types", {
  html <- paste(as.character(app_ui()), collapse = "")

  for (type in gof_types()) {
    expect_true(grepl(type, html, fixed = TRUE), info = paste("missing type:", type))
  }
})

test_that("app_ui exposes the download controls", {
  html <- paste(as.character(app_ui()), collapse = "")

  for (id in c("download_gofs", "download_gof", "download_cov")) {
    expect_true(grepl(id, html, fixed = TRUE), info = paste("missing download:", id))
  }
})

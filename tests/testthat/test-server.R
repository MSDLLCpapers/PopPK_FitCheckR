### Reactive behaviour of app_server(), exercised through shiny::testServer() ###

# Small synthetic uploads keep these tests fast: testServer renders every output
# on each flush, so a full-size NONMEM table would rebuild every plot repeatedly.
sdtab_fixture <- local({
  data      <- make_gof_data(n_id = 6, n_time = 5)
  data$EVID <- rep(c(0, 0, 0, 0, 1), length.out = nrow(data))
  write_nonmem_table(data, file.path(tempdir(), "sdtab_fixture"))
})

patab_fixture <- local({
  set.seed(99)
  n_id <- 8
  data <- data.frame(
    ID   = rep(seq_len(n_id), each = 3),
    CL   = rep(stats::runif(n_id, 1, 5), each = 3),
    V1   = rep(stats::runif(n_id, 5, 15), each = 3),
    ETA1 = rep(stats::rnorm(n_id), each = 3),
    ETA2 = rep(stats::rnorm(n_id), each = 3),
    ETA3 = rep(stats::rnorm(n_id), each = 3)
  )
  write_nonmem_table(data, file.path(tempdir(), "patab_fixture"))
})

upload <- function(path, name) {
  data.frame(name = name, datapath = path, stringsAsFactors = FALSE)
}

test_that("uploading a NONMEM table populates the reactive state", {
  shiny::testServer(app_server, {
    session$setInputs(table_file = upload(sdtab_fixture, "sdtab001"))

    expect_s3_class(table_unfiltered(), "data.frame")
    expect_equal(nrow(table_unfiltered()), 30)
    expect_equal(current_file_name(), "sdtab001")
    # With no filter columns selected the filtered table is the full table
    expect_equal(nrow(table()), nrow(table_unfiltered()))
  })
})

test_that("column mapping is automatic when the file already uses NONMEM names", {
  shiny::testServer(app_server, {
    session$setInputs(table_file = upload(sdtab_fixture, "sdtab001"))

    mapping <- column_mapping()
    expect_named(mapping, c("PRED", "IPRED", "DV", "CWRES", "TIME", "ID"))
    expect_equal(unlist(mapping),
                 c(PRED = "PRED", IPRED = "IPRED", DV = "DV",
                   CWRES = "CWRES", TIME = "TIME", ID = "ID"))
  })
})

test_that("continuous filters subset the table by the requested range", {
  shiny::testServer(app_server, {
    session$setInputs(table_file = upload(sdtab_fixture, "sdtab001"))
    full <- table_unfiltered()

    session$setInputs(select_column    = "ID",
                      variable_type_ID = "Continuous",
                      filter_start_ID  = "2",
                      filter_end_ID    = "4")

    expect_equal(nrow(table()), sum(full$ID >= 2 & full$ID <= 4))
    expect_true(all(table()$ID >= 2 & table()$ID <= 4))
  })
})

test_that("categorical filters keep only the selected values", {
  shiny::testServer(app_server, {
    session$setInputs(table_file = upload(sdtab_fixture, "sdtab001"))
    full <- table_unfiltered()

    session$setInputs(select_column      = "EVID",
                      variable_type_EVID = "Categorical",
                      filter_select_EVID = "0")

    expect_true(all(table()$EVID == 0))
    expect_equal(nrow(table()), sum(full$EVID == 0))
  })
})

test_that("an emptied categorical filter is treated as no filter", {
  shiny::testServer(app_server, {
    session$setInputs(table_file = upload(sdtab_fixture, "sdtab001"))

    session$setInputs(select_column      = "EVID",
                      variable_type_EVID = "Categorical",
                      filter_select_EVID = character(0))

    expect_equal(nrow(table()), nrow(table_unfiltered()))
  })
})

test_that("an incomplete continuous range leaves the data untouched", {
  shiny::testServer(app_server, {
    session$setInputs(table_file = upload(sdtab_fixture, "sdtab001"))

    session$setInputs(select_column    = "ID",
                      variable_type_ID = "Continuous",
                      filter_start_ID  = "2",
                      filter_end_ID    = "")

    expect_equal(nrow(table()), nrow(table_unfiltered()))
  })
})

test_that("the interactive GOF plot reacts to the selected plot type", {
  shiny::testServer(app_server, {
    session$setInputs(table_file = upload(sdtab_fixture, "sdtab001"))
    session$setInputs(x_start = "", x_end = "", y_start = "", y_end = "",
                      x_axis_title = "", y_axis_title = "", plot_title = "")

    for (type in c("Observed vs. Predicted", "Observed vs. Individual Predicted",
                   "CWRES vs. Predicted", "CWRES vs. Time")) {
      session$setInputs(gof_type = type)
      expect_s3_class(gof_plot(), "ggplot")
      expect_equal(gof_plot()$labels$title, type)
    }
  })
})

test_that("plot height grows with the number of facets", {
  shiny::testServer(app_server, {
    session$setInputs(table_file = upload(sdtab_fixture, "sdtab001"))

    expect_equal(facet_plot_height(), 500)

    session$setInputs(select_stratify  = "ID",
                      variable_type_ID = "Categorical")
    expect_gt(facet_plot_height(), 500)
  })
})

test_that("the correlation tab can deduplicate a parameter table by subject", {
  shiny::testServer(app_server, {
    session$setInputs(cov_file = upload(patab_fixture, "patab001"))

    session$setInputs(dedup_by_id = TRUE)
    expect_equal(nrow(cov_data()), length(unique(cov_unfiltered()$ID)))

    session$setInputs(dedup_by_id = FALSE)
    expect_equal(nrow(cov_data()), nrow(cov_unfiltered()))
  })
})

test_that("the correlation tab renders the selected variable pairs", {
  shiny::testServer(app_server, {
    session$setInputs(cov_file        = upload(patab_fixture, "patab001"),
                      dedup_by_id     = TRUE,
                      select_cov_x    = c("CL", "V1"),
                      select_cov_y    = "ETA1",
                      plot_type       = "Line plot",
                      regression_type = "lm",
                      display_ci      = FALSE)

    grDevices::pdf(NULL)
    plot <- cov_plot()
    grDevices::dev.off()

    expect_s3_class(plot, "gtable")
    expect_equal(cov_plot_height(), 400)

    session$setInputs(select_cov_y = c("ETA1", "ETA2", "ETA3"))
    expect_gt(cov_plot_height(), 400)
  })
})

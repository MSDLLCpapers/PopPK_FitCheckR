test_that("auto_classify requires a numeric column", {
  expect_equal(auto_classify(seq_len(100)), "Continuous")
  expect_equal(auto_classify(as.character(seq_len(100))), "Categorical")
  expect_equal(auto_classify(factor(seq_len(100))), "Categorical")
  expect_equal(auto_classify(letters), "Categorical")
})

test_that("auto_classify applies the 10% uniqueness rule with a floor of 10", {
  # 100 rows, 11 unique values: 11 > max(10, 10) -> Continuous
  expect_equal(auto_classify(c(seq_len(11), rep(1, 89))), "Continuous")
  # 100 rows, 10 unique values: 10 is not > 10 -> Categorical
  expect_equal(auto_classify(c(seq_len(10), rep(1, 90))), "Categorical")
  # 1000 rows, 50 unique values: the floor is not binding, 50 < 100 -> Categorical
  expect_equal(auto_classify(rep(seq_len(50), each = 20)), "Categorical")
  # Flags such as EVID/MDV/SEX must never be treated as continuous
  expect_equal(auto_classify(rep(c(0, 1), 50)), "Categorical")
})

test_that("auto_classify treats short numeric columns as categorical", {
  expect_equal(auto_classify(c(1, 2, 3)), "Categorical")
  expect_equal(auto_classify(numeric(0)), "Categorical")
})

test_that("get_variable_type prefers the user override over the automatic guess", {
  weight <- seq_len(100)

  expect_equal(get_variable_type("WT", weight, list()), "Continuous")
  expect_equal(get_variable_type("WT", weight, list(variable_type_WT = "Categorical")),
               "Categorical")
})

test_that("variable_type_lookup resolves any column of the supplied data", {
  data   <- make_gof_data()
  lookup <- variable_type_lookup(data, list(variable_type_SEX = "Categorical"))

  expect_equal(lookup("WT"), "Continuous")
  expect_equal(lookup("SEX"), "Categorical")
  expect_equal(lookup("ID"), auto_classify(data$ID))
})

test_that("header_skip detects the NONMEM TABLE NO. banner", {
  expect_equal(header_skip("TABLE NO.  1"), 1)
  expect_equal(header_skip("  table no.  4"), 1)
  expect_equal(header_skip("TABLE NO. 12: something else"), 1)
  expect_equal(header_skip("ID TIME DV"), 0)
  expect_equal(header_skip("ID,TIME,DV"), 0)
  expect_equal(header_skip(character(0)), 0)
})

test_that("read_uploaded_table skips the banner of a standard NONMEM table", {
  reference <- make_gof_data()
  sdtab     <- write_nonmem_table(reference, tempfile())
  on.exit(unlink(sdtab), add = TRUE)

  data <- read_uploaded_table(sdtab, "sdtab001")

  expect_s3_class(data, "data.frame")
  expect_equal(nrow(data), nrow(reference))
  expect_true(all(c("ID", "TIME", "DV", "PRED", "IPRED", "CWRES") %in% names(data)))
  expect_true(all(vapply(data[c("ID", "TIME", "DV")], is.numeric, logical(1))))
})

test_that("read_uploaded_table handles NOTITLE tables and CSV uploads", {
  reference <- make_gof_data(n_id = 3, n_time = 4)

  notitle <- write_nonmem_table(reference, tempfile(), title = NULL)
  on.exit(unlink(notitle), add = TRUE)
  expect_equal(nrow(read_uploaded_table(notitle, "sdtab001")), nrow(reference))
  expect_equal(names(read_uploaded_table(notitle, "sdtab001")), names(reference))

  csv <- tempfile(fileext = ".csv")
  on.exit(unlink(csv), add = TRUE)
  utils::write.csv(reference, csv, row.names = FALSE)
  # The extension, not the content, selects the comma separator
  expect_equal(names(read_uploaded_table(csv, "cov.csv")), names(reference))
  expect_equal(nrow(read_uploaded_table(csv, "cov.csv")), nrow(reference))
})

test_that("read_uploaded_table is insensitive to the case of the CSV extension", {
  reference <- make_gof_data(n_id = 2, n_time = 3)
  csv       <- tempfile(fileext = ".CSV")
  on.exit(unlink(csv), add = TRUE)
  utils::write.csv(reference, csv, row.names = FALSE)

  expect_equal(names(read_uploaded_table(csv, "COV.CSV")), names(reference))
})

test_that("binary uploads are rejected with a readable message", {
  binary <- tempfile()
  on.exit(unlink(binary), add = TRUE)
  writeBin(as.raw(c(0x50, 0x4b, 0x03, 0x04, 0x00, 0x01, 0x00, 0x02)), binary)

  expect_error(read_table_file(binary, "", 0), "not plain text")
})

test_that("files without a usable table are rejected", {
  empty <- tempfile()
  writeLines(character(0), empty)
  on.exit(unlink(empty), add = TRUE)
  expect_error(read_uploaded_table(empty, "empty.txt"), "No readable data table")

  single_column <- tempfile()
  writeLines(c("VALUE", "1", "2"), single_column)
  on.exit(unlink(single_column), add = TRUE)
  expect_error(read_uploaded_table(single_column, "one.txt"), "No readable data table")

  header_only <- tempfile()
  writeLines("ID TIME DV", header_only)
  on.exit(unlink(header_only), add = TRUE)
  expect_error(read_uploaded_table(header_only, "header.txt"), "No readable data table")
})

test_that("get_timestamp returns a sortable 12-digit stamp", {
  stamp <- get_timestamp()

  expect_type(stamp, "character")
  expect_match(stamp, "^[0-9]{12}$")
  expect_false(is.na(as.POSIXct(stamp, format = "%Y%m%d%H%M")))
})

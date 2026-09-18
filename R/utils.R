### Predefined helper functions shared by the UI and server ###

get_timestamp <- function() {
  format(Sys.time(), "%Y%m%d%H%M")
}

auto_classify <- function(column_data) {
  n_unique <- length(unique(column_data))
  n_total  <- length(column_data)
  # Continuous only if column is numeric AND unique values exceed 10% of rows (floor of 10)
  if (is.numeric(column_data) && n_unique > max(10, ceiling(n_total * 0.10))) {
    "Continuous"
  } else {
    "Categorical"
  }
}

get_variable_type <- function(column_name, column_data, input) {
  input_id <- paste0("variable_type_", column_name)
  variable_type <- input[[input_id]]
  if (is.null(variable_type)) {
    variable_type <- auto_classify(column_data)
  }
  variable_type
}

# Resolves the variable type of any column of `data` for the current session
variable_type_lookup <- function(data, input) {
  function(column) get_variable_type(column, data[[column]], input)
}

# NONMEM writes a "TABLE NO." banner above the header unless NOTITLE is used
header_skip <- function(first_line) {
  if (isTRUE(grepl("^\\s*TABLE NO", first_line, ignore.case = TRUE))) 1 else 0
}

# Extensionless NONMEM tables rule out validating uploads by file extension
read_table_file <- function(path, sep, skip) {
  if (any(readBin(path, "raw", n = 4096) == as.raw(0)))
    stop("This file is not plain text. Please upload a plain-text data table.")

  no_table_message <- "No readable data table was found in this file. Please upload a plain-text data table."

  data <- tryCatch(
    utils::read.table(path, header = TRUE, sep = sep, skip = skip),
    error = function(e) stop(no_table_message)
  )

  if (ncol(data) < 2 || nrow(data) < 1) stop(no_table_message)

  data
}

read_uploaded_table <- function(path, file_name) {
  # Empty separator is read.table's default for whitespace-delimited NONMEM tables
  sep  <- if (tolower(tools::file_ext(file_name)) == "csv") "," else ""
  skip <- header_skip(readLines(path, n = 1))
  read_table_file(path, sep, skip)
}

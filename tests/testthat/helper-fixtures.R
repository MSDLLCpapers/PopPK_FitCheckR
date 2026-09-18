### Shared fixtures for the test suite ###

# Deterministic stand-in for a NONMEM sdtab
make_gof_data <- function(n_id = 12, n_time = 8, seed = 1234) {
  set.seed(seed)
  grid <- expand.grid(TIME = seq_len(n_time), ID = seq_len(n_id))
  n    <- nrow(grid)
  pred <- 10 * exp(-0.15 * grid$TIME)

  data.frame(
    ID    = grid$ID,
    TIME  = grid$TIME,
    DV    = pred * exp(stats::rnorm(n, 0, 0.10)),
    PRED  = pred,
    IPRED = pred * exp(stats::rnorm(n, 0, 0.05)),
    CWRES = stats::rnorm(n),
    SEX   = rep(c(0, 1), length.out = n),
    WT    = round(stats::runif(n, 50, 100), 1),
    stringsAsFactors = FALSE
  )
}

gof_mapping <- function() {
  list(ID = "ID", TIME = "TIME", DV = "DV",
       PRED = "PRED", IPRED = "IPRED", CWRES = "CWRES")
}

# Stand-in for a NONMEM patab/cotab: one row per subject, parameters and ETAs
make_param_data <- function(n_id = 40, seed = 99) {
  set.seed(seed)
  eta1 <- stats::rnorm(n_id, 0, 0.3)
  eta2 <- stats::rnorm(n_id, 0, 0.3)

  data.frame(
    ID   = seq_len(n_id),
    CL   = 5 * exp(eta1),
    V1   = 30 * exp(eta2),
    ETA1 = eta1,
    ETA2 = eta2,
    SEX  = rep(c(0, 1), length.out = n_id),
    WT   = round(stats::runif(n_id, 50, 100), 1),
    stringsAsFactors = FALSE
  )
}

gof_types <- function() {
  c("Observed vs. Predicted", "Observed vs. Individual Predicted",
    "CWRES vs. Predicted", "CWRES vs. Time")
}

# grid.arrange() draws as a side effect, so tests need an open device
draw_quietly <- function(expr) {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  force(expr)
}

# Collects the notifications a plot builder emits instead of sending them to Shiny
notification_recorder <- function() {
  records <- list()
  list(
    notify = function(message, type, duration = 5) {
      records[[length(records) + 1]] <<- list(message = message, type = type)
      invisible(NULL)
    },
    records = function() records
  )
}

write_nonmem_table <- function(data, path, title = "TABLE NO.  1") {
  con <- file(path, open = "wt")
  on.exit(close(con), add = TRUE)
  if (!is.null(title)) writeLines(title, con)
  utils::write.table(data, con, row.names = FALSE, quote = FALSE)
  path
}

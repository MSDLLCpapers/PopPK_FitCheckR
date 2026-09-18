#' Launch the PopPK FitCheckR application
#'
#' @param launch.browser Whether to launch the app in a web browser.
#' @param ... Additional arguments passed to [shiny::runApp()].
#'
#' @return The Shiny app object, invisibly.
#' @export
run_app <- function(launch.browser = TRUE, ...) {
  Sys.setenv(TZ = "America/New_York")
  shiny::runApp(shiny::shinyApp(app_ui(), app_server), launch.browser = launch.browser, ...)
}
